(in-package :cl-postgres)

;; For more information about the PostgreSQL scocket protocol, see
;; http://www.postgresql.org/docs/current/interactive/protocol.html

(defparameter *current-query* nil)

(define-condition database-error (error)
  ((error-code :initarg :code :initform nil :accessor database-error-code
               :documentation "The error code PostgreSQL associated
with this error, if any.")
   (message :initarg :message :accessor database-error-message
            :documentation "Description of this error.")
   (detail :initarg :detail :initform nil :accessor database-error-detail
           :documentation "More elaborate description of this error,
if any.")
   (query :initform *current-query* :accessor database-error-query
          :documentation "Query that led to the error, if any.")
   (position :initarg :position :initform nil :accessor database-error-position))
  (:report (lambda (err stream)
             (format stream "Database error~@[ ~A~]: ~A~@[~%~A~]~@[~%Query: ~A~]"
                     (database-error-code err)
                     (database-error-message err)
                     (database-error-detail err)
                     (database-error-query err))))
  (:documentation "This is the condition type that will be used to
signal virtually all database-related errors \(though in some cases
socket errors may be raised when a connection fails on the IP
level)."))

(define-condition protocol-error (error)
  ((message :initarg :message))
  (:report (lambda (err stream)
             (format stream "Postgresql protocol error: ~A" 
                     (slot-value err 'message))))
  (:documentation "This is raised if something really unexpected
happens in the communcation with the server. Should only happen in
case of a bug or a connection to something that is not a \(supported)
PostgreSQL server at all."))

(defmacro message-case (socket &body clauses)
  "Helper macro for reading messages from the server. A list of cases
\(characters that identify the message) can be given, each with a body
that handles the message, or the keyword :skip to skip the message.
Cases for error and warning messages are always added."
  (let ((socket-name (gensym))
        (size-name (gensym))
        (char-name (gensym))
        (iter-name (gensym))
        (t-found nil))
    (flet ((expand-characters (chars)
             (cond ((eq chars t) (setf t-found t) t)
                   ((consp chars) (mapcar #'char-code chars))
                   (t (char-code chars)))))
      `(let* ((,socket-name ,socket))
        (declare (type stream ,socket-name))
        (labels ((,iter-name ()
                   (let ((,char-name (read-uint1 ,socket-name))
                         (,size-name (read-uint4 ,socket-name)))
                     (declare (type (unsigned-byte 8) ,char-name)
                              (type (unsigned-byte 32) ,size-name)
                              (ignorable ,size-name))
                     (case ,char-name
                       (#.(char-code #\E) (get-error ,socket-name))
                       ;; Continue reading messages when the current one is a warning.
                       (#.(char-code #\N) (get-warning ,socket-name) (,iter-name))
                       ,@(mapcar (lambda (clause)
                                   `(,(expand-characters (first clause))
                                     ,@(if (eq (second clause) :skip)
                                           `((skip-bytes ,socket-name (- ,size-name 4)))
                                           (cdr clause))))
                                 clauses)
                       ,@(unless t-found
                                 `((t (ensure-socket-is-closed ,socket-name)
                                      (error 'protocol-error
                                             :message (format nil "Unexpected message received: ~A"
                                                              (code-char ,char-name))))))))))
          (,iter-name))))))

(defun read-byte-delimited (socket)
  "Read the fields of a null-terminated list of byte + string values
and put them in an alist."
  (loop :for type = (read-uint1 socket)
        :until (zerop type)
        :collect (cons (code-char type) (read-str socket))))

(defun get-error (socket)
  "Read an error message from the socket and raise the corresponding
database-error condition."
  (let ((data (read-byte-delimited socket)))
    (flet ((get-field (char)
             (cdr (assoc char data))))
      (let ((code (get-field #\C)))
        ;; These are the errors "ADMIN SHUTDOWN" and "CRASH SHUTDOWN",
        ;; in which case the server will close the connection right
        ;; away.
        (when (or (string= code "57P01") (string= code "57P02"))
          (ensure-socket-is-closed socket))
        (error 'database-error :code code :message (get-field #\M)
               :detail (or (get-field #\D) (get-field #\H))
               :position (get-field #\p))))))

(define-condition postgresql-warning (simple-warning)
  ())

(defun get-warning (socket)
  "Read a warning from the socket and emit it."
  (let ((data (read-byte-delimited socket)))
    (flet ((get-field (char)
             (cdr (assoc char data))))
      (warn 'postgresql-warning
            :format-control "Postgres warning: ~A~@[~%~A~]"
            :format-arguments (list (get-field #\M) (or (get-field #\D) (get-field #\H)))))))

(defun authenticate (socket user password database)
  "Try to initiate a connection. Caller should close the socket if
this raises a condition."
  (let ((timestamp-format :float))
    (startup-message socket user database)
    (force-output socket)
    (loop
     (message-case socket
       ;; Authentication message
       (#\R (let ((type (read-uint4 socket)))
              (ecase type
                (0 (return))
                (2 (error 'database-error :message "Unsupported Kerberos authentication requested."))
                (3 (plain-password-message socket password)
                   (force-output socket))
                (4 (error 'database-error :message "Unsupported crypt authentication requested."))
                (5 (md5-password-message socket password user (read-bytes socket 4))
                   (force-output socket))
                (6 (error 'database-error :message "Unsupported SCM authentication requested.")))))))
    (loop
     (message-case socket
       ;; BackendKeyData - ignore
       (#\K :skip)
       ;; ParameterStatus
       (#\S (let ((name (read-str socket))
                  (value (read-str socket)))
              (when (string= name "integer_datetimes")
                (setf timestamp-format (if (string= value "on") :integer :float)))))
       ;; ReadyForQuery
       (#\Z (read-uint1 socket)
            (return))))
    timestamp-format))

(defclass field-description ()
  ((name :initarg :name :accessor field-name)
   (type-id :initarg :type-id :accessor field-type)
   (interpreter :initarg :interpreter :accessor field-interpreter)
   (receive-binary-p :initarg :receive-binary-p :accessor field-binary-p))
  (:documentation "Description of a field in a query result."))

(defun read-field-descriptions (socket)
  "Read the field descriptions for a query result and put them into an
array of field-description objects."
  (declare (type stream socket)
           #.*optimize*)
  (let* ((number (read-uint2 socket))
         (descriptions (make-array number)))
    (declare (type fixnum number)
             (type (simple-array field-description) descriptions))
    (dotimes (i number)
      (let* ((name (read-str socket))
             (table-oid (read-uint4 socket))
             (column (read-uint2 socket))
             (type-id (read-uint4 socket))
             (size (read-uint2 socket))
             (type-modifier (read-uint4 socket))
             (format (read-uint2 socket))
             (interpreter (type-interpreter type-id)))
        (declare (ignore table-oid column size type-modifier format)
                 (type string name)
                 (type (unsigned-byte 32) type-id))
        (setf (elt descriptions i)
              (make-instance 'field-description :name name :type-id type-id
                             :interpreter (or interpreter #'interpret-unknown)
                             :receive-binary-p (not (not interpreter))))))
    descriptions))

(defun terminate-connection (socket)
  "Close a connection, notifying the server."
  (terminate-message socket)
  (close socket))

;; This is a hacky way to communicate the amount of effected rows up
;; from look-for-row to the send-execute or send-query that (directly
;; or indirectly) called it.
(defparameter *effected-rows* nil)

(defun look-for-row (socket)
  "Read server messages until either a new row can be read, or there
are no more results. Return a boolean indicating whether any more
results are available, and, if available, stores the amount of
effected rows in *effected-rows*. Also handle getting out of
copy-in/copy-out states \(which are not supported)."
  (declare (type stream socket)
           #.*optimize*)
  (loop
   (message-case socket
     ;; CommandComplete
     (#\C (let* ((command-tag (read-str socket))
                 (space (position #\Space command-tag :from-end t)))
            (when space
              (setf *effected-rows* (parse-integer command-tag :junk-allowed t
                                                   :start (1+ space))))
            (return-from look-for-row nil)))
     ;; CopyInResponse
     (#\G (read-uint1 socket)
          (skip-bytes socket (* 2 (read-uint2 socket))) ;; The field formats
          (copy-done-message socket)
          (error 'database-error :message "Copy-in not supported."))
     ;; CopyOutResponse
     (#\H (read-uint1 socket)
          (skip-bytes socket (* 2 (read-uint2 socket))) ;; The field formats
          (error 'database-error :message "Copy-out not supported."))
      ;; DataRow
     (#\D (skip-bytes socket 2)
          (return-from look-for-row t))
     ;; EmptyQueryResponse
     (#\I (warn "Empty query sent.")
          (return-from look-for-row nil)))))

(defun try-to-sync (socket sync-sent)
  "Try to re-synchronize a connection by sending a sync message if it
hasn't already been sent, and then looking for a ReadyForQuery
message."
  (when (open-stream-p socket)
    (let ((ok nil))
      (unwind-protect
           (progn
             (unless sync-sent
               (sync-message socket)
               (force-output socket))
             ;; TODO initiate timeout on the socket read, signal timeout error
             (loop :named syncing
                :while (open-stream-p socket)
                :do (message-case socket
                      (#\Z (read-uint1 socket)
                           (setf ok t)
                           (return-from syncing))
                      (t (values)))))
        (unless ok
          ;; if we can't sync, make sure the socket is shot
          ;; (e.g. a timeout, or aborting execution with a restart from sldb)
          (ensure-socket-is-closed socket))))))

(defmacro with-syncing (&body body)
  "Macro to wrap a block in a handler that will try to re-sync the
connection if something in the block raises a condition. Not hygienic
at all, only used right below here."
  `(let ((sync-sent nil)
         (ok nil))
    (handler-case
      (unwind-protect
           (multiple-value-prog1
               (progn ,@body)
             (setf ok t))
        (unless ok
          (try-to-sync socket sync-sent)))
      (end-of-file (c)
        (ensure-socket-is-closed socket)
        (error c)))))

(defmacro returning-effected-rows (value &body body)
  "Computes a value, then runs a body, then returns, as multiple
values, that value and the amount of effected rows, if any (see
*effected rows*)."
  (let ((value-name (gensym)))
    `(let* ((*effected-rows* nil)
            (,value-name ,value))
       ,@body
       (if *effected-rows*
           (values ,value-name *effected-rows*)
           ,value-name))))

(defun send-query (socket query row-reader)
  "Send a query to the server, and apply the given row-reader to the
results."
  (declare (type stream socket)
           (type string query)
           #.*optimize*)
  (with-syncing
    (let ((row-description nil)
          (*current-query* query))
      (simple-parse-message socket query)
      (simple-describe-message socket)
      (flush-message socket)
      (force-output socket)
      (message-case socket
        ;; ParseComplete
        (#\1))
      (message-case socket
        ;; ParameterDescription
        (#\t :skip))
      (message-case socket
         ;; RowDescription
        (#\T (setf row-description (read-field-descriptions socket)))
        ;; NoData
        (#\n))
      (simple-bind-message socket (map 'vector 'field-binary-p row-description))
      (simple-execute-message socket)
      (sync-message socket)
      (setf sync-sent t)
      (force-output socket)
      (message-case socket
        ;; BindComplete
        (#\2))
      (returning-effected-rows
          (if row-description
              (funcall row-reader socket row-description)
              (look-for-row socket))
        (message-case socket
           ;; ReadyForQuery, skipping transaction status
          (#\Z (read-uint1 socket)))))))

(defun send-parse (socket name query)
  "Send a parse command to the server, giving it a name."
  (declare (type stream socket)
           (type string name query)
           #.*optimize*)
  (with-syncing
    (let ((*current-query* query))
      (parse-message socket name query)
      (flush-message socket)
      (force-output socket)
      (message-case socket
        ;; ParseComplete
        (#\1)))))

(defun send-execute (socket name parameters row-reader)
  "Execute a previously parsed query, and apply the given row-reader
to the result."
  (declare (type stream socket)
           (type string name)
           (type list parameters)
           #.*optimize*)
  (with-syncing
    (let ((row-description nil)
          (n-parameters 0))
      (declare (type (unsigned-byte 16) n-parameters))
      (describe-prepared-message socket name)
      (flush-message socket)
      (force-output socket)
      (message-case socket
        ;; ParameterDescription
        (#\t (setf n-parameters (read-uint2 socket))
             (skip-bytes socket (* 4 n-parameters))))
      (message-case socket
        ;; RowDescription
        (#\T (setf row-description (read-field-descriptions socket)))
         ;; NoData
        (#\n))
      (unless (= (length parameters) n-parameters)
        (error 'database-error
               :message (format nil "Incorrect number of parameters given for prepared statement ~A." name)))
      (bind-message socket name (map 'vector 'field-binary-p row-description)
                    parameters)
      (simple-execute-message socket)
      (sync-message socket)
      (setf sync-sent t)
      (force-output socket)
      (message-case socket
        ;; BindComplete
        (#\2))
      (returning-effected-rows
          (if row-description
              (funcall row-reader socket row-description)
              (look-for-row socket))
        (message-case socket
          ;; ReadyForQuery, skipping transaction status
          (#\Z (read-uint1 socket)))))))

(defun build-row-reader (function-form fields body)
  "Helper for the following two macros."
  (let ((socket (gensym)))
    `(,@function-form (,socket ,fields)
      (declare (type stream ,socket)
               (type (simple-array field-description) ,fields))
      (flet ((next-row ()
               (look-for-row ,socket))
             (next-field (field)
               (declare (type field-description field))
               (let ((size (read-int4 ,socket)))
                 (declare (type (signed-byte 32) size))
                 (if (eq size -1)
                     :null
                     (funcall (the (function (stream (unsigned-byte 32)) t) (field-interpreter field))
                              ,socket size)))))
        ,@body))))

(defmacro row-reader ((fields) &body body)
  "Create a row-reader, using the given name for the fields argument
and the given body for reading the rows. A row reader is a function
that is used to do something with the results of a query. It has two
local functions: next-row and next-field, the first should be called
once per row and will return a boolean indicating whether there are
any more rows, the second should be called once for every element in
the fields vector, with that field as argument, to read a single value
in a row. See list-row-reader in public.lisp for an example."
  (build-row-reader '(lambda) fields body))

(defmacro def-row-reader (name (fields) &body body)
  "Create a row reader, as in the row-reader macro, and assign a name
to it."
  (build-row-reader `(defun ,name) fields body))

;;; Copyright (c) 2006 Marijn Haverbeke
;;;
;;; This software is provided 'as-is', without any express or implied
;;; warranty. In no event will the authors be held liable for any
;;; damages arising from the use of this software.
;;;
;;; Permission is granted to anyone to use this software for any
;;; purpose, including commercial applications, and to alter it and
;;; redistribute it freely, subject to the following restrictions:
;;;
;;; 1. The origin of this software must not be misrepresented; you must
;;;    not claim that you wrote the original software. If you use this
;;;    software in a product, an acknowledgment in the product
;;;    documentation would be appreciated but is not required.
;;;
;;; 2. Altered source versions must be plainly marked as such, and must
;;;    not be misrepresented as being the original software.
;;;
;;; 3. This notice may not be removed or altered from any source
;;;    distribution.
