;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

(defparameter *current-query* nil)

(defun log-query (query time-units)
  "This function is default value of *QUERY-CALLBACK* and logs queries
to *QUERY-LOG* if it is not NIL."
  (when *query-log*
    (format *query-log* "CL-POSTGRES query (~ams): ~a~%"
            (round (/ (* 1000 time-units)
                      internal-time-units-per-second))
            query)))

(defmacro with-query ((query) &body body)
  (let ((time-name (gensym)))
    `(let ((*current-query* ,query)
           (,time-name (if *query-callback* (get-internal-real-time) 0)))
       (multiple-value-prog1 (progn ,@body)
         (when *query-callback*
           (funcall *query-callback*
                    *current-query*
                    (- (get-internal-real-time) ,time-name)))))))

;;
;; See http://www.postgresql.org/docs/current/protocol-error-fields.html
;; and https://www.postgresql.org/docs/current/errcodes-appendix.html
;; for details, including documentation strings.
;;
(define-condition database-error (error)
  ((error-code :initarg :code :initform nil :reader database-error-code
               :documentation "Code: the Postgresql SQLSTATE code for the error
 (see the Postgresql Manual Appendix A for their meaning). Not localizable.
 Always present.")
   (message :initarg :message :accessor database-error-message
            :documentation "Message: the primary human-readable error message.
This should be accurate but terse (typically one line). Always present.")
   (detail :initarg :detail :initform nil :reader database-error-detail
           :documentation "Detail: an optional secondary error message carrying
more detail about the problem. Might run to multiple lines or NIL if none is
available.")
   (hint :initarg :hint :initform nil :reader database-error-hint
         :documentation "Hint: an optional suggestion what to do about the
problem.")
   (context :initarg :context :initform nil :reader database-error-context
            :documentation "Where: an indication of the context in which the
error occurred. Presently this includes a call stack traceback of active
procedural language functions and internally-generated queries. The trace is
one entry per line, most recent first.")
   (query :initform *current-query* :reader database-error-query
          :documentation "Query that led to the error, or NIL if no query was
involved.")
   (position :initarg :position :initform nil :reader database-error-position
             :documentation "Position: the field value is a decimal ASCII
integer, indicating an error cursor position as an index into the original query
string. The first character has index 1, and positions are measured in
characters not bytes.")
   (cause :initarg :cause :initform nil :reader database-error-cause
          :documentation "The condition that caused this error, or NIL when it
was not caused by another condition."))
  (:report
   (lambda (err stream)
     (format stream
             "Database error~@[ ~A~]: ~A~@[~&DETAIL: ~A~]~@[~&HINT: ~A~]~@[~&CONTEXT: ~A~]~@[~&QUERY: ~A~]~@[~VT^~]"
             (database-error-code err)
             (database-error-message err)
             (database-error-detail err)
             (database-error-hint err)
             (database-error-context err)
             (database-error-query err)
             (database-error-position err))))
  (:documentation "This is the condition type that will be used to signal
virtually all database-related errors \(though in some cases
socket errors may be raised when a connection fails on the IP level). For errors
that you may want to catch by type, the cl-postgres-error package defines a
bucket of subtypes used for specific errors. See the cl-postgres/package.lisp
file for a list."))

(defun database-error-constraint-name (err)
  "For integrity-violation error, given a database-error for an integrity
violation, will attempt to extract and return the constraint name (or nil if no
constraint was found)."
  (labels ((extract-quoted-part (string n)
             "Extracts the Nth quoted substring from STRING."
             (let* ((start-quote-inst (* 2 n))
                    (start-quote-pos (position-nth #\" string start-quote-inst))
                    (end-quote-pos (position #\" string
                                             :start (1+ start-quote-pos))))
               (subseq string (1+ start-quote-pos) end-quote-pos)))
           (position-nth (item seq n)
             "Finds the position of the zero-indexed Nth ITEM in SEQ."
             (loop :with pos = -1 :repeat (1+ n)
                   :do (setf pos (position item seq :start (1+ pos)))
                   :finally (return pos))))
    (let ((message (database-error-message err)))
      (typecase err
        (cl-postgres-error:not-null-violation
         (extract-quoted-part message 0))
        (cl-postgres-error:unique-violation
         (extract-quoted-part message 0))
        (cl-postgres-error:foreign-key-violation
         (extract-quoted-part message 1))
        (cl-postgres-error:check-violation
         (extract-quoted-part message 1))))))

(defun database-error-extract-name (err)
  "For various errors, returns the name provided by the error message
 (or nil if no such name was found)."
  (labels ((extract-quoted-part (string n)
             "Extracts the Nth quoted substring from STRING."
             (let* ((start-quote-inst (* 2 n))
                    (start-quote-pos (position-nth #\" string start-quote-inst))
                    (end-quote-pos (position #\" string
                                             :start (1+ start-quote-pos))))
               (subseq string (1+ start-quote-pos) end-quote-pos)))
           (position-nth (item seq n)
             "Finds the position of the zero-indexed Nth ITEM in SEQ."
             (loop :with pos = -1 :repeat (1+ n)
                   :do (setf pos (position item seq :start (1+ pos)))
                :finally (return pos))))
    (let* ((message (database-error-message err)))
           (typecase err
             (cl-postgres-error:invalid-sql-statement-name
              (extract-quoted-part message 0))
             (cl-postgres-error:duplicate-prepared-statement
              (extract-quoted-part message 0))))))

(define-condition database-connection-error (database-error) ()
  (:documentation "Subtype of database-error. An error of this type (or one of
its subclasses) is signaled when a query is attempted with a connection object
that is no longer connected, or a database connection becomes invalid during a
query. Always provides a :reconnect restart, which will cause the library to
make an attempt to restore the connection and re-try the query.

The following shows an example use of this feature, a way to ensure that the
first connection error causes a reconnect attempt, while others pass through
as normal. A variation on this theme could continue trying to reconnect, with
successively longer pauses.

    (defun call-with-single-reconnect (fun)
      (let ((reconnected nil))
        (handler-bind
            ((database-connection-error
              (lambda (err)
                (when (not reconnected)
                  (setf reconnected t)
                  (invoke-restart :reconnect)))))
          (funcall fun))))"))

(define-condition database-connection-lost (database-connection-error) ()
  (:documentation "Raised when a query is initiated on a disconnected
connection object."))
(define-condition database-socket-error (database-connection-error) ()
  (:documentation "Used to wrap stream-errors and socket-errors,
giving them a database-connection-error superclass."))

(defun wrap-socket-error (err)
  (make-instance 'database-socket-error
                 :message (princ-to-string err)
                 :cause err))

(in-package :cl-postgres-error)

(defparameter *error-table* (make-hash-table :test 'equal))
(defmacro deferror (code typename &optional (superclass 'database-error))
  `(progn (define-condition ,typename (,superclass) ())
          (setf (gethash ,code *error-table*) ',typename)))

;; Connection Exceptions
;; https://www.postgresql.org/docs/current/errcodes-appendix.html
(deferror "08000"	connection-exception)
(deferror "08003"	connection-does-not-exist)
(deferror "08006"	connection-failure)
(deferror "08001"	sqlclient-unable-to-establish-sqlconnection)
(deferror "08004"	sqlserver-rejected-establishment-of-sqlconnection)
(deferror "08007"	transaction-resolution-unknown)
(deferror "08P01" protocol-violation)

(deferror "0A" feature-not-supported)
(deferror "22" data-exception)
(deferror "22012" db-division-by-zero data-exception)
(deferror "22021" invalid-byte-sequence data-exception)
(deferror "22007" invalid-datetime-format data-exception)
(deferror "22003" numeric-value-out-of-range data-exception)
(deferror "22P01" floating-point-exception data-exception)
(deferror "23" integrity-violation)
(deferror "23001" restrict-violation integrity-violation)
(deferror "23502" not-null-violation integrity-violation)
(deferror "23503" foreign-key-violation integrity-violation)
(deferror "23505" unique-violation integrity-violation)
(deferror "23514" check-violation integrity-violation)
(deferror "26000" invalid-sql-statement-name)
(deferror "42" syntax-error-or-access-violation)
(deferror "42501" insufficient-privilege syntax-error-or-access-violation)
(deferror "40" transaction-rollback)
(deferror "40001" serialization-failure transaction-rollback)
(deferror "40002" transaction-integrity-constraint-violation
  transaction-rollback)
(deferror "40003" statement-completion-unknown transaction-rollback)
(deferror "40P01" deadlock-detected transaction-rollback)
(deferror "42P01" undefined-table syntax-error-or-access-violation)
(deferror "42601" columns-error syntax-error-or-access-violation)
(deferror "42703" undefined-column syntax-error-or-access-violation)
(deferror "42704" unrecognized-configuration-parameter data-exception)
(deferror "42701" duplicate-column syntax-error-or-access-violation)
(deferror "42P03" duplicate-cursor syntax-error-or-access-violation)
(deferror "42P04" duplicate-database syntax-error-or-access-violation)
(deferror "42723" duplicate-function syntax-error-or-access-violation)
(deferror "42P05" duplicate-prepared-statement syntax-error-or-access-violation)
(deferror "42P06" duplicate-schema syntax-error-or-access-violation)
(deferror "42P07" duplicate-table syntax-error-or-access-violation)
(deferror "42712" duplicate-alias syntax-error-or-access-violation)
(deferror "42710" duplicate-object syntax-error-or-access-violation)
(deferror "53" insufficient-resources)
(deferror "54" program-limit-exceeded)
(deferror "55" object-state-error)
(deferror "55006" object-in-use object-state-error)
(deferror "55P03" lock-not-available object-state-error)
(deferror "57" operator-intervention)
(deferror "57014" query-canceled operator-intervention)
(define-condition server-shutdown (operator-intervention
                                   database-connection-error)
  ())
(deferror "57P01" admin-shutdown server-shutdown)
(deferror "57P02" crash-shutdown server-shutdown)
(deferror "57P03" cannot-connect-now operator-intervention)
(deferror "58" system-error)
(deferror "XX" internal-error)

(defun get-error-type (code)
  (or (gethash code *error-table*)
      (and code (gethash (subseq code 0 2) *error-table*))
      'database-error))
