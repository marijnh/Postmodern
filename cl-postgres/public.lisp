(in-package :cl-postgres)

(defclass database-connection ()
  ((host :initarg :host :reader connection-host)
   (port :initarg :port :reader connection-port)
   (database :initarg :db :reader connection-db)
   (user :initarg :user :reader connection-user)
   (password :initarg :password :reader connection-password)
   (socket :initarg :socket :accessor connection-socket)
   (meta :initform nil)
   (available :initform t :accessor connection-available)
   (parameters :accessor connection-parameters)
   (timestamp-format :accessor connection-timestamp-format))
  (:documentation "Representatino of a database connection. Contains
login information in order to be able to automatically re-establish a
connection when it is somehow closed."))

(defun connection-meta (connection)
  "Retrieves the meta field of a connection, the primary purpose of
which is to store information about the prepared statements that
exists for it."
  (or (slot-value connection 'meta)
      (let ((meta-data (make-hash-table)))
        (setf (slot-value connection 'meta) meta-data)
        meta-data)))

(defun database-open-p (connection)
  "Returns a boolean indicating whether the given connection is
currently connected."
  (and (connection-socket connection)
       (open-stream-p (connection-socket connection))))

(defun open-database (database user password host &optional (port 5432))
  "Create and connect a database object."
  (let ((conn (make-instance 'database-connection :host host :port port :user user
                             :password password :socket nil :db database)))
    (initiate-connection conn)
    conn))

(defun initiate-connection (conn)
  "Check whether a connection object is connected, try to connect it
if it isn't."
  (let ((socket (usocket:socket-stream
                 (usocket:socket-connect (connection-host conn)
                                         (connection-port conn)
                                         :element-type '(unsigned-byte 8))))
        (finished nil))
    (unwind-protect
         (setf (slot-value conn 'meta) nil
               (connection-parameters conn)
               (authenticate socket (connection-user conn)
                             (connection-password conn) (connection-db conn))
               (connection-timestamp-format conn)
               (if (string= (gethash "integer_datetimes" (connection-parameters conn)) "on")
                   :integer :float)
               (connection-socket conn) socket
               finished t)
      (unless finished
        (ensure-socket-is-closed socket)))
    (values)))

(defun reopen-database (conn)
  "Reconnect a disconnected database connection."
  (unless (database-open-p conn)
    (initiate-connection conn)))

(defun ensure-connection (conn)
  "Used to make sure a connection object is connected before doing
anything with it."
  (unless (database-open-p conn)
    (restart-case (error 'database-connection-lost :message "Connection to database server lost.")
      (:reconnect () :report "Try to reconnect." (initiate-connection conn)))))

(defun close-database (connection)
  "Gracefully disconnect a database connection."
  (when (database-open-p connection)
    (terminate-connection (connection-socket connection)))
  (values))

(defmacro with-availability (connection &body body)
  "This is used to prevent a row-reader from recursively calling some
query function. Because the connection is still returning results from
the previous query when a row-reading is being executed, starting
another query will not work as expected \(or at all, in general). This
might also raise an error when you are using a single database
connection from multiple threads, but you should not do that at all."
  (let ((connection-name (gensym)))
    `(let ((,connection-name ,connection))
      (when (not (connection-available ,connection-name))
        (error 'database-error :message "This connection is still processing another query."))
      (setf (connection-available ,connection-name) nil)
      (unwind-protect (progn ,@body)
        (setf (connection-available ,connection-name) t)))))

(defmacro with-reconnect-restart (connection &body body)
  "When, inside the body, an error occurs that breaks the connection
socket, a condition of type database-connection-error is raised,
offering a :reconnect restart."
  (let ((connection-name (gensym))
        (body-name (gensym))
        (retry-name (gensym)))
  `(let ((,connection-name ,connection))
    (ensure-connection ,connection-name)
    (labels ((,body-name ()
               (handler-case (progn ,@body)
                 (stream-error (e)
                   (cond ((eq (connection-socket ,connection-name) (stream-error-stream e))
                          (ensure-socket-is-closed (connection-socket ,connection-name) :abort t)
                          (,retry-name (wrap-stream-error e)))
                         (t (error e))))
                 (cl-postgres-error:server-shutdown (e)
                   (,retry-name e))))
             (,retry-name (err)
               (restart-case (error err)
                 (:reconnect () :report "Try to reconnect"
                             (reopen-database ,connection-name)
                             (,body-name)))))
      (,body-name)))))

(defun exec-query (connection query &optional (row-reader 'ignore-row-reader))
  "Execute a query string and apply the given row-reader to the
result."
  (check-type query string)
  (with-reconnect-restart connection
    (let ((*timestamp-format* (connection-timestamp-format connection)))
      (with-availability connection
        (send-query (connection-socket connection) query row-reader)))))

(defun prepare-query (connection name query)
  "Prepare a query string and store it under the given name."
  (check-type query string)
  (check-type name string)
  (with-reconnect-restart connection
    (send-parse (connection-socket connection) name query)
    (values)))

(defun exec-prepared (connection name parameters &optional (row-reader 'ignore-row-reader))
  "Execute a previously prepared query with the given parameters,
apply a row-reader to the result."
  (check-type name string)
  (check-type parameters list)
  (with-reconnect-restart connection
    (let ((*timestamp-format* (connection-timestamp-format connection)))
      (with-availability connection
        (send-execute (connection-socket connection)
                      name parameters row-reader)))))

;; A row-reader that returns a list of (field-name . field-value)
;; alist for the returned rows.
(def-row-reader alist-row-reader (fields)
  (loop :while (next-row)
        :collect (loop :for field :across fields
                       :collect (cons (field-name field)
                                      (next-field field)))))

;; Row-reader that returns a list of lists.
(def-row-reader list-row-reader (fields)
  (loop :while (next-row)
        :collect (loop :for field :across fields
                       :collect (next-field field))))

;; Row-reader that returns a vector of vectors.
(def-row-reader vector-row-reader (fields)
  (let ((rows (make-array 1 :adjustable t :fill-pointer 0)))
    (loop :for row = (make-array (length fields))
          :while (next-row)
          :do (progn
                (loop :for field :across fields
                      :for idx :upfrom 0
                      :do (setf (aref row idx) (next-field field)))
                (vector-push-extend row rows)))
    rows))

;; Row-reader that discards the query results.
(def-row-reader ignore-row-reader (fields)
  (loop :while (next-row)
        :do (loop :for field :across fields
                  :do (next-field field)))
  (values))

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
