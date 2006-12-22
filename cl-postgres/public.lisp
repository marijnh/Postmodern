(in-package :cl-postgres)

(defclass database-connection ()
  ((host :initarg :host :reader connection-host)
   (port :initarg :port :reader connection-port)
   (database :initarg :db :reader connection-db)
   (user :initarg :user :reader connection-user)
   (password :initarg :password :reader connection-password)
   (socket :initarg :socket :accessor connection-socket)
   (meta :initform nil)
   (timestamp-format :initform :float
                     :accessor connection-timestamp-format))
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
  (let ((socket (open-stream (connection-host conn)
                             (connection-port conn)
                             :element-type '(unsigned-byte 8)))
        (finished nil))
    (unwind-protect
         (setf (slot-value conn 'meta) nil
               (connection-timestamp-format conn)
               (authenticate socket (connection-user conn)
                             (connection-password conn) (connection-db conn))
               (connection-socket conn) socket
               finished t)
      (unless finished
        (close socket)))
    (values)))

(defun reopen-database (conn)
  "Reconnect a disconnected database connection."
  (unless (database-open-p conn)
    (initiate-connection conn)))

(defun ensure-connection (conn)
  "Used to make sure a connection object is connected before doing
anything with it."
  (unless (database-open-p conn)
    (restart-case (error 'database-error :message "Connection to database server lost.")
      (reconnect () :report "Try to reconnect." (initiate-connection conn)))))

(defun close-database (connection)
  "Gracefully disconnect a database connection."
  (when (database-open-p connection)
    (terminate-connection (connection-socket connection)))
  (values))

(defun exec-query (connection query &optional (row-reader 'ignore-row-reader))
  "Execute a query string and apply the given row-reader to the
result."
  (ensure-connection connection)
  (let ((*timestamp-format* (connection-timestamp-format connection)))
    (send-query (connection-socket connection) query row-reader)))

(defun prepare-query (connection name query)
  "Prepare a query string and store it under the given name."
  (ensure-connection connection)
  (send-parse (connection-socket connection) name query)
  (values))

(defun exec-prepared (connection name parameters &optional (row-reader 'ignore-row-reader))
  "Execute a previously prepared query with the given parameters,
apply a row-reader to the result."
  (ensure-connection connection)
  (let ((*timestamp-format* (connection-timestamp-format connection)))
    (send-execute (connection-socket connection)
                  name parameters row-reader)))

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

;; Row-reader that discards the query results.
(def-row-reader ignore-row-reader (fields)
  (loop :while (next-row)
        :collect (loop :for field :across fields
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
