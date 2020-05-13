;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

(defclass bulk-copier ()
  ((own-connection :initarg :own-connection :reader bulk-copier-own-connection)
   (database :initarg :database :reader copier-database)
   (table :initarg :table :reader copier-table)
   (columns :initarg :columns :reader copier-columns)
   (count :initform 0 :accessor copier-count)))

(defmethod print-object ((self bulk-copier) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a ~a" (copier-table self)
	    (copier-columns self))))


(defun open-db-writer (db-spec table columns)
  "Opens a table stream into which rows can be written one at a time using
db-write-row. db is either a connection object or a list of arguments that
could be passed to open-database. table is the name of an existing table
into which this writer will write rows. If you don't have data for all
columns, use columns to indicate those that you do."
  (let* ((own-connection (listp db-spec))
         (copier (make-instance 'bulk-copier
                   :own-connection own-connection
                   :database (if own-connection
                                 (apply 'open-database db-spec)
                                 db-spec)
                   :table table
                   :columns columns)))
    (initialize-copier copier)
    copier))

(defun close-db-writer (self &key (abort nil))
  "Closes a bulk writer opened by open-db-writer. Will close the associated
database connection when it was created for this copier, or abort is true."
  (unwind-protect
       (let* ((connection (copier-database self))
              (socket (connection-socket connection)))
         (with-reconnect-restart connection
           (using-connection connection
             (send-copy-done socket))))
    (when (or abort (bulk-copier-own-connection self))
      (close-database (copier-database self))))
  (copier-count self))

(defun db-write-row (self row &optional (data (prepare-row self row)))
  "Writes row-data into the table and columns referenced by the writer.
row-data is a list of Lisp objects, one for each column included when
opening the writer. Arrays (the elements of which must all be the same type)
will be serialized into their PostgreSQL representation before being written
into the DB."
  (let* ((connection (copier-database self))
	 (socket (connection-socket connection)))
    (with-reconnect-restart connection
      (using-connection connection
        (with-syncing
          (copy-data-message socket data)))))
  (incf (copier-count self)))

(defun copy-query (self)
  (format nil "~%copy ~a ~@[(~{~a~^,~})~] ~a ~a"
    (copier-table self)
    (copier-columns self)
    "FROM"
    "STDIN"))

(defun send-copy-start (socket query)
  (with-syncing
    (query-message socket query)
    (flush-message socket)
    (force-output socket)
    (message-case socket
      ;; Ignore the field formats because we're only supporting plain
      ;; text for now
      (#\G (read-uint1 socket)
	   (skip-bytes socket (* 2 (read-uint2 socket)))))))

(defun initialize-copier (self)
  (let* ((query (copy-query self))
	 (connection (copier-database self))
	 (socket (connection-socket connection)))
    (with-reconnect-restart connection
      (using-connection connection
	(send-copy-start socket query)))))

(defun copier-write-value (s val)
  (typecase val
    (string
     (let ((pg-string
             (with-output-to-string (str)
               (loop for byte across (cl-postgres-trivial-utf-8:string-to-utf-8-bytes val) do
                 (case (code-char byte)
                   (#\Space (princ " " str))
                   ((#\Newline #\Tab) (format str "\\~a" (code-char byte)))
                   (#\\ (progn (princ #\\ str) (princ #\\ str)))
                   (otherwise (if (and (< 32 byte)
                                       (> 127 byte))
                                  (write-char (code-char byte) str)
                                  (princ (format nil "\\~o" byte) str))))))))
       #+nil(print `(:loading ,pg-string))
       (princ pg-string s)))
    (number (princ val s))
    (null (princ "false" s))
    (symbol
     (case val
       (:null (princ "\\N" s))
       ((t) (princ "true" s))
       (otherwise
        (error "copier-write-val: Symbols shouldn't be getting this far ~a" val))))))

(defun copier-write-sequence (s vector)
  (write-char #\{ s)
  (loop for (item . more-p) on (coerce vector 'list)
     do (cond
          ((null item) (copier-write-value s :null))
          ((atom item) (copier-write-value s item))
          (t (copier-write-sequence s item)))
     when more-p
     do (write-char #\, s))
  (write-char #\} s))

(defmethod prepare-row (self row)
  (declare (ignore self))
  (with-output-to-string (s)
    (loop for (val . more-p) on row
		   do (progn
            (if (typep val '(or string
                             (not vector)))
                (copier-write-value s val)
                (copier-write-sequence s val)))
		   if more-p do (write-char #\Tab s)
		   finally
         (write-char #\Newline s))))

(defun send-copy-done (socket)
  (with-syncing
    (setf sync-sent t)
    (copy-done-message socket)
    (force-output socket)
    (message-case socket
      (#\C (let* ((command-tag (read-str socket))
		  (space (position #\Space command-tag :from-end t)))
	     (when space
	       (parse-integer command-tag :junk-allowed t :start (1+ space))))))
    (block find-ready
      (loop (message-case socket
              (#\Z (read-uint1 socket)
                   (return-from find-ready))
              (t :skip))))))
