
(in-package :cl-postgres)

#|
When inserting a lot of data, it's significantly faster to use the bulk copier.  The
basic API for this is.

open-copier
copy-row
copy-done

Example usage would be as follows...

 (loop with c = (open-copier ...)
    for row = (get-row-from-input-data) then (get-row-from-input-data)
    until (null row)
    do (copy-row row)
    finally (handler-case
               (copy-done c)
               (close-database c)
             (t ()
               (close-database c))))

It's probably a good idea to turn off triggers when loading a lot of data.  This can
be done temporarily using the macro without-auto-triggers
|#

(defvar *bulk-copier*)

(eval-when (compile load eval)
  (export '(*bulk-copier* copier-database copier-table)))

(defclass bulk-copier ()
  ((database :initarg :database :reader copier-database)
   (table :initarg :table :reader copier-table)
   (stream :initarg :stream :reader copier-stream)
   (columns :initarg :columns :reader copier-columns)
   (count :initform 0 :accessor copier-count)
   (from/to :initarg :from/to :reader copier-from/to)
   (format :initarg :format :reader copier-format)
   (header-p :initarg :header-p :reader copier-header-p)
   (delimiter :initarg :delimiter :reader copier-delimiter)
   (null-str :initarg :null-str :reader copier-null-str)
   (quote-str :initarg :quote-str :reader copier-quote-str)
   (escape-str :initarg :escape-str :reader copier-escape-str)))

(defmethod print-object ((self bulk-copier) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a ~a" (copier-table self)
	    (copier-columns self))))



(defun csv-copier-p (self)
  (copier-stream self))

(defun open-copier (db table from/to &key
                     (columns nil)
                     ;; can only use this option when your Postgres can see the temporary file
                     (use-temporary-file nil)
		     (format 'text)
                     (header-p nil)
                     (delimiter #\|)
                     (null "NULL")
                     (quote #\")
                     (escape quote))
  "Opens a stream into which data can be bulk-loaded into a Postgres table (according to
the specified copy statement)"
  (let ((copier (make-instance 'bulk-copier
                  :database db
                  :table table
                  :columns columns
                  :stream  (typecase use-temporary-file
                            (null nil)
                            (string
                             (open (format nil "~a.csv" use-temporary-file) :direction :output
                               :if-does-not-exist :create))
                            (otherwise
                             (open (format nil "~a.csv" (system:make-temp-file-name)) :direction :output
                               :if-does-not-exist :create)))
                  :from/to (intern (symbol-name from/to) :cl-postgres)
                  :header-p header-p
		  :format format
                  :delimiter delimiter
                  :null-str null
                  :quote-str quote
                  :escape-str escape)))
    (unless (csv-copier-p copier)
      (start-copying copier))
    copier))

(defun sql-escape-string (string)
  "Escape string data so it can be used in a query."
  (let ((*print-pretty* nil))
    (with-output-to-string (*standard-output*)
      (princ #\')
      (loop :for char :across string
                :do (princ (case char
                             (#\' "''")
                             (#\\ "\\\\")
                             (otherwise char))))
      (princ #\'))))

(defun copy-query (self)
  (case (intern (copier-format self) #.*package*)
    (text (copy-query-text-mode self))
    (csv (copy-query-csv-mode self))))

(defun copy-query-csv-mode (self)
    ;(break "boom")
  (format nil "
copy ~a ~@[(~{~a~^,~})~] ~a ~a
  with delimiter as '~a'
       null as '~a'
       csv ~:[~;header~]
         quote as '~a'
         escape as '~a'"
    (copier-table self)
    (copier-columns self)
    (copier-from/to self)
    (if (csv-copier-p self)
        (sql-escape-string (namestring (pathname (copier-stream self))))
      (ecase (copier-from/to self)
        (from "STDIN")
        (to "STDOUT")))
    (copier-delimiter self)
    (copier-null-str self)
    (copier-header-p self)
    (copier-quote-str self)
    (copier-escape-str self)))


(defun copy-query-text-mode (self)
  ;(break "boom")
  (format nil "~%copy ~a ~@[(~{~a~^,~})~] ~a ~a
  with (
  format 'text',
  delimiter '~a',
       null '~a')"
    (copier-table self)
    (copier-columns self)
    (copier-from/to self)
    (if (csv-copier-p self)
        (sql-escape-string (namestring (pathname (copier-stream self))))
      (ecase (copier-from/to self)
        (from "STDIN")
        (to "STDOUT")))
    (copier-delimiter self)
    (copier-null-str self)))

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

(defun start-copying (self)
  (let* ((query (copy-query self))
	 (connection (copier-database self))
	 (socket (connection-socket connection)))
    (with-reconnect-restart connection
      (using-connection connection
	(send-copy-start socket query)))))

(declaim (inline quotable-p copy-row))

(defvar *current-copier*)

(let ((quoted-chars '(#\Newline #\" #\' #\,)))
  (defun quotable-p (str)
    (declare (optimize speed))
    (find-if (lambda (ch)
               (member ch (cons (copier-delimiter *current-copier*)
                                quoted-chars)))
	     str)))

(defun copier-write-value (s val copier-quote-str copier-escape-str)
  (case (intern (copier-format *current-copier*) #.*package*)
    (text (copier-write-value-text s val copier-quote-str copier-escape-str))
    (csv (copier-write-value-csv s val copier-quote-str copier-escape-str))))

(defun copier-write-value-csv (s val copier-quote-str copier-escape-str)
  (when (stringp val)
    (setf val (substitute-if #\U (lambda (x)
                                   (cond
                                     ((< (char-code x) 32)
                                      #+xxx(print `(:sub-space-char ,x :in ,val :of ,row))
                                      t)
                                     ((> (char-code x) 127)
                                      #+xxx(print `(:uni-char ,x :in ,val :of ,row))
                                      t)))
                             val)))
  (if (and (stringp val)
           (quotable-p val))
      (progn
        (princ copier-quote-str s)
        (loop for char across val do
          (when (char= char copier-quote-str)
            (write-char copier-escape-str s))
          (write-char char s))
        (write-char copier-quote-str s))
      (when (and val (not (eq :null val)))
        (princ val s))))

(defun copier-write-value-text (s val copier-quote-str copier-escape-str)
  (declare (ignore copier-quote-str copier-escape-str))
  (typecase val
    (string (loop for char across val do
      (cond
	((char= char #\Newline) (princ "\\n" s))
	((char= char #\Space) (princ #\Space s))
	((char= char #\\) (princ #\\ s) (princ #\\ s))
	((char= char (copier-delimiter *current-copier*))
	 (format s "\\~a" char))
	((and (< 32 (char-code char))
	      (> 127 (char-code char))) (write-char char s))
	(t (princ (format nil "\\~o" (char-code char)) s)))))
    (number (princ val s))
    (null (princ "false" s))
    (symbol (case val
	      (:null (princ (copier-null-str *current-copier*) s))
	      ((t) (princ "true" s))
	      (otherwise (error "copier-write-val: Symbols shouldn't be getting this far ~a" val))))))
  
(defun copier-write-sequence (s vector copier-quote-str copier-escape-str)
  (write-char #\{ s)
  (loop for (item . more-p) on (coerce vector 'list)
     do (cond ((null item)
               (copier-write-value s "NULL" copier-quote-str copier-escape-str))
              ((atom item)
               (copier-write-value s item copier-quote-str copier-escape-str))
              (t
               (copier-write-sequence s item copier-quote-str copier-escape-str)))
	when more-p
	  do (write-char #\, s))
  (write-char #\} s))

(defgeneric prepare-row (self row)
  (:method (self (row list))
	   (with-output-to-string (s)
	     (loop for (val . more-p) on row
		   do (progn
			(if (typep val '(or string
					 (not vector)))
			    (copier-write-value s val
						(copier-quote-str self)
						(copier-escape-str self))
			    (copier-write-sequence s val
						   (copier-quote-str self)
						   (copier-escape-str self))))
		   if more-p
		      do (write-char (copier-delimiter self) s)
		   finally
		(write-char #\Newline s))))
  (:method ((self t) (row string))
	   (format nil "~A~%" row)))

(defun copy-row (self row &optional (data (let ((*current-copier* self))
                                            (prepare-row self row))))
  (let* ((connection (copier-database self))
	 (socket (connection-socket connection)))
    (if (csv-copier-p self)
	(write-string data (copier-stream self))
	(with-reconnect-restart connection
	  (using-connection connection
	    (with-syncing
	      (copy-data-message socket data)))))
    (incf (copier-count self))))

(defun send-copy-done (socket)
  (with-syncing
    (copy-done-message socket)
    (force-output socket)
    (message-case socket
      (#\C (let* ((command-tag (read-str socket))
		  (space (position #\Space command-tag :from-end t)))
	     (when space
	       (parse-integer command-tag :junk-allowed t :start (1+ space))))))
    (loop (message-case socket
	    (#\Z (read-uint1 socket)
		 (return-from send-copy-done))
	    (t :skip)))))

(defun copy-done (self &optional (close-connection-p t))
  (flet ((finish-copying-csv ()
	   (close (copier-stream self))
	   (exec-query (copier-database self) (copy-query self))
	   (copier-count self))
	 (finish-copying-socket ()
	   (let* ((connection (copier-database self))
		  (socket (connection-socket connection)))
	     (with-reconnect-restart connection
	       (using-connection connection
		 (send-copy-done socket))))))
    (unwind-protect
	 (if (csv-copier-p self)
	     (finish-copying-csv)
	     (finish-copying-socket))
      (when close-connection-p
	(close-database (copier-database self))))
    (copier-count self)))
