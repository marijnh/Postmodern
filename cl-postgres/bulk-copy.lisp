
(in-package :cl-postgres)

(defclass bulk-copier ()
  ((database :initarg :database :reader copier-database)
   (table :initarg :table :reader copier-table)
   (columns :initarg :columns :reader copier-columns)
   (count :initform 0 :accessor copier-count)))

(defmethod print-object ((self bulk-copier) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a ~a" (copier-table self)
	    (copier-columns self))))


(defun open-db-writer (db-spec table columns)
  (let ((copier (make-instance 'bulk-copier
                  :database (apply 'open-database db-spec)
                  :table table
                  :columns columns)))
    (initialize-copier copier)
    copier))

(defun close-db-writer (self &key (abort t))
  (flet ((finish-copying-socket ()
           (let* ((connection (copier-database self))
                  (socket (connection-socket connection)))
             (with-reconnect-restart connection
               (using-connection connection
                 (send-copy-done socket))))))
    (unwind-protect
         (finish-copying-socket))
    (when abort
      (close-database (copier-database self))))
  (copier-count self))

(defun db-write-row (self row &optional (data (prepare-row self row)))
  (let* ((connection (copier-database self))
	 (socket (connection-socket connection)))
    (with-reconnect-restart connection
      (using-connection connection
        (with-syncing
          (copy-data-message socket data)))))
  (incf (copier-count self)))

(defun copy-query (self)
  (format nil "~%copy ~a ~@[(~{~a~^,~})~] ~a ~a
  with (
  format 'text')"
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
    (string (let ((pg-string (with-output-to-string (str)
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
    (symbol (case val
              (:null (princ "\\N" s))
              ((t) (princ "true" s))
              (otherwise (error "copier-write-val: Symbols shouldn't be getting this far ~a" val))))))

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

