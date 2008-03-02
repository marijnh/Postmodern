(in-package :cl-postgres)

(defparameter *client-encoding* "SQL_ASCII")

(setf (fdefinition 'enc-byte-length) (fdefinition 'length))

(defun enc-read-string (stream &key null-terminated byte-length)
  "Read an ascii-string from a byte stream, until either a null byte
is reached or the given amount of bytes have been read."
  (declare (type stream stream)
           (type (or null fixnum) byte-length)
           #.*optimize*)
  (let ((bytes-read 0)
        (string (make-array 64 :element-type 'character
                               :adjustable t :fill-pointer 0)))
    (loop
       (when (and byte-length (>= bytes-read byte-length))
         (return))
       (let ((next-char (read-byte stream)))
         (incf bytes-read)
         (when (and null-terminated (eq next-char 0))
           (return))
         (vector-push-extend (code-char next-char) string)))
    string))

(defun enc-string-bytes (string)
  "Convert an ascii string to an array of octets."
  (map '(vector (unsigned-byte 8) *) 'char-code string))

(defun enc-write-string (string stream)
  "Write an ascii string to a stream."
  (declare (type stream stream)
           (type string string)
           #.*optimize*)
  (loop :for char :of-type character :across string
     :do (write-byte (char-code char) stream)))
