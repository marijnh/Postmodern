(in-package :cl-postgres)

(defparameter *timestamp-format* :unbound
  "This is used to communicate the format \(integer or float) used for
timestamps and intervals in the current connection, so that the
interpreters for those types know how to parse them.")

(defparameter *sql-readtable* (make-hash-table)
  "The exported special var holding the current read table, a hash
  mapping OIDs to (binary-p . interpreter-function) pairs.")

(defun interpret-as-text (stream size)
  "This interpreter is used for types that we have no specific
interpreter for -- it just reads the value as a string. \(Values of
unknown types are passed in text form.)"
  (enc-read-string stream :byte-length size))

(let ((default-interpreter (cons nil #'interpret-as-text)))
  (defun type-interpreter (oid)
    "Returns a pair representing the interpretation rules for this
type. The car is a boolean indicating whether the type should be
fetched as binary, and the cdr is a function that will read the value
from the socket and build a Lisp value from it."
    (gethash oid *sql-readtable* default-interpreter)))

(defun set-sql-reader (oid function &key (table *sql-readtable*) binary-p)
  "Add an sql reader to a readtable. When the reader is not binary, it
is wrapped by a function that will read the string from the socket."
  (setf (gethash oid table)
        (if binary-p
            (cons t function)
            (cons nil (lambda (stream size)
                        (funcall function
                                 (enc-read-string stream :byte-length size))))))
  table)

(defmacro binary-reader (fields &body value)
  "A slightly convoluted macro for defining interpreter functions. It
allows two forms. The first is to pass a single type identifier, in
which case a value of this type will be read and returned directly.
The second is to pass a list of lists containing names and types, and
then a body. In this case the names will be bound to values read from
the socket and interpreted as the given types, and then the body will
be run in the resulting environment. If the last field is of type
bytes, string, or uint2s, all remaining data will be read and
interpreted as an array of the given type."
  (let ((stream-name (gensym))
        (size-name (gensym))
        (length-used 0))
    (flet ((read-type (type &optional modifier)
             (ecase type
               (bytes `(read-bytes ,stream-name (- ,size-name ,length-used)))
               (string `(enc-read-string ,stream-name :byte-length (- ,size-name ,length-used)))
               (uint2s `(let* ((size (/ (- ,size-name ,length-used) 2))
                               (result (make-array size :element-type '(unsigned-byte 16))))
                         (dotimes (i size)
                           (setf (elt result i) (read-uint2 ,stream-name)))
                         result))
               (int (assert (integerp modifier))
                    (incf length-used modifier)
                    `(,(integer-reader-name modifier t) ,stream-name))
               (uint (assert (integerp modifier))
                     (incf length-used modifier)
                     `(,(integer-reader-name modifier nil) ,stream-name)))))
      `(lambda (,stream-name ,size-name)
         (declare (type stream ,stream-name)
                  (type integer ,size-name)
                  (ignorable ,size-name))
         ,(if (consp fields)
              `(let ,(loop :for field :in fields
                           :collect `(,(first field) ,(apply #'read-type (cdr field))))
                 ,@value)
              (read-type fields (car value)))))))

(defmacro define-interpreter (oid name fields &body value)
  "Shorthand for defining binary readers."
  (declare (ignore name)) ;; Names are there just for clarity
  `(set-sql-reader ,oid (binary-reader ,fields ,@value) :binary-p t))

(define-interpreter 18 "char" int 1)
(define-interpreter 21 "int2" int 2)
(define-interpreter 23 "int4" int 4)
(define-interpreter 20 "int8" int 8)

(define-interpreter 26 "oid" uint 4)

(define-interpreter 16 "bool" ((value int 1))
  (if (zerop value) nil t))

(define-interpreter 17 "bytea" bytes)
(define-interpreter 25 "text" string)
(define-interpreter 1042 "bpchar" string)
(define-interpreter 1043 "varchar" string)

(define-interpreter 700 "float4" ((bits uint 4))
  (ieee-floats:decode-float32 bits))
(define-interpreter 701 "float8" ((bits uint 8))
  (ieee-floats:decode-float64 bits))

;; Numeric types are rather involved. I got some clues on their
;; structure from http://archives.postgresql.org/pgsql-interfaces/2004-08/msg00000.php
(define-interpreter 1700 "numeric"
    ((length uint 2)
     (weight int 2)
     (sign int 2)
     (dscale int 2)
     (digits uint2s))
  (declare (ignore dscale))
  (let ((total (loop :for i :from (1- length) :downto 0
                     :for scale = 1 :then (* scale #.(expt 10 4))
                     :summing (* scale (elt digits i))))
        (scale (- length weight 1)))
    (unless (zerop sign)
      (setf total (- total)))
    (/ total (expt 10000 scale))))

;; Since date and time types are the most likely to require custom
;; readers, there is a hook for easily adding binary readers for them.

(defun set-date-reader (f table)
  (set-sql-reader 1082 (binary-reader ((days int 4))
                         (funcall f days))
                  :table table
                  :binary-p t))

(defun interpret-usec-bits (bits)
  "Decode a 64 bit time-related value based on the timestamp format
used. Correct for sign bit when using integer format."
  (ecase *timestamp-format*
    (:float (round (* (ieee-floats:decode-float64 bits) 1000000)))
    (:integer (if (logbitp 63 bits)
                  (dpb bits (byte 63 0) -1)
                  bits))))

(defun set-interval-reader (f table)
  (set-sql-reader 1186 (binary-reader ((usec-bits uint 8) (days int 4) (months int 4))
                         (funcall f months days (interpret-usec-bits usec-bits)))
                  :table table
                  :binary-p t))

(defun set-usec-reader (oid f table)
  (set-sql-reader oid (binary-reader ((usec-bits uint 8))
                        (funcall f (interpret-usec-bits usec-bits)))
                  :table table
                  :binary-p t))

;; Public interface for adding date/time readers

(defun set-sql-datetime-readers (&key date timestamp timestamp-with-timezone interval time
                                 (table *sql-readtable*))
  (when date (set-date-reader date table))
  (when timestamp (set-usec-reader 1114 timestamp table))
  (when timestamp-with-timezone (set-usec-reader 1184 timestamp-with-timezone table))
  (when interval (set-interval-reader interval table))
  (when time (set-usec-reader 1083 time table))
  table)

;; Provide meaningful defaults for the date/time readers.

(defconstant +start-of-2000+ (encode-universal-time 0 0 0 1 1 2000 0))
(defconstant +seconds-in-day+ (* 60 60 24))

(set-sql-datetime-readers
 :date (lambda (days-since-2000)
         (+ +start-of-2000+ (* days-since-2000 +seconds-in-day+)))
 :timestamp (lambda (useconds-since-2000)
              (+ +start-of-2000+ (floor useconds-since-2000 1000000)))
 :interval (lambda (months days useconds)
             (multiple-value-bind (sec us) (floor useconds 1000000)
               `((:months ,months) (:days ,days) (:seconds ,sec) (:useconds ,us)))))

;; Working with tables.

(defun copy-sql-readtable (&optional (table *sql-readtable*))
  (let ((new-table (make-hash-table)))
    (maphash (lambda (oid interpreter) (setf (gethash oid new-table) interpreter))
             table)
    new-table))

(defparameter *default-sql-readtable* (copy-sql-readtable *sql-readtable*)
  "A copy of the default readtable that client code can fall back
  on.")

(defun default-sql-readtable ()
  *default-sql-readtable*)
