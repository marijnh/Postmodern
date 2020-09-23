;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

(defun escape-bytes (bytes)
  "Escape an array of octets in PostgreSQL's horribly inefficient
textual format for binary data."
  (let ((*print-pretty* nil))
    (with-output-to-string (out)
      (loop :for byte :of-type fixnum :across bytes
            :do (if (or (< byte 32) (> byte 126) (= byte 39) (= byte 92))
                    (progn
                      (princ #\\ out)
                      (princ (digit-char (ldb (byte 3 6) byte) 8) out)
                      (princ (digit-char (ldb (byte 3 3) byte) 8) out)
                      (princ (digit-char (ldb (byte 3 0) byte) 8) out))
                    (princ (code-char byte) out))))))

(defparameter *silently-truncate-ratios* t "Given a ratio, a stream and a
digital-length-limit, if *silently-truncate-ratios* is true,
will return a potentially truncated ratio. If false and the digital-length-limit
is reached, it will throw an error noting the loss of precision and offering to
continue or reset *silently-truncate-ratios* to true. Code contributed by
Attila Lendvai.")

(defun write-ratio-as-floating-point (number stream digit-length-limit)
  "Given a ratio, a stream and a digital-length-limit, if
*silently-truncate-ratios* is true, will return a potentially truncated ratio.
If false and the digital-length-limit is reached, it will throw an error noting
the loss of precision and offering to continue or reset
*silently-truncate-ratios* to true. Code contributed by Attila Lendvai."
  (declare #.*optimize* (type fixnum digit-length-limit))
  (check-type number ratio)
  (let ((silently-truncate? *silently-truncate-ratios*))
    (flet ((fail ()
                 (unless silently-truncate?
                   (restart-case
                    (error 'database-error :message
                           (format nil "Can not write the ratio ~A as a floating point number with only ~A available digits. You may want to (setf ~S t) if you don't mind the loss of precision."
                                   number digit-length-limit '*silently-truncate-ratios*))
                    (continue ()
                              :report (lambda (stream)
                                        (write-string "Ignore this precision loss and continue" stream))
                              (setf silently-truncate? t))
                    (disable-assertion ()
                                       :report (lambda (stream)
                                                 (write-string "Set ~S to true (the precision loss of ratios will be silently ignored in this Lisp VM)." stream))
                                       (setf silently-truncate? t)
                                       (setf *silently-truncate-ratios* t))))))
      (multiple-value-bind (quotient remainder)
          (truncate (if (< number 0)
                        (progn
                          (write-char #\- stream)
                          (- number))
                      number))
        (let* ((quotient-part (princ-to-string quotient))
               (remaining-digit-length (- digit-length-limit
                                          (length quotient-part))))
          (write-string quotient-part stream)
          (when (<= remaining-digit-length 0)
            (fail))
          (unless (zerop remainder)
            (write-char #\. stream))
          (loop
           :for decimal-digits :upfrom 1
           :until (zerop remainder)
           :do (progn
                 (when (> decimal-digits remaining-digit-length)
                   (fail)
                   (return))
                 (multiple-value-bind (quotient rem) (floor (* remainder 10))
                   (princ quotient stream)
                   (setf remainder rem)))))))))

(defparameter *silently-truncate-rationals* t "When a rational number is passed into a query (as per to-sql-string), but it
can not be expressed within 38 decimal digits (for example 1/3), it will be truncated, and lose some precision. Set this variable to nil to suppress
that behaviour and raise an error instead.")

(defun write-rational-as-floating-point (number stream digit-length-limit)
  "DEPRECATED. The same as write-ratio-as-floating point. Note the difference between rational and ratio.
Kept for backwards compatibility.
Given a ratio, a stream and a digital-length-limit, if *silently-truncate-rationals* is true,
will return a potentially truncated ratio. If false and the digital-length-limit is reached,
it will throw an error noting the loss of precision and offering to continue or reset
*silently-truncate-rationals* to true. Code contributed by Attila Lendvai."
  (declare #.*optimize* (type fixnum digit-length-limit))
  (check-type number ratio)
  (let ((silently-truncate? *silently-truncate-rationals*))
    (flet ((fail ()
                 (unless silently-truncate?
                   (restart-case
                    (error 'database-error :message
                           (format nil "Can not write the rational ~A as a floating point number with only ~A available digits. You may want to (setf ~S t) if you don't mind the loss of precision."
                                   number digit-length-limit '*silently-truncate-rationals*))
                    (continue ()
                              :report (lambda (stream)
                                        (write-string "Ignore this precision loss and continue" stream))
                              (setf silently-truncate? t))
                    (disable-assertion ()
                                       :report (lambda (stream)
                                                 (write-string "Set ~S to true (the precision loss of ratios will be silently ignored in this Lisp VM)." stream))
                                       (setf silently-truncate? t)
                                       (setf *silently-truncate-rationals* t))))))
      (multiple-value-bind (quotient remainder)
          (truncate (if (< number 0)
                        (progn
                          (write-char #\- stream)
                          (- number))
                      number))
        (let* ((quotient-part (princ-to-string quotient))
               (remaining-digit-length (- digit-length-limit (length quotient-part))))
          (write-string quotient-part stream)
          (when (<= remaining-digit-length 0)
            (fail))
          (unless (zerop remainder)
            (write-char #\. stream))
          (loop
           :for decimal-digits :upfrom 1
           :until (zerop remainder)
           :do (progn
                 (when (> decimal-digits remaining-digit-length)
                   (fail)
                   (return))
                 (multiple-value-bind (quotient rem) (floor (* remainder 10))
                   (princ quotient stream)
                   (setf remainder rem)))))))))

(defun write-quoted (string out)
  (write-char #\" out)
  (loop :for ch :across string :do
     (when (member ch '(#\" #\\))
       (write-char #\\ out))
     (write-char ch out))
  (write-char #\" out))

(defgeneric to-sql-string (arg)
  (:documentation "Convert a Lisp value to its textual unescaped SQL
representation. Returns a second value indicating whether this value should be
escaped if it is to be put directly into a query. Generally any string is going
to be designated to be escaped.

You can define to-sql-string methods for your own datatypes if you want to be
able to pass them to exec-prepared. When a non-NIL second value is returned,
this may be T to indicate that the first value should simply be escaped as a
string, or a second string providing a type prefix for the value. (This is
used by S-SQL.)")
  (:method ((arg string))
    (values arg t))
  (:method ((arg vector))
    (if (typep arg '(vector (unsigned-byte 8)))
        (values (escape-bytes arg) t)
        (values
         (with-output-to-string (out)
           (write-char #\{  out)
           (loop :for sep := "" :then #\, :for x :across arg :do
              (princ sep out)
              (multiple-value-bind (string escape) (to-sql-string x)
              (if escape (write-quoted string out) (write-string string out))))
           (write-char #\} out))
         t)))
  (:method ((arg array))
    (values
     (with-output-to-string (out)
       (labels ((recur (dims off)
                  (write-char #\{ out)
                  (if (cdr dims)
                      (let ((factor (reduce #'* (cdr dims))))
                        (loop :for i :below (car dims) :for sep := ""
                                :then #\, :do
                           (princ sep out)
                           (recur (cdr dims) (+ off (* factor i)))))
                      (loop :for sep := "" :then #\, :for i :from off
                              :below (+ off (car dims)) :do
                         (princ sep out)
                         (multiple-value-bind (string escape)
                             (to-sql-string (row-major-aref arg i))
                           (if escape (write-quoted string out)
                               (write-string string out)))))
                  (write-char #\} out)))
         (recur (array-dimensions arg) 0)))
     t))
  (:method ((arg integer))
    (princ-to-string arg))
  (:method ((arg float))
    (format nil "~f" arg))
  #-clisp (:method ((arg double-float)) ;; CLISP doesn't allow methods on double-float
            (format nil "~,,,,,,'EE" arg))
  (:method ((arg ratio))
    ;; Possible optimization: we could probably build up the same binary
    ;; structure postgres sends us instead of sending it as a string. See
    ;; the "numeric" interpreter for more details...
    (with-output-to-string (result)
      ;; PostgreSQL happily handles 200+ decimal digits, but the SQL standard
      ;; only requires 38 digits from the NUMERIC type, and Oracle also doesn't
      ;; handle more. For practical reasons we also draw the line there. If
      ;; someone needs full rational numbers then
      ;; 200 wouldn't help them much more than 38...
      (write-rational-as-floating-point arg result 38)))
  (:method ((arg (eql t)))
    "true")
  (:method ((arg (eql nil)))
    "false")
  (:method ((arg (eql :null)))
    "NULL")
  (:method ((arg t))
    (error "Value ~S can not be converted to an SQL literal." arg)))

(defgeneric serialize-for-postgres (arg)
  (:documentation "Conversion function used to turn a lisp value into a value
that PostgreSQL understands when sent through its socket connection. May return
a string or a (vector (unsigned-byte 8)).")
  (:method ((arg integer))
    (int-to-vector arg))
  (:method ((arg single-float))
    (int32-to-vector (cl-postgres-ieee-floats:encode-float32 arg)))
  #-clisp   (:method ((arg double-float)) ;; CLISP doesn't allow methods on double-float
              (int64-to-vector (cl-postgres-ieee-floats:encode-float64 arg)))
  (:method (arg)
    (cond ((typep arg 'boolean)
           (if arg (int8-to-vector 1)
               (int8-to-vector 0)))
          (t (to-sql-string arg)))))

(defun int64-to-vector (int)
  "Takes a 64 byte integer and returns a vector of unsigned bytes with a length of 8"
  (declare (type (signed-byte 64) int))
  (let ((intv (make-array '(8) :element-type '(unsigned-byte 8))))
    (setf (aref intv 0) (ldb (byte 8 56) int))
    (setf (aref intv 1) (ldb (byte 8 48) int))
    (setf (aref intv 2) (ldb (byte 8 40) int))
    (setf (aref intv 3) (ldb (byte 8 32) int))
    (setf (aref intv 4) (ldb (byte 8 24) int))
    (setf (aref intv 5) (ldb (byte 8 16) int))
    (setf (aref intv 6) (ldb (byte 8 8) int))
    (setf (aref intv 7) (ldb (byte 8 0) int))
    intv))

(defun int32-to-vector (int)
  "Takes a 32 byte integer and returns a vector of unsigned bytes with a length of 4"
  (declare (type (signed-byte 32) int))
  (let ((intv (make-array '(4) :element-type '(unsigned-byte 8))))
    (setf (aref intv 0) (ldb (byte 8 24) int))
    (setf (aref intv 1) (ldb (byte 8 16) int))
    (setf (aref intv 2) (ldb (byte 8 8) int))
    (setf (aref intv 3) (ldb (byte 8 0) int))
    intv))

(defun int16-to-vector (int)
    "Takes a 16 byte positive integer and returns a vector of unsigned bytes
with a length of 2. How should it work with negative numbers?"
  (declare (type (signed-byte 16) int))
  (let ((intv (make-array '(2) :element-type '(unsigned-byte 8))))
    (setf (aref intv 0) (ldb (byte 8 8) int))
    (setf (aref intv 1) (ldb (byte 8 0) int))
    intv))

(defun int8-to-vector (int)
    "Takes a 16 byte positive integer and returns a vector of unsigned bytes
with a length of 2. How should it work with negative numbers?"
  (declare (type (signed-byte 8) int))
  (let ((intv (make-array '(1) :element-type '(unsigned-byte 8))))
    (setf (aref intv 0) (ldb (byte 8 0) int))
    intv))


(defun int-to-vector (int)
  "Takes an integer and returns the size of the integer for postgresql
purposes (int2, int4, int8). What to do with negative numbers"
  (when (integerp int)
    (cond ((and (> int -32769)
                (< int 32768))
           (int16-to-vector int))
          ((and (> int -2147483649)
                (< int 2147483648))
           (int32-to-vector int))
          ((and (> int -9223372036854775809)
                (< int 9223372036854775808))
           (int64-to-vector int))
          (t nil))))

(defun get-int-size (int)
  "Takes an integer and returns the size of the integer for postgresql
purposes (int2, int4, int8)"
  (when (integerp int)
    (cond ((and (> int -32769)
                (< int 32768))
           'int2)
          ((and (> int -2147483649)
                (< int 2147483648))
           'int4)
          ((and (> int -9223372036854775809)
                (< int 9223372036854775808))
           'int8)
          (t nil))))

(defun param-to-oid (param)
  (cond ((and (integerp param)
              (> param -32760)
              (< param 32768))
         cl-postgres-oid:+int2+)
        ((and (integerp param)
              (> param -2147483649)
              (< param 2147483648))
         cl-postgres-oid:+int4+)
        ((and (integerp param)
              (> param -9223372036854775809)
              (< param 9223372036854775808))
         cl-postgres-oid:+int8+)
        ((typep param 'single-float)
         cl-postgres-oid:+float4+)
        ((typep param 'double-float)
         cl-postgres-oid:+float8+)
        ((typep param 'boolean)
         cl-postgres-oid:+bool+)
        (t 0)))
