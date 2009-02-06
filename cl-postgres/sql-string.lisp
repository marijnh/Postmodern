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

(defun write-rational-as-foating-point (input-number stream digit-length-limit)
  (declare (optimize speed))
  (flet ((fail ()
           (error "Can not write the rational ~F with only ~A digits" (coerce input-number 'double-float) digit-length-limit)))
    (declare (inline fail))
    (let ((number input-number))
      (when (< number 0)
        (write-char #\- stream)
        (setf number (abs number)))
      (multiple-value-bind (quotient remainder) (floor number)
        (let* ((quotient-part (with-output-to-string (stream)
                                (write quotient :stream stream)))
               (quotient-digit-length (length quotient-part))
               (decimal-length-limit (- digit-length-limit quotient-digit-length)))
          (when (<= decimal-length-limit 0)
            (fail))
          (write-string quotient-part stream)
          (write-char #\. stream)
          (setf number remainder)
          (loop
             :for decimal-digits :upfrom 1
             :until (zerop number)
             :do (progn
                   (when (> decimal-digits decimal-length-limit)
                     (fail))
                   ;; (format *debug-io* "*** step ~A, number is ~A~%" decimal-digits number)
                   (setf number (* number 10))
                   (multiple-value-bind (quotient remainder) (floor number)
                     (write quotient :stream stream)
                     (setf number remainder)))))))))

(defgeneric to-sql-string (arg)
  (:documentation "Turn a lisp value into a string containing its SQL
representation. Returns an optional second value that indicates
whether the string should be escaped before being put into a query.")
  (:method ((arg string))
    (values arg t))
  (:method ((arg vector))
    (assert (typep arg '(vector (unsigned-byte 8))))
    (values (escape-bytes arg) t))
  (:method ((arg integer))
    (princ-to-string arg))
  (:method ((arg float))
    (format nil "~f" arg))
  (:method ((arg ratio))
    ;; Possible optimization: we could probably build up the same binary structure postgres
    ;; sends us instead of sending it as a string. See the "numeric" interpreter for more details...
    (with-output-to-string (result)
      ;; PostgreSQL happily handles 200+ decimal digits, but the SQL standard only requires
      ;; 38 digits from the NUMERIC type, and Oracle also doesn't handle more. For practical
      ;; reasons we also draw the line there. If someone needs full rational numbers then
      ;; 200 wouldn't help them much more than 38...
      (write-rational-as-foating-point arg result 38)))
  (:method ((arg (eql t)))
    "true")
  (:method ((arg (eql nil)))
    "false")
  (:method ((arg (eql :null)))
    "NULL")
  (:method ((arg t))
    (error "Value ~S can not be converted to an SQL literal." arg)))
