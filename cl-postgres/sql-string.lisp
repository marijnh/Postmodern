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

(defparameter *silently-truncate-rationals* t)

(defun write-rational-as-floating-point (number stream digit-length-limit)
  (declare #.*optimize*)
  (flet ((fail ()
           (if *silently-truncate-rationals*
               (return-from write-rational-as-floating-point)
               (error 'database-error :message 
                      (format nil "Can not write the rational ~a with only ~a digits"
                              number digit-length-limit)))))
    (multiple-value-bind (quotient remainder)
        (truncate (if (< number 0) (prog1 (- number) (write-char #\- stream)) number))
      (let* ((quotient-part (princ-to-string quotient))
             (decimal-length-limit (- digit-length-limit (length quotient-part))))
        (write-string quotient-part stream)
        (when (<= decimal-length-limit 0) (fail))
        (unless (zerop remainder) (write-char #\. stream))
        (loop :for decimal-digits :upfrom 1 :until (zerop remainder)
              :do (when (> decimal-digits decimal-length-limit) (fail))
              :do (multiple-value-bind (quotient rem) (floor (* remainder 10))
                    (princ quotient stream)
                    (setf remainder rem)))))))

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
      (write-rational-as-floating-point arg result 38)))
  (:method ((arg (eql t)))
    "true")
  (:method ((arg (eql nil)))
    "false")
  (:method ((arg (eql :null)))
    "NULL")
  (:method ((arg t))
    (error "Value ~S can not be converted to an SQL literal." arg)))
