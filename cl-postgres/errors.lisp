(in-package :cl-postgres)

(defparameter *current-query* nil)

(define-condition database-error (error)
  ((error-code :initarg :code :initform nil :accessor database-error-code
               :documentation "The error code PostgreSQL associated
with this error, if any.")
   (message :initarg :message :accessor database-error-message
            :documentation "Description of this error.")
   (detail :initarg :detail :initform nil :accessor database-error-detail
           :documentation "More elaborate description of this error,
if any.")
   (query :initform *current-query* :accessor database-error-query
          :documentation "Query that led to the error, if any.")
   (position :initarg :position :initform nil :accessor database-error-position))
  (:report (lambda (err stream)
             (format stream "Database error~@[ ~A~]: ~A~@[~%~A~]~@[~%Query: ~A~]"
                     (database-error-code err)
                     (database-error-message err)
                     (database-error-detail err)
                     (database-error-query err))))
  (:documentation "This is the condition type that will be used to
signal virtually all database-related errors \(though in some cases
socket errors may be raised when a connection fails on the IP
level)."))

(in-package :cl-postgres-error)

(defparameter *error-table* (make-hash-table :test 'equal))
(defmacro deferror (code typename &optional (superclass 'database-error))
  `(progn (define-condition ,typename (,superclass) ())
          (setf (gethash ,code *error-table*) ',typename)))

(deferror "0A" feature-not-supported)
(deferror "22" data-exception)
(deferror "22012" db-division-by-zero data-exception)
(deferror "22007" invalid-datetime-format data-exception)
(deferror "22003" numeric-value-out-of-range data-exception)
(deferror "22P01" floating-point-exception data-exception)
(deferror "23" integrity-violation)
(deferror "23501" restrict-violation integrity-violation)
(deferror "23502" not-null-violation integrity-violation)
(deferror "23503" foreign-key-violation integrity-violation)
(deferror "23505" unique-violation integrity-violation)
(deferror "23514" check-violation integrity-violation)
(deferror "53" insufficient-resources)
(deferror "54" program-limit-exceeded)
(deferror "55" object-state-error)
(deferror "55006" object-in-use object-state-error)
(deferror "55P03" lock-not-available object-state-error)
(deferror "57" operator-intervention)
(deferror "57014" query-canceled operator-intervention)
(deferror "57P01" admin-shutdown operator-intervention)
(deferror "57P02" crash-shutdown operator-intervention)
(deferror "57P03" cannot-connect-now operator-intervention)
(deferror "58" system-error)
(deferror "XX" internal-error)

(defun get-error-type (code)
  (or (gethash code *error-table*)
      (gethash (subseq code 0 2) *error-table*)
      'database-error))
