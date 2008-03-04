(in-package :cl-postgres)

(defparameter *current-query* nil)
(defparameter *query-log* nil)

(defmacro with-query (query &body body)
  (let ((q-name (gensym)))
    `(let* ((,q-name ,query)
            (*current-query* ,q-name))
       (when *query-log*
         (format *query-log* "CL-POSTGRES: ~a~%" ,q-name))
       ,@body)))

(define-condition database-error (error)
  ((error-code :initarg :code :initform nil :reader database-error-code
               :documentation "The error code PostgreSQL associated
with this error, if any.")
   (message :initarg :message :accessor database-error-message
            :documentation "Description of this error.")
   (detail :initarg :detail :initform nil :reader database-error-detail
           :documentation "More elaborate description of this error,
if any.")
   (query :initform *current-query* :reader database-error-query
          :documentation "Query that led to the error, if any.")
   (position :initarg :position :initform nil :reader database-error-position)
   (cause :initarg :cause :initform nil :reader database-error-cause))
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

(define-condition database-connection-error (database-error) ()
  (:documentation "Conditions of this type are signalled when an error
occurs that breaks the connection socket. They offer a :reconnect
restart."))
(define-condition database-connection-lost (database-connection-error) ()
  (:documentation "Raised when a query is initiated on a disconnected
connection object."))
(define-condition database-stream-error (database-connection-error) ()
  (:documentation "Used to wrap stream-errors, giving them a
database-connection-error superclass."))

(defun wrap-stream-error (stream-error)
  (make-instance 'database-stream-error
                 :message (princ-to-string stream-error)
                 :cause stream-error))

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
(deferror "42" syntax-error-or-access-violation)
(deferror "42501" insufficient-privilege syntax-error-or-access-violation)
(deferror "53" insufficient-resources)
(deferror "54" program-limit-exceeded)
(deferror "55" object-state-error)
(deferror "55006" object-in-use object-state-error)
(deferror "55P03" lock-not-available object-state-error)
(deferror "57" operator-intervention)
(deferror "57014" query-canceled operator-intervention)
(define-condition server-shutdown (operator-intervention database-connection-error) ())
(deferror "57P01" admin-shutdown server-shutdown)
(deferror "57P02" crash-shutdown server-shutdown)
(deferror "57P03" cannot-connect-now operator-intervention)
(deferror "58" system-error)
(deferror "XX" internal-error)

(defun get-error-type (code)
  (or (gethash code *error-table*)
      (gethash (subseq code 0 2) *error-table*)
      'database-error))

;;; Copyright (c) Marijn Haverbeke
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
