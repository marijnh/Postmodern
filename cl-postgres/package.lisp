(defpackage :cl-postgres
  (:use :common-lisp)
  (:export #:database-error
           #:database-connection-lost
           #:database-error-message
           #:database-error-code
           #:database-error-detail
           #:database-error-query
           #:database-error-cause
           #:database-connection
           #:database-connection-error
           #:database-socket-error
           #:connection-meta
           #:connection-parameters
           #:open-database
           #:reopen-database
           #:database-open-p
           #:close-database
           #:exec-query
           #:prepare-query
           #:exec-prepared
           #:field-name
           #:field-type
           #:row-reader
           #:def-row-reader
           #:next-row
           #:next-field
           #:list-row-reader
           #:vector-row-reader
           #:alist-row-reader
           #:postgresql-warning
           #:ignore-row-reader
           #:*sql-readtable*
           #:copy-sql-readtable
           #:default-sql-readtable
           #:set-sql-reader
           #:set-sql-datetime-readers
           #:to-sql-string
           #:*query-log*))

(defpackage :cl-postgres-error
  (:use :common-lisp :cl-postgres)
  (:export #:admin-shutdown
           #:cannot-connect-now
           #:check-violation
           #:crash-shutdown
           #:data-exception
           #:db-division-by-zero
           #:feature-not-supported
           #:floating-point-exception
           #:foreign-key-violation
           #:insufficient-resources
           #:insufficient-privilege
           #:integrity-violation
           #:internal-error
           #:invalid-datetime-format
           #:lock-not-available
           #:not-null-violation
           #:numeric-value-out-of-range
           #:object-in-use
           #:object-state-error
           #:operator-intervention
           #:program-limit-exceeded
           #:query-canceled
           #:restrict-violation
           #:server-shutdown
           #:syntax-error-or-access-violation
           #:system-error
           #:unique-violation))

(in-package :cl-postgres)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Optimization settings (only used by functions that need it).
  (defparameter *optimize*
    '(optimize (speed 3) (safety 0) (space 1) (debug 1)
      (compilation-speed 0))))

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
