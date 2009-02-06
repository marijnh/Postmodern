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
           #:log-query
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
           #:*silently-truncate-rationals*
           #:*query-callback*
           #:*query-log*
           #:*ssl-certificate-file*
           #:*ssl-key-file*))

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
           #:transaction-rollback
           #:serialization-failure
           #:transaction-integrity-constraint-violation
           #:statement-completion-unknown
           #:deadlock-detected
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
