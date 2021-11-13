;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER; -*-

(defpackage :cl-postgres
  (:use :common-lisp)
  (:export #:database-error
           #:database-connection-lost
           #:database-error-message
           #:database-error-code
           #:database-error-detail
           #:database-error-query
           #:database-error-cause
           #:database-error-constraint-name
           #:database-error-extract-name
           #:database-connection
           #:database-connection-error
           #:database-socket-error
           #:connection-meta
           #:connection-parameters
           #:connection-use-binary
           #:use-binary-parameters
           #:get-postgresql-version
           #:postgresql-version-at-least
           #:open-database
           #:reopen-database
           #:database-open-p
           #:close-database
           #:wait-for-notification
           #:exec-query
           #:prepare-query
           #:unprepare-query
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
           #:postgresql-notification
           #:postgresql-notification-channel
           #:postgresql-notification-payload
           #:postgresql-notification-pid
           #:postgresql-warning
           #:ignore-row-reader
           #:*sql-readtable*
           #:copy-sql-readtable
           #:default-sql-readtable
           #:set-sql-reader
           #:set-sql-datetime-readers
           #:serialize-for-postgres
           #:to-sql-string
           #:*read-row-values-as-binary*
           #:with-binary-row-values
           #:with-text-row-values
           #:*silently-truncate-ratios*
           #:*query-callback*
           #:*query-log*
           #:open-db-writer
           #:db-write-row
           #:close-db-writer
           #:*ssl-certificate-file*
           #:*ssl-key-file*
           #:*retry-connect-times*
           #:*retry-connect-delay*
           #:string-mapped-to-nothing
           #:string-mapped-to-space
           #:saslprep-normalize
           #:string-printable-ascii-p
           #:int4
           #:int8
           #:uuid-string
           #:-uuip-p
           #:types-match-p
           #:oid-types-match-p
           #:parameter-lists-match-oid-types-p
           #:parameter-list-types
           #:param-to-oid
           #:*on-evidence-of-man-in-the-middle-attack*
           #+(and sbcl unix) #:*unix-socket-dir*))

(defpackage :cl-postgres-error
  (:use :common-lisp :cl-postgres)
  (:export #:admin-shutdown
           #:cannot-connect-now
           #:check-violation
           #:columns-error
           #:crash-shutdown
           #:data-exception
           #:db-division-by-zero
           #:undefined-column
           #:undefined-table
           #:duplicate-column
           #:duplicate-cursor
           #:duplicate-database
           #:duplicate-function
           #:duplicate-prepared-statement
           #:duplicate-schema
           #:duplicate-table
           #:duplicate-alias
           #:duplicate-object
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
           #:invalid-sql-statement-name
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
           #:unique-violation
           #:protocol-violation
           #:connection-exception
           #:connection-does-not-exist
           #:connection-failure
           #:sqlclient-unable-to-establish-sqlconnection
           #:sqlserver-rejected-establishment-of-sqlconnection
           #:transaction-resolution-unknown))

(defpackage :cl-postgres-oid
  (:use :common-lisp)
  (:nicknames :oid)
  (:export #:+bool+
           #:+bytea+
           #:+char+
           #:+name+
           #:+int8+
           #:+int2+
           #:+int2vector+
           #:+int4+
           #:+regproc+
           #:+text+
           #:+oid+
           #:+tid+
           #:+xid+
           #:+cid+
           #:+oid-vector+
           #:+json+
           #:+xml+
           #:+pgnodetree+
           #:+pgddlcommand+
           #:+point+
           #:+lseg+
           #:+path+
           #:+box+
           #:+polygon+
           #:+line+
           #:+float4+
           #:+float8+
           #:+abstime+
           #:+reltime+
           #:+tinterval+
           #:+unknown+
           #:+circle+
           #:+cash+
           #:+macaddr+
           #:+inet+
           #:+cidr+
           #:+bool-array+
           #:+bytea-array+
           #:+char-array+
           #:+name-array+
           #:+int2-array+
           #:+int4-array+
           #:+text-array+
           #:+bpchar-array+
           #:+varchar-array+
           #:+int8-array+
           #:+point-array+
           #:+lseg-array+
           #:+box-array+
           #:+float4-array+
           #:+float8-array+
           #:+oid-array+
           #:+aclitem+
           #:+cstring-array+
           #:+bpchar+
           #:+varchar+
           #:+date+
           #:+time+
           #:+timestamp+
           #:+timestamp-array+
           #:+date-array+
           #:+time-array+
           #:+timestamptz+
           #:+timestamptz-array+
           #:+interval+
           #:+interval-array+
           #:+timetz+
           #:+bit+
           #:+bit-array+
           #:+varbit+
           #:+varbit-array+
           #:+numeric+
           #:+numeric-array+
           #:+refcursor+
           #:+regprocedure+
           #:+regoper+
           #:+regoperator+
           #:+regclass+
           #:+regtype+
           #:+regrole+
           #:+regnamespace+
           #:+regtype-array+
           #:+uuid+
           #:+lsn+
           #:+tsvector+
           #:+gtsvector+
           #:+tsquery+
           #:+regconfig+
           #:+regdictionary+
           #:+jsonb+
           #:+int4range+
           #:+record+
           #:+record-array+
           #:+cstring+
           #:+any+
           #:+any-array+
           #:+v-oid+
           #:+trigger+
           #:+evttrigger+
           #:+language-handler+
           #:+internal+
           #:+opaque+
           #:+anyelement+
           #:+anynon-array+
           #:+anyenum+
           #:+fdw-handler+
           #:+index-am-handler+
           #:+tsm-handler+
           #:+anyrange+))

(defpackage :cl-postgres.features
  (:use :common-lisp)
  (:export #:sbcl-available
           #:sbcl-ipv6-available))

(defpackage :cl-postgres-trivial-utf-8
  (:use :common-lisp)
  (:nicknames :clp-utf8)
  (:export #:utf-8-byte-length
           #:string-to-utf-8-bytes
           #:write-utf-8-bytes
           #:utf-8-group-size
           #:utf-8-bytes-to-string
           #:read-utf-8-string
           #:utf-8-decoding-error))

(defpackage :cl-postgres-ieee-floats
  (:use :common-lisp)
  (:nicknames :clp-ieee-floats)
  (:export :make-float-converters
	   :encode-float32
	   :decode-float32
	   :encode-float64
	   :decode-float64))

(in-package :cl-postgres)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Optimization settings (only used by functions that need it).
  (defparameter *optimize*
    '(optimize
      (speed 3)
      #-ecl(safety 0) #+ecl(safety 1)
      (space 1)
      (debug 1)
      (compilation-speed 0))))
