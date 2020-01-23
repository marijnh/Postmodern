;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER; -*-

(defpackage :s-sql
  (:use :common-lisp)
  (:export #:smallint
           #:bigint
           #:numeric
           #:real
           #:double-precision
           #:bytea
           #:text
           #:varchar
           #:db-null
           #:sql-type-name
           #:*standard-sql-strings*
           #:*downcase-symbols*
           #:sql-escape-string
           #:sql-escape
           #:from-sql-name
           #:to-sql-name
           #:*escape-sql-names-p*
           #:sql
           #:sql-compile
           #:sql-template
           #:$$
           #:register-sql-operators
           #:enable-s-sql-syntax
           #:sql-error))
