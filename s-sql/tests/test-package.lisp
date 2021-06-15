;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER; -*-

(defpackage :s-sql-tests
  (:use :common-lisp :fiveam :s-sql
        :cl-postgres-error :cl-postgres-tests :postmodern)
  (:shadow #:with-test-connection))
