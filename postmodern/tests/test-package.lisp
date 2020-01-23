;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER; -*-

(defpackage :postmodern-tests
  (:use :common-lisp :fiveam :postmodern :simple-date :cl-postgres-tests)
  (:shadow #:with-test-connection))
