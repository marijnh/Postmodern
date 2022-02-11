;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

(defpackage :s-sql-system
  (:use :common-lisp :asdf))
(in-package :s-sql-system)

(defsystem "s-sql"
  :description "Lispy DSL for SQL"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :maintainer "Sabra Crolleton <sabra.crolleton@gmail.com>"
  :license "zlib"
  :version "1.33.4"
  :depends-on ("cl-postgres"
               "alexandria")
  :components
  ((:module "s-sql"
	  :components ((:file "package")
                 (:file "config" :depends-on ("package"))
			           (:file "s-sql" :depends-on ("package" "config")))))
  :in-order-to ((test-op (test-op "s-sql/tests"))))

(defsystem "s-sql/tests"
  :depends-on ("postmodern" "s-sql" "cl-postgres/tests" "fiveam")
  :components
  ((:module "s-sql/tests"
            :components ((:file "test-package")
			                   (:file "tests")
                         (:file "test-arrays" :depends-on ("tests"))
                         (:file "test-intervals" :depends-on ("tests"))
                         (:file "test-tables" :depends-on ("tests")))))
  :perform (test-op (o c)
             (uiop:symbol-call :s-sql-tests '#:prompt-connection)
             (uiop:symbol-call :fiveam '#:run! :s-sql)))
