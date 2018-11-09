(defpackage :s-sql-system
  (:use :common-lisp :asdf))
(in-package :s-sql-system)


(defsystem "s-sql"
  :description "Lispy dsl for sql"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :maintainer "Sabra Crolleton <sabra.crolleton@gmail.com>"
  :license "zlib"
  :depends-on ("cl-postgres"
               "alexandria")
  :components
  ((:module "s-sql"
    :components ((:file "s-sql"))))
  :in-order-to ((test-op (test-op "s-sql/tests"))))

(defsystem "s-sql/tests"
  :depends-on ("postmodern" "s-sql" "cl-postgres/tests" "fiveam" )
  :components
  ((:module "s-sql/tests"
            :components ((:file "tests")
                         (:file "test-arrays" :depends-on ("tests"))
                         (:file "test-intervals" :depends-on ("tests"))
                         (:file "test-tables" :depends-on ("tests")))))
  :perform (test-op (o c)
             (uiop:symbol-call :s-sql-tests '#:prompt-connection)
             (uiop:symbol-call :fiveam '#:run! :s-sql)))
