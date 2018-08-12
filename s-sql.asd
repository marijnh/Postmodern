(defpackage :s-sql-system
  (:use :common-lisp :asdf))
(in-package :s-sql-system)


(defsystem "s-sql"
  :depends-on ("cl-postgres")
  :components
  ((:module "s-sql"
    :components ((:file "s-sql")))))

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
