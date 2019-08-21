(defpackage :postmodern-system
  (:use :common-lisp :asdf)
  (:export :*threads*))
(in-package :postmodern-system)

;; Change this to manually turn threading support on or off.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or allegro armedbear clasp cmu corman (and digitool ccl-5.1)
        ecl lispworks openmcl sbcl genera)
  (pushnew :postmodern-thread-safe *features*)

  #+(or allegro clasp clisp ecl lispworks mcl openmcl cmu sbcl)
  (pushnew :postmodern-use-mop *features*))

(defsystem "postmodern"
  :description "PostgreSQL programming API"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :maintainer "Sabra Crolleton <sabra.crolleton@gmail.com>"
  :license "zlib"
  :depends-on ("alexandria"
               "cl-postgres"
               "s-sql"
               "global-vars"
               "split-sequence"
               (:feature :postmodern-use-mop "closer-mop")
               (:feature :postmodern-thread-safe "bordeaux-threads"))
  :components
  ((:module "postmodern"
            :components ((:file "package")
                         (:file "connect" :depends-on ("package"))
                         (:file "query" :depends-on ("connect"))
                         (:file "prepare" :depends-on ("query"))
                         (:file "util" :depends-on ("query"))
                         (:file "transaction" :depends-on ("query"))
                         (:file "namespace" :depends-on ("query"))
                         (:file "execute-file" :depends-on ("query"))
                         (:file "table" :depends-on ("util" "transaction" "query")
                                :if-feature :postmodern-use-mop)
                         (:file "deftable" :depends-on
                                ("query" (:feature :postmodern-use-mop "table"))))))
  :in-order-to ((test-op (test-op "postmodern/tests"))))

(defsystem "postmodern/tests"
  :depends-on ("postmodern" "fiveam" "simple-date" "simple-date/postgres-glue"
                            "cl-postgres/tests" "s-sql/tests")
  :components
  ((:module "postmodern/tests"
            :components ((:file "tests")
                         (:file "test-dao")
                         (:file "test-execute-file"))))
  :perform (test-op (o c)
             (uiop:symbol-call :cl-postgres-tests '#:prompt-connection)
             (uiop:symbol-call :fiveam '#:run! :postmodern)))
