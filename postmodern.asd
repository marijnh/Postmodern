;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

(defpackage :postmodern-system
  (:use :common-lisp :asdf)
  (:export :*threads*))
(in-package :postmodern-system)

;; Change this to manually turn threading support on or off.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or allegro armedbear clasp cmu corman (and digitool ccl-5.1)
        ecl lispworks openmcl sbcl genera)
  (pushnew :postmodern-thread-safe *features*)

  #+(or allegro clasp clisp ecl lispworks mcl openmcl cmu sbcl armedbear)
  (pushnew :postmodern-use-mop *features*))

(defsystem "postmodern"
  :description "PostgreSQL programming API"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :maintainer "Sabra Crolleton <sabra.crolleton@gmail.com>"
  :homepage  "https://github.com/marijnh/Postmodern"
  :license "zlib"
  :version "1.32.7"
  :depends-on ("alexandria"
               "cl-postgres"
               "s-sql"
               "global-vars"
               "split-sequence"
               "cl-unicode"
               (:feature :postmodern-use-mop "closer-mop")
               (:feature :postmodern-thread-safe "bordeaux-threads"))
  :components
  ((:module "postmodern"
            :components ((:file "package")
                         (:file "connect" :depends-on ("package"))
                         (:file "json-encoder" :depends-on ("package"))
                         (:file "query" :depends-on ("connect" "json-encoder"))
                         (:file "prepare" :depends-on ("query"))
                         (:file "roles" :depends-on ("query"))
                         (:file "util" :depends-on ("query" "roles"))
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
                            "cl-postgres/tests" "s-sql/tests" "local-time"
                            "cl-postgres+local-time")
  :components
  ((:module "postmodern/tests"
            :components ((:file "test-package")
                         (:file "tests")
                         (:file "test-prepared-statements" :depends-on ("test-package"))
                         (:file "test-dao" :depends-on ("test-package")
                          :if-feature :postmodern-use-mop)
                         (:file "test-return-types" :depends-on ("test-package"))
                         (:file "test-return-types-timestamps" :depends-on ("test-package"))
                         (:file "test-transactions" :depends-on ("test-package"))
                         (:file "test-roles" :depends-on ("test-package"))
                         (:file "test-execute-file" :depends-on ("test-package")))))
  :perform (test-op (o c)
             (uiop:symbol-call :cl-postgres-tests '#:prompt-connection)
             (uiop:symbol-call :fiveam '#:run! :postmodern)))
