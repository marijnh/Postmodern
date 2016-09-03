(defpackage :postmodern-system
  (:use :common-lisp :asdf)
  (:export :*threads*))
(in-package :postmodern-system)

;; Change this to manually turn threading support on or off.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or allegro armedbear cmu corman (and digitool ccl-5.1)
        ecl lispworks openmcl sbcl)
  (pushnew :postmodern-thread-safe *features*)

  #+(or allegro clisp ecl lispworks mcl openmcl cmu sbcl)
  (pushnew :postmodern-use-mop *features*))

(defsystem :postmodern
  :description "PostgreSQL programming API"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :license "BSD"
  :depends-on (:cl-postgres :s-sql #+postmodern-use-mop :closer-mop
                            #+postmodern-thread-safe :bordeaux-threads)
  :components
  ((:module :postmodern
            :components ((:file "package")
                         (:file "connect" :depends-on ("package"))
                         (:file "query" :depends-on ("connect"))
                         (:file "prepare" :depends-on ("query"))
                         (:file "util" :depends-on ("query"))
                         (:file "transaction" :depends-on ("query"))
                         (:file "namespace" :depends-on ("query"))
                         #+postmodern-use-mop
                         (:file "table" :depends-on ("util" "transaction"))
                         (:file "deftable" :depends-on
                                ("query" #+postmodern-use-mop "table")))))
  :in-order-to ((test-op (test-op :postmodern-tests))))

(defsystem :postmodern-tests
  :depends-on (:postmodern :fiveam :simple-date :simple-date-postgres-glue
               :cl-postgres-tests)
  :components
  ((:module :postmodern
            :components ((:file "tests"))))
  :perform (test-op (o c)
             (uiop:symbol-call :cl-postgres-tests '#:prompt-connection)
             (uiop:symbol-call :fiveam '#:run! :postmodern)))
