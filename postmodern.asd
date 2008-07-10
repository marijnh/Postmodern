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
  :depends-on (:cl-postgres :s-sql #+postmodern-use-mop :closer-mop
                            #+postmodern-thread-safe :bordeaux-threads)
  :components 
  ((:module :postmodern
            :components ((:file "package")
                         (:file "connect" :depends-on ("package"))
                         (:file "query" :depends-on ("connect"))
                         (:file "prepare" :depends-on ("query"))
                         (:file "util" :depends-on ("query"))
                         #+postmodern-use-mop
                         (:file "table" :depends-on ("util"))
                         (:file "deftable" :depends-on
                                ("query" #+postmodern-use-mop "table"))))))

(defsystem :postmodern-tests
  :depends-on (:postmodern :fiveam)
  :components
  ((:module :postmodern
            :components ((:file "tests")))))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :postmodern))))
  (asdf:oos 'asdf:load-op :postmodern)
  (asdf:oos 'asdf:load-op :cl-postgres-tests)
  (asdf:oos 'asdf:load-op :postmodern-tests)
  (funcall (intern (string :prompt-connection) (string :cl-postgres-tests))
           (eval (intern (string :*test-connection*) (string :postmodern-tests))))
  (funcall (intern (string :run!) (string :it.bese.FiveAM)) :postmodern))
