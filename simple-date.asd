(defpackage :simple-date-system
  (:use :common-lisp :asdf))
(in-package :simple-date-system)

(defsystem :simple-date
  :components 
  ((:module :simple-date
            :components ((:file "simple-date")))))

(defsystem :simple-date-postgres-glue
  :depends-on (:simple-date :cl-postgres)
  :components
  ((:module :simple-date
            :components
            ((:file "cl-postgres-glue")))))

(defsystem :simple-date-tests
  :depends-on (:eos :simple-date)
  :components
  ((:module :simple-date
            :components ((:file "tests")))))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :simple-date))))
  (asdf:oos 'asdf:load-op :simple-date-tests)
  (funcall (intern (string :run!) (string :Eos)) :simple-date))

(defmethod perform :after ((op asdf:load-op) (system (eql (find-system :simple-date))))
  (when (and (find-package :cl-postgres)
             (not (find-symbol (symbol-name '#:+postgres-day-offset+) :simple-date)))
    (asdf:oos 'asdf:load-op :simple-date-postgres-glue)))
