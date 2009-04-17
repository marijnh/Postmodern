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
  :depends-on (:fiveam :simple-date)
  :components
  ((:module :simple-date
            :components ((:file "tests")))))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :simple-date))))
  (asdf:oos 'asdf:load-op :simple-date-tests)
  (funcall (intern (string :run!) (string :it.bese.FiveAM)) :simple-date))

(defmethod perform :after ((op asdf:load-op) (system (eql (find-system :simple-date))))
  (let ((cl-postgres (asdf:find-system :cl-postgres nil)))
   (when (and cl-postgres
              (asdf:operation-done-p (make-instance 'asdf:load-op) cl-postgres))
     (asdf:oos 'asdf:load-op :simple-date-postgres-glue))))