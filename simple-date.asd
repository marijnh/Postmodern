(defpackage :simple-date-system
  (:use :common-lisp :asdf))
(in-package :simple-date-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *glue*
    (if (find-package :cl-postgres)
        '((:file "cl-postgres-glue" :depends-on ("simple-date"))))))

(defsystem :simple-date
  :components 
  ((:module :simple-date
            :components ((:file "simple-date")
                         . #.*glue*))))

(defsystem :simple-date-tests
  :depends-on (:fiveam :simple-date)
  :components
  ((:module :simple-date
            :components ((:file "tests")))))

(defmethod perform ((op asdf:test-op) (system (eql (find-system :simple-date))))
  (asdf:oos 'asdf:load-op :simple-date-tests)
  (funcall (intern (string :run!) (string :it.bese.FiveAM)) :simple-date))

