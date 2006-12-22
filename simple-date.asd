(defpackage :simple-date-system
  (:use :common-lisp :asdf))
(in-package :simple-date-system)

(defsystem :simple-date
  :components 
  ((:module :simple-date
            :components ((:file "simple-date")))))

(defsystem :simple-date-tests
  :depends-on (:fiveam :simple-date)
  :components
  ((:module :simple-date
            :components ((:file "tests")))))
