(defpackage :postmodern-system
  (:use :common-lisp :asdf)
  (:export :*threads*))
(in-package :postmodern-system)

;; Change this to manually turn threading support on or off.
(defparameter *threads* #+(or allegro armedbear cmu corman (and digitool ccl-5.1)
                              ecl lispworks openmcl sbcl) t
                        #-(or allegro armedbear cmu corman (and digitool ccl-5.1)
                              ecl lispworks openmcl sbcl) nil)

(defsystem :postmodern
  :depends-on (:cl-postgres :s-sql . #.(if *threads* '(:bordeaux-threads)))
  :components 
  ((:module :postmodern
            :components ((:file "package")
                         (:file "connect" :depends-on ("package"))
                         (:file "query" :depends-on ("connect"))
                         (:file "prepare" :depends-on ("query"))
                         (:file "util" :depends-on ("query"))
                         (:file "table" :depends-on ("util"))))))

(defsystem :postmodern-tests
  :depends-on (:postmodern :fiveam)
  :components
  ((:module :postmodern
            :components ((:file "tests")))))
