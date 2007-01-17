(defpackage :cl-postgres-system
  (:use :common-lisp :asdf)
  (:export :*unicode*))
(in-package :cl-postgres-system)

;; Change this to enable/disable unicode manually (mind that it won't
;; work unless your implementation supports it).
(defparameter *unicode*
  #+(or sb-unicode unicode)t
  #-(or sb-unicode unicode)nil)

(defsystem :cl-postgres
  :depends-on (:md5 :usocket :ieee-floats :simple-date
                    . #.(if *unicode* '(:trivial-utf-8)))
  :components 
  ((:module :cl-postgres
            :components ((:file "package")
                         (:file "communicate" :depends-on ("package"))
                         (:file "messages" :depends-on ("communicate"))
                         (:file "interpret" :depends-on ("communicate"))
                         (:file "protocol" :depends-on ("interpret" "messages"))
                         (:file "public" :depends-on ("protocol"))))))

(defsystem :cl-postgres-tests
  :depends-on (:cl-postgres :fiveam)
  :components
  ((:module :cl-postgres
            :components ((:file "tests")))))