(defpackage :cl-postgres-system
  (:use :common-lisp :asdf))
(in-package :cl-postgres-system)

;; Change this to enable/disable unicode manually (mind that it won't
;; work unless your implementation supports it).
(defparameter *unicode*
  #+(or sb-unicode unicode ics openmcl-unicode-strings) t
  #-(or sb-unicode unicode ics openmcl-unicode-strings) nil)
(defparameter *string-file* (if *unicode* "strings-utf-8" "strings-ascii"))

(defsystem :cl-postgres
  :description "Low-level client library for PostgreSQL"
  :depends-on (:md5
               #-(or sbcl allegro ccl) :usocket
               #+sbcl                  :sb-bsd-sockets)
  :components
  ((:module :cl-postgres
            :components ((:file "trivial-utf-8")
                         (:file "ieee-floats")
                         (:file "features")
                         (:file "package" :depends-on ("features"))
                         (:file "errors" :depends-on ("package"))
                         (:file "sql-string" :depends-on ("package"))
                         (:file #.*string-file* :depends-on ("package" "trivial-utf-8"))
                         (:file "communicate" :depends-on (#.*string-file* "sql-string"))
                         (:file "messages" :depends-on ("communicate"))
                         (:file "oid" :depends-on ("package"))
                         (:file "interpret" :depends-on ("oid" "communicate" "ieee-floats"))
                         (:file "protocol" :depends-on ("interpret" "messages" "errors"))
                         (:file "public" :depends-on ("protocol"))
                         (:file "bulk-copy" :depends-on ("public")))))
  :in-order-to ((test-op (test-op :cl-postgres-tests)
                         (test-op :cl-postgres-simple-date-tests))))

(defmethod perform :after ((op asdf:load-op) (system (eql (find-system :cl-postgres))))
  (when (and (find-package :simple-date)
             (not (find-symbol (symbol-name '#:+postgres-day-offset+) :simple-date)))
    (asdf:oos 'asdf:load-op :simple-date-postgres-glue)))

(defsystem :cl-postgres-tests
  :depends-on (:cl-postgres :fiveam)
  :components
  ((:module :cl-postgres
            :components ((:file "tests"))))
  :perform (test-op (o c)
             (uiop:symbol-call :cl-postgres-tests '#:prompt-connection)
             (uiop:symbol-call :fiveam '#:run! :cl-postgres)))

(defsystem :cl-postgres-simple-date-tests
  :depends-on (:cl-postgres :cl-postgres-tests :fiveam :simple-date)
  :components
  ((:module :cl-postgres
            :components ((:file "simple-date-tests"))))
  :perform (test-op (o c)
             (uiop:symbol-call :cl-postgres-simple-date-tests '#:prompt-connection)
             (uiop:symbol-call :fiveam '#:run! :cl-postgres-simple-date)))

