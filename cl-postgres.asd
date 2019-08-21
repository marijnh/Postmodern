(defpackage :cl-postgres-system
  (:use :common-lisp :asdf))
(in-package :cl-postgres-system)

;; Change this to enable/disable unicode manually (mind that it won't
;; work unless your implementation supports it).
(defparameter *unicode*
  #+(or sb-unicode unicode ics openmcl-unicode-strings) t
  #-(or sb-unicode unicode ics openmcl-unicode-strings) nil)
(defparameter *string-file* (if *unicode* "strings-utf-8" "strings-ascii"))

(defsystem "cl-postgres"
  :description "Low-level client library for PostgreSQL"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :maintainer "Sabra Crolleton <sabra.crolleton@gmail.com>"
  :license "zlib"
  :depends-on ("md5"
               (:feature (:or :sbcl :allegro :ccl :clisp :genera :armedbear :cmucl) "usocket")
               (:feature :sbcl (:require :sb-bsd-sockets)))
  :components
  ((:module "cl-postgres"
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
  :in-order-to ((test-op (test-op "cl-postgres/tests")
                         (test-op "cl-postgres/simple-date-tests"))))

(defsystem "cl-postgres/tests"
  :depends-on ("cl-postgres" "fiveam")
  :components
  ((:module "cl-postgres/tests"
            :components ((:file "tests"))))
  :perform (test-op (o c)
             (uiop:symbol-call :cl-postgres-tests '#:prompt-connection)
             (uiop:symbol-call :fiveam '#:run! :cl-postgres)))

(defsystem "cl-postgres/simple-date-tests"
  :depends-on ("cl-postgres" "cl-postgres/tests" "fiveam" "simple-date/postgres-glue")
  :components
  ((:module "cl-postgres/tests"
            :components ((:file "simple-date-tests"))))
  :perform (test-op (o c)
             (uiop:symbol-call :cl-postgres-simple-date-tests '#:prompt-connection)
             (uiop:symbol-call :fiveam '#:run! :cl-postgres-simple-date)))

#|
;; The definitions below should work, unlike the bogus method they replace;
;; but I recommend instead explicit dependency on simple-date/postgres-glue.
(load-system "asdf-system-connections")
(defsystem-connection "postgres/with-simple-date"
  :requires ("simple-date" "cl-postgres")
  :depends-on ("simple-date/postgres-glue"))
|#
