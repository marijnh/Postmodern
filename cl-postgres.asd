;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

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
  :version "1.32.7"
  :depends-on ("md5" "split-sequence" "ironclad" "cl-base64" "uax-15"
                     (:feature (:or :sbcl :allegro :ccl :clisp :genera
                                :armedbear :cmucl :lispworks)
                               "usocket")
                     (:feature :sbcl (:require :sb-bsd-sockets)))
  :components
  ((:module "cl-postgres"
    :components ((:file "package")
                 (:file "features")
                 (:file "errors" :depends-on ("package"))
                 (:file "sql-string" :depends-on ("package"))
                 (:file "trivial-utf-8" :depends-on ("package"))
                 (:file #.*string-file*
                  :depends-on ("package" "trivial-utf-8"))
                 (:file "communicate"
                  :depends-on (#.*string-file* "sql-string"))
                 (:file "messages" :depends-on ("communicate"))
                 (:file "oid" :depends-on ("package"))
                 (:file "ieee-floats")
                 (:file "interpret"
                  :depends-on ("oid" "communicate" "ieee-floats"))
                 (:file "saslprep")
                 (:file "scram"
                  :depends-on ("messages" "errors" "saslprep"
                                          "trivial-utf-8"))
                 (:file "protocol"
                  :depends-on ("interpret" "messages" "errors" "scram"
                                           "saslprep" "trivial-utf-8"))
                 (:file "public" :depends-on ("protocol" "features"))
                 (:file "bulk-copy"
                  :depends-on ("public" "trivial-utf-8")))))
  :in-order-to ((test-op (test-op "cl-postgres/tests")
                         (test-op "cl-postgres/simple-date-tests"))))

(defsystem "cl-postgres/tests"
  :depends-on ("cl-postgres" "fiveam")
  :components
  ((:module "cl-postgres/tests"
    :components ((:file "test-package")
                 (:file "tests")
                 (:file "tests-scram" :depends-on ("test-package")))))
  :perform (test-op (o c)
                    (uiop:symbol-call :cl-postgres-tests '#:prompt-connection)
                    (uiop:symbol-call :fiveam '#:run! :cl-postgres)))


(defsystem "cl-postgres/simple-date-tests"
  :depends-on ("cl-postgres" "cl-postgres/tests" "fiveam" "simple-date"
                             "simple-date/postgres-glue")
  :components
  ((:module "cl-postgres/tests"
    :components ((:file "test-package")
                 (:file "simple-date-tests"))))
  :perform (test-op (o c)
                    (uiop:symbol-call :cl-postgres-simple-date-tests
                                      '#:prompt-connection)
                    (uiop:symbol-call :fiveam '#:run! :cl-postgres-simple-date)))

#|
;; The definitions below should work, unlike the bogus method they replace;
;; but I recommend instead explicit dependency on simple-date/postgres-glue.
(load-system "asdf-system-connections")
(defsystem-connection "postgres/with-simple-date"
:requires ("simple-date" "cl-postgres")
:depends-on ("simple-date/postgres-glue"))
|#
