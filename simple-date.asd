;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

(defsystem "simple-date"
  :description "Simple date library that can be used with postmodern"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :maintainer "Sabra Crolleton <sabra.crolleton@gmail.com>"
  :license "zlib"
  :version "1.31"
  :components
  ((:module "simple-date"
            :components ((:file "package")
			 (:file "simple-date"))))
  :in-order-to ((test-op (test-op "simple-date/tests"))))

(defsystem "simple-date/tests"
  :depends-on ("fiveam" "simple-date")
  :components
  ((:module "simple-date"
            :components ((:file "test-package")
			 (:file "tests"))))
  :perform (test-op (o c)
             (uiop:symbol-call :fiveam '#:run! :simple-date)))

(defsystem "simple-date/postgres-glue"
  :depends-on ("simple-date" "cl-postgres" "cl-postgres/tests")
  :components
  ((:module "simple-date"
            :components
            ((:file "cl-postgres-glue")))))

#|
;; The definitions below should work, unlike the bogus method they replace;
;; but I recommend instead explicit dependency on simple-date/postgres-glue.
(load-system "asdf-system-connections")
(defsystem-connection "simple-date/with-postgres"
  :requires ("simple-date" "cl-postgres")
  :depends-on ("simple-date/postgres-glue"))
|#
