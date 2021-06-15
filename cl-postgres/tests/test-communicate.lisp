;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES-TESTS; -*-
(in-package :cl-postgres-tests)

(def-suite :cl-postgres-communicate
    :description "Test suite for cl-postgres functions in communicate.lisp"
    :in :cl-postgres)

(in-suite :cl-postgres-communicate)

(test connection-pid
  (is (equal (length (with-test-connection
                (cl-postgres::parameter-list-types
                 (cl-postgres::connection-pid connection))))
             2)))

(test postgresql-versions
    (with-test-connection
      (is (postgresql-version-at-least "9.5.4" connection))
      (is (postgresql-version-at-least
           (cl-postgres::get-postgresql-version connection)
           connection))
      (is (not (postgresql-version-at-least "11000" connection)))))
