;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES-SIMPLE-DATE-TESTS; -*-
(defpackage :cl-postgres-simple-date-tests
  (:use :common-lisp :fiveam :cl-postgres :cl-postgres-error :simple-date)
  (:import-from #:cl-postgres-tests
                #:prompt-connection))

(in-package :cl-postgres-simple-date-tests)

(defmacro with-simple-date-readtable (&body body)
  `(let ((*sql-readtable* (simple-date-cl-postgres-glue:simple-date-sql-readtable)))
    ,@body))

(defmacro with-test-connection (&body body)
  `(let ((connection (apply 'open-database (prompt-connection))))
     (with-simple-date-readtable
       (unwind-protect (progn ,@body)
         (close-database connection)))))

(def-suite :cl-postgres-simple-date)
(in-suite :cl-postgres-simple-date)

(test row-timestamp-without-time-zone-binary
  (with-test-connection
    (with-binary-row-values
      (is (time= (caaar (exec-query connection "select row('2010-04-05 14:42:21.500'::timestamp without time zone)"
                                    'list-row-reader))
                 (encode-timestamp 2010 4 5 14 42 21 500))))))

(test row-timestamp-with-time-zone-binary
  (with-test-connection
    (exec-query connection "set time zone 'GMT'")
    (with-binary-row-values
      (destructuring-bind (gmt pdt)
          (caar
           (exec-query
            connection
            (concatenate 'string
                         "select row('2010-04-05 14:42:21.500'::timestamp with time zone at time zone 'GMT', "
                         " '2010-04-05 14:42:21.500'::timestamp with time zone at time zone 'PST')")
            'list-row-reader))
        (is (time= gmt (encode-timestamp 2010 4 5 14 42 21 500)))
        (is (time= pdt (encode-timestamp 2010 4 5 6 42 21 500)))))))

(test row-timestamp-without-time-zone-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (time= (elt (caaar (exec-query connection "select row(ARRAY['2010-04-05 14:42:21.500'::timestamp without time zone])"
                                         'list-row-reader)) 0)
                 (encode-timestamp 2010 4 5 14 42 21 500))))))

(test row-time-binary
  (with-test-connection
    (with-binary-row-values
      (is (time= (caaar (exec-query connection "select row('05:00'::time)"
                                    'list-row-reader))
                 (encode-time-of-day 5 0))))))

(test row-timestamp-binary
  (with-test-connection
    (with-binary-row-values
      (is (time= (caaar (exec-query connection "select row('2010-04-05 14:42:21.500'::timestamp)"
                                    'list-row-reader))
                 (encode-timestamp 2010 4 5 14 42 21 500))))))
