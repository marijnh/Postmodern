
(in-package :cl-postgres-tests)

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
    (with-binary-row-values
      (is (time= (caaar (exec-query connection "select row('2010-04-05 14:42:21.500'::timestamp with time zone)"
                                    'list-row-reader))
                 (encode-timestamp 2010 4 5 14 42 21 500))))))

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

