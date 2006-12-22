(defpackage :cl-postgres-tests
  (:use :common-lisp :fiveam :simple-date :cl-postgres))

(in-package :cl-postgres-tests)

(defparameter *test-connection* '("test" "test" "" "localhost"))

;; Adjust the above to some db/user/pass/host/[port] combination that
;; refers to a valid postgresql database, then after loading the file,
;; run the tests with (fiveam:run! :cl-postgres)

(def-suite :cl-postgres)
(in-suite :cl-postgres)

(defmacro with-test-connection (&body body)
  `(let ((connection (apply 'open-database *test-connection*)))
    (unwind-protect (progn ,@body)
      (close-database connection))))

(test connect-sanity
  (with-test-connection
    (is (database-open-p connection))))

(test simple-query
  (with-test-connection
    (destructuring-bind ((a b c d e))
        (exec-query connection "select 22::integer, 44.5::double precision, 'abcde'::varchar, true::boolean, 4.5::numeric(5,2)"
                    'list-row-reader)
      (is (eql a 22))
      (is (eql b 44.5d0))
      (is (string= c "abcde"))
      (is (eql d t))
      (is (eql e 9/2)))))

(test date-query
  (with-test-connection
    (destructuring-bind ((a b c))
        (exec-query connection "select '1980-02-01'::date, '2010-04-05 14:42:21.500'::timestamp, '2 years -4 days'::interval"
                    'list-row-reader)
      (is (time= a (encode-date 1980 2 1)))
      (is (time= b (encode-timestamp 2010 4 5 14 42 21 500)))
      (is (time= c (encode-interval :year 2 :day -4))))))

(test alist-row-reader
  (with-test-connection
    (is (equal (exec-query connection "select 42 as foo, 99 as bar" 'alist-row-reader)
               '((("foo" . 42) ("bar" . 99)))))))

(test prepared-statement
  (with-test-connection
    (prepare-query connection "test" "select $1::integer, $2::integer")
    (is (equal (exec-prepared connection "test" '("42" "99") 'list-row-reader)
               '((42 99))))))

(test recover-error
  (with-test-connection
    (signals database-error
      (exec-query connection "gubble gubble gabble goo"))
    (is (equal (exec-query connection "select false" 'list-row-reader)
               '((nil))))))
