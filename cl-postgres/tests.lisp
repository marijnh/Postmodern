(defpackage :cl-postgres-tests
  (:use :common-lisp :fiveam :cl-postgres :cl-postgres-error)
  (:export #:prompt-connection #:*test-connection* #:with-test-connection))

(in-package :cl-postgres-tests)

(defparameter *test-connection* '("test" "test" "" "localhost"))

(defun prompt-connection (&optional (list *test-connection*))
  (flet ((ask (name pos)
           (format *query-io* "~a (enter to keep '~a'): " name (nth pos list))
           (finish-output *query-io*)
           (let ((answer (read-line *query-io*)))
             (unless (string= answer "") (setf (nth pos list) answer)))))
    (format *query-io* "~%To run this test, you must configure a database connection.~%")
    (ask "Database name" 0)
    (ask "User" 1)
    (ask "Password" 2)
    (ask "Hostname" 3)))

;; Adjust the above to some db/user/pass/host/[port] combination that
;; refers to a valid postgresql database, then after loading the file,
;; run the tests with (fiveam:run! :cl-postgres)

(def-suite :cl-postgres)
(in-suite :cl-postgres)

(defmacro with-test-connection (&body body)
  `(let ((connection (apply 'open-database *test-connection*)))
    (unwind-protect (progn ,@body)
      (close-database connection))))

(defmacro with-default-readtable (&body body)
  `(let ((*sql-readtable* (default-sql-readtable)))
    ,@body))

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

(test sql-strings
  (is (string= (to-sql-string :null) "NULL"))
  (is (string= (to-sql-string t) "true"))
  (is (string= (to-sql-string 400) "400"))
  (is (string= (to-sql-string "foo") "foo"))
  (is (eq t (nth-value 1 (to-sql-string "bar"))))
  (is (eq nil (nth-value 1 (to-sql-string 10)))))

(test date-query
  (with-default-readtable
    (with-test-connection
      (destructuring-bind ((a b c))
          (exec-query connection "select '1980-02-01'::date, '2010-04-05 14:42:21.500'::timestamp, '2 years -4 days'::interval"
                      'list-row-reader)
        (is (= a 2527200000))
        (is (= b 3479467341))
        (is (equal c '((:MONTHS 24) (:DAYS -4) (:SECONDS 0) (:USECONDS 0))))))))

(test alist-row-reader
  (with-test-connection
    (is (equal (exec-query connection "select 42 as foo, 99 as bar" 'alist-row-reader)
               '((("foo" . 42) ("bar" . 99)))))))

(test prepared-statement
  (with-test-connection
    (prepare-query connection "test" "select $1::integer, $2::boolean, $3::text")
    (is (equal (exec-prepared connection "test" '(42 nil "foo") 'list-row-reader)
               '((42 nil "foo"))))))

(test unprepare-statement
  (with-test-connection
    (prepare-query connection "test" "select true")
    (unprepare-query connection "test")
    (prepare-query connection "test" "select false")
    (is (equal (exec-prepared connection "test" '() 'list-row-reader)
               '((nil))))))
      
(test prepared-array-param
  (with-test-connection
    (prepare-query connection "test" "select ($1::int[])[2]")
    (is (equal (exec-prepared connection "test" '(#(1 2 3)) 'list-row-reader)
               '((2))))
    (prepare-query connection "test2" "select ($1::text[])[2]")
    (is (equal (exec-prepared connection "test2" '(#("A" "B" "C")) 'list-row-reader)
               '(("B"))))))

(test blob
  (with-test-connection
    (let* ((str "foobar42")
           (bytes (coerce #(102 111 111 98 97 114 52 50) '(vector (unsigned-byte 8)))))
      (prepare-query connection "test" "select $1::varchar, $2::bytea")
      (is (equalp (exec-prepared connection "test" (list str bytes) 'list-row-reader)
                  (list (list str bytes)))))))

(test recover-error
  (with-test-connection
    (signals cl-postgres-error:syntax-error-or-access-violation
      (exec-query connection "gubble gubble gabble goo"))
    (is (equal (exec-query connection "select false" 'list-row-reader)
               '((nil))))))

(test unique-violation-error
  (with-test-connection
    (exec-query connection "create table test (id int not null primary key, name text)")
    (exec-query connection "insert into test values (1, 'bert')")
    (signals unique-violation
      (exec-query connection "insert into test values (1, 'harry')"))
    (exec-query connection "drop table test")))

(test sql-reader
  (with-test-connection
    (let ((*sql-readtable* (copy-sql-readtable)))
      (set-sql-reader 2249 (lambda (text)
                             (with-input-from-string (*standard-input* text)
                               (read-char) ;; opening paren
                               (let ((x (read)))
                                 (read-char) ;; comma
                                 (cons x (read))))))
      (is (equal (exec-query connection "select (10,20)" 'list-row-reader)
                 '(((10 . 20))))))
    (is (equal (exec-query connection "select (30,40)" 'list-row-reader)
               '(("(30,40)"))))))

(test sql-reader-binary
  (with-test-connection
    (with-binary-row-values
      (let ((*sql-readtable* (copy-sql-readtable)))
        (set-sql-reader 2249 (lambda (text)
                               (with-input-from-string (*standard-input* text)
                                 (read-char) ;; opening paren
                                 (let ((x (read)))
                                   (read-char) ;; comma
                                   (cons x (read))))))
        (is (equal (exec-query connection "select (10,20)" 'list-row-reader)
                   '(((10 . 20))))))
      (is (equal (exec-query connection "select (30,40)" 'list-row-reader)
                 '(((30 40))))))))

(test bulk-writer
  (with-test-connection
    (exec-query connection "create table test (a int, b text, c date, d timestamp, e int[])")
    (let ((stream (open-db-writer *test-connection* 'test '(a b c d e))))
      ;; test a variety of types (int, text, date, timstamp, int array)
      (loop for row in '((1 "one" "2012-01-01" "2012-01-01 00:00" #(1 2 3 42))
                         (2 "two" "2012-01-01" "2012-01-01 00:00" #(3 2 1 42))

                         ;; make sure utf-8 gets through ok
                         (3 "κόσμε" "2012-01-01" "2012-01-01 00:00" #(1))

                         ;; make sure tabs get through ok
                         (4 "one two	three" "2012-01-01" "2012-01-01 00:00" #(1)))
           do
           (db-write-row stream row))
      (close-db-writer stream))
    (print (exec-query connection "select * from test"))
    (exec-query connection "drop table test")))

(test row-boolean-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[TRUE, FALSE, TRUE])" 'list-row-reader)
                '(("(\"{t,f,t}\")"))))))

(test row-boolean-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[TRUE, FALSE, TRUE])" 'list-row-reader)
                  '(((#(T NIL T)))))))))

(test cast-to-bits
  (with-test-connection
    (is (equalp (exec-query connection "select cast(255 as bit(8)), cast(-44 as bit(128))" 'list-row-reader)
                '((#*11111111
                   #*11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111010100))))
    (is (equalp (exec-query connection "select row(cast(32 as bit(12)))" 'list-row-reader)
                '(("(000000100000)"))))
    (is (equalp (exec-query connection "select ARRAY[cast(32 as bit(16))]" 'list-row-reader)
                '((#(#*0000000000100000)))))
    (is (equalp (exec-query connection "select row(ARRAY[cast(32 as bit(16))])" 'list-row-reader)
                '(("({0000000000100000})"))))))

(test cast-to-bits-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select cast(255 as bit(8)), cast(-44 as bit(128))" 'list-row-reader)
                  '((#*11111111
                     #*11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111010100))))
      (is (equalp (exec-query connection "select row(cast(32 as bit(12)))" 'list-row-reader)
                  '(((#*000000100000)))))
      (is (equalp (exec-query connection "select ARRAY[cast(32 as bit(16))]" 'list-row-reader)
                  '((#(#*0000000000100000)))))
      (is (equalp (exec-query connection "select row(ARRAY[cast(32 as bit(16))])" 'list-row-reader)
                  '(((#(#*0000000000100000)))))))))

(test cast-to-varbits
  (with-test-connection
    (is (equalp (exec-query connection "select 255::bit(8)::varbit(8), 44::bit(128)::varbit(128)" 'list-row-reader)
                '((#*11111111
                   #*00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101100))))
    (is (equalp (exec-query connection "select row(32::bit(12)::varbit(12))" 'list-row-reader)
                '(("(000000100000)"))))
    (is (equalp (exec-query connection "select ARRAY[32::bit(16)::varbit(16)]" 'list-row-reader)
                '((#(#*0000000000100000)))))
    (is (equalp (exec-query connection "select row(ARRAY[32::bit(16)::varbit(16)])" 'list-row-reader)
                '(("({0000000000100000})"))))))

(test cast-to-varbits-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select 255::bit(8)::varbit(8), 44::bit(128)::varbit(128)" 'list-row-reader)
                  '((#*11111111
                     #*00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101100))))
      (is (equalp (exec-query connection "select row(32::bit(12)::varbit(12))" 'list-row-reader)
                  '(((#*000000100000)))))
      (is (equalp (exec-query connection "select ARRAY[32::bit(16)::varbit(16)]" 'list-row-reader)
                  '((#(#*0000000000100000)))))
      (is (equalp (exec-query connection "select row(ARRAY[32::bit(16)::varbit(16)])" 'list-row-reader)
                  '(((#(#*0000000000100000)))))))))



(test row-integer-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[1,2,4,8])" 'list-row-reader)
                '(("(\"{1,2,4,8}\")"))))))

(test row-integer-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[1,2,4,8])" 'list-row-reader)
                  '(((#(1 2 4 8)))))))))

(test row-string-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY['foo', 'bar', 'baz'])" 'list-row-reader)
                '(("(\"{foo,bar,baz}\")"))))))

(test row-string-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY['foo', 'bar', 'baz'])" 'list-row-reader)
                  '(((#("foo" "bar" "baz")))))))))

(test row-bpchar-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[cast('foo' as bpchar)])" 'list-row-reader)
                '(("({foo})"))))))

(test row-bpchar-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[cast('foo' as bpchar)])" 'list-row-reader)
                  '(((#("foo")))))))))

(test row-varchar-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY['foo'::varchar])" 'list-row-reader)
                '(("({foo})"))))))

(test row-varchar-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY['foo'::varchar])" 'list-row-reader)
                  '(((#("foo")))))))))

(test row-oid-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[1234::oid, 5678::oid])" 'list-row-reader)
                '(("(\"{1234,5678}\")"))))))

(test row-oid-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[1234::oid, 5678::oid])" 'list-row-reader)
                  '(((#(1234 5678)))))))))

(test row-int2-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[1234::int2])" 'list-row-reader)
                '(("({1234})"))))))

(test row-int2-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[1234::int2])" 'list-row-reader)
                  '(((#(1234)))))))))

(test row-int8-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[123456789012::int8])" 'list-row-reader)
                '(("({123456789012})"))))))

(test row-int8-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[123456789012::int8])" 'list-row-reader)
                  '(((#(123456789012)))))))))

(test row-float-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[3.14::float])" 'list-row-reader)
                '(("({3.14})"))))))

(test row-float-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[3.14::float])" 'list-row-reader)
                  '(((#(3.14d0)))))))))

(test row-double-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[cast(3.14 as double precision)])" 'list-row-reader)
                '(("({3.14})"))))))

(test row-double-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[cast(3.14 as double precision)])" 'list-row-reader)
                  '(((#(3.14d0)))))))))

(test row-date-array
  (with-default-readtable
    (with-test-connection
      (is (equalp (elt (exec-query connection "select row(ARRAY['1980-02-01'::date])" 'list-row-reader) 0)
                  '("({1980-02-01})"))))))

(test row-date-array-binary
  (with-default-readtable
    (with-test-connection
      (with-binary-row-values
        (is (= (elt (caaar (exec-query connection "select row(ARRAY['1980-02-01'::date])" 'list-row-reader)) 0)
               2527200000))))))

(test row-timestamp
  (with-test-connection
    (is (equalp (exec-query connection "select row('2010-04-05 14:42:21.500'::timestamp)"
                            'list-row-reader)
                '(("(\"2010-04-05 14:42:21.5\")"))))))

(test row-timestamp-without-time-zone
  (with-test-connection
    (is (equalp (exec-query connection "select row('2010-04-05 14:42:21.500'::timestamp without time zone)"
                            'list-row-reader)
                '(("(\"2010-04-05 14:42:21.5\")"))))))

(test row-timestamp-with-time-zone
  (with-test-connection
    (exec-query connection "set time zone 'GMT'")
    (is (equalp (exec-query connection "select row('2010-04-05 14:42:21.500'::timestamp with time zone)"
                            'list-row-reader)
                '(("(\"2010-04-05 14:42:21.5+00\")"))))))

(test row-timestamp-with-time-zone-binary
  (with-default-readtable
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
          (is (equalp (multiple-value-list gmt)
                      '(3479467341)))
          (is (equalp (multiple-value-list pdt)
                      '(3479438541))))))))

(test row-timestamp-array
  (with-default-readtable
    (with-test-connection
      (is (equalp (elt (exec-query connection "select row(ARRAY['2010-04-05 14:42:21.500'::timestamp])"
                                   'list-row-reader) 0)
                  '("(\"{\"\"2010-04-05 14:42:21.5\"\"}\")"))))))

(test row-timestamp-array-binary
  (with-default-readtable
    (with-binary-row-values
        (with-test-connection
          (is (equalp (elt (exec-query connection "select row(ARRAY['2010-04-05 14:42:21.500'::timestamp])"
                                  'list-row-reader) 0)
                      '((#(3479467341)))))))))

(test row-timestamp-without-time-zone-array
  (with-test-connection
    (is (equalp (elt (exec-query connection "select row(ARRAY['2010-04-05 14:42:21.500'::timestamp without time zone])"
                                 'list-row-reader) 0)
                '("(\"{\"\"2010-04-05 14:42:21.5\"\"}\")")))))

(test row-time
  (with-test-connection
    (is (equalp (exec-query connection "select row('05:00'::time)"
                            'list-row-reader)
                '(("(05:00:00)"))))))

(test row-interval-array
  (with-default-readtable
    (with-test-connection
      (with-binary-row-values
        (is (equalp (elt (caaar (exec-query connection "select row(ARRAY['2 years -4 days'::interval])"
                                            'list-row-reader)) 0)
                    '((:MONTHS 24) (:DAYS -4) (:SECONDS 0) (:USECONDS 0))))))))

(defparameter *random-byte-count* 8192)

(test write-bytea
  (with-test-connection
    (exec-query connection "create table test (a bytea)")
    (unwind-protect
         (let ((random-bytes (make-array *random-byte-count* :element-type '(unsigned-byte 8))))
           (loop for i below *random-byte-count*
                         do (setf (aref random-bytes i)
                                  (random #x100)))
           (prepare-query connection "bytea-insert" "insert into test values ($1)")
           (exec-prepared connection "bytea-insert" (list random-bytes))
           (is (equalp (exec-query connection "select a from test;" 'list-row-reader)
                       `((,random-bytes)))))
      (exec-query connection "drop table test"))))

(defun vector-to-hex-string (vec)
    (with-output-to-string (s)
      (map nil (lambda (x)
                 (format s "~(~2,\'0x~)" x))
           vec)
      s))

(test write-row-bytea
  (with-test-connection
    (exec-query connection "create table test (a bytea)")
    (let ((*random-byte-count* 16))
      (unwind-protect
           (let ((random-bytes (make-array *random-byte-count* :element-type '(unsigned-byte 8))))
             (loop for i below *random-byte-count*
                do (setf (aref random-bytes i)
                         (random #x100)))
             (prepare-query connection "bytea-insert" "insert into test values ($1)")
             (exec-prepared connection "bytea-insert" (list random-bytes))
             (is (equalp (exec-query connection "select row(a) from test;" 'list-row-reader)
                         `((,(concatenate 'string
                                          "(\"\\\\x"
                                          (vector-to-hex-string random-bytes)
                                          "\")"))))))
        (exec-query connection "drop table test")))))

(test write-row-array-bytea
  (with-test-connection
    (exec-query connection "create table test (a bytea)")
    (let ((*random-byte-count* 16))
      (unwind-protect
           (let ((random-bytes (make-array *random-byte-count* :element-type '(unsigned-byte 8))))
             (loop for i below *random-byte-count*
                do (setf (aref random-bytes i)
                         (random #x100)))
             (prepare-query connection "bytea-insert" "insert into test values ($1)")
             (exec-prepared connection "bytea-insert" (list random-bytes))
             (is (equalp (exec-query connection "select row(ARRAY[a]) from test;" 'list-row-reader)
                         `(((#(,random-bytes)))))))
        (exec-query connection "drop table test")))))

(test write-row-array-bytea
  (with-test-connection
    (with-binary-row-values
      (exec-query connection "create table test (a bytea)")
      (unwind-protect
           (let ((random-bytes (make-array *random-byte-count* :element-type '(unsigned-byte 8))))
             (loop for i below *random-byte-count*
                do (setf (aref random-bytes i)
                         (random #x100)))
             (prepare-query connection "bytea-insert" "insert into test values ($1)")
             (exec-prepared connection "bytea-insert" (list random-bytes))
             (is (equalp (exec-query connection "select row(ARRAY[a]) from test;" 'list-row-reader)
                         `(((#(,random-bytes)))))))
        (exec-query connection "drop table test")))))

(test write-row-array-bytea-binary
  (with-test-connection
    (with-binary-row-values
      (exec-query connection "create table test (a bytea)")
      (unwind-protect
           (let ((random-bytes (make-array *random-byte-count* :element-type '(unsigned-byte 8))))
             (loop for i below *random-byte-count*
                do (setf (aref random-bytes i)
                         (random #x100)))
             (prepare-query connection "bytea-insert" "insert into test values ($1)")
             (exec-prepared connection "bytea-insert" (list random-bytes))
             (is (equalp (exec-query connection "select row(ARRAY[a]) from test;" 'list-row-reader)
                         `(((#(,random-bytes)))))))
        (exec-query connection "drop table test")))))

(test row-name-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY['foo'::name])" 'list-row-reader)
                '(("({foo})"))))))

(test row-name-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY['foo'::name])" 'list-row-reader)
                  '(((#("foo")))))))))

(test point
  (with-test-connection
    (is (equalp (exec-query connection "select point(1,2)" 'list-row-reader)
                '(((1.0d0 2.0d0)))))))

(test row-point
  (with-test-connection
    (is (equalp (exec-query connection "select row(point(1,2))" 'list-row-reader)
                '(("(\"(1,2)\")"))))))

(test row-point-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(point(1,2))" 'list-row-reader)
                  '((((1.0d0 2.0d0)))))))))

(test row-point-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[point(1,2)])" 'list-row-reader)
                '(("(\"{\"\"(1,2)\"\"}\")"))))))

(test row-point-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[point(1,2)])" 'list-row-reader)
                  '(((#((1.0d0 2.0d0))))))))))

(test lseg
  (with-test-connection
    (is (equalp (exec-query connection "select lseg(point(1,2),point(3,4))" 'list-row-reader)
                '((((1.0d0 2.0d0) (3.0d0 4.0d0))))))))

(test row-lseg
  (with-test-connection
    (is (equalp (exec-query connection "select row(lseg(point(1,2),point(3,4)))" 'list-row-reader)
                '(("(\"[(1,2),(3,4)]\")"))))))

(test row-lseg-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(lseg(point(1,2),point(3,4)))" 'list-row-reader)
                  '(((((1.0d0 2.0d0) (3.0d0 4.0d0))))))))))

(test row-lseg-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[lseg(point(1,2),point(3,4))])" 'list-row-reader)
                '(("(\"{\"\"[(1,2),(3,4)]\"\"}\")"))))))

(test row-lseg-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[lseg(point(1,2),point(3,4))])" 'list-row-reader)
                  '(((#(((1.0d0 2.0d0) (3.0d0 4.0d0)))))))))))

(test box
  (with-test-connection
    (is (equalp (exec-query connection "select box(point(1,2),point(3,4))" 'list-row-reader)
                '((((3.0d0 4.0d0) (1.0d0 2.0d0))))))))

(test row-box
  (with-test-connection
    (is (equalp (exec-query connection "select row(box(point(1,2),point(3,4)))" 'list-row-reader)
                '(("(\"(3,4),(1,2)\")"))))))

(test row-box-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(box(point(1,2),point(3,4)))" 'list-row-reader)
                  '(((((3.0d0 4.0d0) (1.0d0 2.0d0))))))))))

(test row-box-array
  (with-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[box(point(1,2),point(3,4))])" 'list-row-reader)
                '(("(\"{(3,4),(1,2)}\")"))))))

(test row-box-array-binary
  (with-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[box(point(1,2),point(3,4))])" 'list-row-reader)
                  '(((#(((3.0d0 4.0d0) (1.0d0 2.0d0)))))))))))

(test row-array-nulls
  (with-test-connection
    (is (equalp (exec-query connection "select row((ARRAY[1,3,4])[5:99])" 'list-row-reader)
                '(("({})"))))))

(test row-array-nulls-binary
  (with-test-connection
    (cl-postgres::with-binary-row-values
      (is (equalp (exec-query connection "select row((ARRAY[1,3,4])[5:99])" 'list-row-reader)
                  '(((NIL))))))))

(test row-array-nulls-binary-2
  (with-test-connection
    (cl-postgres::with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[NULL, NULL]);" 'list-row-reader)
                  '(((#(:null :null)))))))))

(test row-array-table-nulls-binary
  (with-binary-row-values
    (with-test-connection
      (exec-query connection "create table test (a integer[])")
      (unwind-protect
           (progn
             (prepare-query connection "integer-array-insert" "insert into test values ($1)")
             (exec-prepared connection "integer-array-insert" (list "{{0,0},{0,0}}"))
             (exec-prepared connection "integer-array-insert" (list "{{1,1}}"))
             (exec-prepared connection "integer-array-insert" (list "{{2,2}, {2,2}}"))
             (exec-prepared connection "integer-array-insert" (list "{{3,3}}"))
             (exec-prepared connection "integer-array-insert" (list "{{4,4}}"))
             (is (equalp
                  (exec-query
                   connection
                   "select row(a[2:45]) from test"
                   'list-row-reader)
                  '(((#2A((0 0)))) ((NIL)) ((#2A((2 2)))) ((NIL)) ((NIL))))))
        (exec-query connection "drop table test")))))
