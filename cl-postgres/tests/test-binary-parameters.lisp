;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES-TESTS; -*-
(in-package :cl-postgres-tests)

(def-suite :cl-postgres-binary-parameters
    :description "Test suite for cl-postgres when using binary connection."
    :in :cl-postgres)

(in-suite :cl-postgres-binary-parameters)

(test binary-date-query
  (with-default-readtable
    (with-binary-test-connection
      (destructuring-bind ((a b c))
          (exec-query connection "select '1980-02-01'::date, '2010-04-05 14:42:21.500'::timestamp, '2 years -4 days'::interval"
                      'list-row-reader)
        (is (= a 2527200000))
        (is (= b 3479467341))
        (is (equal c '((:MONTHS 24) (:DAYS -4) (:SECONDS 0) (:USECONDS 0))))))))

(test binary-timestamp-with-time-zone
  (with-default-readtable
    (with-binary-test-connection
      (with-rollbacked-transaction
        ;; 1. set local time to GMT -- returned time should be what we
        ;; pass in -- note we lose the 500 millesconds here :(
        (exec-query connection "set local time zone 'GMT'")
        (is (equalp (exec-query connection "select '2010-04-05 14:42:21.500'::timestamp with time zone"
                                'list-row-reader)
                    '((3479467341))))
        ;; 2. set local time to PST8PDT, now we should get back a time
        ;; that is 7 hours later than. This means that the input time
        ;; is specified in PST8PDT, but the returned timestamp is in
        ;; GMT.
        (exec-query connection "set time zone 'PST8PDT'")
        (is (equalp (exec-query connection "select '2010-04-05 14:42:21.500'::timestamp with time zone"
                                'list-row-reader)
                    '((3479492541))))
        ;; 3. now we specify the time zone to be GMT but the input time
        ;; is in EDT, so the returned time should be 4 hours later.
        (exec-query connection "set time zone 'GMT'")
        (is (equalp (exec-query connection "select '2010-04-05 14:42:21.500'::timestamp at time zone 'America/New_York'"
                                'list-row-reader)
                    '((3479481741))))))))

(test binary-timestamp-with-time-zone-text
  (let ((*sql-readtable* (copy-sql-readtable)))
    (set-sql-reader oid:+timestamptz+ nil)
    (with-binary-test-connection
      (with-rollbacked-transaction
        ;; 1. GMT input and GMT output
        (exec-query connection "set time zone 'GMT'")
        (is (equalp (exec-query connection "select '2010-04-05 14:42:21.500'::timestamp with time zone"
                                'list-row-reader)
                    '(("2010-04-05 14:42:21.5+00"))))
        ;; 2. PST8PDT input and text representation of the same input
        ;; time with a -7 hour offset
        (exec-query connection "set time zone 'PST8PDT'")
        (is (equalp (exec-query connection "select '2010-04-05 14:42:21.500'::timestamp with time zone"
                                'list-row-reader)
                    '(("2010-04-05 14:42:21.5-07"))))
        ;; 3. EDT input with GMT output (4 hours later, 0 hour offset)
        (exec-query connection "set time zone 'GMT'")
        (is (equalp (exec-query connection "select '2010-04-05 14:42:21.500'::timestamp at time zone 'America/New_York'"
                                'list-row-reader)
                    '(("2010-04-05 18:42:21.5+00"))))))))

(test binary-alist-row-reader
  (with-binary-test-connection
    (is (equal (exec-query connection "select 42 as foo, 99 as bar" 'alist-row-reader)
               '((("foo" . 42) ("bar" . 99)))))))

(test binary-vector-row-reader
  (with-binary-test-connection
    (is (equalp (exec-query connection "select 12, 'a'" 'vector-row-reader)
                #(#(12 "a"))))))

(test binary-ignore-row-reader
  (with-binary-test-connection
    (is (not (exec-query connection "select 12, 'a'" 'ignore-row-reader)))))

(test binary-prepare-query-empty-names
  (with-binary-test-connection
    (prepare-query connection "" "select $1")
    (is (equal (exec-query connection "select * from pg_prepared_statements" 'list-row-reader)
               nil))
    (prepare-query connection "tte" "select $1")
    (is (equalp (exec-query connection
                           "select name,statement, parameter_types from pg_prepared_statements"
                           'list-row-reader)
                '(("tte" "select $1" "{text}"))))))

(test binary-exec-queries-with-binary
  (with-binary-test-connection
    (is (equal (exec-query connection "select 1" 'list-row-reader)
               '((1))))
    (is (equal (exec-query connection "select 1::integer" 'list-row-reader)
               '((1))))
    (is (equal (exec-query connection "select 1::int2" 'list-row-reader)
               '((1))))
    ;; The next one has specified a small int, but that is out of range and triggers error
    (signals error (exec-query connection "select 123456789::int2" 'list-row-reader))
    (is (equal (exec-query connection "select 123456789123456789" 'list-row-reader)
               '((123456789123456789))))
    (is (equal (exec-query connection "select 'text'" 'list-row-reader)
               '(("text"))))))

(test binary-prepare-and-exec-query-with-binary-unspecified-parameters
  "The prepared queries do not provide sample data types, so any parameter passed will fail
unless it would have been valid as a text parameter."
  (with-binary-test-connection
    (prepare-query connection "test-integer-1" "select $1")
    (prepare-query connection "test-float-1" "select $1")
    (prepare-query connection "test-text-1" "select $1")
    (signals error (exec-prepared connection "test-integer-1" '(42) 'list-row-reader))
    (signals error (exec-prepared connection "test-float-1" '(4.2) 'list-row-reader))
    (is (equal  (exec-prepared connection "test-text-1" '("a") 'list-row-reader)
                '(("a"))))))

(test binary-prepare-and-exec-query-with-binary-specified-parameters
  (with-binary-test-connection
    (prepare-query connection "test-integer-2" "select $1" '(12))
    (prepare-query connection "test-float-2" "select $1" '(4.2))
    (prepare-query connection "test-text-2" "select $1" '("something"))
    (is (equal (exec-prepared connection "test-integer-2" '(42) 'list-row-reader)
               '((42))))
    (is (equal (exec-prepared connection "test-float-2" '(4.2) 'list-row-reader)
               '((4.2))))
    (is (equal (exec-prepared connection "test-text-2" '("a") 'list-row-reader)
               '(("a"))))
    (prepare-query connection "test2" "select $1, $2" '(1 t))
    (is (equal (exec-prepared connection "test2" '(42 nil) 'list-row-reader)
               '((42 nil))))
    (prepare-query connection "test3" "select $1, $2, $3" '(1 t "something"))
    (is (equal (exec-prepared connection "test3" '(42 t "foo") 'list-row-reader)
               '((42 t "foo"))))
    (is (equal (exec-prepared connection "test3" '(42 nil "foo") 'list-row-reader)
               '((42 nil "foo"))))
    (prepare-query connection "test4" "select $1::integer")
    (signals error (exec-prepared connection "test4" '(42) 'list-row-reader))
    (prepare-query connection "test5" "select $1::integer" '(3))
    (is (equal (exec-prepared connection "test5" '(42) 'list-row-reader)
               '((42))))))

(test binary-prepare-and-exec-query-with-bool-binary
  (with-binary-test-connection
    (prepare-query connection "test-bool-1" "select $1")
    (prepare-query connection "test-bool-2" "select $1" '(t))
    (is (not (equal (exec-prepared connection "test-bool-1" '(t) 'list-row-reader)
                    '((t)))))
    (is (equal (exec-prepared connection "test-bool-2" '(t) 'list-row-reader)
               '((t))))
    (is (equal (exec-prepared connection "test-bool-2" '(nil) 'list-row-reader)
               '((nil))))))

(test unprepare-query
  (with-binary-test-connection
    (prepare-query connection "test" "select true")
    (unprepare-query connection "test")
    (prepare-query connection "test" "select false")
    (is (equal (exec-prepared connection "test" '() 'list-row-reader)
               '((nil))))))

(test binary-prepared-array-param
  (with-binary-test-connection
    (prepare-query connection "test" "select ($1::int[])[2]")
    (is (equal (exec-prepared connection "test" '(#(1 2 3)) 'list-row-reader)
               '((2))))
    (prepare-query connection "test2" "select ($1::text[])[2]")
    (is (equal (exec-prepared connection "test2" '(#("A" "B" "C")) 'list-row-reader)
               '(("B"))))))

(test binary-blob
  (with-binary-test-connection
    (let* ((str "foobar42")
           (bytes (coerce #(102 111 111 98 97 114 52 50) '(vector (unsigned-byte 8)))))
      (prepare-query connection "test" "select $1::varchar, $2::bytea")
      (is (equalp (exec-prepared connection "test" (list str bytes) 'list-row-reader)
                  (list (list str bytes)))))))

(test binary-recover-error
  (with-binary-test-connection
    (signals cl-postgres-error:syntax-error-or-access-violation
      (exec-query connection "gubble gubble gabble goo"))
    (is (equal (exec-query connection "select false" 'list-row-reader)
               '((nil))))))

(test binary-unique-violation-error
  (with-binary-test-connection
    (exec-query connection "create temporary table test (id int not null primary key, name text)")
    (exec-query connection "insert into test values (1, 'bert')")
    (signals unique-violation
      (exec-query connection "insert into test values (1, 'harry')"))))

(test binary-sql-reader
  (with-binary-test-connection
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

(test binary-sql-reader-binary
  (with-binary-test-connection
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

(test binary-bulk-writer
  (with-binary-test-connection
    (exec-query connection "create table if not exists test_bulk_writer (a int, b text, c date, d timestamp, e int[])")
    (let ((stream (open-db-writer *cl-postgres-test-connection* 'test_bulk_writer '(a b c d e))))
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
        (is (equalp (second
                     (first
                      (exec-query connection "select * from test_bulk_writer" 'list-row-reader)))
                    "one"))
        (is (equal (first
                    (fourth
                     (exec-query connection "select * from test_bulk_writer" 'list-row-reader)))
                   4))
        (exec-query connection "drop table test_bulk_writer")))

(test binary-row-boolean-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[TRUE, FALSE, TRUE])" 'list-row-reader)
                '(("(\"{t,f,t}\")"))))))

(test binary-row-boolean-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[TRUE, FALSE, TRUE])" 'list-row-reader)
                  '(((#(T NIL T)))))))))

(test binary-cast-to-bits
  (with-binary-test-connection
    (is (equalp (exec-query connection "select cast(255 as bit(8)), cast(-44 as bit(128))" 'list-row-reader)
                '((#*11111111
                   #*11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111010100))))
    (is (equalp (exec-query connection "select row(cast(32 as bit(12)))" 'list-row-reader)
                '(("(000000100000)"))))
    (is (equalp (exec-query connection "select ARRAY[cast(32 as bit(16))]" 'list-row-reader)
                '((#(#*0000000000100000)))))
    (is (equalp (exec-query connection "select row(ARRAY[cast(32 as bit(16))])" 'list-row-reader)
                '(("({0000000000100000})"))))))

(test binary-cast-to-bits-binary
  (with-binary-test-connection
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

(test binary-cast-to-varbits
  (with-binary-test-connection
    (is (equalp (exec-query connection "select 255::bit(8)::varbit(8), 44::bit(128)::varbit(128)" 'list-row-reader)
                '((#*11111111
                   #*00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101100))))
    (is (equalp (exec-query connection "select row(32::bit(12)::varbit(12))" 'list-row-reader)
                '(("(000000100000)"))))
    (is (equalp (exec-query connection "select ARRAY[32::bit(16)::varbit(16)]" 'list-row-reader)
                '((#(#*0000000000100000)))))
    (is (equalp (exec-query connection "select row(ARRAY[32::bit(16)::varbit(16)])" 'list-row-reader)
                '(("({0000000000100000})"))))))

(test binary-cast-to-varbits-binary
  (with-binary-test-connection
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



(test binary-row-integer-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[1,2,4,8])" 'list-row-reader)
                '(("(\"{1,2,4,8}\")"))))))

(test binary-row-integer-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[1,2,4,8])" 'list-row-reader)
                  '(((#(1 2 4 8)))))))))

(test binary-row-string-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY['foo', 'bar', 'baz'])" 'list-row-reader)
                '(("(\"{foo,bar,baz}\")"))))))

(test binary-row-string-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY['foo', 'bar', 'baz'])" 'list-row-reader)
                  '(((#("foo" "bar" "baz")))))))))

(test binary-row-bpchar-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[cast('foo' as bpchar)])" 'list-row-reader)
                '(("({foo})"))))))

(test binary-row-bpchar-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[cast('foo' as bpchar)])" 'list-row-reader)
                  '(((#("foo")))))))))

(test binary-row-varchar-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY['foo'::varchar])" 'list-row-reader)
                '(("({foo})"))))))

(test binary-row-varchar-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY['foo'::varchar])" 'list-row-reader)
                  '(((#("foo")))))))))

(test binary-row-oid-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[1234::oid, 5678::oid])" 'list-row-reader)
                '(("(\"{1234,5678}\")"))))))

(test binary-row-oid-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[1234::oid, 5678::oid])" 'list-row-reader)
                  '(((#(1234 5678)))))))))

(test binary-row-int2-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[1234::int2])" 'list-row-reader)
                '(("({1234})"))))))

(test binary-row-int2-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[1234::int2])" 'list-row-reader)
                  '(((#(1234)))))))))

(test binary-row-int8-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[123456789012::int8])" 'list-row-reader)
                '(("({123456789012})"))))))

(test binary-row-int8-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[123456789012::int8])" 'list-row-reader)
                  '(((#(123456789012)))))))))

(test binary-row-float-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[3.14::float])" 'list-row-reader)
                '(("({3.14})"))))))

(test binary-row-float-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[3.14::float])" 'list-row-reader)
                  '(((#(3.14d0)))))))))

(test binary-row-double-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[cast(3.14 as double precision)])" 'list-row-reader)
                '(("({3.14})"))))))

(test binary-row-double-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[cast(3.14 as double precision)])" 'list-row-reader)
                  '(((#(3.14d0)))))))))

(test binary-row-date-array
  (with-default-readtable
    (with-binary-test-connection
      (is (equalp (elt (exec-query connection "select row(ARRAY['1980-02-01'::date])" 'list-row-reader) 0)
                  '("({1980-02-01})"))))))

(test binary-row-date-array-binary
  (with-default-readtable
    (with-binary-test-connection
      (with-binary-row-values
        (is (= (elt (caaar (exec-query connection "select row(ARRAY['1980-02-01'::date])" 'list-row-reader)) 0)
               2527200000))))))

(test binary-row-timestamp
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row('2010-04-05 14:42:21.500'::timestamp)"
                            'list-row-reader)
                '(("(\"2010-04-05 14:42:21.5\")"))))))

(test binary-row-timestamp-without-time-zone
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row('2010-04-05 14:42:21.500'::timestamp without time zone)"
                            'list-row-reader)
                '(("(\"2010-04-05 14:42:21.5\")"))))))

(test binary-row-timestamp-with-time-zone
  (with-binary-test-connection
    (with-rollbacked-transaction
      (exec-query connection "set time zone 'GMT'")
      (is (equalp (exec-query connection "select row('2010-04-05 14:42:21.500'::timestamp with time zone)"
                              'list-row-reader)
                  '(("(\"2010-04-05 14:42:21.5+00\")")))))))

(test binary-row-timestamp-with-time-zone-binary
  (with-default-readtable
    (with-binary-test-connection
      (with-rollbacked-transaction
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
                        '(3479438541)))))))))

(test binary-row-timestamp-array
  (with-default-readtable
    (with-binary-test-connection
      (is (equalp (elt (exec-query connection "select row(ARRAY['2010-04-05 14:42:21.500'::timestamp])"
                                   'list-row-reader) 0)
                  '("(\"{\"\"2010-04-05 14:42:21.5\"\"}\")"))))))

(test binary-row-timestamp-array-binary
  (with-default-readtable
    (with-binary-row-values
      (with-binary-test-connection
        (is (equalp (elt (exec-query connection "select row(ARRAY['2010-04-05 14:42:21.500'::timestamp])"
                                     'list-row-reader) 0)
                    '((#(3479467341)))))))))

(test binary-row-timestamp-without-time-zone-array
  (with-binary-test-connection
    (is (equalp (elt (exec-query connection "select row(ARRAY['2010-04-05 14:42:21.500'::timestamp without time zone])"
                                 'list-row-reader) 0)
                '("(\"{\"\"2010-04-05 14:42:21.5\"\"}\")")))))

(test binary-row-time
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row('05:00'::time)"
                            'list-row-reader)
                '(("(05:00:00)"))))))

(test binary-row-interval-array
  (with-default-readtable
    (with-binary-test-connection
      (with-binary-row-values
        (is (equalp (elt (caaar (exec-query connection "select row(ARRAY['2 years -4 days'::interval])"
                                            'list-row-reader)) 0)
                    '((:MONTHS 24) (:DAYS -4) (:SECONDS 0) (:USECONDS 0))))))))

(test binary-write-bytea
  (with-binary-test-connection
    (exec-query connection "create temporary table test (a bytea)")
    (unwind-protect
         (let ((random-bytes (make-array *random-byte-count*
                                         :element-type '(unsigned-byte 8)
                                         :initial-element 0)))
           (loop for i below *random-byte-count*
                 do (setf (aref random-bytes i)
                          (random #x100)))
           (prepare-query connection "bytea-insert" "insert into test values ($1)")
           (exec-prepared connection "bytea-insert" (list random-bytes))
           (is (equalp (exec-query connection "select a from test;" 'list-row-reader)
                       `((,random-bytes))))))))

(test binary-write-row-bytea
  (with-binary-test-connection
    (exec-query connection "create temporary table test (a bytea)")
    (let ((*random-byte-count* 16))
      (unwind-protect
           (let ((random-bytes (make-array *random-byte-count*
                                           :element-type '(unsigned-byte 8)
                                           :initial-element 0)))
             (loop for i below *random-byte-count*
                   do (setf (aref random-bytes i)
                            (random #x100)))
             (prepare-query connection "bytea-insert" "insert into test values ($1)")
             (exec-prepared connection "bytea-insert" (list random-bytes))
             (is (equalp (exec-query connection "select row(a) from test;" 'list-row-reader)
                         `((,(concatenate 'string
                                          "(\"\\\\x"
                                          (vector-to-hex-string random-bytes)
                                          "\")"))))))))))

(test binary-write-row-array-bytea
  (with-binary-test-connection
    (with-binary-row-values
      (exec-query connection "create temporary table test (a bytea)")
      (unwind-protect
           (let ((random-bytes (make-array *random-byte-count*
                                           :element-type '(unsigned-byte 8)
                                           :initial-element 0)))
             (loop for i below *random-byte-count*
                   do (setf (aref random-bytes i)
                            (random #x100)))
             (prepare-query connection "bytea-insert" "insert into test values ($1)")
             (exec-prepared connection "bytea-insert" (list random-bytes))
             (is (equalp (exec-query connection "select row(ARRAY[a]) from test;" 'list-row-reader)
                         `(((#(,random-bytes)))))))))))

(test binary-write-row-array-bytea-binary
  (with-binary-test-connection
    (with-binary-row-values
      (exec-query connection "create temporary table test (a bytea)")
      (unwind-protect
           (let ((random-bytes (make-array *random-byte-count*
                                           :element-type '(unsigned-byte 8)
                                           :initial-element 0)))
             (loop for i below *random-byte-count*
                   do (setf (aref random-bytes i)
                            (random #x100)))
             (prepare-query connection "bytea-insert" "insert into test values ($1)")
             (exec-prepared connection "bytea-insert" (list random-bytes))
             (is (equalp (exec-query connection "select row(ARRAY[a]) from test;" 'list-row-reader)
                         `(((#(,random-bytes)))))))))))

(test binary-row-name-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY['foo'::name])" 'list-row-reader)
                '(("({foo})"))))))

(test binary-row-name-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY['foo'::name])" 'list-row-reader)
                  '(((#("foo")))))))))

(test binary-point
  (with-binary-test-connection
    (is (equalp (exec-query connection "select point(1,2)" 'list-row-reader)
                '(((1.0d0 2.0d0)))))))

(test binary-row-point
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(point(1,2))" 'list-row-reader)
                '(("(\"(1,2)\")"))))))

(test binary-row-point-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(point(1,2))" 'list-row-reader)
                  '((((1.0d0 2.0d0)))))))))

(test binary-row-point-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[point(1,2)])" 'list-row-reader)
                '(("(\"{\"\"(1,2)\"\"}\")"))))))

(test binary-row-point-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[point(1,2)])" 'list-row-reader)
                  '(((#((1.0d0 2.0d0))))))))))

(test binary-lseg
  (with-binary-test-connection
    (is (equalp (exec-query connection "select lseg(point(1,2),point(3,4))" 'list-row-reader)
                '((((1.0d0 2.0d0) (3.0d0 4.0d0))))))))

(test binary-row-lseg
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(lseg(point(1,2),point(3,4)))" 'list-row-reader)
                '(("(\"[(1,2),(3,4)]\")"))))))

(test binary-row-lseg-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(lseg(point(1,2),point(3,4)))" 'list-row-reader)
                  '(((((1.0d0 2.0d0) (3.0d0 4.0d0))))))))))

(test binary-row-lseg-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[lseg(point(1,2),point(3,4))])" 'list-row-reader)
                '(("(\"{\"\"[(1,2),(3,4)]\"\"}\")"))))))

(test binary-row-lseg-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[lseg(point(1,2),point(3,4))])" 'list-row-reader)
                  '(((#(((1.0d0 2.0d0) (3.0d0 4.0d0)))))))))))

(test binary-box
  (with-binary-test-connection
    (is (equalp (exec-query connection "select box(point(1,2),point(3,4))" 'list-row-reader)
                '((((3.0d0 4.0d0) (1.0d0 2.0d0))))))))

(test binary-row-box
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(box(point(1,2),point(3,4)))" 'list-row-reader)
                '(("(\"(3,4),(1,2)\")"))))))

(test binary-row-box-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(box(point(1,2),point(3,4)))" 'list-row-reader)
                  '(((((3.0d0 4.0d0) (1.0d0 2.0d0))))))))))

(test binary-row-box-array
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row(ARRAY[box(point(1,2),point(3,4))])" 'list-row-reader)
                '(("(\"{(3,4),(1,2)}\")"))))))

(test binary-row-box-array-binary
  (with-binary-test-connection
    (with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[box(point(1,2),point(3,4))])" 'list-row-reader)
                  '(((#(((3.0d0 4.0d0) (1.0d0 2.0d0)))))))))))

(test binary-row-array-nulls
  (with-binary-test-connection
    (is (equalp (exec-query connection "select row((ARRAY[1,3,4])[5:99])" 'list-row-reader)
                '(("({})"))))))

(test binary-row-array-nulls-binary
  (with-binary-test-connection
    (cl-postgres::with-binary-row-values
      (is (equalp (exec-query connection "select row((ARRAY[1,3,4])[5:99])" 'list-row-reader)
                  '(((NIL))))))))

(test binary-row-array-nulls-binary-2
  (with-binary-test-connection
    (cl-postgres::with-binary-row-values
      (is (equalp (exec-query connection "select row(ARRAY[NULL, NULL]);" 'list-row-reader)
                  '(((#(:null :null)))))))))

(test binary-row-array-table-nulls-binary
  (with-binary-row-values
    (with-binary-test-connection
      (exec-query connection "create temporary table test (a integer[])")
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
                  '(((#2A((0 0)))) ((NIL)) ((#2A((2 2)))) ((NIL)) ((NIL))))))))))

(test binary-array-row-text
  (with-binary-test-connection
    (is (equalp (exec-query connection "select array_agg(row(1,2,3));" 'list-row-reader)
                '(("{\"(1,2,3)\"}"))))))

(test binary-array-row-binary
  (with-binary-test-connection
    (cl-postgres::with-binary-row-values
      (is (equalp (exec-query connection "select array_agg(row(1,2,3));" 'list-row-reader)
                  '((#((1 2 3)))))))))
