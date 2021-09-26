;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES-TESTS; -*-
(in-package :cl-postgres-tests)

(defvar *cl-postgres-test-connection* nil
  "A list of 6 connection parameters to use when running the tests.  The
  order is: database name, user, password, hostname, port (an integer) and a keyword for use-ssl.")

(defparameter *random-byte-count* 8192)

(defun make-keyword (str)
  "Upcases and interns a string in the keyword package. If the string has a leading colon, drop it."
  (when (eq #\: (aref str 0))
    (setf str (subseq str 1)))
  (intern (string-upcase str) :keyword))

(defun vector-to-hex-string (vec)
  (with-output-to-string (s)
    (map nil (lambda (x)
               (format s "~(~2,\'0x~)" x))
         vec)
    s))

(defun prompt-connection (&optional (defaults '("test" "test" "" "localhost" 5432 :no)))
  (when *cl-postgres-test-connection*
    (return-from prompt-connection *cl-postgres-test-connection*))
  (let* ((descriptions '("Database name" "User" "Password" "Hostname" "Port" "Use SSL"))
         (env-vars '("DB_NAME" "DB_USER" "DB_PASS" "DB_HOST" "DB_PORT" "USE_SSL"))
         (provided (mapcar #'uiop:getenv env-vars))
         (prospective (mapcar (lambda (a b) (if a a b)) provided defaults)))
    (setq *cl-postgres-test-connection*
          (handler-case
              (let ((connection (apply #'open-database prospective)))
                (close-database connection)
                prospective)
            (error (condition)
              (flet ((ask (name provided default)
                       (format *query-io*
                               "~A (enter to keep '~:[~A~;~:*~A~]'): "
                               name provided default)
                       (finish-output *query-io*)
                       (let ((answer (read-line *query-io*)))
                         (if (string= answer "")
                             (if provided provided default)
                             answer))))
                (format *query-io* "~&~
Could not connect to test database:~%~%  ~A

To run tests, you must provide database connection parameters.  To
avoid interactive input you can set the following environment
variables:~:{~%  ~A: ~(~A~), ~:[defaults to \"~A\"~;~:*provided \"~A\"~]~}~%"
                        condition
                        (mapcar #'list env-vars descriptions provided defaults))
                (mapcar #'ask descriptions provided defaults)))))
    (when (stringp (elt *cl-postgres-test-connection* 4))
      (setf (elt *cl-postgres-test-connection* 4)
            (parse-integer (elt *cl-postgres-test-connection* 4))))
    (when (stringp (elt *cl-postgres-test-connection* 5))
      (setf (elt *cl-postgres-test-connection* 5)
            (make-keyword (elt *cl-postgres-test-connection* 5))))
    *cl-postgres-test-connection*))

;; Adjust the above to some db/user/pass/host/[port] combination that
;; refers to a valid postgresql database, then after loading the file,
;; run the tests with (run! :cl-postgres)

(def-suite :cl-postgres
  :description "Test suite for cl-postgres")
(in-suite :cl-postgres)

(defmacro with-test-connection (&body body)
  `(let ((connection (apply 'open-database (prompt-connection))))
     (unwind-protect (progn ,@body)
       (close-database connection))))

;; Remember that open-database takes optional parameters, not keyword parameters
;; so you need to pass in service and application name before use-binary
(defmacro with-binary-test-connection (&body body)
  `(let ((connection (apply 'open-database (append (prompt-connection) '("postgres" "" t)))))
     (unwind-protect (progn ,@body)
       (close-database connection))))


(defmacro with-default-readtable (&body body)
  `(let ((*sql-readtable* (default-sql-readtable)))
     ,@body))

(defmacro with-rollbacked-transaction (&body body)
  `(progn
     (exec-query connection "start transaction")
     (unwind-protect (progn ,@body)
       (exec-query connection "rollback"))))

;; Needs to be within a with-test-connection scope
(defmacro with-binary (&body body)
  `(let ((old-use-binary-parameters (connection-use-binary connection)))
     (setf (connection-use-binary connection) t)
     (unwind-protect (progn ,@body)
       (setf (connection-use-binary connection) old-use-binary-parameters))))

(defmacro without-binary (&body body)
  `(let ((old-use-binary-parameters (connection-use-binary connection)))
     (setf (connection-use-binary connection) nil)
     (unwind-protect (progn ,@body)
       (setf (connection-use-binary connection) old-use-binary-parameters))))

(test connect-sanity
  (with-test-connection
    (is (database-open-p connection))
    (close-database connection)
    (is (not (database-open-p connection)))
    (reopen-database connection)
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

(test timestamp-with-time-zone
  (with-default-readtable
    (with-test-connection
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

(test timestamp-with-time-zone-text
  (let ((*sql-readtable* (copy-sql-readtable)))
    (set-sql-reader oid:+timestamptz+ nil)
    (with-test-connection
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

(test alist-row-reader
  (with-test-connection
    (is (equal (exec-query connection "select 42 as foo, 99 as bar" 'alist-row-reader)
               '((("foo" . 42) ("bar" . 99)))))))

(test vector-row-reader
  (with-test-connection
    (is (equalp (exec-query connection "select 12, 'a'" 'vector-row-reader)
                #(#(12 "a"))))))

(test ignore-row-reader
  (with-test-connection
    (is (not (exec-query connection "select 12, 'a'" 'ignore-row-reader)))))

(test prepare-query-empty-names
  (with-test-connection
    (prepare-query connection "" "select $1")
    (is (equal (exec-query connection "select * from pg_prepared_statements" 'list-row-reader)
               nil))
    (prepare-query connection "tte" "select $1")
    (is (equalp (exec-query connection
                           "select name,statement, parameter_types from pg_prepared_statements"
                           'list-row-reader)
                '(("tte" "select $1" "{text}"))))))

(test exec-queries
  (with-test-connection
    (is (equal (exec-query connection "select 1" 'list-row-reader)
               '((1))))
    (is (equal (exec-query connection "select 1::integer" 'list-row-reader)
               '((1))))
    (signals error (exec-query connection "select 123456789::int2" 'list-row-reader))
    (is (equal (exec-query connection "select 123456789" 'list-row-reader)
               '((123456789))))
    (is (equal (exec-query connection "select 'text'" 'list-row-reader)
               '(("text"))))))

(test prepare-and-exec-query-without-binary
  (with-test-connection
    (prepare-query connection "test1" "select $1::integer")
    (is (equal (exec-prepared connection "test1" '(42) 'list-row-reader)
               '((42))))
    (prepare-query connection "test2" "select $1::integer, $2::boolean")
    (is (equal (exec-prepared connection "test2" '(42 nil) 'list-row-reader)
               '((42 nil))))
    (prepare-query connection "test3" "select $1::integer, $2::boolean, $3::text")
    (is (equal (exec-prepared connection "test3" '(42 t "foo") 'list-row-reader)
               '((42 t "foo"))))
    (is (equal (exec-prepared connection "test3" '(42 nil "foo") 'list-row-reader)
               '((42 nil "foo"))))
    (prepare-query connection "test4" "select $1")
    (is (equal (exec-prepared connection "test4" '(42) 'list-row-reader)
               '(("42"))))))

(test unprepare-query
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
    (exec-query connection "create temporary table test (id int not null primary key, name text)")
    (exec-query connection "insert into test values (1, 'bert')")
    (signals unique-violation
      (exec-query connection "insert into test values (1, 'harry')"))))

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
    (with-rollbacked-transaction
      (exec-query connection "set time zone 'GMT'")
      (is (equalp (exec-query connection "select row('2010-04-05 14:42:21.500'::timestamp with time zone)"
                              'list-row-reader)
                  '(("(\"2010-04-05 14:42:21.5+00\")")))))))

(test row-timestamp-with-time-zone-binary
  (with-default-readtable
    (with-test-connection
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

(test write-bytea
  (with-test-connection
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

(test write-row-bytea
  (with-test-connection
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

(test write-row-array-bytea
  (with-test-connection
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
             (is (equalp (exec-query connection "select row(ARRAY[a]) from test;" 'list-row-reader)
                         `(((#(,random-bytes)))))))))))

(test write-row-array-bytea
  (with-test-connection
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

(test write-row-array-bytea-binary
  (with-test-connection
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

(test array-row-text
  (with-test-connection
    (is (equalp (exec-query connection "select array_agg(row(1,2,3));" 'list-row-reader)
                '(("{\"(1,2,3)\"}"))))))

(test array-row-binary
  (with-test-connection
    (cl-postgres::with-binary-row-values
      (is (equalp (exec-query connection "select array_agg(row(1,2,3));" 'list-row-reader)
                  '((#((1 2 3)))))))))

(test write-ratio-as-floating-point
  (let ((old-silently-truncate *silently-truncate-ratios*))
    (setf cl-postgres:*silently-truncate-ratios* nil)
    (signals error (cl-postgres::write-ratio-as-floating-point (/ 1321 7) *standard-output* 5))
    (setf cl-postgres:*silently-truncate-ratios* t)
    (is (equal  (with-output-to-string (s)
                  (cl-postgres::write-ratio-as-floating-point (/ 1321 7) s 5))
                "188.71"))
    (setf cl-postgres:*silently-truncate-ratios* old-silently-truncate)))
