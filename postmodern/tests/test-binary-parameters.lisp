;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(def-suite :postmodern-binary-parameters
  :description "Prepared query suite for postmodern"
  :in :postmodern)

(in-suite :postmodern-binary-parameters)

(defun create-data-type-table ()
  (with-test-connection
    (query
     (:create-table 'data-type-tests
                    ((id :type integer :primary-key t :identity-always t)
                     (int2 :type int2 :default 2)
                     (int4 :type int4 :default 2147483645)
                     (int8 :type int8 :default 9223372036854775803)
                     (float :type float :default 782.31)
                     (dfloat :type double-float :default 2.38921379231d8)
                     (numeric :type (or (numeric 19 5) db-null))
                     (char :type (or char db-null))
                     (varchar :type (or varchar db-null))
                     (json :type (or json db-null))
                     (jsonb :type (or jsonb db-null))
                     (point :type (or point db-null))
                     (path :type (or path db-null))
                     (box :type (or box db-null))
                     (polygon :type (or polygon db-null))
                     (line :type (or line db-null))
                     (circle :type (or circle db-null))
                     (cidr :type (or cidr db-null))
                     (inet :type (or inet db-null))
                     (cidr6 :type (or cidr db-null))
                     (inet6 :type (or inet db-null))
                     (macaddr :type (or macaddr db-null))
                     (macaddr8 :type (or macaddr8 db-null))
                     (bit :type (or bit db-null))
                     (xml :type (or xml db-null))
                     (int4range :type (or int4range db-null))
                     (uuid :type (or uuid db-null))
                     (text-array :type (or text[] db-null))
                     (integer-array :type (or integer[] db-null))
                     (bytea :type (or bytea db-null))
                     (lseg :type (or lseg db-null))
                     (timestamp-without-time-zone
                      :type (or timestamp-without-time-zone db-null))
                     (timestamp-with-time-zone
                      :type (or timestamp-with-time-zone db-null))
                     (timestamptz
                      :type (or timestamptz db-null))
                     (timestamp
                      :type (or timestamp db-null))
                     (time
                      :type (or time db-null))
                     (date :type (or date db-null)))))))

(defun add-data-to-data-type-table ()
  (with-test-connection
    (query
     (:insert-into 'data-type-tests
      :set
      'int2 2
      'int4 232
      'int8 3211223
      'float -72.3
      'dfloat 2312321.321d0
      'numeric -2312321987/1000
      'char "A"
      'varchar "a1b2"
      'json "{ \"customer\": \"John Doe\", \"items\": {\"product\": \"Beer\",\"qty\": 6}}"
      'jsonb "{\"title\": \"Sleeping Beauties\", \"genres\": [\"Fiction\", \"Thriller\", \"Horror\"]}"
      'point "(1, 2)"
      'path "((1,1),(2,3),(5,2))"
      'box "((1,1),(2,2))"
      'polygon "((1, 2) , (6,8), (23, 4))"
      'line "{-1, 3,5}"
      'circle "<(1, 2) , 5>"
      'cidr "192.168.100.128/25"
      'inet "192.168.100.128"
      'cidr6 "2001:4f8:3:ba::/64"
      'inet6 "2001:4f8:3:ba::"
      'macaddr "08:00:2b:01:02:03"
      'macaddr8 "08:00:2b:01:02:03:04:05"
      'bit (:type 1 'bit)     ; or could be "1"
      'xml "<foo>bar</foo>"
      'int4range "(15, 25)"
      'uuid "40e6215d-b5c6-4896-987c-f30f3678f608"
      'text-array #("one" "two" "three")
      'integer-array #(1 2 3)
      'bytea "\xDEADBEEF"
      'lseg "[(1, 2) , (6,8)]"
      'timestamp-without-time-zone "2018-03-19 13:10:25-07"
      'timestamp-with-time-zone "2018-03-19 13:10:25-07"
      'timestamptz "2018-03-19 13:10:25-07"
      'timestamp "2018-07-21 14:10:25-05"
      'time "13:00:00"
      'date "1998-03-17"))
    (query
     (:insert-into 'data-type-tests
      :set
      'int2 5
      'int4 798
      'int8 97837892237897
      'float -97.57
      'dfloat 4532323412321.321d0
      'numeric -64892312321987/1000
      'char "b"
      'varchar "yze1b2"
      'json "{ \"customer\": \"Lara Smith\", \"items\": {\"product\": \"Wine\",\"qty\": 3}}"
      'jsonb "{\"title\": \"UTF8 For Fun and Profit\", \"genres\": [\"Fiction\", \"Computer Programming\", \"Horror\"]}"
      'point "(8, 4)"
      'path "((1,2),(982,233),(65,32))"
      'box "((1,1),(7,7))"
      'polygon "((1, 2) , (97,4), (37, 6))"
      'line "{-4,4,9}"
      'circle "<(1,5),8>"
      'cidr "192.168.101.128/25"
      'inet "192.168.101.128"
      'cidr6 "2001:4e8:3:ba::/64"
      'inet6 "2001:4f7:3:ba::"
      'macaddr "08:00:2b:01:02:04"
      'macaddr8 "08:00:2b:01:02:04:05:06"
      'bit (:type 0 'bit)     ; or could be "1"
      'xml "<bar>zebra</bar>"
      'int4range "(12, 23)"
      'uuid "40e6214d-b5c6-4896-987c-f30f3678f608"
      'text-array #("five" "six" "seven" "eight")
      'integer-array #(8 1 353)
      'bytea "\xDEADAEEA"
      'lseg "[(1, 4) , (5,9)]"
      'timestamp-without-time-zone "2018-04-17 13:10:25-07"
      'timestamp-with-time-zone "2018-05-19 14:10:25-07"
      'timestamptz "2019-07-17 13:12:25-07"
      'timestamp "2020-08-16 15:11:25-05"
      'time "15:12:00"
      'date "2001-04-16"))))

(defmacro with-build-binary-fixture (&body body)
  `(progn
     (when (table-exists-p 'data-type-tests) (drop-table 'data-type-tests))
     (create-data-type-table)
     (add-data-to-data-type-table)
     (unwind-protect (progn ,@body)
       (execute (:drop-table :if-exists 'data-type-tests :cascade)))))

(test basic-binary
  (with-test-connection
    (with-binary
      (is (equal (pomo:query "select $1" 1 :single) 1))
      (is (equal (pomo:query (:select '$1) -11 :single) -11))
      (is (equal (pomo:query "select $1" 2 :single) 2))
      (is (equal (pomo:query (:select '$1) -1 :single) -1))
      (is (equal (pomo:query "select $1" 1.1 :single) 1.1))
      (is (equal (pomo:query (:select '$1) -11.2 :single) -11.2))
      (is (equal (pomo:query "select $1" 1.3 :single) 1.3))
      (is (equal (pomo:query (:select '$1) -1.4 :single) -1.4))
      (is (equal (pomo:query "select $1" 2312321.321 :single) 2312321.3))
      (is (equal (pomo:query "select $1" -2312321.321 :single) -2312321.3))
      #-clisp (is (equal (pomo:query "select $1" 2312321.321d0 :single) 2312321.321d0))
      #-clisp (is (equal (pomo:query "select $1" -2312321.321d0 :single) -2312321.321d0))
      #+clisp (is (equal (pomo:query "select $1" 2312321.321d0 :single) 2312321.3))
      #+clisp (is (equal (pomo:query "select $1" -2312321.321d0 :single) -2312321.3))
      (is (equal (pomo:query "select $1" "foo" :single) "foo"))
      (is (equal (pomo:query (:select '$1) t :single) T))
      (is (equal (pomo:query "select $1" t :single) T))
      (is (equal (query "select $1" -64892312321987/1000 :single) "-64892312321.987"))
      #-clisp (is (equal (pomo:query "select $1::numeric(19,5)" -2312321.987d0 :single)
                         -2312321987/1000))
      #+clisp (is (equal (pomo:query "select $1::numeric(19,5)" -2312321.987d0 :single)
                 -2312320))
      ;; The previous had an explicit double-float. If youremove the double float, it rounds down. Why?
      (is (equal (pomo:query "select $1::numeric(19,5)" -2312321.321 :single)
                         -2312320))
            ;; The following is because Postmodern will back out of binary parameters if any
      ;; parameters are strings or other items that Postgresql might interpret as
      ;; different types.
      (is (equal (pomo:query (:select '$1 '$2) 1 "foo") '((1 "foo")))))))

(test binary-parameters-with-table
  (with-test-connection
    (with-binary
      (with-build-binary-fixture
        (query (:update 'data-type-tests :set 'numeric -2312321987/1000 :where (:= 'id 1)))
        (is (equal (query (:select 'numeric :from 'data-type-tests :where (:= 'id 1)) :single)
                   -2312321987/1000))
        (is (equal (query "select numeric from data_type_tests where int2 = $1" 2 :single)
                   -2312321987/1000))
        (is (equal (query "select id from data_type_tests where int2 = $1" 5  :single)
                   2))
        (signals error (query "select id from data_type_tests where int2 = $1" "A" :single))
        (is (not (query "select id from data_type_tests where int2 = $1" 144220  :single)))
        (is (equal (query "select id from data_type_tests where int4 = $1" 232 :single)
                   1))
        (is (equal (query "select id from data_type_tests where int8 = $1" 3211223 :single)
                   1))
        (is (equal (query "select id from data_type_tests where char = $1" "A"  :single)
                   1))
        (is (equal (query "select id from data_type_tests where varchar = $1" "a1b2" :single)
                   1))
        (is (equal (query "select id from data_type_tests where cidr = $1" "192.168.100.128/25"
                          :single)
                   1))
        (is (equal (query "select id from data_type_tests where inet = $1" "192.168.100.128"  :single)
                   1))
        (is (equal (query "select id from data_type_tests where inet6 = $1" "2001:4f8:3:ba::"
                          :single)
                   1))
        (is (equal (query "select id from data_type_tests where cidr6 = $1" "2001:4f8:3:ba::/64"
                          :single)
                   1))
        (is (equal (query "select id from data_type_tests where macaddr = $1" "08:00:2b:01:02:03"
                          :single)
                   1))
        (is (equal (query "select id from data_type_tests where macaddr8 = $1" "08:00:2b:01:02:03:04:05"
                          :single)
                   1))
        (is (equal (query "select id from data_type_tests where bit = $1::bit" "1"  :single)
                   1))
        (is (equal (query "select id from data_type_tests where bit = $1" "1"  :single)
                   1))
        (is (equal (query "select id from data_type_tests where bit = $1" "B1"  :single)
                   1))
        ;; The following triggers an error because postgresql does not have cast from
        ;; small integer to bit and postmodern is typing the 1 as a int2.
        (signals error (query "select id from data_type_tests where bit = $1::bit" 1  :single))
        (signals error (query "select id from data_type_tests where bit = $1" 1  :single))
        (signals error (query "select id from data_type_tests where bit = $1"
                              (coerce 1 'bit) :single))
        (is (equal (query "select id from data_type_tests where uuid = $1"
                          "40e6215d-b5c6-4896-987c-f30f3678f608"  :single)
                   1))
        (signals error (query "select id from data_type_tests where xml = $1" "<foo>bar</foo>"
                              :single))
        ;; Per Postgresql docs https://www.postgresql.org/docs/current/datatype-xml.html
        ;; The xml data type is unusual in that it does not provide any comparison operators.
        ;; This is because there is no well-defined and universally useful comparison algorithm
        ;; for XML data.
        (is (equal (query "select id from data_type_tests where point ~= $1" "(1,2)"  :single)
                   1))
        (is (equal (query "select id from data_type_tests where polygon ~= $1"
                          "((1, 2) , (6,8), (23, 4))"  :single)
                   1))
        (is (equal (query "select id from data_type_tests where bytea = $1" "\xDEADBEEF"  :single)
                   1))
        (is (equal (query "select id from data_type_tests where text_array = $1" #("one" "two" "three")
                          :single)
                   1))

        (is (equal (query "select id from data_type_tests where integer_array = $1" #(1 2 3)
                          :single)
                   1))
        (is (equal (query "select id from data_type_tests where box = $1" "((1,1),(2,2))"  :single)
                   1))

        (is (equal (query "select id from data_type_tests where box = $1" "((1,1),(2,2))"  :single)
                   1))
        (is (equal (query "select id from data_type_tests where line = $1" "{-1, 3,5}" :single)
                   1))
        (is (equal (query "select id from data_type_tests where circle = $1" "<(1, 2) , 5>" :single)
                   1))))))

(test return-binary-types
  (with-test-connection
    (with-binary
      (with-build-binary-fixture
        (is (equal (query (:select 'int2 :from 'data-type-tests :where (:= 'id 1)) :single)
                   2))
        (is (equal (query (:select 'int4 :from 'data-type-tests :where (:= 'id 1)) :single)
                   232))
        (is (equal (query (:select 'int8 :from 'data-type-tests :where (:= 'id 1)) :single)
                   3211223))
        (is (equal (query (:select 'float :from 'data-type-tests :where (:= 'id 1)) :single)
                   -72.3))
        (is (equal (query (:select 'dfloat :from 'data-type-tests :where (:= 'id 1)) :single)
                   2312321.321d0))
        (is (equal (query (:select 'numeric :from 'data-type-tests :where (:= 'id 1)) :single)
                   -2312321987/1000))
        (is (equal (query (:select 'char :from 'data-type-tests :where (:= 'id 1)) :single)
                   "A"))
        (is (equal (query (:select 'varchar :from 'data-type-tests :where (:= 'id 1)) :single)
                   "a1b2"))
        (is (equal (query (:select 'point :from 'data-type-tests :where (:= 'id 1)) :single)
                   '(1.0d0 2.0d0)))
        (is (equal (query (:select 'box :from 'data-type-tests :where (:= 'id 1)) :single)
                   '((2.0d0 2.0d0) (1.0d0 1.0d0))))
        (is (equal (query (:select 'polygon :from 'data-type-tests :where (:= 'id 1)) :single)
                   "((1,2),(6,8),(23,4))"))
        (is (equal (query (:select 'line :from 'data-type-tests :where (:= 'id 1)) :single)
                   "{-1,3,5}"))
        (is (equal (query (:select 'circle :from 'data-type-tests :where (:= 'id 1)) :single)
                   "<(1,2),5>"))
        (is (equal (query (:select 'cidr :from 'data-type-tests :where (:= 'id 1)) :single)
                   "192.168.100.128/25"))
        (is (equal (query (:select 'cidr6 :from 'data-type-tests :where (:= 'id 1)) :single)
                   "2001:4f8:3:ba::/64"))
        (is (equal (query (:select 'inet :from 'data-type-tests :where (:= 'id 1)) :single)
                   "192.168.100.128"))
        (is (equal (query (:select 'inet6 :from 'data-type-tests :where (:= 'id 1)) :single)
                   "2001:4f8:3:ba::"))
        (is (equal (query (:select 'macaddr :from 'data-type-tests :where (:= 'id 1)) :single)
                   "08:00:2b:01:02:03"))
        (is (equal (query (:select 'macaddr8 :from 'data-type-tests :where (:= 'id 1)) :single)
                   "08:00:2b:01:02:03:04:05"))
        (is (equal (query (:select 'bit :from 'data-type-tests :where (:= 'id 1)) :single)
                   #*1))
        (is (equal (query (:select 'xml :from 'data-type-tests :where (:= 'id 1)) :single)
                   "<foo>bar</foo>"))
        (is (equal (query (:select 'int4range :from 'data-type-tests :where (:= 'id 1)) :single)
                   "[16,25)"))
        (is (equal (query (:select 'uuid :from 'data-type-tests :where (:= 'id 1)) :single)
                   "40e6215d-b5c6-4896-987c-f30f3678f608"))
        (is (equalp (query (:select 'text-array :from 'data-type-tests :where (:= 'id 1)) :single)
                    #("one" "two" "three")))
        (is (equalp (query (:select 'integer-array :from 'data-type-tests :where (:= 'id 1)) :single)
                    #(1 2 3)))
        (is (equalp (query (:select 'bytea :from 'data-type-tests :where (:= 'id 1)) :single)
                    #(120 68 69 65 68 66 69 69 70)))
        (is (equal (query (:select 'lseg :from 'data-type-tests :where (:= 'id 1)) :single)
                   '((1.0d0 2.0d0) (6.0d0 8.0d0))))))))

(test binary-parameters-basic-2-parameters-nil-1
  (with-test-connection
    (with-build-binary-fixture
      (is (equal (pomo:query "select $1, $2" "10" 20)
                 '(("10" "20"))))
      (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0))
                 '(("10" "0.33333334"))))
      #-clisp (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0d0))
                         '(("10" "3.333333333333333E-1"))))
      #+clisp (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0d0))
                 '(("10" "0.3333333333333333"))))
      (is (equal (pomo:query "select $1, $2" "10" t)
                 '(("10" "true"))))
      (is (equal (pomo:query "select $1, $2" "10" nil)
                 '(("10" "false"))))
      (is (equal (pomo:query "select xml from data_type_tests where int2=$1 and float=$2"
                             2 -72.3 :single)
                 "<foo>bar</foo>"))
      (is (equal (pomo:query "select xml from data_type_tests where int4=$1 and dfloat=$2"
                             232 2312321.321d0 :single)
                 "<foo>bar</foo>"))
      (is (equal (pomo:query "select xml from data_type_tests where char=$1 and varchar=$2"
                             "A" "a1b2" :single)
                 "<foo>bar</foo>"))
      (is (equal (pomo:query "select xml from data_type_tests where point~=$1 and circle=$2"
                             "(8, 4)" "<(1,5),8>" :single)
                 "<bar>zebra</bar>"))
      (is (equal (query (:select 'xml
                         :from 'data-type-tests
                         :where (:and (:~= 'point '$1)
                                      (:= 'circle '$2)))
                        "(8,4)" "<(1,5),8>" :single)
                 "<bar>zebra</bar>"))
      (is (equal (pomo:query "select xml from data_type_tests where uuid=$1 and numeric=$2"
                             "40e6214d-b5c6-4896-987c-f30f3678f608" -64892312321987/1000 :single)
                 "<bar>zebra</bar>")))))

(test binary-parameters-basic-2-parameters-nil-2
  (with-binary-test-connection
    (without-binary
      (with-build-binary-fixture
        (is (equal (pomo:query "select $1, $2" "10" 20)
                   '(("10" "20"))))
        (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0))
                   '(("10" "0.33333334"))))
        #-clisp (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0d0))
                           '(("10" "3.333333333333333E-1"))))
        #+clisp (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0d0))
                   '(("10" "0.3333333333333333"))))
        (is (equal (pomo:query "select $1, $2" "10" t)
                   '(("10" "true"))))
        (is (equal (pomo:query "select $1, $2" "10" nil)
                   '(("10" "false"))))
        (is (equal (pomo:query "select xml from data_type_tests where int2=$1 and float=$2"
                               2 -72.3 :single)
                   "<foo>bar</foo>"))
        (is (equal (pomo:query "select xml from data_type_tests where int4=$1 and dfloat=$2"
                               232 2312321.321d0 :single)
                   "<foo>bar</foo>"))
        (is (equal (pomo:query "select xml from data_type_tests where char=$1 and varchar=$2"
                               "A" "a1b2" :single)
                   "<foo>bar</foo>"))
        (is (equal (pomo:query "select xml from data_type_tests where point~=$1 and circle=$2"
                               "(8, 4)" "<(1,5),8>" :single)
                   "<bar>zebra</bar>"))
        (is (equal (query (:select 'xml
                           :from 'data-type-tests
                           :where (:and (:~= 'point '$1)
                                        (:= 'circle '$2)))
                          "(8,4)" "<(1,5),8>" :single)
                   "<bar>zebra</bar>"))
        (is (equal (pomo:query "select xml from data_type_tests where uuid=$1 and numeric=$2"
                               "40e6214d-b5c6-4896-987c-f30f3678f608" -64892312321987/1000 :single)
                   "<bar>zebra</bar>"))))))

;; setting up binary while setting up the connection
(test binary-parameters-basic-2-parameters-t-1
  (with-binary-test-connection
    (with-build-binary-fixture
      (is (equal (pomo:query "select $1, $2" "10" 20)
                 '(("10" 20))))
      (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0))
                 '(("10" 0.33333334))))
      #-clisp (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0d0))
                         '(("10" 0.3333333333333333d0))))
      #+clisp (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0d0))
                 '(("10" 0.33333334))))
      #-clisp (is (equal (pomo:query "select $1, $2" 10 (/ 1 3.0d0))
                 '((10 0.3333333333333333d0))))
      #+clisp (is (equal (pomo:query "select $1, $2" 10 (/ 1 3.0d0))
                 '((10 0.33333334))))
      (is (equal (pomo:query "select $1, $2" "10" t)
                 '(("10" T))))
      (is (equal (pomo:query "select $1, $2" "10" nil)
                 '(("10" NIL))))
      (is (equal (pomo:query "select xml from data_type_tests where int2=$1 and float=$2"
                             2 -72.3 :single)
                 "<foo>bar</foo>"))
      #-clisp (is (equal (pomo:query "select xml from data_type_tests where int4=$1 and dfloat=$2"
                             232 2312321.321d0 :single)
                         "<foo>bar</foo>"))
      (is (equal (pomo:query "select xml from data_type_tests where char=$1 and varchar=$2"
                             "A" "a1b2" :single)
                 "<foo>bar</foo>"))
      (is (equal (pomo:query "select xml from data_type_tests where point~=$1 and circle=$2"
                             "(8, 4)" "<(1,5),8>" :single)
                 "<bar>zebra</bar>"))
      (is (equal (query (:select 'xml
                         :from 'data-type-tests
                         :where (:and (:~= 'point '$1)
                                      (:= 'circle '$2)))
                        "(8,4)" "<(1,5),8>" :single)
                 "<bar>zebra</bar>"))
      (is (equal (pomo:query "select xml from data_type_tests where uuid=$1 and numeric=$2"
                             "40e6214d-b5c6-4896-987c-f30f3678f608" -64892312321987/1000 :single)
                 "<bar>zebra</bar>")))))

;; adding binary after creating the database connection
(test binary-parameters-basic-2-parameters-t-2
  (with-test-connection
    (with-binary
      (with-build-binary-fixture
        (is (equal (pomo:query "select $1, $2" "10" 20)
                   '(("10" 20))))
        (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0))
                   '(("10" 0.33333334))))
        #-clisp (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0d0))
                   '(("10" 0.3333333333333333d0))))
        #-clisp (is (equal (pomo:query "select $1, $2" 10 (/ 1 3.0d0))
                           '((10 0.3333333333333333d0))))
        #+clisp (is (equal (pomo:query "select $1, $2" "10" (/ 1 3.0d0))
                   '(("10" 0.33333334))))
        #+clisp (is (equal (pomo:query "select $1, $2" 10 (/ 1 3.0d0))
                   '((10 0.33333334))))
        (is (equal (pomo:query "select $1, $2" "10" t)
                   '(("10" T))))
        (is (equal (pomo:query "select $1, $2" "10" nil)
                   '(("10" NIL))))
        (is (equal (pomo:query "select xml from data_type_tests where int2=$1 and float=$2"
                               2 -72.3 :single)
                   "<foo>bar</foo>"))
        #-clisp (is (equal (pomo:query "select xml from data_type_tests where int4=$1 and dfloat=$2"
                               232 2312321.321d0 :single)
                   "<foo>bar</foo>"))
        (is (equal (pomo:query "select xml from data_type_tests where char=$1 and varchar=$2"
                               "A" "a1b2" :single)
                   "<foo>bar</foo>"))
        (is (equal (pomo:query "select xml from data_type_tests where point~=$1 and circle=$2"
                               "(8, 4)" "<(1,5),8>" :single)
                   "<bar>zebra</bar>"))
        (is (equal (query (:select 'xml
                           :from 'data-type-tests
                           :where (:and (:~= 'point '$1)
                                        (:= 'circle '$2)))
                          "(8,4)" "<(1,5),8>" :single)
                   "<bar>zebra</bar>"))
        (is (equal (pomo:query "select xml from data_type_tests where uuid=$1 and numeric=$2"
                               "40e6214d-b5c6-4896-987c-f30f3678f608" -64892312321987/1000 :single)
                   "<bar>zebra</bar>"))))))
