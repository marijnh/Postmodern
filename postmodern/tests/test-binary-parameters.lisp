;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(fiveam:def-suite :postmodern-binary-parameters
    :description "Prepared query suite for postmodern"
    :in :postmodern)

(fiveam:in-suite :postmodern-binary-parameters)

(defun create-data-type-table ()
  (pomo:query (:create-table 'data-type-tests
                        ((id :type integer :primary-key t :identity-always t)
                         (int2 :type int2 :default 2)
                         (int4 :type int4 :default 2147483645)
                         (int8 :type int8 :default 9223372036854775803)
                         (float :type float :default 782.31)
                         (dfloat :type double-float :default 2.38921379231d8)
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
                         (date :type (or date db-null))
                         (len :type (or interval db-null) :interval :hour-to-minute)))))

(defun add-data-to-data-type-table ()
  (pomo:query (:insert-into 'data-type-tests
               :set
               'int2 2
               'int4 232
               'int8 3211223
               'float -72.3
               'dfloat 2312321.321
               'char "A"
               'json "{ \"customer\": \"John Doe\", \"items\": {\"product\": \"Beer\",\"qty\": 6}}"
               'jsonb "{\"title\": \"Sleeping Beauties\", \"genres\": [\"Fiction\", \"Thriller\", \"Horror\"]}"
               'uuid "40e6215d-b5c6-4896-987c-f30f3678f608"
               'varchar "a1b2"
               'integer-array #(1 2 3)
               'text-array #("one" "two" "three")
               'point "(1, 2)"))
  (pomo:query (:update 'data-type-tests :set 'point "(1, 2)" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'line "{-1, 3,5}" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'lseg "[(1, 2) , (6,8)]" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'lseg "[(1, 2) , (6,8)]" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'polygon "((1, 2) , (6,8), (23, 4))" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'circle "<(1, 2) , 5>" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'bit "1" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'cidr "192.168.100.128/25" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'inet "192.168.100.128" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'cidr6 "2001:4f8:3:ba::/64" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'inet6 "2001:4f8:3:ba::" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'macaddr8 "08:00:2b:01:02:03:04:05" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'macaddr "08:00:2b:01:02:03" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'xml "<foo>bar</foo>" :where (:= 'id 1)))
  (pomo:query (:update 'data-type-tests :set 'int4range "(15, 25)" :where (:= 'id 1))))
