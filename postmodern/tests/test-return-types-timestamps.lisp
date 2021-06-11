;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(def-suite :postmodern-return-types-timestamps
    :description "Timestamp Return types"
    :in :postmodern)

(in-suite :postmodern-return-types-timestamps)


(defun get-local-timezone (&optional (timezone nil))
  "If timezone parameter is set, just pass that through, otherwise use local-time
to determine the timezone for the local machine. This is not the Postgresql server
timezone, it is the timezone locale for the box running this lisp instance."
  (if timezone
      timezone
      (multiple-value-bind (offset x timezone)
          (local-time:timestamp-subtimezone (local-time:now)
                                            local-time:*default-timezone*)
        (declare (ignore offset x))
        timezone)))

(defun return-types-timestamps-fixture (&optional (timezone "UTC"))
  (when (table-exists-p 'test-data)
    (query (:drop-table :if-exists 'test-data :cascade)))
  (execute (:create-table test-data ((id :type integer :primary-key t)
                                     (timestamp-without-time-zone
                                      :type (or timestamp-without-time-zone
                                                db-null))
                                     (timestamp-with-time-zone
                                      :type (or timestamp-with-time-zone db-null))
                                     (timestamptz :type (or timestamptz db-null))
                                     (timestamp :type (or timestamp db-null))
                                     (time :type (or time db-null))
                                     (date :type (or date db-null))
                                     (interval :type (or interval db-null)))))
  ;; set the timezone to UTC so we know what timezone we are in
  (query (format nil "set timezone='~a'" timezone))
  (query (:insert-rows-into 'test-data
          :columns 'id 'timestamp-without-time-zone
          'timestamp-with-time-zone 'timestamptz 'timestamp 'time 'date 'interval
          :values '((1
                     "2020-12-30 13:30:54  -00:00" "2019-12-30 13:30:54 -00:00" "2018-12-30 13:30:54"
                     "2017-12-30 13:30:54"
                     "14:31:54" "2016-12-30" "2 hours 10 minutes")
                    (2
                     "1920-12-30 13:30:54" "1919-12-30 13:30:54" "1918-12-30 13:30:54"
                     "1917-12-30 13:30:54"
                     "14:32:54" "1916-12-30" "3 months 2 hours 10 minutes")
                    (3
                     "1980-12-30 13:30:54" "1981-12-30 13:30:54" "1982-12-30 13:30:54"
                     "1983-12-30 13:30:54"
                     "14:33:54" "1983-12-30" "5 years 3 months 2 hours 10 minutes")))))

(test return-types-timestamps-default
  (with-test-connection
    (return-types-timestamps-fixture  "UTC")
    (setf cl-postgres:*sql-readtable*
          (cl-postgres:copy-sql-readtable
           cl-postgres::*default-sql-readtable*))
    (is (equal (query "show timezone" :single)
               "UTC"))
    (is (equal (query (:select 'timestamp-without-time_zone 'timestamp-with-time-zone
                               'timestamptz 'timestamp 'time 'date 'interval
                               :from 'test-data
                       :where (:= 'id 1)))
               '((3818323854 3786701454 3755165454 3723629454
                  ((:HOURS 14) (:MINUTES 31) (:SECONDS 54) (:MICROSECONDS 0)) 3692044800
                  ((:MONTHS 0) (:DAYS 0) (:SECONDS 7800) (:USECONDS 0))))))
    (is (equal (query (:select (:to-char 'timestamp-without-time_zone "YYYY-MM-DD HH24:MI:SS")
                               (:to-char 'timestamp-with-time_zone "YYYY-MM-DD HH24:MI:SS")
                               (:to-char 'timestamptz "YYYY-MM-DD HH24:MI:SS")
                               (:to-char 'timestamp "YYYY-MM-DD HH24:MI:SS")
                               (:to-char 'time "HH24:MI:SS")
                               (:to-char 'date "YYYY-MM-DD")
                               (:to-char 'interval "YYYY-MM-DD HH24:MI:SS")
                               :from 'test-data :where (:= 'id 1)))
               '(("2020-12-30 13:30:54" "2019-12-30 13:30:54" "2018-12-30 13:30:54"
                  "2017-12-30 13:30:54" "14:31:54" "2016-12-30" "0000-00-00 02:10:00"))))
    (is (equal (query "SELECT to_char(timestamp_without_time_zone, E'YYYY-MM-DD HH24:MI:SS OF')
                       from test_data where id=1" :single)
               "2020-12-30 13:30:54 +00"))
    (is (equal (query "SELECT to_char(timestamp_without_time_zone, E'YYYY-MM-DD HH24:MI:SS')
                       from test_data where id=1" :single)
               "2020-12-30 13:30:54"))
    (is (equal (query "SELECT to_char(timestamp_with_time_zone, E'YYYY-MM-DD HH24:MI:SS')
                       from test_data where id=1" :single)
               "2019-12-30 13:30:54"))
    (is (equal (query "SELECT to_char(timestamp_with_time_zone, E'YYYY-MM-DD HH24:MI:SS OF')
                       from test_data where id=1" :single)
               "2019-12-30 13:30:54 +00"))
    (is (equal (query "SELECT to_char(timestamp_with_time_zone, E'YYYY-MM-DD HH24:MI:SS OF') as ts
                       from test_data where id<3" :json-strs)
               '("{\"ts\":\"2019-12-30 13:30:54 +00\"}" "{\"ts\":\"1919-12-30 13:30:54 +00\"}")))
    (is (equal (query "SELECT to_char(timestamp_with_time_zone, E'YYYY-MM-DD HH24:MI:SS OF') as ts
                       from test_data where id<3" :json-str)
               "{\"ts\":\"2019-12-30 13:30:54 +00\"}"))
    (is (equal (query "SELECT to_char(timestamp_with_time_zone, E'YYYY-MM-DD HH24:MI:SS OF') as ts
                       from test_data where id<3" :json-array-str)
               "[{\"ts\":\"2019-12-30 13:30:54 +00\"}, {\"ts\":\"1919-12-30 13:30:54 +00\"}]"))))

(test return-types-timestamps-simple-date
  (with-test-connection
    (return-types-timestamps-fixture "UTC")
    ;; make sure we start with the default readtable
    (setf cl-postgres:*sql-readtable*
          (cl-postgres:copy-sql-readtable
           cl-postgres::*default-sql-readtable*))
    ;; now load the simple-date readtable
    (setf cl-postgres:*sql-readtable*
          (cl-postgres:copy-sql-readtable
           simple-date-cl-postgres-glue:*simple-date-sql-readtable*))
    (is (equal
         (mapcar #'type-of
                 (first (query (:select 'timestamp-without-time_zone 'timestamp-with-time-zone
                                        'timestamptz 'timestamp 'time 'date 'interval
                                :from 'test-data
                                :where (:= 'id 1)))))
         '(SIMPLE-DATE:TIMESTAMP SIMPLE-DATE:TIMESTAMP SIMPLE-DATE:TIMESTAMP
           SIMPLE-DATE:TIMESTAMP SIMPLE-DATE:TIME-OF-DAY SIMPLE-DATE:DATE
           SIMPLE-DATE:INTERVAL)))
    (is (equal (query (:select 'timestamp-without-time-zone
                       :from 'test-data
                       :where (:< 'id 3)) :json-strs)
               '("{\"timestampWithoutTimeZone\":\"2020-12-30 13:30:54:0\"}"
                 "{\"timestampWithoutTimeZone\":\"1920-12-30 13:30:54:0\"}")))
    (is (equal (query (:select 'timestamp-without-time-zone
                       :from 'test-data
                       :where (:= 'id 1)) :json-str)
               "{\"timestampWithoutTimeZone\":\"2020-12-30 13:30:54:0\"}"))
    (is (equal (query (:select 'timestamp-without-time-zone
                       :from 'test-data
                       :where (:< 'id 3)) :json-array-str)
               "[{\"timestampWithoutTimeZone\":\"2020-12-30 13:30:54:0\"}, {\"timestampWithoutTimeZone\":\"1920-12-30 13:30:54:0\"}]"))
    (is (equal (query (:select 'timestamp-with-time-zone
                       :from 'test-data
                       :where (:= 'id 1)) :json-strs)
               '("{\"timestampWithTimeZone\":\"2019-12-30 13:30:54:0\"}")))
    (is (equal (query (:select 'timestamp-with-time-zone
                       :from 'test-data
                       :where (:< 'id 3)) :json-strs)
               '("{\"timestampWithTimeZone\":\"2019-12-30 13:30:54:0\"}"
                 "{\"timestampWithTimeZone\":\"1919-12-30 13:30:54:0\"}")))
    (is (equal (query (:select 'timestamp-with-time-zone
                       :from 'test-data
                       :where (:= 'id 1)) :json-str)
               "{\"timestampWithTimeZone\":\"2019-12-30 13:30:54:0\"}"))
    (is (equal (query (:select 'timestamp-with-time-zone
                       :from 'test-data
                       :where (:= 'id 1)) :json-array-str)
               "[{\"timestampWithTimeZone\":\"2019-12-30 13:30:54:0\"}]"))
    (is (equal (query (:select 'timestamptz
                       :from 'test-data
                       :where (:< 'id 3)) :json-strs)
               '("{\"timestamptz\":\"2018-12-30 13:30:54:0\"}"
                 "{\"timestamptz\":\"1918-12-30 13:30:54:0\"}")))
    (is (equal (query (:select 'timestamptz
                       :from 'test-data
                       :where (:< 'id 3)) :json-str)
               "{\"timestamptz\":\"2018-12-30 13:30:54:0\"}"))
    (is (equal (query (:select 'timestamptz
                       :from 'test-data
                       :where (:< 'id 3)) :json-array-str)
               "[{\"timestamptz\":\"2018-12-30 13:30:54:0\"}, {\"timestamptz\":\"1918-12-30 13:30:54:0\"}]"))
    (is (equal (query (:select 'timestamp
                       :from 'test-data
                       :where (:< 'id 3)) :json-strs)
               '("{\"timestamp\":\"2017-12-30 13:30:54:0\"}"
                 "{\"timestamp\":\"1917-12-30 13:30:54:0\"}")))
    (is (equal (query (:select 'timestamp
                       :from 'test-data
                       :where (:< 'id 3)) :json-str)
               "{\"timestamp\":\"2017-12-30 13:30:54:0\"}"))
    (is (equal (query (:select 'timestamp
                       :from 'test-data
                       :where (:< 'id 3)) :json-array-str)
               "[{\"timestamp\":\"2017-12-30 13:30:54:0\"}, {\"timestamp\":\"1917-12-30 13:30:54:0\"}]"))
    (is (equal (type-of (query (:select 'time
                                :from 'test-data
                                :where (:= 'id 1)) :single))
               'SIMPLE-DATE:TIME-OF-DAY))
    (is (equal (query (:select 'time
                       :from 'test-data
                       :where (:= 'id 1)) :json-strs)
               '("{\"time\":\"14:31:54:0\"}")))
    (is (equal (query (:select 'time
                       :from 'test-data
                       :where (:< 'id 3)) :json-strs)
               '("{\"time\":\"14:31:54:0\"}" "{\"time\":\"14:32:54:0\"}")))
    (is (equal (query (:select 'time
                       :from 'test-data
                       :where (:< 'id 3)) :json-str)
               "{\"time\":\"14:31:54:0\"}"))
    (is (equal (query (:select 'time
                       :from 'test-data
                       :where (:< 'id 3)) :json-array-str)
               "[{\"time\":\"14:31:54:0\"}, {\"time\":\"14:32:54:0\"}]"))
    (is (equal (query (:select 'date
                       :from 'test-data
                       :where (:= 'id 1)) :json-strs)
               '("{\"date\":\"2016-12-30\"}")))
    (is (equal (query (:select 'date
                       :from 'test-data
                       :where (:< 'id 3)) :json-str)
               "{\"date\":\"2016-12-30\"}"))
    (is (equal (query (:select 'date
                       :from 'test-data
                       :where (:< 'id 3)) :json-array-str)
               "[{\"date\":\"2016-12-30\"}, {\"date\":\"1916-12-30\"}]"))

    ;; Now back to the default readtable
    (setf cl-postgres:*sql-readtable*
          (cl-postgres:copy-sql-readtable
           cl-postgres::*default-sql-readtable*))))

(test encode-json-timestamps
  (let ((old-local-time-default-timezone local-time:*default-timezone*))
    (with-test-connection
      (return-types-timestamps-fixture  "UTC")
      ;; make sure we start with the default readtable
      (setf cl-postgres:*sql-readtable*
            (cl-postgres:copy-sql-readtable
             cl-postgres::*default-sql-readtable*))
      ;; now load the simple-date readtable
      (setf cl-postgres:*sql-readtable*
            (cl-postgres:copy-sql-readtable
             simple-date-cl-postgres-glue:*simple-date-sql-readtable*))
      ;; Try the simple-date-timestamps
      (is (equal (pomo::encode-json-to-string
                  (query (:select 'timestamp-without-time-zone
                          :from 'test-data
                          :where (:= 'id 1)) :single))
                 "\"2020-12-30 13:30:54:0\""))
      (is (equal (pomo::encode-json-to-string
                  (query (:select 'time
                          :from 'test-data
                          :where (:= 'id 1)) :single))
                 "\"14:31:54:0\""))
      (is (equal (pomo::encode-json-to-string
                  (query (:select 'date
                          :from 'test-data
                          :where (:= 'id 1)) :single))
                 "\"2016-12-30\""))
      (setf cl-postgres:*sql-readtable*
            (cl-postgres:copy-sql-readtable
             cl-postgres::*default-sql-readtable*))
      ;; Set the local-time readtable
      (setf local-time:*default-timezone* local-time:+utc-zone+)
      (local-time:set-local-time-cl-postgres-readers)
      ;; Try the local-time time-stamps
      (is (equal (pomo::encode-json-to-string (query (:select 'timestamp-without-time-zone
                                                      :from 'test-data
                                                      :where (:= 'id 1)) :single))
                 "\"{2020-12-30T13:30:54.000000Z}\""))
      (is (equal (pomo::encode-json-to-string (query (:select 'time
                                                      :from 'test-data
                                                      :where (:= 'id 1)) :single))
                 "\"{2000-03-01T14:31:54.000000Z}\""))
      (is (equal (pomo::encode-json-to-string (query (:select 'date
                                                      :from 'test-data
                                                      :where (:= 'id 1)) :single))
                 "\"{2016-12-30T00:00:00.000000Z}\""))
      ;; make sure we start with the default readtable
      (setf local-time:*default-timezone* old-local-time-default-timezone)
      (setf cl-postgres:*sql-readtable*
            (cl-postgres:copy-sql-readtable
             cl-postgres::*default-sql-readtable*)))))

;; THESE LOCAL-TIME TESTS MAY FAIL DEPENDING ON YOUR TIMEZONE
(test return-types-timestamps-local-time
  (let ((old-local-time-default-timezone local-time:*default-timezone*))
    (with-test-connection
      (setf local-time:*default-timezone* local-time:+utc-zone+)
      (return-types-timestamps-fixture "UTC")
      ;; Ensure we are starting from the default readtable
      (setf cl-postgres:*sql-readtable*
            (cl-postgres:copy-sql-readtable
             cl-postgres::*default-sql-readtable*))
      ;; Set the local-time readtable
      (local-time:set-local-time-cl-postgres-readers)
      (is (equal (query "SELECT to_char(timestamp_with_time_zone, E'YYYY-MM-DD HH24:MI:SS OF')
                      from test_data where id=1" :single)
                 "2019-12-30 13:30:54 +00"))
      (is (equal (type-of (query "SELECT timestamp_with_time_zone from test_data where id=1" :single))
                 'LOCAL-TIME:TIMESTAMP))
      (is (stringp
           (query "SELECT timestamp_with_time_zone from test_data where id=1" :json-str)))
      (is (equal (local-time:format-rfc3339-timestring
                  nil
                  (query "SELECT timestamp_with_time_zone
                        from test_data where id=1" :single)
                  :timezone local-time:+utc-zone+)
                 "2019-12-30T13:30:54.000000Z"))
      (is (listp (query (:select 'timestamp-without-time-zone
                         :from 'test-data
                         :where (:< 'id 3)) :json-strs)))
      (is (stringp (first (query (:select 'timestamp-without-time-zone
                                  :from 'test-data
                                  :where (:< 'id 3)) :json-strs))))

      (is (equal (first (split-sequence:split-sequence
                         #\:
                         (first (query (:select 'timestamp-without-time-zone
                                        :from 'test-data
                                        :where (:< 'id 3)) :json-strs))))
                 "{\"timestampWithoutTimeZone\""))
      (is (equal (second (split-sequence:split-sequence
                          #\:
                          (first (query (:select 'timestamp-without-time-zone
                                         :from 'test-data
                                         :where (:< 'id 3)) :json-strs))))
                 "\"{2020-12-30T13"))
      (is (equal (first
                  (split-sequence:split-sequence
                   #\T
                   (second
                    (split-sequence:split-sequence
                     #\:
                     (first (query (:select 'timestamp-without-time-zone
                                    :from 'test-data
                                    :where (:< 'id 3)) :json-strs))))))
                 "\"{2020-12-30"))
      (is (equal (query (:select 'timestamp-without-time-zone
                         :from 'test-data
                         :where (:< 'id 3)) :json-strs)
                 '("{\"timestampWithoutTimeZone\":\"{2020-12-30T13:30:54.000000Z}\"}"
 "{\"timestampWithoutTimeZone\":\"{1920-12-30T13:30:54.000000Z}\"}")))
      (is (equal (query (:select 'timestamp-without-time-zone
                         :from 'test-data
                         :where (:= 'id 1)) :json-str)
                 "{\"timestampWithoutTimeZone\":\"{2020-12-30T13:30:54.000000Z}\"}"))
      (is (equal (query (:select 'timestamp-without-time-zone
                         :from 'test-data
                         :where (:< 'id 3)) :json-array-str)
                 "[{\"timestampWithoutTimeZone\":\"{2020-12-30T13:30:54.000000Z}\"}, {\"timestampWithoutTimeZone\":\"{1920-12-30T13:30:54.000000Z}\"}]"))
      (is (equal (query (:select 'timestamp-with-time-zone
                         :from 'test-data
                         :where (:= 'id 1)) :json-strs)
                 '("{\"timestampWithTimeZone\":\"{2019-12-30T13:30:54.000000Z}\"}")))
      (is (equal (query (:select 'timestamp-with-time-zone
                         :from 'test-data
                         :where (:< 'id 3)) :json-strs)
                 '("{\"timestampWithTimeZone\":\"{2019-12-30T13:30:54.000000Z}\"}"
 "{\"timestampWithTimeZone\":\"{1919-12-30T13:30:54.000000Z}\"}")))
      (is (equal (query (:select 'timestamp-with-time-zone
                         :from 'test-data
                         :where (:= 'id 1)) :json-str)
                 "{\"timestampWithTimeZone\":\"{2019-12-30T13:30:54.000000Z}\"}"))
      (is (equal (query (:select 'timestamp-with-time-zone
                         :from 'test-data
                         :where (:= 'id 1)) :json-array-str)
                 "[{\"timestampWithTimeZone\":\"{2019-12-30T13:30:54.000000Z}\"}]"))
      (is (equal (query (:select 'timestamptz
                         :from 'test-data
                         :where (:< 'id 3)) :json-strs)
                 '("{\"timestamptz\":\"{2018-12-30T13:30:54.000000Z}\"}"
 "{\"timestamptz\":\"{1918-12-30T13:30:54.000000Z}\"}")))
      (is (equal (query (:select 'timestamptz
                         :from 'test-data
                         :where (:< 'id 3)) :json-str)
                 "{\"timestamptz\":\"{2018-12-30T13:30:54.000000Z}\"}"))
      (is (equal (query (:select 'timestamptz
                         :from 'test-data
                         :where (:< 'id 3)) :json-array-str)
                 "[{\"timestamptz\":\"{2018-12-30T13:30:54.000000Z}\"}, {\"timestamptz\":\"{1918-12-30T13:30:54.000000Z}\"}]"))
      (is (equal (query (:select 'timestamp
                         :from 'test-data
                         :where (:< 'id 3)) :json-strs)
                 '("{\"timestamp\":\"{2017-12-30T13:30:54.000000Z}\"}"
 "{\"timestamp\":\"{1917-12-30T13:30:54.000000Z}\"}")))
      (is (equal (query (:select 'timestamp
                         :from 'test-data
                         :where (:< 'id 3)) :json-str)
                 "{\"timestamp\":\"{2017-12-30T13:30:54.000000Z}\"}"))
      (is (equal (query (:select 'timestamp
                         :from 'test-data
                         :where (:< 'id 3)) :json-array-str)
                 "[{\"timestamp\":\"{2017-12-30T13:30:54.000000Z}\"}, {\"timestamp\":\"{1917-12-30T13:30:54.000000Z}\"}]"))
      (is (equal (type-of (query (:select 'time
                                  :from 'test-data
                                  :where (:= 'id 1)) :single))
                 'LOCAL-TIME:TIMESTAMP))
      (is (equal (query (:select 'time
                         :from 'test-data
                         :where (:= 'id 1)) :json-strs)
                 '("{\"time\":\"{2000-03-01T14:31:54.000000Z}\"}")))
      (is (equal (query (:select 'time
                         :from 'test-data
                         :where (:< 'id 3)) :json-strs)
                 '("{\"time\":\"{2000-03-01T14:31:54.000000Z}\"}"
 "{\"time\":\"{2000-03-01T14:32:54.000000Z}\"}")))
      (is (equal (query (:select 'time
                         :from 'test-data
                         :where (:< 'id 3)) :json-str)
                 "{\"time\":\"{2000-03-01T14:31:54.000000Z}\"}"))
      (is (equal (query (:select 'time
                         :from 'test-data
                         :where (:< 'id 3)) :json-array-str)
                 "[{\"time\":\"{2000-03-01T14:31:54.000000Z}\"}, {\"time\":\"{2000-03-01T14:32:54.000000Z}\"}]"))
      (is (equal (query (:select 'date
                         :from 'test-data
                         :where (:= 'id 1)) :json-strs)
                 '("{\"date\":\"{2016-12-30T00:00:00.000000Z}\"}")))
      (is (equal (query (:select 'date
                         :from 'test-data
                         :where (:< 'id 3)) :json-str)
                 "{\"date\":\"{2016-12-30T00:00:00.000000Z}\"}"))
      (is (equal (query (:select 'date
                         :from 'test-data
                         :where (:< 'id 3)) :json-array-str)
                 "[{\"date\":\"{2016-12-30T00:00:00.000000Z}\"}, {\"date\":\"{1916-12-30T00:00:00.000000Z}\"}]"))
      (setf local-time:*default-timezone* old-local-time-default-timezone)
      (setf cl-postgres:*sql-readtable*
            (cl-postgres:copy-sql-readtable
             cl-postgres::*default-sql-readtable*)))))
