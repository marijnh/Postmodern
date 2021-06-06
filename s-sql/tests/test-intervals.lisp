;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: S-SQL-TESTS; -*-
(in-package :s-sql-tests)

(def-suite :s-sql-intervals
    :description "Interval suite for s-sql"
    :in :s-sql)

(in-suite :s-sql-intervals)

;; Basic DateTime Functions
(test basic-time-operators-default-readtable
  "Testing basic date/time operators based on https://www.postgresql.org/docs/current/functions-datetime.html"
  (with-test-connection
    (setf cl-postgres:*sql-readtable*
          (cl-postgres:copy-sql-readtable
           cl-postgres::*default-sql-readtable*))
    (is (equal (query (:select (:+ (:type "2001-09-28" date) (:type 7 integer))) :single)
               3211228800))
    (is (equal (query (:select (:+ (:type "2001-09-28" date) (:type "1 hour" interval))) :single)
               3210627600))
    (is (equal (query (:select (:+ (:type "2001-09-28" date) (:type "03:00" time))) :single)
               3210634800))
    (is (equal (query (:select (:+ (:type "1 day" interval) (:type "1 hour" interval))) :single)
               '((:MONTHS 0) (:DAYS 1) (:SECONDS 3600) (:USECONDS 0))))
    (is (equal (query (:select (:+ (:type "2001-09-28 01:00" timestamp) (:type "23 hours" interval))) :single)
               3210710400))
    (is (equal (sql (:select (:+ (:type "2001-09-28 01:00" timestamp) (:type "23 hours" interval))))
               "(SELECT (E'2001-09-28 01:00'::TIMESTAMP + E'23 hours'::INTERVAL))"))
    (is (equal (query (:select (:+ (:type "01:00" time) (:type "3 hours" interval))) :single)
               '((:HOURS 4) (:MINUTES 0) (:SECONDS 0) (:MICROSECONDS 0))))
    (is (equal (query (:select (:type (:select (:- (:type "23 hours" interval))) interval)) :single)
               '((:MONTHS 0) (:DAYS 0) (:SECONDS -82800) (:USECONDS 0))))
    (is (equal (query (:select (:type
                                (:select (:- (:type "2001-10-01" date)
                                             (:type "2001-09-28" date)))
                                integer))
                      :single)
               3))
    (is (equal (sql (:select (:type (:- (:type "2001-10-01" date) (:type 7 integer)) date)))
               "(SELECT (E'2001-10-01'::DATE - 7::INTEGER)::DATE)"))
    (is (equal (sql (:select (:- (:type "2001-09-28" date) (:type "1 hour" interval))))
               "(SELECT (E'2001-09-28'::DATE - E'1 hour'::INTERVAL))"))
    (is (equal (query (:select (:- (:type "05:00" time) (:type "03:00" time))))
               '((((:MONTHS 0) (:DAYS 0) (:SECONDS 7200) (:USECONDS 0))))))
    (is (equal (query (:select (:- (:type "05:00" time) (:type "2 hours" interval))) :single)
               '((:HOURS 3) (:MINUTES 0) (:SECONDS 0) (:MICROSECONDS 0))))
    (is (equal (sql (:select (:- (:type "2001-09-28 23:00" timestamp) (:type "23 hours" interval))))
               "(SELECT (E'2001-09-28 23:00'::TIMESTAMP - E'23 hours'::INTERVAL))"))
    (is (equal (sql (:select (:- (:type "2001-09-29 03:00" timestamp)  (:type "2001-09-27 12:00" timestamp))))
               "(SELECT (E'2001-09-29 03:00'::TIMESTAMP - E'2001-09-27 12:00'::TIMESTAMP))"))
    (is (equal (query (:select (:age (:type "2001-04-10" timestamp) (:type "1957-06-13" timestamp))))
               '((((:MONTHS 525) (:DAYS 27) (:SECONDS 0) (:USECONDS 0))))))
    (is (equal (sql (:select (:age (:type "2001-04-10" timestamp) (:type "1957-06-13" timestamp))))
               "(SELECT AGE (E'2001-04-10'::TIMESTAMP, E'1957-06-13'::TIMESTAMP))"))
    (is (equal (sql (:select (:age (:type "1957-06-13" timestamp))))
               "(SELECT AGE (E'1957-06-13'::TIMESTAMP))"))
    (is (equal (sql (:select (:date-part "hour" (:type "2001-02-16 20:38:40" timestamp))))
               "(SELECT date_part(E'hour', E'2001-02-16 20:38:40'::TIMESTAMP))"))
    (is (equal (sql (:select (:date-part "month" (:type "2 years 3 months" interval))))
               "(SELECT date_part(E'month', E'2 years 3 months'::INTERVAL))"))
    (is (equal (sql (:select (:date-trunc "hour" (:type "2001-02-16 20:38:40" timestamp))))
               "(SELECT date_trunc(E'hour', E'2001-02-16 20:38:40'::TIMESTAMP))"))
    (is (equal (query (:select (:date-trunc "hour" (:type "2 days 3 hours 40 minutes" interval))) :single)
               '((:MONTHS 0) (:DAYS 2) (:SECONDS 10800) (:USECONDS 0))))
    (is (equal (sql (:select (:- (:type "1 day" interval) (:type "1 hour" interval))))
               "(SELECT (E'1 day'::INTERVAL - E'1 hour'::INTERVAL))"))
    (is (equal (sql (:select (:to-char (:* 900 (:type "1 second" interval)) "HH24:MI:SS")))
               "(SELECT to_char((900 * E'1 second'::INTERVAL), E'HH24:MI:SS'))"))
    (is (equal (query (:select (:extract "hour" (:type "2001-02-16 20:38:40" timestamp))) :single)
        20.0d0))
    (is (equal (query (:select (:isfinite (:type "2001-02-16" date))) :single) t))
    (is (equal (query (:select (:justify_days (:type  "35 days" interval))) :single)
               '((:MONTHS 1) (:DAYS 5) (:SECONDS 0) (:USECONDS 0))))
    (is (equal (query (:select (:justify_hours (:type  "27 hours" interval))) :single)
               '((:MONTHS 0) (:DAYS 1) (:SECONDS 10800) (:USECONDS 0))))
    (is (equal (query (:select (:justify_interval (:type  "1 mon -1 hour" interval))) :single)
               '((:MONTHS 0) (:DAYS 29) (:SECONDS 82800) (:USECONDS 0))))
    (is (equal (sql (:select (:current-time)))
               "(SELECT current_time)"))
    (is (equal (sql (:select (:current-date)))
               "(SELECT current_date)"))
    (is (equal (sql (:select (:local-timestamp)))
               "(SELECT localtimestamp)"))
    (is (equal (sql (:select (:local-time)))
               "(SELECT localtime)"))
    (is (equal (sql (:select (:make-date 2013 7 15)))
               "(SELECT make_date(2013, 7, 15))"))
    (is (equal (query (:select (:make-interval ("days" 4) ("hours" 10) ("secs" 1.2))) :single)
               '((:MONTHS 0) (:DAYS 4) (:SECONDS 36001) (:USECONDS 200000))))
    (is (equal (query (:select (:make-interval ("days" 10))) :single)
               '((:MONTHS 0) (:DAYS 10) (:SECONDS 0) (:USECONDS 0))))
    (is (equal (query (:select
                       (:make-timestamptz ("year" 2014) ("month" 1) ("mday" 13)
                                          ("hour" 21) ("min" 50) ("sec" 0) ("timezone" "Asia/Tokyo")))
                      :single)
               3598606200))))

;;; Intervals
(test interval-table
  "Build interval table"
  (with-test-connection
    (when (table-exists-p 'interval)
      (query (:drop-table 'interval)))
    (query (:create-table interval ((f1 :type interval))))
    (query (:insert-rows-into 'interval
                              :columns 'f1
                              :values '(("@ 1 minute")
                                        ("@ 5 hour")
                                        ("@ 10 day")
                                        ("@ 34 year")
                                        ("@ 3 months")
                                        ("@ 14 seconds ago")
                                        ("1 day 2 hours 3 minutes 4 seconds")
                                        ("6 years")
                                        ("5 months")
                                        ("5 months 12 hours"))))
    (is-true (table-exists-p 'interval))
    (query (:drop-table 'interval))))

(test intervals
  "Testing intervals"
  (with-test-connection
    (setf cl-postgres:*sql-readtable*
          (cl-postgres:copy-sql-readtable
           cl-postgres::*default-sql-readtable*))
    ;;Testing sql standard format
    (is (equal (query (:select
                       (:interval "1-2")) :single)
               '((:MONTHS 14) (:DAYS 0) (:SECONDS 0) (:USECONDS 0))))
    ;;Testing sql standard format
    (is (equal
         (query (:select (:interval "3 4:05:06")) :single)
         '((:MONTHS 0) (:DAYS 3) (:SECONDS 14706) (:USECONDS 0))))
    ;;Testing traditional postgresql format format
    (is (equal (query (:select (:interval "1 year 2 months 3 days 4 hours 5 minutes 6 seconds")) :single)
               '((:MONTHS 14) (:DAYS 3) (:SECONDS 14706) (:USECONDS 0))))
    ;;Testing iso 8601 format with designators
    (is (equal (query (:select (:interval "P1Y2M3DT4H5M6S")) :single)
               '((:MONTHS 14) (:DAYS 3) (:SECONDS 14706) (:USECONDS 0))))
    ;; Testing ISO 8601 alternative format
    (is (equal (query (:select (:interval "P0001-02-03T04:05:06") ) :single)
               '((:MONTHS 14) (:DAYS 3) (:SECONDS 14706) (:USECONDS 0))))))

(test interval-extraction
  "Testing extracting subparts of intervals"
  (with-test-connection
    (is (equal
         (query (:select (:extract "minute" (:interval "5 hours 21 minutes"))) :single)
         21.0d0))
    (is (equal
         (query (:select (:extract "hour" (:interval "35 hours 21 minutes"))) :single)
         35.0d0))
    (is (equal (query (:select (:extract "day" (:interval "6 years 5 months 4 days 3 hours 2 minutes 1 second"))) :single)
               4.0d0))
    (is (equal
         (query (:select (:extract "year" (:interval "6 years 5 months 4 days 3 hours 2 minutes 1 second"))) :single)
         6.0d0))
    (is (equal
         (query (:select (:extract "year" (:interval "6 years 15 months 4 days 3 hours 2 minutes 1 second"))) :single)
         7.0d0))
    (is (equal
         (query (:select (:extract "month" (:interval "6 years 15 months 4 days 3 hours 2 minutes 1 second"))) :single)
         3.0d0))))

(test interval-addition
  "Testing interval addition"
  (with-test-connection
    (is (equal
         (query (:select (:+ (:interval "2h 50min") (:interval "10min"))) :single)
         '((:MONTHS 0) (:DAYS 0) (:SECONDS 10800) (:USECONDS 0))))
    (is (equal
         (query (:select (:to-char (:+ (:date "2016-12-31") (:interval "25 hours")) "YYYY-MM-DD")) :single)
         "2017-01-01"))))

(test interval-subtraction
  "Testing interval subtraction"
  (with-test-connection
    (is (equal
         (query (:select
                 (:to-char
                  (:- (:date "2016-12-31") (:interval "25 hours"))
                  "YYYY-MM-DD"))
                :single)
         "2016-12-29"))
    (is (equal
         (query (:select
                 (:type (:- (:timestamp "2016-12-31 03:00") (:timestamp "2016-12-29 13:00"))
                        interval))
                :single)
         '((:MONTHS 0) (:DAYS 1) (:SECONDS 50400) (:USECONDS 0))))))

(test to-char-on-intervals
  "Testing to-char on intervals"
    (with-test-connection
      (is (equal
           (query (:select
                   (:to-char
                    (:interval "17h 20min 05s") "HH24:MI:SS"))
                  :single)
           "17:20:05"))))

(test justify-on-intervals
  "testing justify-days, hours and interval"
  (with-test-connection
    (setf cl-postgres:*sql-readtable*
          (cl-postgres:copy-sql-readtable
           cl-postgres::*default-sql-readtable*))
    (is (equal
         (sql (:select (:justify-days (:interval "30 days"))))
         "(SELECT justify_days(INTERVAL  E'30 days'))"))
    (is (equal
         (query (:select (:justify-days (:interval "30 days"))) :single)
         '((:MONTHS 1) (:DAYS 0) (:SECONDS 0) (:USECONDS 0))))
    (is (equal
         (sql (:select (:justify-hours (:interval "24 hours"))))
         "(SELECT justify_hours(INTERVAL  E'24 hours'))"))
    (is (equal
         (query (:select (:justify-hours (:interval "24 hours"))) :single)
         '((:MONTHS 0) (:DAYS 1) (:SECONDS 0) (:USECONDS 0))))
    (is (equal
         (sql (:select (:justify-interval (:interval "1 year - 1 hour"))))
         "(SELECT justify_interval(INTERVAL  E'1 year - 1 hour'))"))
    (is (equal
         (query (:select (:justify-interval (:interval "1 year - 1 hour"))) :single)
         '((:MONTHS 11) (:DAYS 29) (:SECONDS 82800) (:USECONDS 0))))))

(test select-sum-group-interval
  "test select-sum-group-interval"
  (with-test-connection
    (is (equal
         (sql (:select 'city (:as (:sum (:- (:timestamp "2018-04-10") 'start-date)) 'total-days)
                              :from 'employee
                              :group-by 'city
                              :having (:> (:sum (:- (:timestamp "2018-04-10") 'start-date)) (:interval "1 year"))))
         "(SELECT city, SUM((timestamp E'2018-04-10' - start_date)) AS total_days FROM employee GROUP BY city HAVING (SUM((timestamp E'2018-04-10' - start_date)) > INTERVAL  E'1 year'))"))))

(test timestamp-functions
  "Testing timestamp functions"
  (with-test-connection
    (setf cl-postgres:*sql-readtable*
          (cl-postgres:copy-sql-readtable
           cl-postgres::*default-sql-readtable*))
    (is (integerp
         (query (:select (:current-date))
                :single)))
    (is (equal
         (query (:select (:- (:timestamp "1999-12-30") (:timestamp "1999-12-11"))) :single)
         '((:MONTHS 0) (:DAYS 19) (:SECONDS 0) (:USECONDS 0))))
    (is (equal
         (query (:select (:age (:timestamp "2001-04-10")
                               (:timestamp "1957-06-13"))))
         '((((:MONTHS 525) (:DAYS 27) (:SECONDS 0) (:USECONDS 0))))))
    (is (equal
         (query
          (:select (:to-char (:age (:timestamp "2001-04-10")
                                   (:timestamp "1957-06-13"))
                             "YYYY-MM-DD hh24:mi:ss"))
          :single)
         "0043-09-27 00:00:00"))
    (is (equal
         (query (:select (:to-char (:+ (:date "2001-09-28")
                                       (:integer "7"))
                                   "YYYY-MM-DD hh24:mi:ss")))
         '(("2001-10-05 00:00:00"))))
    (is (equal
         (query (:select
                 (:to-char
                  (:- (:timestamp "2016-12-31 03:00") (:timestamp "2016-12-29 13:00"))
                  "YYYY-MM-DD hh24:mm:ss"))
                :single)
         "0000-00-01 14:00:00"))))

(test timestamp-extraction
  "Testing timestamp extraction"
  (with-test-connection
    (is (equal
         (query (:select (:extract "year" (:timestamp "2016-12-31 13:30:15"))) :single)
         2016.0d0))
    (is (equal
         (query (:select (:extract "quarter" (:timestamp "2016-12-31 13:30:15"))) :single)
         4.0d0))
    (is (equal
         (query (:select (:extract "month" (:timestamp "2016-12-31 13:30:15"))) :single)
         12.0d0))
    (is (equal
         (query (:select (:extract "day" (:timestamp "2016-12-31 13:30:15"))) :single)
         31.0d0))
    (is (equal
         (query (:select (:extract "century" (:timestamp "2016-12-31 13:30:15"))) :single)
         21.0d0))
    (is (equal
         (query (:select (:extract "dow" (:timestamp "2016-12-31 13:30:15"))) :single)
         6.0d0))
    (is (equal
         (query (:select (:extract "doy" (:timestamp "2016-12-31 13:30:15"))) :single)
         366.0d0))
    (is (equal
         (query (:select (:extract "hour" (:timestamp "2016-12-31 13:30:15"))) :single)
         13.0d0))
    (is (equal
         (query (:select (:extract "minute" (:timestamp "2016-12-31 13:30:15"))) :single)
         30.0d0))))
