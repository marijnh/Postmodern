(in-package :s-sql-tests)

(fiveam:def-suite :s-sql-intervals
    :description "Interval suite for s-sql"
    :in :s-sql)

(in-suite :s-sql-intervals)

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
    (is-true (table-exists-p 'interval))))

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
      (is (equal
           (query (:select (:justify-days (:interval "30 days"))) :single)
           '((:MONTHS 1) (:DAYS 0) (:SECONDS 0) (:USECONDS 0))))
      (is (equal
           (query (:select (:justify-hours (:interval "24 hours"))) :single)
           '((:MONTHS 0) (:DAYS 1) (:SECONDS 0) (:USECONDS 0))))
      (is (equal
           (query (:select (:justify-interval (:interval "1 year - 1 hour"))) :single)
           '((:MONTHS 11) (:DAYS 29) (:SECONDS 82800) (:USECONDS 0))))))

(test select-sum-group-interval
  "test select-sum-group-interval"
  (with-test-connection
    (is (equal
         (query (:select 'city (:as (:sum (:- (:timestamp "2018-04-10") 'start-date)) 'total-days)
                              :from 'employee
                              :group-by 'city
                              :having (:> (:sum (:- (:timestamp "2018-04-10") 'start-date)) (:interval "1 year"))))
         '(("Vancouver" ((:MONTHS 0) (:DAYS 21746) (:SECONDS 0) (:USECONDS 0)))
           ("New York" ((:MONTHS 0) (:DAYS 22751) (:SECONDS 0) (:USECONDS 0)))
           ("Toronto" ((:MONTHS 0) (:DAYS 20374) (:SECONDS 0) (:USECONDS 0))))))))

(test timestamp-functions
  "Testing timestamp functions"
  (with-test-connection
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
