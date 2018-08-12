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
  (is (equal
       (query (:select (:+ (:interval ("2h 50min")) (:interval ("10min")))))
      '((((:MONTHS 0) (:DAYS 0) (:SECONDS 10800) (:USECONDS 0))))))
  (is (equal
       (query (:select (:+ (:interval ("2h 50min")) (:interval ("10min")))) :single)
       '((:MONTHS 0) (:DAYS 0) (:SECONDS 10800) (:USECONDS 0))))
  (is (equal
       (query (:select
               (:to-char
                (:interval ("17h 20min 05s" "HH24:MI:SS"))))
              :single)
       "17:20:05"))
  (is (equal
       (query (:select
               (:to-char (:interval ("17h 20m 05s"))
                         "hh24:mi:ss"))
              :single)
       "17:20:05"))

  (is (equal
       (query (:select (:interval ("6 years 5 months 4 days 3 hours 2 minutes 1 second"))))
       '((((:MONTHS 77) (:DAYS 4) (:SECONDS 10921) (:USECONDS 0))))))
  (is (equal
       (query (:select (:extract "minute" (:interval ("5 hours 21 minutes")))) :single)
       21.0d0))
  (is (equal
       (query (:select (:justify-days (:interval ("30 days")))) :single)
       '((:MONTHS 1) (:DAYS 0) (:SECONDS 0) (:USECONDS 0))))
  (is (equal
       (query (:select (:justify-hours (:interval ("24 hours")))) :single)
       '((:MONTHS 0) (:DAYS 1) (:SECONDS 0) (:USECONDS 0))))
  (is (equal
       (query (:select (:justify-interval (:interval ("1 year - 1 hour")))) :single)
       '((:MONTHS 11) (:DAYS 29) (:SECONDS 82800) (:USECONDS 0))))
  (is (equal
       (query (:select (:interval ("17h 20m 05s" "hh24:mi:ss"))))
       '((((:MONTHS 0) (:DAYS 0) (:SECONDS 62405) (:USECONDS 0)) "hh24:mi:ss"))))
  (is (equal
       (query (:select (:to-char (:interval ("17h 20m 05s" "hh24:mi:ss"))))
              :single)
       "17:20:05"))
  (is (integerp
       (query (:select (:current-date))
              :single)))
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
       '(("2001-10-05 00:00:00"))))))

(test select-sum-group-interval
      "CAUTION: DOES NOT WORK. VALIDATE THAT THIS ACTUALLY WORKS WITH THE ESCAPED INTERVAL. To sum the column len of all films, group the results by kind and show those group totals that are less than 5 hours:"
;;; SELECT kind, sum(len) AS total
;;;    FROM films
;;;    GROUP BY kind
;;; HAVING sum(len) < interval '5 hours';

      (is (equal (sql (:select 'kind (:as (:sum 'len) 'total)
                               :from 'films
                               :group-by 'kind
                               :having (:< (:sum 'len) 'interval "5 hours")))
                 "(SELECT kind, SUM(len) AS total FROM films GROUP BY kind HAVING (SUM(len) < interval < E'5 hours'))")))
