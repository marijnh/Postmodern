(in-package :simple-date)

;; Postgresql days are measured from 01-01-2000, whereas simple-date
;; uses 01-03-2000.

(defconstant +postgres-day-offset+ -60)
(defconstant +millisecs-in-day+ (* 1000 3600 24))

(cl-postgres:set-sql-datetime-readers
 :date
 (lambda (days)
   (make-instance 'date :days (+ days +postgres-day-offset+)))
 :timestamp
 (lambda (millisecs)
   (multiple-value-bind (days millisecs) (floor millisecs +millisecs-in-day+)
     (make-instance 'timestamp :days (+ days +postgres-day-offset+)
                    :ms millisecs)))
 :interval
 (lambda (months days millisecs)
   (make-instance 'interval :months months
                  :ms (+ (* days +millisecs-in-day+) millisecs))))

(defmethod cl-postgres:to-sql-string ((arg date))
  (multiple-value-bind (year month day) (decode-date arg)
    (values (format nil "~4,'0d-~2,'0d-~2,'0d" year month day) "date")))

(defmethod cl-postgres:to-sql-string ((arg timestamp))
  (multiple-value-bind (year month day hour min sec ms) (decode-timestamp arg)
    (values
     (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~@[.~3,'0d~]"
             year month day hour min sec (if (zerop ms) nil ms))
     "timestamp")))

(defmethod cl-postgres:to-sql-string ((arg interval))
  (multiple-value-bind (year month day hour min sec ms) (decode-interval arg)
    (flet ((not-zero (x) (if (zerop x) nil x)))
      (values
       (format nil "~@[~d years ~]~@[~d months ~]~@[~d days ~]~@[~d hours ~]~@[~d minutes ~]~@[~d seconds ~]~@[~d milliseconds~]"
               (not-zero year) (not-zero month) (not-zero day)
               (not-zero hour) (not-zero min) (not-zero sec) (not-zero ms))
       "interval"))))
