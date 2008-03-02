(in-package :simple-date)

;; Postgresql days are measured from 01-01-2000, whereas simple-date
;; uses 01-03-2000.

(defconstant +postgres-day-offset+ -60)
(defconstant +millisecs-in-day+ (* 1000 3600 24))

(cl-postgres:binary-datetime-readers
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
