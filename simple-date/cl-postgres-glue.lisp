;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: SIMPLE-DATE-CL-POSTGRES-GLUE; -*-
(in-package :simple-date-cl-postgres-glue)

(defparameter *simple-date-sql-readtable*
  (cl-postgres:copy-sql-readtable))

;; PostgreSQL days are measured from 01-01-2000, whereas simple-date
;; uses 01-03-2000.

(defconstant +postgres-day-offset+ -60)
(defconstant +usecs-in-one-day+ (* 1000 1000 3600 24))

(flet ((interpret-timestamp (usecs)
         (multiple-value-bind (days usecs) (floor usecs +usecs-in-one-day+)
           (make-instance 'timestamp :days (+ days +postgres-day-offset+)
                          :ms (floor usecs 1000)))))
  (cl-postgres:set-sql-datetime-readers
   :date (lambda (days)
           (make-instance 'date :days (+ days +postgres-day-offset+)))
   :timestamp #'interpret-timestamp
   :timestamp-with-timezone #'interpret-timestamp
   :interval (lambda (months days usecs)
               (make-instance 'interval :months months
                              :ms (floor (+ (* days +usecs-in-one-day+) usecs) 1000)))
   :time (lambda (usecs)
           (multiple-value-bind (seconds usecs)
               (floor usecs 1000000)
             (multiple-value-bind (minutes seconds)
                 (floor seconds 60)
               (multiple-value-bind (hours minutes)
                   (floor minutes 60)
                 (make-instance 'time-of-day
                                :hours hours
                                :minutes minutes
                                :seconds seconds
                                :microseconds usecs)))))
   :table *simple-date-sql-readtable*))

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
    (if (= year month day hour min sec ms 0)
        (values "0 milliseconds" "interval")
        (flet ((not-zero (x) (if (zerop x) nil x)))
          (values
           (format nil "~@[~d years ~]~@[~d months ~]~@[~d days ~]~@[~d hours ~]~@[~d minutes ~]~@[~d seconds ~]~@[~d milliseconds~]"
                   (not-zero year) (not-zero month) (not-zero day)
                   (not-zero hour) (not-zero min) (not-zero sec) (not-zero ms))
           "interval")))))

(defmethod cl-postgres:to-sql-string ((arg time-of-day))
  (with-accessors ((hours hours)
                   (minutes minutes)
                   (seconds seconds)
                   (microseconds microseconds))
      arg
    (format nil "~2,'0d:~2,'0d:~2,'0d~@[.~6,'0d~]"
            hours minutes seconds (if (zerop microseconds) nil microseconds))))

;;
;; install a copy of the readtable we just modified, leaving our
;; readtable safe from further modification, for better or worse.
(setf cl-postgres:*sql-readtable*
      (cl-postgres:copy-sql-readtable *simple-date-sql-readtable*))

(defun simple-date-sql-readtable ()
  "An sql-readtable that has the simple-date-cl-postgres-glue reader
functions installed."
  *simple-date-sql-readtable*)
