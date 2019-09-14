;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER; -*-

(defpackage :simple-date
  (:use :common-lisp)
  (:export #:date #:encode-date #:decode-date #:day-of-week
           #:timestamp #:encode-timestamp #:decode-timestamp
           #:timestamp-to-universal-time #:universal-time-to-timestamp
           #:interval #:encode-interval #:decode-interval
           #:time-of-day #:hours #:minutes #:seconds #:microseconds
           #:encode-time-of-day #:decode-time-of-day
           #:time-add #:time-subtract
           #:time= #:time> #:time< #:time<= #:time>=))

(defpackage :simple-date-cl-postgres-glue
  (:use :common-lisp :simple-date)
  (:export *simple-date-sql-readtable*
           :simple-date-sql-readtable))

