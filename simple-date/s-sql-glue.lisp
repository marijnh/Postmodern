(in-package :simple-date)

(defmethod s-sql:sql-ize ((arg date))
  (multiple-value-bind (year month day) (decode-date arg)
    (values (format nil "~4,'0d-~2,'0d-~2,'0d" year month day) "date")))

(defmethod s-sql:sql-ize ((arg timestamp))
  (multiple-value-bind (year month day hour min sec ms) (decode-timestamp arg)
    (values
     (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~@[.~3,'0d~]"
             year month day hour min sec (if (zerop ms) nil ms))
     "timestamp")))

(defmethod s-sql:sql-ize ((arg interval))
  (multiple-value-bind (year month day hour min sec ms) (decode-interval arg)
    (flet ((not-zero (x) (if (zerop x) nil x)))
      (values
       (format nil "~@[~d years ~]~@[~d months ~]~@[~d days ~]~@[~d hours ~]~@[~d minutes ~]~@[~d seconds ~]~@[~d milliseconds~]"
               (not-zero year) (not-zero month) (not-zero day)
               (not-zero hour) (not-zero min) (not-zero sec) (not-zero ms))
       "interval"))))
