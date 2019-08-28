;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: SIMPLE-DATE; -*-
(in-package :simple-date)

(defun to-external-date (year month day)
  "Convert a internal date representation to an external one.
Internally, months and days start at 0 and the 1st of March 2000 is
the year 0."
  (let ((day (1+ day))
        (month (+ month 3))
        (year (+ year 2000)))
    (if (> month 12)
        (values (1+ year) (- month 12) day)
        (values year month day))))

(defun to-internal-date (year month day)
  "Convert an external date representation to an internal one."
  (declare (type integer year)
           (type (integer 1 12) month)
           (type (integer 1 31) day))
  (let ((day (1- day))
        (month (- month 3))
        (year (- year 2000)))
    (if (< month 0)
        (values (1- year) (+ month 12) day)
        (values year month day))))

(let ((lookup #(0 31 61 92 122 153 184 214 245 275 306 337)))
  (defun days-before-month (month)
    "Find the amount of days in a year before the start of a given
month. \(This works because in the internal representation march is
the first month, so leap days only happen at the end of the year.)"
    (aref lookup month)))

(defun month-at-day (days)
  "Find the month in which a given day of the year falls. It's a
manually constructed binary search. Which is pretty ugly."
  (if (< days 184)
      (if (< days 61)
          (if (< days 31) 0 1)
          (if (< days 122)
              (if (< days 92) 2 3)
              (if (< days 153) 4 5)))
      (if (< days 245)
          (if (< days 214) 6 7)
          (if (< days 306)
              (if (< days 275) 8 9)
              (if (< days 337) 10 11)))))

;; These assume a typical set of years, they are used by first taking
;; care of the 4-century periods, then the centuries, etc. That way,
;; the centuries will never have to deal with a year divisible by 400,
;; the 4-year periods will never contain century-years, and the
;; one-year periods are never leap years.
(defconstant +days-in-400-years+ (+ (* 400 365) 97))
(defconstant +days-in-100-years+ (+ (* 100 365) 24))
(defconstant +days-in-4-years+ (+ (* 4 365) 1))
(defconstant +days-in-year+ 365)

(defconstant +millisecs-in-day+ (* 1000 3600 24))

(defun encode-days (year month day)
  "Get the number of days since March 1st 2000 for a given date \(in
internal format.)"
  (incf day (days-before-month month))
  (flet ((adjust (factor days)
           (multiple-value-bind (parts remainder) (floor year factor)
             (incf day (* parts days))
             (setf year remainder))))
    (adjust 400 +days-in-400-years+)
    (adjust 100 +days-in-100-years+)
    (adjust 4 +days-in-4-years+)
    (adjust 1 +days-in-year+)
    day))

(defun decode-days (days)
  "Transform a number of days since March 1st 2000 to a date \(in
internal format.)"
  (let ((year 0)
        (month 0))
    (flet ((adjust (factor factor-days prev)
             (multiple-value-bind (parts remainder) (floor days factor-days)
               (when (and prev (= parts prev))
                 (decf parts)
                 (incf remainder factor-days))
               (incf year (* factor parts))
               (setf days remainder))))
      (adjust 400 +days-in-400-years+ nil)
      (adjust 100 +days-in-100-years+ 4)
      (adjust 4 +days-in-4-years+ 25)
      (adjust 1 +days-in-year+ 4)
      (setf month (month-at-day days))
      (values year month (- days (days-before-month month))))))

(defun encode-millisecs (hour minute second millisecond)
  "Get the amount of milliseconds from a number of bigger units."
  (+ millisecond (* 1000 (+ second (* 60 (+ minute (* 60 hour)))))))

(defun decode-millisecs (millisecs)
  "Decompose a number of milliseconds into hours, minutes, seconds and
milliseconds."
  (multiple-value-bind (seconds millisecs) (floor millisecs 1000)
    (multiple-value-bind (minutes seconds) (floor seconds 60)
      (multiple-value-bind (hours minutes) (floor minutes 60)
        (values hours minutes seconds millisecs)))))

(defun leap-year-p (year)
  "Is this year a leap year?"
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

(defun days-in-month (month year)
  "Days in a certain month -- note that these months use internal
encoding."
  (case month
    (11 (if (leap-year-p (1+ year)) 29 28))
    ((1 3 6 8) 30)
    (t 31)))

(defun normalize-timestamp (days millisecs)
  "Make sure that a number of milliseconds falls within a day, correct
the amount of days if necessary."
  (multiple-value-bind (extra-days millisecs) (floor millisecs +millisecs-in-day+)
    (values (+ days extra-days) millisecs)))

(defun date-add (base-days months)
  "Add a number of months to a date \(expressed in days)."
  (multiple-value-bind (year month day) (decode-days base-days)
    (multiple-value-bind (extra-years month) (floor (+ month months) 12)
      (incf year extra-years)
      (encode-days year month (min day (1- (days-in-month month year)))))))

(defun invert-interval (interval)
  "Invert the components of an interval."
  (make-instance 'interval :ms (- (millisecs interval))
                 :months (- (months interval))))

(defclass date ()
  ((days :initarg :days :accessor days))
  (:documentation "This class is used to represent dates where the
time of day is not important."))

(defmethod print-object ((date date) stream)
  (print-unreadable-object (date stream :type t)
    (multiple-value-bind (year month day) (decode-date date)
      (format stream "~2,'0d-~2,'0d-~4,'0d" day month year))))

(defun encode-date (year month day)
  "Create a date object."
  (multiple-value-bind (year month day) (to-internal-date year month day)
    (make-instance 'date :days (encode-days year month day))))

(defun decode-date (date)
  "Get the date elements from a date object."
  (multiple-value-bind (year month day) (decode-days (days date))
    (to-external-date year month day)))

(defun day-of-week (date)
  "Returns the weekday of the given date as a number between 0 and 6,
0 being Sunday and 6 being Saturday."
  (+ (mod (+ (days date) 3) 7)))

(defclass time-of-day ()
  ((hours :initarg :hours :accessor hours)
   (minutes :initarg :minutes :accessor minutes)
   (seconds :initarg :seconds :accessor seconds)
   (microseconds :initarg :microseconds :accessor microseconds))
  (:documentation "This class is used to represent time of day in
  hours, minutes, seconds and microseconds."))

(defmethod print-object ((time time-of-day) stream)
  (print-unreadable-object (time stream :type t)
    (with-accessors ((hours hours)
                     (minutes minutes)
                     (seconds seconds)
                     (microseconds microseconds))
        time
      (format stream "~2,'0d:~2,'0d:~2,'0d~@[.~6,'0d~]"
              hours minutes seconds (if (zerop microseconds) nil microseconds)))))

(defun encode-time-of-day (hour minute &optional (second 0) (microsecond 0))
  "Create a timestamp object."
  (make-instance 'time-of-day
                 :hours hour
                 :minutes minute
                 :seconds second
                 :microseconds microsecond))

(defun decode-time-of-day (time)
  (with-accessors ((hours hours)
                   (minutes minutes)
                   (seconds seconds)
                   (microseconds microseconds))
      time
    (values hours minutes seconds microseconds)))

(defclass timestamp (date)
  ((millisecs :initarg :ms :accessor millisecs))
  (:documentation "A timestamp specifies a time with a precision up to
milliseconds."))

(defmethod print-object ((stamp timestamp) stream)
  (print-unreadable-object (stamp stream :type t)
    (multiple-value-bind (year month day hour min sec ms) (decode-timestamp stamp)
      (format stream "~2,'0d-~2,'0d-~4,'0dT~2,'0d:~2,'0d:~2,'0d~@[,~3,'0d~]"
              day month year hour min sec (if (zerop ms) nil ms)))))

(defun encode-timestamp (year month day &optional (hour 0) (minute 0) (second 0) (millisecond 0))
  "Create a timestamp object."
  (multiple-value-bind (year month day) (to-internal-date year month day)
    (make-instance 'timestamp :days (encode-days year month day)
                   :ms (encode-millisecs hour minute second millisecond))))

(defun decode-timestamp (timestamp)
  "Extract the date and time from a timestamp object."
  (multiple-value-bind (year month day) (decode-days (days timestamp))
    (multiple-value-bind (year month day) (to-external-date year month day)
      (multiple-value-bind (hour minute second millisec) (decode-millisecs (millisecs timestamp))
        (values year month day hour minute second millisec)))))

(defconstant +universal-time-offset+ (encode-universal-time 0 0 0 1 3 2000 0))

(defun timestamp-to-universal-time (timestamp)
  "Convert a timestamp to a Lisp universal time."
  (+ +universal-time-offset+ (round (+ (* +millisecs-in-day+ (days timestamp))
                                       (millisecs timestamp)) 1000)))

(defun universal-time-to-timestamp (utime)
  "Convert a Lisp universal time to a timestamp."
  (multiple-value-bind (days millisecs) (floor (* 1000 (- utime +universal-time-offset+))
                                               +millisecs-in-day+)
    (make-instance 'timestamp :days days :ms millisecs)))

(defclass interval ()
  ((millisecs :initarg :ms :accessor millisecs)
   (months :initform 0 :initarg :months :accessor months))
  (:documentation "Intervals can be added to date and timestamp units
to get relative times. The amount of time added for the month part of
a timestamp depends on the time it is being added to."))

(defmethod print-object ((interval interval) stream)
  (print-unreadable-object (interval stream :type t)
    (multiple-value-bind (year month day hour min sec ms) (decode-interval interval)
      (flet ((not-zero (x) (if (zerop x) nil x)))
        (format stream "P~@[~dY~]~@[~dM~]~@[~dD~]~@[~dH~]~@[~dm~]~@[~d~@[,~3,'0d~]S~]"
                (not-zero year) (not-zero month) (not-zero day)
                (not-zero hour) (not-zero min)
                (if (and (zerop sec) (zerop ms)) nil sec) (not-zero ms))))))

(defun encode-interval (&key (year 0) (month 0) (week 0) (day 0)
                        (hour 0) (minute 0) (second 0) (millisecond 0))
  "Create an interval object. Parameters may be negative."
  (make-instance 'interval
                 :ms (+ (* +millisecs-in-day+ (+ day (* 7 week)))
                        (encode-millisecs hour minute second millisecond))
                 :months (+ month (* 12 year))))

(defun decode-interval (interval)
  (multiple-value-bind (day millisecs) (floor (millisecs interval) +millisecs-in-day+)
    (multiple-value-bind (hour min sec ms) (decode-millisecs millisecs)
      (multiple-value-bind (year month) (floor (months interval) 12)
        (values year month day hour min sec ms)))))

(defgeneric time-add (a b)
  (:documentation "Generic function for combining datetime objects.
Adding an interval to a date or timestamp will return a new date or
timestamp, increased by the value of the interval. Adding two
intervals returns a new interval with the sum of the two arguments.
Integers can be used in place of intervals, and will be interpreted as
an amount of milliseconds."))

(defmethod time-add ((stamp timestamp) (interval interval))
  (multiple-value-bind (days millisecs)
      (normalize-timestamp (days stamp) (+ (millisecs stamp) (millisecs interval)))
    (unless (zerop (months interval))
      (setf days (date-add days (months interval))))
    (make-instance 'timestamp :ms millisecs :days days)))

(defmethod time-add ((interval interval) (stamp timestamp))
  (time-add stamp interval))

(defmethod time-add ((date date) (interval interval))
  (multiple-value-bind (days remainder) (floor (millisecs interval) +millisecs-in-day+)
    (unless (zerop remainder)
      (error "Can not add an interval spanning a fractional number of days to a date."))
    (make-instance 'date :days (date-add (+ (days date) days) (months interval)))))

(defmethod time-add ((interval interval) (date date))
  (time-add date interval))

(defmethod time-add ((a interval) (b interval))
  (make-instance 'interval :ms (+ (millisecs a) (millisecs b))
                 :months (+ (months a) (months b))))

(defmethod time-add ((interval interval) (millisecs integer))
  (make-instance 'interval :ms (+ (millisecs interval) millisecs)
                 :months (months interval)))

(defmethod time-add ((millisecs integer) (interval interval))
  (time-add interval millisecs))

(defmethod time-add ((stamp timestamp) (millisecs integer))
  (multiple-value-bind (days millisecs)
      (normalize-timestamp (days stamp) (+ (millisecs stamp) millisecs))
    (make-instance 'timestamp :ms millisecs :days days)))

(defmethod time-add ((millisecs integer) (stamp timestamp))
  (time-add stamp millisecs))

(defmethod time-add ((a integer) (b integer))
  (+ a b))

(defgeneric time-subtract (a b)
  (:documentation "Subtracts datetime objects from each other.
Subtracting two dates or timestamps results in an interval that
represents the difference between them. Similarly subtracting two
intervals gives their difference."))

(defmethod time-subtract ((a date) (b date))
  (make-instance 'interval :ms (* +millisecs-in-day+ (- (days a) (days b)))))

(defmethod time-subtract ((a timestamp) (b date))
  (make-instance 'interval :ms (+ (* +millisecs-in-day+ (- (days a) (days b)))
                                  (millisecs a))))

(defmethod time-subtract ((a timestamp) (b timestamp))
  (make-instance 'interval :ms (+ (* +millisecs-in-day+ (- (days a) (days b)))
                                  (- (millisecs a) (millisecs b)))))

(defmethod time-subtract ((date date) (interval interval))
  (time-add date (invert-interval interval)))

(defmethod time-subtract ((stamp timestamp) (millisecs integer))
  (time-add stamp (- millisecs)))

(defmethod time-subtract ((a interval) (b interval))
  (time-add a (invert-interval b)))

(defmethod time-subtract ((interval interval) (millisecs integer))
  (time-add interval (- millisecs)))

(defmethod time-subtract ((millisecs integer) (interval interval))
  (time-add millisecs (invert-interval interval)))

(defgeneric time= (a b)
  (:documentation "Compare two time-related values, returns a boolean
indicating whether they denote the same time or period."))

(defmethod time= ((a date) (b date))
  (= (days a) (days b)))

(defmethod time= ((a date) (b timestamp))
  (and (= (days a) (days b))
       (= (millisecs b) 0)))

(defmethod time= ((a timestamp) (b date))
  (time= b a))

(defmethod time= ((a timestamp) (b timestamp))
  (and (= (days a) (days b))
       (= (millisecs a) (millisecs b))))

(defmethod time= ((a interval) (b interval))
  (and (= (millisecs a) (millisecs b))
       (= (months a) (months b))))

(defmethod time= ((a time-of-day) (b time-of-day))
  (and (= (hours a) (hours b))
       (= (minutes a) (minutes b))
       (= (seconds a) (seconds b))
       (= (microseconds a) (microseconds b))))

(defgeneric time< (a b)
  (:documentation "Compare two time-related values, returns a boolean
indicating whether the first is less than the second."))

(defmethod time< ((a date) (b date))
  (< (days a) (days b)))

(defmethod time< ((a date) (b timestamp))
  (or (< (days a) (days b))
      (and (= (days a) (days b))
           (> (millisecs b) 0))))

(defmethod time< ((a timestamp) (b date))
  (time> b a))

(defmethod time< ((a timestamp) (b timestamp))
  (or (< (days a) (days b))
      (and (= (days a) (days b))
           (< (millisecs a) (millisecs b)))))
              
(defmethod time< ((a interval) (b interval))
  (or (< (months a) (months b))
      (and (= (months a) (months b))
           (< (millisecs a) (millisecs b)))))

(defgeneric time> (a b)
  (:documentation "Compare two time-related values, returns a boolean
indicating whether the first is greater than the second."))

(defmethod time> ((a date) (b date))
  (> (days a) (days b)))

(defmethod time> ((a date) (b timestamp))
  (> (days a) (days b)))

(defmethod time> ((a timestamp) (b date))
  (time< b a))

(defmethod time> ((a timestamp) (b timestamp))
  (or (> (days a) (days b))
      (and (= (days a) (days b))
           (> (millisecs a) (millisecs b)))))
              
(defmethod time> ((a interval) (b interval))
  (or (> (months a) (months b))
      (and (= (months a) (months b))
           (> (millisecs a) (millisecs b)))))

(defun time<= (a b)
  "Compare two time-related values, returns a boolean indicating
whether the first is less or equal than the second."
  (not (time> a b)))

(defun time>= (a b)
  "Compare two time-related values, returns a boolean indicating
whether the first is greater or equal than the second."
  (not (time< a b)))
