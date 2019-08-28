;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: SIMPLE-DATE-TESTS; -*-
(in-package :simple-date-tests)

;; After loading the file, run the tests with (fiveam:run! :simple-date)

(def-suite :simple-date)
(in-suite :simple-date)

(test days-in-month
  ;; Note: internal date numbers, so 0 is March
  (is (= 31 (simple-date::days-in-month 0 2000)))
  (is (= 30 (simple-date::days-in-month 1 2000)))
  (is (= 31 (simple-date::days-in-month 2 2000)))
  (is (= 30 (simple-date::days-in-month 3 2000)))
  (is (= 31 (simple-date::days-in-month 4 2000)))
  (is (= 31 (simple-date::days-in-month 5 2000)))
  (is (= 30 (simple-date::days-in-month 6 2000)))
  (is (= 31 (simple-date::days-in-month 7 2000)))
  (is (= 30 (simple-date::days-in-month 8 2000)))
  (is (= 31 (simple-date::days-in-month 9 2000)))
  (is (= 31 (simple-date::days-in-month 10 2000)))
  (is (= 28 (simple-date::days-in-month 11 2001))))

(defmacro with-random-dates (amount &body body)
  (let ((i (gensym)))
    `(dotimes (,i ,amount)
      (let ((year (+ 1900 (random 300)))
            (month (1+ (random 12)))
            (day (1+ (random 28)))
            (hour (random 24))
            (min (random 60))
            (sec (random 60))
            (millisec (random 1000)))
        ,@body))))

(test encode-date
  (with-random-dates 100
    (declare (ignore hour min sec millisec))
    (multiple-value-bind (year* month* day*) (decode-date (encode-date year month day))
      (is (and (= year* year)
               (= month* month)
               (= day* day))))))

(test leap-year
  (flet ((test-date (y m d)
           (multiple-value-bind (y2 m2 d2) (decode-date (encode-date y m d))
             (and (= y y2) (= m m2) (= d d2)))))
    (is (test-date 2000 2 29))
    (is (test-date 2004 2 29))
    (is (test-date 2108 2 29))
    (is (test-date 1992 2 29))))

(test encode-timestamp
  (with-random-dates 100
    (multiple-value-bind (year* month* day* hour* min* sec* millisec*)
        (decode-timestamp (encode-timestamp year month day hour min sec millisec))
      (is (and (= year* year)
               (= month* month)
               (= day* day)
               (= hour* hour)
               (= min* min)
               (= sec* sec)
               (= millisec* millisec))))))

(test timestamp-universal-times
  (with-random-dates 100
    (declare (ignore millisec))
    (let ((stamp (encode-timestamp year month day hour min sec 0))
          (utime (encode-universal-time sec min hour day month year 0)))
      (is (= (timestamp-to-universal-time stamp) utime))
      (is (time= (universal-time-to-timestamp utime) stamp)))))

(test add-month
  (with-random-dates 100
    (multiple-value-bind (year* month* day* hour* min* sec* millisec*)
        (decode-timestamp (time-add (encode-timestamp year month day hour min sec millisec)
                                    (encode-interval :month 1)))
      (is (and (or (and (= year* year) (= month* (1+ month)))
                   (and (= year* (1+ year)) (= month* 1)))
               (= day* day)
               (= hour* hour)
               (= min* min)
               (= sec* sec)
               (= millisec* millisec))))))

(test subtract-month
  (with-random-dates 100
    (multiple-value-bind (year* month* day* hour* min* sec* millisec*)
        (decode-timestamp (time-add (encode-timestamp year month day hour min sec millisec)
                                    (encode-interval :month -1)))
      (is (and (or (and (= year* year) (= month* (1- month)))
                   (and (= year* (1- year)) (= month* 12)))
               (= day* day)
               (= hour* hour)
               (= min* min)
               (= sec* sec)
               (= millisec* millisec))))))

(test add-hour
  (with-random-dates 100
    (declare (ignore millisec))
    (is (= (- (timestamp-to-universal-time (time-add (encode-timestamp year month day hour min sec 0)
                                                     (encode-interval :hour 1)))
              (encode-universal-time sec min hour day month year 0))
           3600))))

(test time<
  (with-random-dates 100
    (is (time< (encode-date year month day)
               (encode-date (1+ year) month day)))
    (is (time< (encode-timestamp year month day hour min sec millisec)
               (encode-timestamp year month day hour min (1+ sec) millisec)))
    (is (time< (encode-interval :month month :hour hour)
               (encode-interval :month month :hour hour :minute 30)))))
