;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES-TESTS; -*-
(in-package :cl-postgres-tests)

(def-suite :cl-postgres-ieee-float
    :description "Test suite for cl-postgres functions in ieee-floats.lisp"
    :in :cl-postgres)

(in-suite :cl-postgres-ieee-float)

(test ieee-float
  (is (equal (cl-postgres-ieee-floats::encode-float64 1234567.89)
             4698053240367874048))
  (is (equal (cl-postgres-ieee-floats::decode-float64 4698053240367874048)
             1234567.875d0))
  (is (equal (cl-postgres-ieee-floats::encode-float64 -4698053240.28)
             13975092309851635712))
  (is (equal (cl-postgres-ieee-floats::decode-float64 13975092309851635712)
             -4.69805312d9))
  (is (equal (cl-postgres-ieee-floats::encode-float32 1234567.89)
             1234613311))
  (is (equal (cl-postgres-ieee-floats::decode-float32 1234613311)
             1234567.9))
  (is (equal (cl-postgres-ieee-floats::encode-float32 -4698053240.28)
             3482059597))
  (is (equal (cl-postgres-ieee-floats::decode-float32 3482059597)
            -4.698053e9))
  (cl-postgres-ieee-floats::make-float-converters encode-float128 decode-float128 15 112 nil)
  (is (equal (encode-float128 1234567.89)
             85170166357702954636935397302398353408))
  (is (equal (decode-float128 85170166357702954636935397302398353408)
             1234567.875d0)))
