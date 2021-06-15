;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES-TESTS; -*-
(in-package :cl-postgres-tests)

(def-suite :cl-postgres-oid
  :description "Test suite for cl-postgres"
  :in :cl-postgres)

(in-suite :cl-postgres-oid)

(test int2
      (is (typep 2 'cl-postgres::int2))
      (is (not (typep 327682 'cl-postgres::int2))))

(test int4
      (is (typep 2 'cl-postgres::int4))
      (is (typep 327682 'cl-postgres::int4))
      (is (not (typep -2147483650 'cl-postgres::int4))))

(test int8
      (is (typep 2 'cl-postgres::int8))
      (is (typep 327682 'cl-postgres::int8))
      (is (typep -9223372036854775808 'cl-postgres::int8))
      (is (not (typep -9223372036854775821 'cl-postgres::int8))))

(test uuid
      (is (cl-postgres::uuip-p "c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd"))
      (is (not (cl-postgres::uuip-p "c73bcdcc-2669-4bf6-81d3-e4an73fb11fd")))
      (is (cl-postgres::uuip-p "123e4567-e89b-12d3-a456-426655440000"))
      (is (cl-postgres::uuip-p "C73BCDCC-2669-4Bf6-81d3-E4AE73FB11FD"))
      (is (cl-postgres::uuip-p "c73bcdcc-2669-4bf6-81d3-e4ae73fb11fD"))
      (is (typep "c73bcdcc-2669-4bf6-81d3-e4ae73fb11fD" 'cl-postgres::uuid-string)))

(test int64-to-vector
  (is (equalp (cl-postgres::int64-to-vector 92233720368547758)
             #(1 71 174 20 122 225 71 174)))
  (is (equalp (cl-postgres::int64-to-vector -9223372036854758)
             #(255 223 59 100 90 28 172 26))))

(test int32-to-vector
  (is (equalp (cl-postgres::int32-to-vector -327681)
             #(255 250 255 255)))
  (is (equalp (cl-postgres::int32-to-vector 327682)
             #(0 5 0 2)))
  (is (not (cl-postgres::int32-to-vector 21474836481))))

(test int16-to-vector
  (is (equalp (cl-postgres::int16-to-vector -34)
             #(255 222)))
  (is (equalp (cl-postgres::int16-to-vector 32767)
             #(127 255)))
  (is (equalp (cl-postgres::int16-to-vector -32767)
              #(128 1))))

(test param-to-oid
  (is (equal (cl-postgres:param-to-oid 32)
             21))
  (is (equal (cl-postgres:param-to-oid 144220)
             23))
  (is (equal (cl-postgres:param-to-oid "abba")
             0))
  (is (equal (cl-postgres:param-to-oid :NULL)
             0))
  (is (equal (cl-postgres:param-to-oid nil)
             16)))
