;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES-TESTS; -*-
(in-package :cl-postgres-tests)

(def-suite :cl-postgres-data-types
    :description "Test suite for cl-postgres functions in data-types.lisp"
    :in :cl-postgres)

(in-suite :cl-postgres-data-types)


(test data-types-integers
  (is (eq (cl-postgres::get-int-size 12)
          'cl-postgres::INT2))
  (is (eq (cl-postgres::get-int-size 123456789)
          'cl-postgres::INT4))
  (is (eq (cl-postgres::get-int-size 123456789123456)
          'cl-postgres::INT8))
  (is (eq (cl-postgres::get-int-size 123456789123456789123456)
          nil))
  (is (cl-postgres::int2p 12))
  (is (cl-postgres::int4p -123456789))
  (is (cl-postgres::int8p -123456789123456)))

(test data-types-uuid-to-byte-array
  (is (equalp (cl-postgres::uuid-to-byte-array "2ef91ac6-aa47-4486-9531-362a72615c1f")
              #(46 249 26 198 170 71 68 134 149 49 54 42 114 97 92 31)))
  (is (equalp (cl-postgres::uuid-to-byte-array "2EF91AC6-AA47-4486-9531-362A72615C1F")
              #(46 249 26 198 170 71 68 134 149 49 54 42 114 97 92 31))))

(test data-types-text-array-p
  (is (cl-postgres::text-array-p (vector "A" "b" "d")))
  (is (not (cl-postgres::text-array-p (vector "A" "b" 1))))
  (is (typep (vector "a" "b" "c") 'cl-postgres::text-array))
  (is (typep (vector "a" "b" "c") '(cl-postgres::text-array 3)))
  (is (not (typep (vector "a" "b" "c") '(cl-postgres::text-array 2)))))


(test data-types-int2-array-p
  (is (typep (vector 1 2 3) '(cl-postgres::int2-array 3)))
  (is (not (typep (vector 1 2 3) '(cl-postgres::int2-array 4))))
  (is (typep (vector 1 2 3 4) '(cl-postgres::int2-array)))
  (is (cl-postgres::int2-array-p (vector 1 2 3 4))))

(test data-types-int4-array-p
  (is (typep (vector 1 4 3) '(cl-postgres::int4-array 3)))
  (is (not (typep (vector 1 4 3) '(cl-postgres::int4-array 4))))
  (is (typep (vector 1 4 3 4) '(cl-postgres::int4-array)))
  (is (cl-postgres::int4-array-p (vector 1 4 3 4))))

(test data-types-int8-array-p
  (is (typep (vector 1 -8 3) '(cl-postgres::int8-array 3)))
  (is (not (typep (vector 1 8 3) '(cl-postgres::int8-array 8))))
  (is (typep (vector 1 8 3 8) '(cl-postgres::int8-array)))
  (is (cl-postgres::int8-array-p (vector 1 8 3 8)))
  (is (typep (vector 1 -123456789123456789 3) '(cl-postgres::int8-array 3)))
  (is (not (typep (vector 1 -123456789123456789123456 3) '(cl-postgres::int8-array 3)))))

(test types-match
  (is (cl-postgres::types-match-p "a" "c"))
  (is (cl-postgres::oid-types-match-p "a" "c")))

(test parameter-list-types
  (is (equal (cl-postgres::parameter-list-types '("a" 1 123456789 123456789123456
                                                  123456789123456987654 'b #\d 12.2
                                                  nil t))
             '(0 21 23 20 0 0 0 700 16 16)))
  #-clisp (is (equal (cl-postgres::parameter-list-types '(12413212.98324d0))
                     '(701)))
  #+clisp (is (equal (cl-postgres::parameter-list-types '(12413212.98324d0))
                     '(700))))

(test parameter-lists-match-oid-types
  (is (cl-postgres::parameter-lists-match-oid-types-p '(12413212.98324d0) '(124212.98324d0)))
  (is (not (cl-postgres::parameter-lists-match-oid-types-p '(12413212.98324d0 "a")
                                                           '(124212.98324d0))))
  (is (not (cl-postgres::parameter-lists-match-oid-types-p '("a")
                                                           '(3)))))

(test param-to-oid
  #-clisp (is (equal (loop for x in '(integer int2 int4 int8 boolean t nil float
                              double-float "jeff" 2 2000 12345678909 1.0 2.7d0 "12a")
                   collect (list x (param-to-oid x)))
             '((INTEGER 0) (INT2 0) (INT4 0) (INT8 0) (BOOLEAN 0) (T 16) (NIL 16) (FLOAT 0)
               (DOUBLE-FLOAT 0) ("jeff" 0) (2 21) (2000 21) (12345678909 20) (1.0 700)
               (2.7d0 701) ("12a" 0))))
  #+clisp (is (equal (loop for x in '(integer int2 int4 int8 boolean t nil float
                              double-float "jeff" 2 2000 12345678909 1.0 2.7d0 "12a")
                   collect (list x (param-to-oid x)))
             '((INTEGER 0) (INT2 0) (INT4 0) (INT8 0) (BOOLEAN 0) (T 16) (NIL 16) (FLOAT 0)
               (DOUBLE-FLOAT 0) ("jeff" 0) (2 21) (2000 21) (12345678909 20) (1.0 700)
               (2.7d0 700) ("12a" 0)))))
