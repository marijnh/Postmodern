;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

(defun int64-to-vector (int)
  "Takes a 64 byte integer and returns a vector of unsigned bytes with a length of 8"
  (when (and (integerp int) (< int 18446744073709551615))
    (let ((intv (make-array '(8)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
        (setf (aref intv 0) (ldb (byte 8 56) int))
        (setf (aref intv 1) (ldb (byte 8 48) int))
        (setf (aref intv 2) (ldb (byte 8 40) int))
        (setf (aref intv 3) (ldb (byte 8 32) int))
        (setf (aref intv 4) (ldb (byte 8 24) int))
        (setf (aref intv 5) (ldb (byte 8 16) int))
        (setf (aref intv 6) (ldb (byte 8 8) int))
        (setf (aref intv 7) (ldb (byte 8 0) int))
        intv)))

(defun int32-to-vector (int)
  "Takes a 32 byte integer and returns a vector of unsigned bytes with a length of 4"
  (when (and (integerp int) (< int 4294967296))
    (let ((intv (make-array '(4)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
     (setf (aref intv 0) (ldb (byte 8 24) int))
     (setf (aref intv 1) (ldb (byte 8 16) int))
     (setf (aref intv 2) (ldb (byte 8 8) int))
     (setf (aref intv 3) (ldb (byte 8 0) int))
     intv)))

(defun int16-to-vector (int)
    "Takes a 16 byte integer and returns a vector of unsigned bytes
with a length of 2."
  (when (and (integerp int) (< int 65536))
    (let ((intv (make-array '(2)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
      (setf (aref intv 0) (ldb (byte 8 8) int))
      (setf (aref intv 1) (ldb (byte 8 0) int))
      intv)))

(defun int8-to-vector (int)
    "Takes a 8 byte positive integer and returns a vector of unsigned bytes
with a length of 1 byte."
  (let ((intv (make-array '(1)
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
    (setf (aref intv 0) (ldb (byte 8 0) int))
    intv))

(defun int-to-vector (int)
  "Takes a signed integer and returns a vector of unsigned bytes."
  (if (integerp int)
   (case (get-int-size int)
     (int2 (int16-to-vector int))
     (int4 (int32-to-vector int))
     (int8 (int64-to-vector int)))
   nil))

(defun get-int-size (int)
  "Takes an integer and returns the size of the integer for postgresql
purposes (int2, int4, int8)"
  (declare (integer int))
  (cond ((and (> int -32769)
              (< int 32768))
         'int2)
        ((and (> int -2147483649)
              (< int 2147483648))
         'int4)
        ((and (> int -9223372036854775809)
              (< int 9223372036854775808))
         'int8)
        (t nil)))

(defun int2p (item)
  "Checking whether the item is an int2"
  (and (integerp item)
       (and (> item -32769)
            (< item 32768))))

(defun int4p (item)
  "Checking whether the item is an int4"
  (and (integerp item)
       (and (> item -2147483649)
            (< item 2147483648))))

(defun int8p (item)
  "Checking whether the item is an int8"
  (and (integerp item)
       (and (> item -9223372036854775809)
            (< item 9223372036854775808))))

(deftype int2 ()
  '(integer -32769 32768))

(deftype int4 ()
  '(integer -2147483648 2147483647))

(deftype int8 ()
  '(integer -9223372036854775808 9223372036854775808))

(defun uuid-to-byte-array (uuid)
  "Takes a uuid string and creates a vector of unsigned bytes"
  (let ((array (make-array 16
                           :element-type '(unsigned-byte 8)
                           :initial-element 0))
        (sec1 (parse-integer uuid :start 0 :end 8 :radix 16))
        (sec2 (parse-integer uuid :start 9 :end 13 :radix 16))
        (sec3 (parse-integer uuid :start 14 :end 18 :radix 16))
        (sec4 (parse-integer uuid :start 19 :end 23 :radix 16))
        (sec5 (parse-integer uuid :start 24 :end 36 :radix 16)))
    (loop for i from 3 downto 0
		      do (setf (aref array (- 3 i)) (ldb (byte 8 (* 8 i)) sec1)))
		(loop for i from 5 downto 4
		      do (setf (aref array i) (ldb (byte 8 (* 8 (- 5 i))) sec2)))
		(loop for i from 7 downto 6
		      do (setf (aref array i) (ldb (byte 8 (* 8 (- 7 i))) sec3)))
    (loop for i from 9 downto 8
          do (setf (aref array i) (ldb (byte 8 (* 8 (- 9 i))) sec4)))
		(loop for i from 15 downto 10
		      do (setf (aref array i) (ldb (byte 8 (* 8 (- 15 i))) sec5)))
    array))


(defun uuip-p (item)
  "Checking whether a string is a uuid. It does require the uuid string to be in hyphenated form. Like Postgresql, it will accept both upper and lower case, so looser than the specification which requires lower case only."
  (and (stringp item)
       (cl-ppcre:scan "\\b[0-9a-fA-F]{8}\\b-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-\\b[0-9a-fA-F]{12}\\b"
                      item)))

(deftype uuid-string ()
  `(and (string)
        (satisfies uuip-p)))

(defun text-array-p (item)
  "Checking whether every item in an array is text"
  (and (arrayp item)
       (every #'stringp item)))

(defun int2-array-p (item)
  "Checking whether every item in an array is an int4"
  (and (arrayp item)
       (every #'int2p item)))

(defun int4-array-p (item)
  "Checking whether every item in an array is an int4"
  (and (arrayp item)
       (every #'int4p item)))

(defun int8-array-p (item)
  "Checking whether every item in an array is an int4"
  (and (arrayp item)
       (every #'int8p item)))

(deftype text-array (&optional size)
  "Text-array is an array of strings"
  `(and (array string (,size))
        (satisfies text-array-p)))

(deftype int2-array (&optional size)
  "Int4-array is an array of integers of size 2"
  `(and (array integer (,size))
        (satisfies int2-array-p)))

(deftype int4-array (&optional size)
  "Int4-array is an array of integers of size 4"
  `(and (array integer (,size))
        (satisfies int4-array-p)))

(deftype int8-array (&optional size)
  "Int8-array is an array of integers of size 8"
  `(and (array int8 (,size))
        (satisfies int8-array-p)))

(defun param-to-oid (param)
  "Returns the postgresql oid for parameters which are going to be passed
from postmodern to postgresql in binary. Currently that only includes integers,
single-floats, double-floats and boolean. Everything else will be passed as
text for postgresql to interpret. We do not do arrays because passing them in Postgresql's
binary format is actually more overhead than sending the string literal version. See
https://www.codesynthesis.com/pipermail/odb-users/2012-August/000688.html.

If you are wondering why text is not included in this function, many Postgresql
data types have no common lisp equivalent and therefore must be
passed as string literals. Specifying that something was text
when it is not will result in Postgresql throwing type mismatch errors."
  (typecase param
    (int2 cl-postgres-oid:+int2+)
    (int4 cl-postgres-oid:+int4+)
    (int8 cl-postgres-oid:+int8+)
    #-clisp (single-float cl-postgres-oid:+float4+)
    #+clisp (float cl-postgres-oid:+float4+)
    (double-float cl-postgres-oid:+float8+)
    (boolean cl-postgres-oid:+bool+)
    (t 0)))

(defun types-match-p (x y)
  (equal (type-of x) (type-of y)))

(defun oid-types-match-p (x y)
  "Returns t if the two parameters have matching types"
  (eq (param-to-oid x) (param-to-oid y)))

(defun parameter-list-types (lst)
  "Takes a list of parameters and returns the matching postgresql oid types"
  (mapcar #'param-to-oid lst))

(defun parameter-lists-match-oid-types-p (x y)
  "Takes two lists and validates that the lists have matching postgresql oid types."
  (let ((lst1 (mapcar #'param-to-oid x))
        (lst2 (mapcar #'param-to-oid y)))
    (equal lst1 lst2)))
