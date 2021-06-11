;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

(defparameter *client-encoding* "UNICODE")

(declaim (inline enc-byte-length))
(defun enc-byte-length (sequence)
  (cl-postgres-trivial-utf-8:utf-8-byte-length sequence))

(declaim (inline enc-write-string))
(defun enc-write-string (string output &key null-terminate)
  (cl-postgres-trivial-utf-8:write-utf-8-bytes string output
                                               :null-terminate null-terminate))

(declaim (inline enc-read-string))
(declaim (ftype (function (t &key (:null-terminated t)
                             (:byte-length fixnum))
                          string)
                enc-read-string))
(defun enc-read-string (input &key null-terminated (byte-length -1))
  (cl-postgres-trivial-utf-8:read-utf-8-string
   input :null-terminated null-terminated :stop-at-eof t :byte-length byte-length))

(declaim (inline enc-string-bytes))
(defun enc-string-bytes (string &key null-terminate)
  (cl-postgres-trivial-utf-8:string-to-utf-8-bytes
   string :null-terminate null-terminate))
