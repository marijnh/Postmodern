;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES-TESTS; -*-
(in-package :cl-postgres-tests)

(def-suite :cl-postgres-saslprep
    :description "Saslpre Test suite for cl-postgres"
    :in :cl-postgres)

(in-suite :cl-postgres-saslprep)

(test bad-char-error)

(test char-prinable-ascii-p
  (is (cl-postgres::char-printable-ascii-p (code-char 45)))
  (is (not (cl-postgres::char-printable-ascii-p (code-char 16))))
  (is (not (cl-postgres::char-printable-ascii-p (code-char 163)))))

(test string-printable-ascii-p
  (is (not (string-printable-ascii-p "eleveÖn")))
  (is (string-printable-ascii-p "eleven")))

(test code-point-printable-ascii-p
  (is (cl-postgres::code-point-printable-ascii-p 45))
  (is (not (cl-postgres::code-point-printable-ascii-p 16)))
  (is (not (cl-postgres::code-point-printable-ascii-p 163))))

(test char-mapped-to-nothing-p
  (is (not (cl-postgres::char-mapped-to-nothing-p (code-char 214))))
  (is (cl-postgres::char-mapped-to-nothing-p (code-char 8203))))

(test char-mapped-to-space-p
  (is (not (cl-postgres::char-mapped-to-space-p (code-char 214))))
  (is (cl-postgres::char-mapped-to-space-p (code-char 8203)))
  (is (cl-postgres::char-mapped-to-space-p (code-char 5760))))

(test string-mapped-to-nothing-p
  (is (equal (coerce (vector #\a (code-char 65025) #\c #\d) 'string)
             "a︁cd"))
  (is (equal (cl-postgres::string-mapped-to-nothing
              (coerce (vector #\a (code-char 65025) #\c #\d) 'string))
             "acd")))

(test string-mapped-to-space-p
  (is (equal (cl-postgres::string-mapped-to-nothing
              (coerce (vector #\a (code-char 8193) #\c #\d) 'string))
             "a cd"))
  (is (equal (cl-postgres::string-mapped-to-space
              (coerce (vector #\a (code-char 8193) #\c #\d) 'string))
             "a cd"))
  (is (not (equal (cl-postgres::string-mapped-to-nothing
                   (coerce (vector #\a (code-char 8193) #\c #\d) 'string))
                  (cl-postgres::string-mapped-to-space
                   (coerce (vector #\a (code-char 8193) #\c #\d) 'string)))))
  (is (not (equal (coerce (vector #\a (code-char 8193) #\c #\d) 'string)
                  (cl-postgres::string-mapped-to-space
                   (coerce (vector #\a (code-char 8193) #\c #\d) 'string))))))

(test saslprep-normalize
  (is (equal (cl-postgres::saslprep-normalize
              (coerce (vector #\a (code-char 214)
                              (code-char 8193) #\c (code-char 8203)
                              (code-char 65025)
                              (code-char 1214) #\d)
                      'string))
             "aÖ cҾd")))
