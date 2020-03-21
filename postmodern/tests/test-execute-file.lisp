;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(fiveam:def-suite :postmodern-execute-file
    :description "Execute file suite for postmodern"
    :in :postmodern)

(fiveam:in-suite :postmodern-execute-file)

(defparameter good-file (asdf:system-relative-pathname :postmodern "postmodern/tests/test-execute-file.sql"))
(defparameter bad-file (asdf:system-relative-pathname :postmodern "postmodern/tests/test-execute-file-broken.sql"))
(defparameter bad-file-with-transaction (asdf:system-relative-pathname :postmodern "postmodern/tests/test-execute-file-broken-transaction.sql"))

(test simple-execute-file
  (with-test-connection
    (when (table-exists-p 'company-employees)
      (query (:drop-table :if-exists 'company-employees :cascade)))
    (pomo:execute-file good-file)
    (is (table-exists-p 'company-employees))
    (is (equal "paul" (query (:select 'name :from 'company-employees :where (:= 'id 1)) :single)))
    (is (equal 6 (query (:select (:count 'id) :from 'company-employees) :single)))
    (query (:drop-table :if-exists 'company-employees :cascade))))

(test broken-execute-file
  (with-test-connection
    (when (table-exists-p 'company-employees)
      (query (:drop-table :if-exists 'company-employees :cascade)))
    (signals error (pomo:execute-file bad-file))
    (is (table-exists-p 'company-employees))
    (is (equal "paul" (query (:select 'name :from 'company-employees :where (:= 'id 1)) :single)))
;; the bad-file should stop executing on the attempt to insert a record with the same id as the first insertion
    (is (equal 1 (query (:select (:count 'id) :from 'company-employees) :single)))
    (query (:drop-table :if-exists 'company-employees :cascade))))

(test broken-execute-file-wrapped-in-transaction
  (with-test-connection
    (when (table-exists-p 'company-employees)
      (query (:drop-table :if-exists 'company-employees :cascade)))
    (signals error (pomo:execute-file bad-file-with-transaction)))
  (with-test-connection
    (is (not (table-exists-p 'company-employees)))
    (query (:drop-table :if-exists 'company-employees :cascade))))
