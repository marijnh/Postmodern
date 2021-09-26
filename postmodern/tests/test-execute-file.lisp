;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(def-suite :postmodern-execute-file
    :description "Execute file suite for postmodern"
    :in :postmodern)

(in-suite :postmodern-execute-file)

(defparameter *good-file* (asdf:system-relative-pathname :postmodern "postmodern/tests/test-execute-file.sql"))

(defparameter *first-include-good-file* (asdf:system-relative-pathname :postmodern "postmodern/tests/test-first-include-execute-file.sql"))

(defparameter *fail-include-file* (asdf:system-relative-pathname :postmodern "postmodern/tests/test-fail-include-execute-file.sql"))

(defparameter *bad-file* (asdf:system-relative-pathname :postmodern "postmodern/tests/test-execute-file-broken.sql"))
(defparameter *bad-file-with-transaction* (asdf:system-relative-pathname :postmodern "postmodern/tests/test-execute-file-broken-transaction.sql"))

;; Test Parse Comments

(test basic-multi-line1
  (is (equal (postmodern::parse-comments " something1 /* comment */ something2")
             " something1  something2")))

(test basic-multi-line2
  (is (equal (postmodern::parse-comments " something1 /*
  comment */ something2")
             " something1  something2")))

(test basic-single-line
  (is (equal (postmodern::parse-comments " something1 -- comment */ something2")
             " something1 ")))

(test multi-line-within-single-line
  (is (equal (postmodern::parse-comments " something1 -- /* comment */ something2")
             " something1 ")))

(test multi-line-within-multi-line
  (is (equal (postmodern::parse-comments " something1 /* outside comment
 /* inside comment */ bad-something2 */ something2")
             " something1  something2")))

(test broken-nested-muli-line-comments
  (is (equal (pomo::parse-comments "/* comment /* still the same comment */")
             "")))

(test single-line-within-multi-line
  (is (equal (postmodern::parse-comments " something1 /* comm -- ent */ something2")
             " something1  something2")))

(test basic-fake-single-line
  (is (equal (postmodern::parse-comments " something1 - something2")
             " something1 - something2")))

(test basic-fake-muli-line
  (is (equal (postmodern::parse-comments " something1 / something2")
             " something1 / something2")))

(test multi-line-within-sql-string1
  (is (equal (postmodern::parse-comments " something ('my wonder /* something */ company ')")
             " something ('my wonder /* something */ company ')")))

(test multi-line-within-sql-string2
  (is (equal (postmodern::parse-comments "insert into a (d) values ('/*');")
             "insert into a (d) values ('/*');")))

(test single-line-within-sql-string1
  (is (equal (postmodern::parse-comments " something ('my wonder -- company ')")
             " something ('my wonder -- company ')")))

(test single-line-within-sql-string2
  (is (equal (postmodern::parse-comments "insert into a (d) values ('-- /*');")
             "insert into a (d) values ('-- /*');")))

(test asterisk-no-comment
  (is (equal (pomo::parse-comments "select * from x")
             "select * from x")))

(test unicode-escapes
  (is (equal (pomo::parse-comments "U&'d\\0061t\\+000061'")
             "U&'d\\0061t\\+000061'")))

(test dollar-quoted-string-constants1
  (is (equal (pomo::parse-comments "$$Dianne's horse$$")
             "$$Dianne's horse$$")))

(test dollar-quoted-string-constants2
  (is (equal (pomo::parse-comments "$function$
BEGIN
    RETURN ($1 ~ $q$[\\t\\r\\n\\v\\]$q$);
END;
$function$")
             "$function$
BEGIN
    RETURN ($1 ~ $q$[\\t\\r\\n\\v\\]$q$);
END;
$function$")))

(test single-quote-sql
  (is (equal (pomo::parse-comments "REAL '1.23'  -- string style")
             "REAL '1.23'  ")))

(test simple-execute-file
  (with-test-connection
    (when (table-exists-p 'company-employees)
      (query (:drop-table :if-exists 'company-employees :cascade)))
    (pomo:execute-file *good-file*)
    (is (table-exists-p 'company-employees))
    (is (equal "Paul" (query (:select 'name :from 'company-employees :where (:= 'id 1)) :single)))
    (is (equal 11 (query (:select (:count 'id) :from 'company-employees) :single)))))

(test broken-execute-file
  (with-test-connection
    (when (table-exists-p 'company-employees)
      (query (:drop-table :if-exists 'company-employees :cascade)))
    (signals error (pomo:execute-file *bad-file*))
    (is (table-exists-p 'company-employees))
    (is (equal "paul" (query (:select 'name :from 'company-employees :where (:= 'id 1)) :single)))
;; the bad-file should stop executing on the attempt to insert a record with the same id as the first insertion
    (is (equal 1 (query (:select (:count 'id) :from 'company-employees) :single)))
    (query (:drop-table :if-exists 'company-employees :cascade))))

(test broken-execute-file-wrapped-in-transaction
  (with-test-connection
    (when (table-exists-p 'company-employees)
      (query (:drop-table :if-exists 'company-employees :cascade)))
    (signals error (pomo:execute-file *bad-file-with-transaction*)))
  (with-test-connection
    (is (not (table-exists-p 'company-employees)))
    (query (:drop-table :if-exists 'company-employees :cascade))))

(test fail-include-execute-file
  (with-test-connection
    (when (table-exists-p 'company-employees)
      (query (:drop-table :if-exists 'company-employees :cascade)))
    (signals error (pomo:execute-file *fail-include-file*))
    (query (:drop-table :if-exists 'company-employees :cascade))))
