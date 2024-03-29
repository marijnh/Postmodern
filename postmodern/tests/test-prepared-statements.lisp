;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(def-suite :postmodern-prepare
  :description "Prepared query suite for postmodern"
  :in :postmodern)

(in-suite :postmodern-prepare)

(defun prepare-fixture ()
  (drop-prepared-statement "all")
  (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
  (execute (:create-table test-data ((a :type integer :primary-key t)
                                     (b :type real)
                                     (c :type (or text db-null)))))
  (execute (:insert-into 'test-data :set 'a 1 'b 5.4 'c "foobar"))
  (execute (:insert-into 'test-data :set 'a 2 'b 88 'c :null)))

(defmacro with-non-binary-fixture (&body body)
  `(progn
     (prepare-fixture)
     (unwind-protect (progn ,@body)
       (execute (:drop-table :if-exists 'test-data :cascade)))))

(test prepare-basic-statement
  (with-test-connection
    (without-binary
      (defprepared 'select1 "select $1" :single)
      (is (equal (funcall 'select1 13)
                 "13")))))

(test prepare-any-statement
  (with-test-connection
    (without-binary
      (with-non-binary-fixture
        (defprepared 'select1_any "select a from test_data where c = any($1)" :single)
        (defprepared 'select2_any "select c from test_data where a = any($1)" :single)
        (is (equal (funcall 'select1_any '("bar" "foobar" "foo"))
                   1))
        (is (equal (funcall 'select1_any #("bar" "foobar" "foo"))
                   1))
        (is (equal (funcall 'select2_any '(1 3 4))
                   "foobar"))
        (is (equal (funcall 'select2_any #(1 3 4))
                   "foobar"))
        (signals error (eq (funcall 'select2_any '()))) ;passing an empty list will throw an error
        (is (eq (funcall 'select2_any #())
                nil))))))

(test prepare-proving-life-limited-to-connection
  (with-test-connection
    (defprepared 'select1 "select $1" :single)
    (funcall 'select1 13)
    (is (equal (list-postmodern-prepared-statements t)
               '("SELECT1")))
    (is (equal (list-prepared-statements t)
               '("SELECT1"))))
  (with-test-connection
    (is (equal (list-postmodern-prepared-statements t)
               nil))
    (is (equal (list-prepared-statements t)
               nil))))

(test prepare-tracking-statements-1
  "This test just checks that prepared statements are being logged both in the
postmodern meta connection and in Postgresql"
  (with-test-connection
    (without-binary
      (with-non-binary-fixture
          (let ((select-int (prepare (:select (:type '$1 integer)) :single))
                (byte-arr (make-array 10 :element-type '(unsigned-byte 8) :initial-element 10))
                (select-bytes (prepare (:select (:type '$1 bytea)) :single)))
            (defprepared 'select1 "select a from test_data where c = $1" :single)
            ;; Defprepared does not change the prepared statements logged in the postmodern connection or
            ;; in the postgresql connection. That will only happen when the prepared statement is
            ;; funcalled.
            (is (equal 0 (length (list-postmodern-prepared-statements t))))
            (is (equal 0 (length (list-prepared-statements t))))
            (is (= (funcall select-int 10) 10))
            (is (= (funcall select-int -40) -40))

            ;; CHANGE HERE TO SIGNALS ERROR

            (is (equal (funcall select-int :null)
                       :NULL)) ;mismatched parameter types if the connection is set for binary
            ;; parameter passing
            ;; the funcall creates the prepared statements logged in the postmodern connection
            ;; and the postgresql connection
            (is (equal 1 (length (list-postmodern-prepared-statements t))))
            (is (equal 1 (length (list-prepared-statements t))))
            (is (equalp (funcall select-bytes byte-arr) byte-arr))
            (is (equal 2 (length (list-prepared-statements t))))
            (is (not (prepared-statement-exists-p "select1")))
            (is (equal 1 (funcall 'select1 "foobar")))
            (is (prepared-statement-exists-p "select1"))
            (is (prepared-statement-exists-p 'select1))
            (is (equal 3 (length (list-postmodern-prepared-statements t))))
            (is (equal 3 (length (list-prepared-statements t)))))))))

(test prepare-tracking-statements-with-drops-1
  (with-test-connection
    (with-non-binary-fixture
    (without-binary
      (defprepared 'select1 "select a from test_data where c = $1" :single)
      ;; Defprepared does not change the prepared statements logged in the postmodern connection or
      ;; in the postgresql connection. That will happen when the prepared statement is funcalled.

      ;; the funcall creates the prepared statements logged in the postmodern connection
      ;; and the postgresql connection
      ;; drop the defprepared statement from postgresql, but not from postmodern has no
      (is (not (prepared-statement-exists-p "select1")))
      (is (not (list-postmodern-prepared-statements t)))
      ;; the following has no impact
      (drop-prepared-statement "select1" :location :postgresql)
      (is (equal 0 (length (list-postmodern-prepared-statements t))))
      (is (equal 0 (length (list-prepared-statements t))))
      ;; recreate the defprepared statement into postgresql
      (is (equal 1 (funcall 'select1 "foobar")))
      (is (prepared-statement-exists-p "select1"))
      ;; recreate the first prepared statement back into both postgresql and postmodern
      (is (member "select a from test_data where c = $1"
                  (list-postmodern-prepared-statements) :test 'equal :key 'cadr))
      (is (member "SELECT1" (list-postmodern-prepared-statements t) :test 'equal))
      (is (equal '("select a from test_data where c = $1" ("foobar"))
                 (find-postmodern-prepared-statement "select1")))
      (drop-prepared-statement "select1" :location :postgresql)
      (is (equal 0 (length (list-prepared-statements t))))))))

(test prepare-select-no-table-two-parameters
  (with-test-connection
    (without-binary
      (is (equal (query (:select '$1 '$2) 1 "a")
                 '(("1" "a"))))
      (let ((select-two (prepare (:select (:type '$1 'integer) (:type '$2 'string)))))
        (is (equal (funcall select-two 1 "a")
                   '((1 "a")))))
      (let ((select-two (prepare (:select (:type '$1 integer) (:type '$2 string)))))
        (signals error (funcall select-two 1)))
      (let ((select-two (prepare (:select (:type '$1 integer) (:type '$2 string)))))
        (signals error (funcall select-two "a" 1))))))

(test defprepared-select-no-table
  (with-test-connection
    (without-binary
      (defprepared 'test8e (:select '$1))
      (is (equal (test8e 1) '(("1"))))
      (is (equal (test8e 189) '(("189"))))
      (is (equal (test8e 11.5) '(("11.5")))))))

(test prepare-3-drop-no-table
  (with-test-connection
    (without-binary
      (drop-prepared-statement "all")
      (defprepared select2 "select $1" :single)
      (is (equal (funcall 'select2 "foobar")
                 "foobar"))
      (is (equal (select2 "foobar")
                 "foobar"))
      (drop-prepared-statement 'select2 :location :postgresql)
      (is (equal (funcall 'select2 "foobar")
                 "foobar"))
      (is (equal (select2 "foobar")
                 "foobar"))
      (drop-prepared-statement 'select2 :location :postmodern)
      (signals error (funcall 'select2 "foobar")) ; function undefined
      (defprepared select2 "select $1" :single)
      (is (equal (funcall 'select2 "foobar")
                 "foobar"))
      (drop-prepared-statement 'select2)
      (signals error (funcall 'select2 "foobar")))))

(test prepare-3-drop-with-table
  (with-test-connection
    (without-binary
      (with-non-binary-fixture
          (defprepared 'select1 "select a from test_data where c = $1" :single)
          ;; Defprepared does not change the prepared statements logged in the postmodern
          ;; connection or in the postgresql connection. That will happen when the
          ;; prepared statement is funcalled.
          ;; drop the prepared select1 statement from both postgresql and postmodern
          (is (equal 1 (funcall 'select1 "foobar")))
        (is (equal 1 (select1 "foobar")))
        (drop-prepared-statement 'select1)
        (signals error (funcall 'select1))
        (is (not (prepared-statement-exists-p "select1")))))))

(test prepare-3-overwrite
  "Testing Overwriting prepared statements"
  (with-test-connection
    (without-binary
      (with-non-binary-fixture
          (defprepared select1 "select a from test_data where c = $1" :single)
          (is (equal 1 (funcall 'select1 "foobar")))
        ;; Testing overwrites. Now change the defprepared statement
        (defprepared select1 "select c from test_data where a = $1" :single)
        ;; Defprepared does not change the prepared statements logged in the postmodern connection or
        ;; in the postgresql connection. That happens at funcall.
        ;; Test still the original in both postgresql and postmodern
        (is (equal "select a from test_data where c = $1"
                   (find-postgresql-prepared-statement "select1")))
        (is (equal '("select a from test_data where c = $1" ("foobar"))
                   (find-postmodern-prepared-statement "select1")))
        ;; funcall now drops the old version and create the new version.
        ;; The old parameter no longer works
        (is (equal (funcall 'select1 1)
                   "foobar"))
        (signals error (funcall 'select1 "foobar"))))))

(test prepare-3-partial
  (with-test-connection
    (without-binary
      (with-non-binary-fixture
      (defprepared select1 "select a from test_data where c = $1" :single)
      (is (equal 1 (funcall 'select1 "foobar")))
      (defprepared select1 "select c from test_data where a = $1" :single)
      (is (equal (funcall 'select1 1)
                 "foobar"))))))

(test prepare-4
  (with-test-connection
    (with-non-binary-fixture
        (without-binary
          (defprepared select1 "select c from test_data where a = $1" :single)
          (is (eq :null (funcall 'select1 2)))
          (drop-prepared-statement "all")
          (is (equal 0 (length (list-prepared-statements t))))
          (is (equal 0 (length (list-postmodern-prepared-statements t))))
          ;; recreate select1, then drop the connection and call select1
          (defprepared select1 "select c from test_data where a = $1" :single)
          (disconnect *database*)
          (signals error (query "select c from test_data where a = 2" :single))
          (is (eq :null (funcall 'select1 2)))))))

(test prepare-5
  "Test to ensure that we do not recreate the statement each time it is funcalled"
  (with-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared select1 "select $1" :single)
      (is (equal (funcall 'select1 10)
                 "10"))
      (let ((time1 (query "select prepare_time from pg_prepared_statements where name = 'select1'"
                          :single)))
        (sleep 1)
        (funcall 'select1 2)
        (is (equal time1
                   (query "select prepare_time from pg_prepared_statements where name = 'select1'"
                          :single)))))))

(test prepare-change-param-no-table-txt
  (with-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-text "select $1" :single)
      (is (equal (funcall 'select-text "A")
                 "A"))
      (is (equal (funcall 'select-text "BCE")
                 "BCE"))
      (is (equal (funcall 'select-text 1)
                 "1"))
      (is (equal (funcall 'select-text "ABC")
                 "ABC")))))

(test prepare-change-param-no-table-float
  (with-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-float "select $1" :single)
      (is (equal (funcall 'select-float 1.5)
                 "1.5"))
      (is (equal (funcall 'select-float 2.5)
                 "2.5"))

      (is (equal (funcall 'select-float "abc")
                 "abc"))
      (is (equal (funcall 'select-float t)
                 "true"))
      (is (equal (funcall 'select-float "5")
                 "5"))
      (is (equal (funcall 'select-float "5.5")
                 "5.5"))
      (is (equal (funcall 'select-float 1.0)
                 "1.0")))))

(test prepare-change-param-no-table-bool
  (with-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-bool "select $1" :single)
      (is (equal (funcall 'select-bool t)
                 "true"))
      (is (equal (funcall 'select-bool nil)
                 "false"))
      (is (equal (funcall 'select-bool "14")
                 "14"))
      (is (equal (funcall 'select-bool 14.2)
                 "14.2"))
      (is (equal (funcall 'select-bool t)
                 "true"))
      (is (equal (select-bool t)
                 "true")))))

(test prepare-change-param-no-table-int
  (with-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-int "select $1" :single)
      (is (equal (funcall 'select-int 14)
                 "14"))
      (is (equal (select-int 6)
                 "6"))
      (is (equal (funcall 'select-int "14")
                 "14"))
      (is (equal (select-int "14")
                 "14"))
      (is (equal (funcall 'select-int 14.2)
                 "14.2"))
      (is (equal (select-int 14.2)
                 "14.2"))
      (is (equal (funcall 'select-int "abc")
                 "abc"))
      (is (equal (funcall 'select-int t)
                 "true"))
      (is (equal (funcall 'select-int 5)
                 "5"))
      (is (equal (select-int 6)
                 "6")))))

(test prepare-change-param-no-table-txt-with-disconnect
  (with-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-text "select $1" :single)
      (is (equal (funcall 'select-text "A")
                 "A"))
      (disconnect *database*)
      (is (equal (funcall 'select-text "BCE")
                 "BCE"))
      (is (equal (funcall 'select-text 1)
                 "1"))
      (is (equal (funcall 'select-text "ABC")
                 "ABC")))))

(test prepare-change-param-no-table-float-with-disconnect
  (with-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-float "select $1" :single)
      (is (equal (funcall 'select-float 1.5)
                 "1.5"))
      (disconnect *database*)
      (is (equal (funcall 'select-float 2.5 )
                 "2.5"))
      (is (equal (funcall 'select-float "abc")
                 "abc"))
      (is (equal (funcall 'select-float t)
                 "true"))
      (is (equal (funcall 'select-float "5")
                 "5"))
      (is (equal (funcall 'select-float "5.5")
                 "5.5"))
      (is (equal (funcall 'select-float 1.0)
                 "1.0"))
      (is (equal (select-float 1.0)
                 "1.0")))))

(test prepare-change-param-no-table-bool-with-disconnect
  (with-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-bool "select $1" :single)
      (is (equal (funcall 'select-bool t)
                 "true"))
      (disconnect *database*)
      (is (equal (funcall 'select-bool nil)
                 "false"))
      (is (equal (funcall 'select-bool "14")
                 "14"))
      (is (equal (funcall 'select-bool 14.2)
                 "14.2"))
      (is (equal (funcall 'select-bool t)
                 "true")))))

(test prepare-change-param-no-table-int-with-disconnect
  (with-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-int "select $1" :single)
      (is (equal (funcall 'select-int 14)
                 "14"))
      (disconnect *database*)
      (is (equal (funcall 'select-int 6)
                 "6"))
      (is (equal (funcall 'select-int "14")
                 "14"))
      (is (equal (funcall 'select-int "abc")
                 "abc"))
      (is (equal (funcall 'select-int 14.2)
                 "14.2"))
      (is (equal (funcall 'select-int t)
                 "true"))
      (is (equal (funcall 'select-int 5)
                 "5"))
      (is (equal (select-int 5)
                 "5")))))

(test prepare-change-params-with-table
  (with-test-connection
    (without-binary
      (with-non-binary-fixture
      ;; Defprepared does not change the prepared statements logged in the postmodern connection or
      ;; in the postgresql connection. That will happen when the prepared statement is funcalled.
      (defprepared select-1 "select c from test_data where a = $1" :single)
      (is (eq :null (funcall 'select-1 2)))
      ;; recreate select1, then drop the connection and call select1
      (disconnect *database*)
      (signals error (query "select c from test_data where a = 2" :single))
      (signals error (query "select c from test_data where a = 2" :single))
      (signals error (funcall 'select-1 "2a"))
      (is (eq :null (funcall 'select-1 2)))))))

(test prepare-reserved-words
  (with-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (when (table-exists-p 'from-test) (execute (:drop-table 'from-test)))
      (execute "CREATE TABLE from_test (id SERIAL NOT NULL, flight INTEGER DEFAULT NULL, \"from\" VARCHAR(100) DEFAULT NULL, to_destination VARCHAR(100) DEFAULT NULL, PRIMARY KEY (id, \"from\"))")
      (execute (:insert-into 'from-test :set 'flight 1 'from "Stykkishólmur"
                             :to-destination "Reykjavík"))
      (execute (:insert-into 'from-test :set 'flight 2 'from "Reykjavík"
                             :to-destination "Seyðisfjörður"))
      (defprepared select1 "select \"from\" from from_test where to_destination = $1" :single)
      ;; the funcall creates the prepared statements logged in the postmodern connection
      ;; and the postgresql connection
      (is (equal "Reykjavík" (funcall 'select1 "Seyðisfjörður")))
      (execute (:drop-table 'from-test)))))

(test prepare-tracking-statements-1-pooled
  (with-pooled-test-connection
    (with-non-binary-fixture
        (without-binary
          (let ((select-int (prepare (:select (:type '$1 integer)) :single))
                (byte-arr (make-array 10 :element-type '(unsigned-byte 8) :initial-element 10))
                (select-bytes (prepare (:select (:type '$1 bytea)) :single)))
            (defprepared 'select1 "select a from test_data where c = $1" :single)
            ;; Defprepared does not change the prepared statements logged in the postmodern connection or
            ;; in the postgresql connection. That will happen when the prepared statement is funcalled.
            (is (equal 0 (length (list-postmodern-prepared-statements t))))
            (is (equal 0 (length (list-prepared-statements t))))
            (is (= (funcall select-int 10) 10))
            (is (= (funcall select-int -40) -40))

            ;; CHANGE HERE TO SIGNALS ERROR if bnary

            (is (equal (funcall select-int :null)
                       :NULL))
            ;; the funcall creates the prepared statements logged in the postmodern connection
            ;; and the postgresql connection
            (is (equal 1 (length (list-postmodern-prepared-statements t))))
            (is (equal 1 (length (list-prepared-statements t))))
            (is (equalp (funcall select-bytes byte-arr) byte-arr))
            (is (equal 2 (length (list-prepared-statements t))))
            (is (not (prepared-statement-exists-p "select1")))
            (is (equal 1 (funcall 'select1 "foobar")))
            (is (prepared-statement-exists-p "select1"))
            (is (equal 3 (length (list-postmodern-prepared-statements t))))
            (is (equal 3 (length (list-prepared-statements t))))
            (drop-prepared-statement "all"))))))

(test proof-that-prepared-statements-survive-in-pool
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (defprepared 'test8e (:select '$1))
    (is (equal (test8e 17)
               '(("17")))))
  (with-pooled-test-connection
    (is (equal (list-postmodern-prepared-statements t)
               '("TEST8E")))
    (is (equal (list-prepared-statements t)
               '("TEST8E")))))

(test prepare-tracking-statements-with-drops-1-pooled
  (with-pooled-test-connection
    (with-non-binary-fixture
        (drop-prepared-statement "all")
        (without-binary
          (defprepared 'select1 "select a from test_data where c = $1" :single)
          ;; Defprepared does not change the prepared statements logged in the postmodern connection or
          ;; in the postgresql connection. That will happen when the prepared statement is funcalled.

          ;; the funcall creates the prepared statements logged in the postmodern connection
          ;; and the postgresql connection
          ;; drop the defprepared statement from postgresql, but not from postmodern has no
          (is (not (prepared-statement-exists-p "select1")))
          (is (not (list-postmodern-prepared-statements t)))
          ;; the following has no impact
          (drop-prepared-statement "select1" :location :postgresql)
          (is (equal 0 (length (list-postmodern-prepared-statements t))))
          (is (equal 0 (length (list-prepared-statements t))))
          ;; recreate the defprepared statement into postgresql
          (is (equal 1 (funcall 'select1 "foobar")))
          (is (prepared-statement-exists-p "select1"))
          ;; recreate the first prepared statement back into both postgresql and postmodern
          (is (member "select a from test_data where c = $1"
                      (list-postmodern-prepared-statements) :test 'equal :key 'cadr))
          (is (member "SELECT1" (list-postmodern-prepared-statements t) :test 'equal))
          (is (equal '("select a from test_data where c = $1" ("foobar"))
                     (find-postmodern-prepared-statement "select1")))
          (drop-prepared-statement "select1" :location :postgresql)
          (is (equal 0 (length (list-prepared-statements t))))
          (drop-prepared-statement "all")))))

(test prepare-select-no-table-two-parameters-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (is (equal (query (:select '$1 '$2) 1 "a")
                 '(("1" "a"))))
      (let ((select-two (prepare (:select (:type '$1 'integer) (:type '$2 'string)))))
        (is (equal (funcall select-two 1 "a")
                   '((1 "a")))))
      (let ((select-two (prepare (:select (:type '$1 integer) (:type '$2 string)))))
        (signals error (funcall select-two 1)))
      (let ((select-two (prepare (:select (:type '$1 integer) (:type '$2 string)))))
        (signals error (funcall select-two "a" 1)))
      (drop-prepared-statement "all"))))

(test defprepared-select-no-table-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'test8e (:select '$1))
      (is (equal (test8e 1)
                 '(("1"))))
      (is (equal (test8e 189)
                 '(("189"))))
      (is (equal (test8e 11.5)
                 '(("11.5"))))
      (drop-prepared-statement "all"))))

(test prepare-3-drop-no-table-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared select2 "select $1" :single)
      (is (equal (funcall 'select2 "foobar")
                 "foobar"))
      (is (equal (select2 "foobar")
                 "foobar"))
      (drop-prepared-statement 'select2 :location :postgresql)
      (is (equal (funcall 'select2 "foobar")
                 "foobar"))
      (is (equal (select2 "foobar")
                 "foobar"))
      (drop-prepared-statement 'select2 :location :postmodern)
      (signals error (funcall 'select2 "foobar")) ; function undefined
      (defprepared select2 "select $1" :single)
      (is (equal (funcall 'select2 "foobar")
                 "foobar"))
      (drop-prepared-statement 'select2)
      (signals error (funcall 'select2 "foobar"))
      (drop-prepared-statement "all"))))

(test prepare-3-drop-with-table-pooled
  (with-pooled-test-connection
    (without-binary
      (with-non-binary-fixture
          (defprepared 'select1 "select a from test_data where c = $1" :single)
          ;; Defprepared does not change the prepared statements logged in the
          ;; postmodern connection or
          ;; in the postgresql connection. That will happen when the prepared statement is funcalled.
          ;; drop the prepared select1 statement from both postgresql and postmodern
          (is (equal 1 (funcall 'select1 "foobar")))
        (is (equal 1 (select1 "foobar")))
        (drop-prepared-statement 'select1)
        (signals error (funcall 'select1))
        (is (not (prepared-statement-exists-p "select1")))
        (drop-prepared-statement "all")))))

(test prepare-3-overwrite-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (with-non-binary-fixture
      (defprepared select1 "select a from test_data where c = $1" :single)
      (is (equal 1 (funcall 'select1 "foobar")))
      ;; Testing overwrites. Now change the defprepared statement
      (defprepared select1 "select c from test_data where a = $1" :single)
      ;; Defprepared does not change the prepared statements logged in the postmodern connection or
      ;; in the postgresql connection. That happens at funcall.
      ;; Test still the original in both postgresql and postmodern
      (is (equal "select a from test_data where c = $1"
                 (find-postgresql-prepared-statement "select1")))
      (is (equal '("select a from test_data where c = $1" ("foobar"))
                 (find-postmodern-prepared-statement "select1")))
      ;; funcall now drops the old version and create the new version. The old parameter no longer works
      (is (equal (funcall 'select1 1)
                 "foobar"))
      (signals error (funcall 'select1 "foobar"))
      (drop-prepared-statement "all")))))

(test prepare-3-partial-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (with-non-binary-fixture
      (defprepared select1 "select a from test_data where c = $1" :single)
      (is (equal 1 (funcall 'select1 "foobar")))
      (defprepared select1 "select c from test_data where a = $1" :single)
      (is (equal (funcall 'select1 1)
                 "foobar"))
      (is (equal (select1 1)
                 "foobar"))
      (drop-prepared-statement "all")))))

(test prepare-4-pooled
  (with-pooled-test-connection
    (with-non-binary-fixture
    (without-binary
      (defprepared select1 "select c from test_data where a = $1" :single)
      (is (eq :null (funcall 'select1 2)))
      (drop-prepared-statement "all")
      (is (equal 0 (length (list-prepared-statements t))))
      (is (equal 0 (length (list-postmodern-prepared-statements t))))
      ;; recreate select1, then drop the connection and call select1
      (defprepared select1 "select c from test_data where a = $1" :single)
      (disconnect *database*)
      (is (eq :null (query "select c from test_data where a = 2" :single)))
      (is (eq :null (funcall 'select1 2)))
      (drop-prepared-statement "all")))))

(test prepare-5-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared select1 "select $1" :single)
      (is (equal (funcall 'select1 10)
                 "10"))
      ;; Test to ensure that we do not recreate the statement each time it is funcalled
      (let ((time1 (query "select prepare_time from pg_prepared_statements where name = 'select1'"
                          :single)))
        (sleep 1)
        (funcall 'select1 2)
        (is (equal time1
                   (query "select prepare_time from pg_prepared_statements where name = 'select1'"
                          :single))))
      (drop-prepared-statement "all"))))

(test prepare-change-param-no-table-txt-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-text "select $1" :single)
      (is (equal (funcall 'select-text "A")
                 "A"))
      (is (equal (funcall 'select-text "BCE")
                 "BCE"))
      (is (equal (funcall 'select-text 1)
                 "1"))
      (is (equal (funcall 'select-text "ABC")
                 "ABC"))
      (drop-prepared-statement "all"))))

(test prepare-change-param-no-table-float-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-float "select $1" :single)
      (is (equal (funcall 'select-float 1.5)
                 "1.5"))
      (is (equal (funcall 'select-float 2.5)
                 "2.5"))
      (is (equal (funcall 'select-float "abc")
                 "abc"))
      (is (equal (funcall 'select-float t)
                 "true"))
      (is (equal (funcall 'select-float "5")
                 "5"))
      (is (equal (funcall 'select-float "5.5")
                 "5.5"))
      (is (equal (funcall 'select-float 1.0)
                 "1.0"))
      (drop-prepared-statement "all"))))

(test prepare-change-param-no-table-bool-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-bool "select $1" :single)
      (is (equal (funcall 'select-bool t)
                 "true"))
      (is (equal (funcall 'select-bool nil)
                 "false"))
      (is (equal (funcall 'select-bool "14")
                 "14"))
      (is (equal (funcall 'select-bool 14.2)
                 "14.2"))
      (is (equal (funcall 'select-bool t)
                 "true"))
      (is (equal (select-bool t)
                 "true"))
      (drop-prepared-statement "all"))))

(test prepare-change-param-no-table-int-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-int "select $1" :single)
      (is (equal (funcall 'select-int 14)
                 "14"))
      (is (equal (select-int 6)
                 "6"))
      (is (equal (funcall 'select-int "14")
                 "14"))
      (is (equal (select-int "14")
                 "14"))
      (is (equal (funcall 'select-int 14.2)
                 "14.2"))
      (is (equal (select-int 14.2)
                 "14.2"))
      (is (equal (funcall 'select-int "abc")
                 "abc"))
      (is (equal (funcall 'select-int t)
                 "true"))
      (is (equal (funcall 'select-int 5)
                 "5"))
      (is (equal (select-int 6)
                 "6"))
      (drop-prepared-statement "all"))))

(test prepare-change-param-no-table-txt-with-disconnect-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-text "select $1" :single)
      (is (equal (funcall 'select-text "A")
                 "A"))
      (disconnect *database*)
      (is (equal (funcall 'select-text "BCE")
                 "BCE"))
      (is (equal (funcall 'select-text 1)
                 "1"))
      (is (equal (funcall 'select-text "ABC")
                 "ABC"))
      (drop-prepared-statement "all"))))

(test prepare-change-param-no-table-float-with-disconnect-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-float "select $1" :single)
      (is (equal (funcall 'select-float 1.5 )
                 "1.5"))
      (disconnect *database*)
      (is (equal (funcall 'select-float 2.5 )
                 "2.5"))
      (is (equal (funcall 'select-float "abc")
                 "abc"))
      (is (equal (funcall 'select-float t)
                 "true"))
      (is (equal (funcall 'select-float "5")
                 "5"))
      (is (equal (funcall 'select-float "5.5")
                 "5.5"))
      (is (equal (funcall 'select-float 1.0)
                 "1.0"))
      (is (equal (select-float 1.0)
                 "1.0"))
      (drop-prepared-statement "all"))))

(test prepare-change-param-no-table-bool-with-disconnect-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-bool "select $1" :single)
      (is (equal (funcall 'select-bool t)
                 "true"))
      (disconnect *database*)
      (is (equal (funcall 'select-bool nil)
                 "false"))
      (is (equal (funcall 'select-bool "14")
                 "14"))
      (is (equal (funcall 'select-bool 14.2)
                 "14.2"))
      (is (equal (funcall 'select-bool t)
                 "true"))
      (drop-prepared-statement "all"))))

(test prepare-change-param-no-table-int-with-disconnect-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (defprepared 'select-int "select $1" :single)
      (is (equal (funcall 'select-int 14)
                 "14"))
      (disconnect *database*)
      (is (equal (funcall 'select-int 6)
                 "6"))
      (is (equal (funcall 'select-int "14")
                 "14"))
      (is (equal (funcall 'select-int "abc")
                 "abc"))
      (is (equal (funcall 'select-int 14.2)
                 "14.2"))
      (is (equal (funcall 'select-int t)
                 "true"))
      (is (equal (funcall 'select-int 5)
                 "5"))
      (is (equal (select-int 5)
                 "5"))
      (drop-prepared-statement "all"))))

(test prepare-change-params-with-table-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (with-non-binary-fixture
      ;; Defprepared does not change the prepared statements logged in the postmodern connection or
      ;; in the postgresql connection. That will happen when the prepared statement is funcalled.
      (defprepared select-1 "select c from test_data where a = $1" :single)
      (is (eq :null (funcall 'select-1 2)))
      ;; recreate select1, then drop the connection and call select1
      (disconnect *database*)
      (is (eq :null (query "select c from test_data where a = 2" :single)))
      (signals error (funcall 'select-1 "2a"))
      (is (eq :null (funcall 'select-1 2)))
      (is (eq :null (select-1 2)))
      (drop-prepared-statement "all")))))

(test prepare-reserved-words-pooled
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (when (table-exists-p 'from-test) (execute (:drop-table 'from-test)))
      (execute "CREATE TABLE from_test (id SERIAL NOT NULL, flight INTEGER DEFAULT NULL, \"from\" VARCHAR(100) DEFAULT NULL, to_destination VARCHAR(100) DEFAULT NULL, PRIMARY KEY (id, \"from\"))")
      (execute (:insert-into 'from-test :set 'flight 1 'from "Stykkishólmur"
                             :to-destination "Reykjavík"))
      (execute (:insert-into 'from-test :set 'flight 2 'from "Reykjavík"
                             :to-destination "Seyðisfjörður"))
      (defprepared select1 "select \"from\" from from_test where to_destination = $1" :single)
      ;; the funcall creates the prepared statements logged in the postmodern connection
      ;; and the postgresql connection
      (is (equal "Reykjavík" (funcall 'select1 "Seyðisfjörður")))
      (execute (:drop-table 'from-test))
      (drop-prepared-statement "all"))))

(test prepared-statement-over-reconnect
  (let ((terminate-backend
          (prepare
              "SELECT pg_terminate_backend($1) WHERE pg_backend_pid() = $1"
              :rows))
        (getpid (prepare "SELECT pg_backend_pid()" :single)))
    (with-test-connection
      (is (equal (query "select pg_backend_pid()" :single)
                 (funcall getpid)))
      (is (equal (funcall getpid) (pomo:get-pid-from-postmodern)))
      (let ((pid (pomo:get-pid)))
        (pomo:terminate-backend pid)
        (signals database-connection-error
          (query "select pg_backend_pid()" :single)))
      (is (integerp (funcall getpid))))

    ;; Demonstrate that a prepared statement will reconnect
    ;; even if it is a termination
    (with-test-connection
      (is (equal (query "select pg_backend_pid()" :single)
                 (funcall getpid)))
      (is (equal (funcall getpid) (pomo:get-pid-from-postmodern)))
      (funcall getpid)
      (is-true (query "select pg_backend_pid()" :single)))

    ;; A regular query does not have the built-in exception handling
    ;; available to prepared statements, so this will trigger the
    ;; exception handling below, setting reconnected to true.
    (with-test-connection
      (let ((original-pid (funcall getpid))
            (reconnectedp nil))
        (block done
          (handler-bind
              ((database-connection-error
                 (lambda (condition)
                   (let ((restart (find-restart :reconnect condition)))
                     (is (not (null restart)))
                     (setq reconnectedp t)
                     (invoke-restart restart)))))
            (pomo:terminate-backend original-pid)
            (is-true (query "select pg_backend_pid()" :single))
            (is-true reconnectedp)
            (is (/= original-pid (funcall getpid)))))

        ;; Re-using the prepared statement on the new connection.
        (multiple-value-bind (rows count)
            (funcall terminate-backend 0)
          (is (null rows))
          (is (zerop count)))))

    ;; A funcall to a prepared statement reconnects on its own
    ;; without acdessing the database-connection-error handler
    ;; above, so reconnectedp will still be nil
    (with-test-connection
      (let ((original-pid (funcall getpid))
            (reconnectedp nil))
        (block done
          (handler-bind
              ((database-connection-error
                 (lambda (condition)
                   (let ((restart (find-restart :reconnect condition)))
                     (is (not (null restart)))
                     (setq reconnectedp t)
                     (invoke-restart restart)))))
            (pomo:terminate-backend original-pid)
            (is-true (funcall getpid))
            (is-false reconnectedp)
            (is (/= original-pid (funcall getpid)))))

        ;; Re-using the prepared statement on the new connection.
        (multiple-value-bind (rows count)
            (funcall terminate-backend 0)
          (is (null rows))
          (is (zerop count)))))))

(test prepared-statement-over-reconnect-pooled-1
  (with-pooled-test-connection
    (drop-prepared-statement "all")
    (without-binary
      (let ((terminate-backend
              (prepare
               "SELECT pg_terminate_backend($1) WHERE pg_backend_pid() = $1"
               :rows))
            (getpid (prepare "SELECT pg_backend_pid()" :single)))
        ;; Demonstrate that a prepared statement will reconnect
        ;; even if it is a termination

        (is (equal (query "select pg_backend_pid()" :single)
                   (funcall getpid)))
        (is (equal (funcall getpid) (pomo:get-pid-from-postmodern)))
        (let ((pid (pomo:get-pid)))
          (pomo:terminate-backend pid)
          (signals database-connection-error
            (query "select pg_backend_pid()" :single)))

        (funcall getpid)
        (sleep 1)
        (is (integerp (query "select pg_backend_pid()" :single)))
        (is (equal (funcall getpid) (pomo:get-pid-from-postmodern)))
        (funcall getpid)
        (is-true (query "select pg_backend_pid()" :single))

        ;; A regular query does not have the built-in exception handling
        ;; available to prepared statements, so this will trigger the
        ;; exception handling below, setting reconnected to true.
        (let ((original-pid (funcall getpid))
              (reconnectedp nil))
          (block done
            (handler-bind
                ((database-connection-error
                   (lambda (condition)
                     (let ((restart (find-restart :reconnect condition)))
                       (is (not (null restart)))
                       (setq reconnectedp t)
                       (invoke-restart restart)))))
              (pomo:terminate-backend original-pid)
              (is-true (query "select pg_backend_pid()" :single))
              (is-true reconnectedp)
              (is (/= original-pid (funcall getpid)))))

          ;; Re-using the prepared statement on the new connection.
          (multiple-value-bind (rows count)
              (funcall terminate-backend 0)
            (is (null rows))
            (is (zerop count))))

        ;; A funcall to a prepared statement reconnects on its own
        ;; without acdessing the database-connection-error handler
        ;; above, so reconnectedp will still be nil
        (let ((original-pid (funcall getpid))
              (reconnectedp nil))
          (block done
            (handler-bind
                ((database-connection-error
                   (lambda (condition)
                     (let ((restart (find-restart :reconnect condition)))
                       (is (not (null restart)))
                       (setq reconnectedp t)
                       (invoke-restart restart)))))
              (pomo:terminate-backend original-pid)
              (is-true (funcall getpid))
              (is-false reconnectedp)
              (is (/= original-pid (funcall getpid)))))

          ;; Re-using the prepared statement on the new connection.
          (multiple-value-bind (rows count)
              (funcall terminate-backend 0)
            (is (null rows))
            (is (zerop count))))))))

(test prepared-statement-over-reconnect-pooled
  (let ((terminate-my-backend
          (prepare
              "SELECT pg_terminate_backend($1) WHERE pg_backend_pid() = $1"
              :rows))
        (getpid (prepare "SELECT pg_backend_pid()" :single)))
    (with-pooled-test-connection
      (is (equal (query "select pg_backend_pid()" :single)
                 (funcall getpid)))
      (is (equal (funcall getpid) (pomo:get-pid-from-postmodern)))
      (let ((pid (pomo:get-pid)))
        (pomo:terminate-backend pid)
        (signals database-connection-error
          (query "select pg_backend_pid()" :single)))
      (is (integerp (funcall getpid))))

    ;; Demonstrate that a prepared statement will reconnect
    ;; even if it is a termination
    (with-pooled-test-connection
      (is (equal (query "select pg_backend_pid()" :single)
                 (funcall getpid)))
      (is (equal (funcall getpid) (pomo:get-pid-from-postmodern)))
      (funcall getpid)
      (is-true (query "select pg_backend_pid()" :single)))

    ;; A regular query does not have the built-in exception handling
    ;; available to prepared statements, so this will trigger the
    ;; exception handling below, setting reconnected to true.
    (with-pooled-test-connection
      (let ((original-pid (funcall getpid))
            (reconnectedp nil))
        (block done
          (handler-bind
              ((database-connection-error
                 (lambda (condition)
                   (let ((restart (find-restart :reconnect condition)))
                     (is (not (null restart)))
                     (setq reconnectedp t)
                     (invoke-restart restart)))))
            (pomo:terminate-backend original-pid)
            (is-true (query "select pg_backend_pid()" :single))
            (is-true reconnectedp)
            (is (/= original-pid (funcall getpid)))))

        ;; Re-using the prepared statement on the new connection.
        (multiple-value-bind (rows count)
            (funcall terminate-my-backend 0)
          (is (null rows))
          (is (zerop count)))))
    ;; A funcall to a prepared statement reconnects on its own
    ;; without acessing the database-connection-error handler
    ;; above, so reconnectedp will still be nil
    (with-pooled-test-connection
      (let ((original-pid (funcall getpid))
            (reconnectedp nil))
        (block done
          (handler-bind
              ((database-connection-error
                 (lambda (condition)
                   (let ((restart (find-restart :reconnect condition)))
                     (is (not (null restart)))
                     (setq reconnectedp t)
                     (invoke-restart restart)))))
            (pomo:terminate-backend original-pid)
            (is-true (funcall getpid))
            (is-false reconnectedp)
            (is (/= original-pid (funcall getpid)))))
        ;; Re-using the prepared statement on the new connection.
        (multiple-value-bind (rows count)
            (funcall terminate-my-backend 0)
          (is (null rows))
          (is (zerop count)))))
    (drop-prepared-statement "all")))
