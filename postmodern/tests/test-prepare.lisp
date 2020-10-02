;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(fiveam:def-suite :postmodern-prepare
    :description "Prepared query suite for postmodern"
    :in :postmodern)

(fiveam:in-suite :postmodern-prepare)

(defun prepare-fixture ()
  (drop-prepared-statement "all")
  (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
    (execute (:create-table test-data ((a :type integer :primary-key t)
                                       (b :type real)
                                       (c :type (or text db-null)))))
    (execute (:insert-into 'test-data :set 'a 1 'b 5.4 'c "foobar"))
    (execute (:insert-into 'test-data :set 'a 2 'b 88 'c :null)))

(test prepare-tracking-statements-1
  (with-test-connection
    (prepare-fixture)
    (let ((select-int (prepare (:select (:type '$1 integer)) :single))
          (byte-arr (make-array 10 :element-type '(unsigned-byte 8) :initial-element 10))
          (select-bytes (prepare (:select (:type '$1 bytea)) :single))
          (select-int-internal-name nil))
      (defprepared 'select1 "select a from test_data where c = $1" :single)
      ;; Defprepared does not change the prepared statements logged in the postmodern connection or
      ;; in the postgresql connection. That will happen when the prepared statement is funcalled.
      (is (equal 0 (length (list-postmodern-prepared-statements t))))
      (is (equal 0 (length (list-prepared-statements t))))
      (is (= (funcall select-int 10) 10))
      (is (= (funcall select-int -40) -40))

      ;; CHANGE HERE TO SIGNALS ERROR

      (signals error (funcall select-int :null))
      (setf select-int-internal-name (car (list-prepared-statements t)))
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
      (is (equal 3 (length (list-prepared-statements t)))))))

(test prepare-tracking-statements-with-drops
  (with-test-connection
    (prepare-fixture)
    (let ((select-int (prepare (:select (:type '$1 integer)) :single))
          (byte-arr (make-array 10 :element-type '(unsigned-byte 8) :initial-element 10))
          (select-bytes (prepare (:select (:type '$1 bytea)) :single))
          (select-int-internal-name nil))
      (funcall select-bytes byte-arr)
      (funcall select-int 10)
      (defprepared 'select1 "select a from test_data where c = $1" :single)
      ;; Defprepared does not change the prepared statements logged in the postmodern connection or
      ;; in the postgresql connection. That will happen when the prepared statement is funcalled.
      (setf select-int-internal-name (car (list-prepared-statements t)))
      ;; the funcall creates the prepared statements logged in the postmodern connection
      ;; and the postgresql connection
      ;; drop the defprepared statement from postgresql, but not from postmodern`
      (drop-prepared-statement "select1" :location :postgresql)
      (is (equal 2 (length (list-postmodern-prepared-statements t))))
      (is (equal 2 (length (list-prepared-statements t))))
      ;; drop one of the prepared statements from both postgresql and postmodern
      (drop-prepared-statement select-int-internal-name)
      (is (not (prepared-statement-exists-p "select1")))
      (is (equal 1 (length (list-prepared-statements t))))
      (is (equal 1 (length (list-postmodern-prepared-statements t))))
      ;; recreate the defprepared statement into postgresql
      (is (equal 1 (funcall 'select1 "foobar")))
      (is (prepared-statement-exists-p "select1"))
      (is (equal 2 (length (list-prepared-statements t))))
      ;; recreate the first prepared statement back into both postgresql and postmodern
      (is (= (funcall select-int 10) 10))

      (is (equal 3 (length (list-postmodern-prepared-statements t))))
      (is (equal 3 (length (list-prepared-statements t))))

      (is (member "select a from test_data where c = $1"
                  (list-postmodern-prepared-statements) :test 'equal :key 'cadr))
      (is (member "SELECT1" (list-postmodern-prepared-statements t) :test 'equal))
      (is (equal "select a from test_data where c = $1"
                 (first (find-postmodern-prepared-statement "select1"))))
      (execute (:drop-table 'test-data)))))

(test prepare-select-no-table-two-parameters
  (with-test-connection
    (is (equal (query (:select '$1 '$2) 1 "a")
               '((1 "a"))))
    (let ((select-two (prepare (:select (:type '$1 'integer) (:type '$2 'string)))))
      (is (equal (funcall select-two 1 "a")
                 '((1 "a")))))
    (let ((select-two (prepare (:select (:type '$1 integer) (:type '$2 string)))))
      (signals error (funcall select-two 1)))
    (let ((select-two (prepare (:select (:type '$1 integer) (:type '$2 string)))))
      (signals error (funcall select-two "a" 1)))))

(test defprepared-select-no-table
  (with-test-connection
    (defprepared 'test8e (:select '$1))
    (is (equal (test8e 1)
               '((1))))
    (is (equal (test8e 189)
               '((189))))
    (signals error (test8e 11.5))))

(test prepare-3-drop-no-table
  (with-test-connection
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
    (signals error (funcall 'select2 "foobar"))
    (defprepared select2 "select $1" :single)
    (is (equal (funcall 'select2 "foobar")
               "foobar"))
    (drop-prepared-statement 'select2)
    (signals error (funcall 'select2 "foobar"))))

(test prepare-3-drop-with-table
  (with-test-connection
    (drop-prepared-statement "all")
    (when (table-exists-p 'test-data)
      (execute (:drop-table 'test-data)))
    (execute (:create-table test-data ((a :type integer :primary-key t)
                                       (b :type real)
                                       (c :type (or text db-null)))))
    (execute (:insert-into 'test-data :set 'a 1 'b 5.4 'c "foobar"))
    (execute (:insert-into 'test-data :set 'a 2 'b 88 'c :null))
    (defprepared 'select1 "select a from test_data where c = $1" :single)
    ;; Defprepared does not change the prepared statements logged in the postmodern connection or
    ;; in the postgresql connection. That will happen when the prepared statement is funcalled.
    ;; drop the prepared select1 statement from both postgresql and postmodern
    (is (equal 1 (funcall 'select1 "foobar")))
    (is (equal 1 (select1 "foobar")))
    (drop-prepared-statement 'select1)
    (signals error (funcall 'select1))
    (is (not (prepared-statement-exists-p "select1")))
    (execute (:drop-table 'test-data))))

(test prepare-3-overwrite
  (with-test-connection
    (drop-prepared-statement "all")
    (when (table-exists-p 'test-data)
      (execute (:drop-table 'test-data)))
    (execute (:create-table test-data ((a :type integer :primary-key t)
                                       (b :type real)
                                       (c :type (or text db-null)))))
    (execute (:insert-into 'test-data :set 'a 1 'b 5.4 'c "foobar"))
    (execute (:insert-into 'test-data :set 'a 2 'b 88 'c :null))
    (defprepared select1 "select a from test_data where c = $1" :single)
    (is (equal 1 (funcall 'select1 "foobar")))
    ;; Testing overwrites. Now change the defprepared statement
    (defprepared select1 "select c from test_data where a = $1" :single)
    ;; Defprepared does not change the prepared statements logged in the postmodern connection or
    ;; in the postgresql connection. That happens at funcall.
    ;; Test still the original in both postgresql and postmodern
    (is (equal "select a from test_data where c = $1"
               (find-postgresql-prepared-statement "select1")))
    (is (equal "select a from test_data where c = $1"
               (first (find-postmodern-prepared-statement "select1"))))
    ;; funcall now drops the old version and create the new version. The old parameter no longer works
    (is (equal (funcall 'select1 1)
               "foobar"))
    (signals error (funcall 'select1 "foobar"))))

(test prepare-3-partial
  (with-test-connection
    (drop-prepared-statement "all")
    (when (table-exists-p 'test-data)
      (execute (:drop-table 'test-data)))
    (execute (:create-table test-data ((a :type integer :primary-key t)
                                       (b :type real)
                                       (c :type (or text db-null)))))
    (execute (:insert-into 'test-data :set 'a 1 'b 5.4 'c "foobar"))
    (defprepared select1 "select a from test_data where c = $1" :single)
    (log:info "~%1. ~a~%~a~%" (list-prepared-statements)(list-postmodern-prepared-statements))
    (is (equal 1 (funcall 'select1 "foobar")))
    (log:info "~%2. ~a~%~a~%" (list-prepared-statements) (list-postmodern-prepared-statements))
    (defprepared select1 "select c from test_data where a = $1" :single)
    (log:info "~%3. ~a ~%~a~%~%" (list-prepared-statements) (list-postmodern-prepared-statements))
    (funcall 'select1 1)))

(test prepare-4
  (with-test-connection
    (prepare-fixture)
    (defprepared select1 "select c from test_data where a = $1" :single)
    (is (eq :null (funcall 'select1 2)))
    (drop-prepared-statement "all")
    (is (equal 0 (length (list-prepared-statements t))))
    (is (equal 0 (length (list-postmodern-prepared-statements t))))
    ;; recreate select1, then drop the connection and call select1
    (defprepared select1 "select c from test_data where a = $1" :single)
    (disconnect *database*)
    (signals error (query "select c from test_data where a = 2" :single))
    (is (eq :null (funcall 'select1 2)))
    (execute (:drop-table 'test-data))))

(test prepare-5
 (with-test-connection
   (drop-prepared-statement "all")
   (defprepared select1 "select $1" :single)
   (is (equal (funcall 'select1 10)
              10))
   ;; Test to ensure that we do not recreate the statement each time it is funcalled
   (let ((time1 (query "select prepare_time from pg_prepared_statements where name = 'select1'" :single)))
        (log:info "Sleep 1 to allow prepare_time comparison~%")
        (sleep 1)
        (funcall 'select1 2)
        (is (equal time1 (query "select prepare_time from pg_prepared_statements where name = 'select1'" :single))))))

(test prepare-change-param-no-table-txt
  (with-test-connection
    (drop-prepared-statement "all")
    (defprepared 'select-text "select $1" :single)
    (is (equal (funcall 'select-text "A")
               "A"))
    (is (equal (funcall 'select-text "BCE")
               "BCE"))
    (signals error (funcall 'select-text 1)) ; prepared statements cannot change parameter type
    (is (equal (funcall 'select-text "ABC")
               "ABC"))))

(test prepare-change-param-no-table-float
  (with-test-connection
    (drop-prepared-statement "all")
    (defprepared 'select-float "select $1" :single)
    (is (equal (funcall 'select-float 1.5 )
               1.5))
    (is (equal (funcall 'select-float 2.5 )
               2.5))
;    (signals error (funcall 'select-float 1)) ; prepared statements cannot change parameter type
    (signals error  (funcall 'select-float "abc")) ; prepared statements cannot change parameter type
    (signals error (funcall 'select-float t)) ; prepared statements cannot change parameter type
    (is (equal (funcall 'select-float "5")
               5.0)) ;postgresql knows what the param of the prepared statement should be, converts the text number to the required format
    (is (equal (funcall 'select-float "5.5")
               5.5)) ;postgresql knows what the param of the prepared statement should be, converts the text number to the required format
    (is (equal (funcall 'select-float 1.0)
               1.0))))

(test prepare-change-param-no-table-bool
  (with-test-connection
    (drop-prepared-statement "all")
    (defprepared 'select-bool "select $1" :single)
    (is (equal (funcall 'select-bool t)
               t))
    (is (equal (funcall 'select-bool nil)
               NIL))
    (signals error (funcall 'select-bool "14"))
    (signals error (funcall 'select-bool 14.2))
    (is (equal (funcall 'select-bool t)
               T))))

(test prepare-change-param-no-table-int
  (with-test-connection
    (drop-prepared-statement "all")
    (defprepared 'select-int "select $1" :single)
    (is (equal (funcall 'select-int 14)
               14))
    (is (equal (funcall 'select-int 6)
               6))
    (is (equal (funcall 'select-int "14")
               14))
    (is (equal (funcall 'select-int 14.2)
               14.2))
    (signals error (funcall 'select-int "abc"))
    (signals error (funcall 'select-int t))
    (is (equal (funcall 'select-int 5)
               5))))

(test prepare-change-param-no-table-txt-with-disconnect
  (with-test-connection
    (drop-prepared-statement "all")
    (defprepared 'select-text "select $1" :single)
    (is (equal (funcall 'select-text "A")
               "A"))
    (disconnect *database*)
    (is (equal (funcall 'select-text "BCE")
               "BCE"))
    (signals error (funcall 'select-text 1)) ; prepared statements cannot change parameter type
    (is (equal (funcall 'select-text "ABC")
               "ABC"))))

(test prepare-change-param-no-table-float-with-disconnect
  (with-test-connection
    (drop-prepared-statement "all")
    (defprepared 'select-float "select $1" :single)
    (is (equal (funcall 'select-float 1.5 )
               1.5))
    (disconnect *database*)
    (is (equal (funcall 'select-float 2.5 )
               2.5))
;    (signals error (funcall 'select-float 1)) ; prepared statements cannot change parameter type
    (signals error  (funcall 'select-float "abc")) ; prepared statements cannot change parameter type
    (signals error (funcall 'select-float t)) ; prepared statements cannot change parameter type
    (is (equal (funcall 'select-float "5")
               5.0)) ;postgresql knows what the param of the prepared statement should be, converts the text number to the required format
    (is (equal (funcall 'select-float "5.5")
               5.5)) ;postgresql knows what the param of the prepared statement should be, converts the text number to the required format
    (is (equal (funcall 'select-float 1.0)
               1.0))))

(test prepare-change-param-no-table-bool-with-disconnect
  (with-test-connection
    (drop-prepared-statement "all")
    (defprepared 'select-bool "select $1" :single)
    (is (equal (funcall 'select-bool t)
               t))
    (disconnect *database*)
    (is (equal (funcall 'select-bool nil)
               NIL))
    (signals error (funcall 'select-bool "14"))
    (signals error (funcall 'select-bool 14.2))
    (is (equal (funcall 'select-bool t)
               T))))

(test prepare-change-param-no-table-int-with-disconnect
  (with-test-connection
    (drop-prepared-statement "all")
    (defprepared 'select-int "select $1" :single)
    (is (equal (funcall 'select-int 14)
               14))
    (disconnect *database*)
    (is (equal (funcall 'select-int 6)
               6))
    (is (equal (funcall 'select-int "14")
               14))
    (signals error (funcall 'select-int "abc"))
;    (signals error (funcall 'select-int 14.2))
    (signals error (funcall 'select-int t))
    (is (equal (funcall 'select-int 5)
               5))))

(test prepare-change-params-with-table
  (with-test-connection
    (drop-prepared-statement "all")
    (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
    (execute (:create-table test-data ((a :type integer :primary-key t)
                                       (b :type real)
                                       (c :type (or text db-null)))))
    (execute (:insert-into 'test-data :set 'a 1 'b 5.4 'c "foobar"))
    (execute (:insert-into 'test-data :set 'a 2 'b 88 'c :null))
      ;; Defprepared does not change the prepared statements logged in the postmodern connection or
      ;; in the postgresql connection. That will happen when the prepared statement is funcalled.
    (defprepared select-1 "select c from test_data where a = $1" :single)
    (is (eq :null (funcall 'select-1 2)))
      ;; recreate select1, then drop the connection and call select1
    (disconnect *database*)
    (signals error (query "select c from test_data where a = 2" :single))
    (signals error (query "select c from test_data where a = 2" :single))
    (signals error (funcall 'select-1 "2a"))
    (is (eq :null (funcall 'select-1 2)))
    (execute (:drop-table 'test-data))))

(test prepare-reserved-words
  (with-test-connection
    (drop-prepared-statement "all")
    (when (table-exists-p 'from-test) (execute (:drop-table 'from-test)))
    (execute "CREATE TABLE from_test (id SERIAL NOT NULL, flight INTEGER DEFAULT NULL, \"from\" VARCHAR(100) DEFAULT NULL, to_destination VARCHAR(100) DEFAULT NULL, PRIMARY KEY (id, \"from\"))")
    (execute (:insert-into 'from-test :set 'flight 1 'from "Stykkishólmur" :to-destination "Reykjavík"))
    (execute (:insert-into 'from-test :set 'flight 2 'from "Reykjavík" :to-destination "Seyðisfjörður"))
    (defprepared select1 "select \"from\" from from_test where to_destination = $1" :single)
      ;; the funcall creates the prepared statements logged in the postmodern connection
      ;; and the postgresql connection
    (is (equal "Reykjavík" (funcall 'select1 "Seyðisfjörður")))
      (execute (:drop-table 'from-test))))

(test prepare-pooled
  (with-pooled-test-connection
        (drop-prepared-statement "all")
    (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
    (execute (:create-table test-data ((a :type integer :primary-key t)
                                       (b :type real)
                                       (c :type (or text db-null)))))
    (execute (:insert-into 'test-data :set 'a 1 'b 5.4 'c "foobar"))
    (execute (:insert-into 'test-data :set 'a 2 'b 88 'c :null))
    (let ((select-int (prepare (:select (:type '$1 integer)) :single))
          (byte-arr (make-array 10 :element-type '(unsigned-byte 8) :initial-element 10))
          (select-bytes (prepare (:select (:type '$1 bytea)) :single))
          (select-int-internal-name nil))
      (defprepared select1 "select a from test_data where c = $1" :single)
      ;; Defprepared does not change the prepared statements logged in the postmodern connection or
      ;; in the postgresql connection. That will happen when the prepared statement is funcalled.
      (is (equal 0 (length (list-postmodern-prepared-statements t))))
      (is (equal 0 (length (list-prepared-statements t))))
      (is (= (funcall select-int 10) 10))
      (is (= (funcall select-int -40) -40))
      (is (eq (funcall select-int :null) :null))
      (setf select-int-internal-name (car (list-prepared-statements t)))
      ;; the funcall creates the prepared statements logged in the postmodern connection
      ;; and the postgresql connection
;;; A
      (is (equal 1 (length (list-postmodern-prepared-statements t))))
      (is (equal 1 (length (list-prepared-statements t))))
      (is (equalp (funcall select-bytes byte-arr) byte-arr))
      (is (equal 2 (length (list-prepared-statements t))))
      (is (not (prepared-statement-exists-p "select1")))
      (is (equal 1 (funcall 'select1 "foobar")))
      (is (prepared-statement-exists-p "select1"))
      (is (equal 3 (length (list-postmodern-prepared-statements t))))
      (is (equal 3 (length (list-prepared-statements t))))
;;; A1
      ;; drop the defprepared statement from postgresql, but not from postmodern`
      (drop-prepared-statement "select1" :location :postgresql)
      (is (equal 3 (length (list-postmodern-prepared-statements t))))
      (is (equal 2 (length (list-prepared-statements t))))
      ;; drop one of the prepared statements from both postgresql and postmodern
      (drop-prepared-statement select-int-internal-name)
      (is (not (prepared-statement-exists-p "select1")))
      (is (equal 1 (length (list-prepared-statements t))))
      (is (equal 2 (length (list-postmodern-prepared-statements t))))
;;; A2
      (log:info "~%~%AAAA postmodern ~a~% postgresql ~a~%~%"
              (list-postmodern-prepared-statements)
              (list-prepared-statements))

      ;; recreate the defprepared statement into postgresql
      (is (equal 1 (funcall 'select1 "foobar")))
;;; A3

      (is (prepared-statement-exists-p "select1"))
      (is (equal 2 (length (list-prepared-statements t))))
      ;; recreate the first prepared statement back into both postgresql and postmodern
      (is (= (funcall select-int 10) 10))
      (is (equal 3 (length (list-postmodern-prepared-statements t))))
      (is (equal 3 (length (list-prepared-statements t))))
      (is (member '("SELECT1" . ("select a from test_data where c = $1" ("foobar")))
                  (list-postmodern-prepared-statements) :test 'equal))
      (log:info "~%A3   ~a~%~%" (list-postmodern-prepared-statements t))
      (is (member "SELECT1" (list-postmodern-prepared-statements t) :test 'equal))
      (is (equal '("select a from test_data where c = $1" ("foobar"))
                 (find-postmodern-prepared-statement "select1")))
;;; B
      ;; drop the prepared select1 statement from both postgresql and postmodern
      (drop-prepared-statement 'select1)
      (signals error (funcall 'select1))
      (is (not (prepared-statement-exists-p "select1")))
      ;; Testing overwrites. Now change the defprepared statement
      (defprepared select1 "select a from test_data where c = $1" :single)
      (is (equal 1 (funcall 'select1 "foobar")))
      (defprepared select1 "select c from test_data where a = $1" :single)
      ;; Defprepared does not change the prepared statements logged in the postmodern connection or
      ;; in the postgresql connection. That happens at funcall.
      ;; Test still the original in both postgresql and postmodern
      (is (equal  "select a from test_data where c = $1"
                  (find-postgresql-prepared-statement "select1")))
      (is (equal '("select a from test_data where c = $1" ("foobar"))
                 (find-postmodern-prepared-statement "select1")))
      ;; funcall now creates the new version
      (is (eq :null (funcall 'select1 2)))
;;; B1
      ;; Test to ensure that we do not recreate the statement each time it is funcalled
      (let ((time1
              (query "select prepare_time from pg_prepared_statements where name = 'select1'"
                     :single)))
        (log:info "Sleep 1 to allow prepare_time comparison~%")
        (sleep 1)
        (funcall 'select1 2)
        (is (equal time1
                   (query "select prepare_time from pg_prepared_statements where name = 'select1'"
                          :single))))
      (drop-prepared-statement "select1")
      (signals error (funcall 'select1 2))
      (defprepared select1 "select c from test_data where a = $1" :single)
      (is (eq :null (funcall 'select1 2)))
      (drop-prepared-statement "all")
      (is (equal 0 (length (list-prepared-statements t))))
      (is (equal 0 (length (list-postmodern-prepared-statements t))))
      ;; recreate select1, then drop the connection and call select1
      (defprepared select1 "select c from test_data where a = $1" :single)
      (disconnect *database*)
      (is (eq :null (funcall 'select1 2)))

      (execute (:drop-table 'test-data)))))

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
             (is (zerop count)))))))
