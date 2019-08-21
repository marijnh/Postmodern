(defpackage :postmodern-tests
  (:use :common-lisp :fiveam :postmodern :simple-date :cl-postgres-tests)
  (:shadow #:with-test-connection))

(in-package :postmodern-tests)

;; Adjust the above to some db/user/pass/host combination that refers
;; to a valid postgresql database in which no table named test_data
;; currently exists. Then after loading the file, run the tests with
;; (fiveam:run! :postmodern)

(fiveam:def-suite :postmodern
    :description "Test suite for postmodern subdirectory files")

(fiveam:in-suite :postmodern)

(defmacro with-test-connection (&body body)
  `(with-connection (prompt-connection) ,@body))

(defmacro with-pooled-test-connection (&body body)
  `(with-connection (append (prompt-connection) '(:pooled-p t)) ,@body))

(defmacro protect (&body body)
  `(unwind-protect (progn ,@(butlast body)) ,(car (last body))))

(fiveam:def-suite :postmodern-base
    :description "Base test suite for postmodern"
    :in :postmodern)

(fiveam:in-suite :postmodern-base)

(test connect-sanely
  (with-test-connection
    (is (not (null *database*)))))

(test connection-pool
  (let* ((db-params (append (prompt-connection) '(:pooled-p t)))
         (pooled (apply 'connect db-params)))
    (disconnect pooled)
    (let ((pooled* (apply 'connect db-params)))
      (is (eq pooled pooled*))
      (disconnect pooled*))
    (clear-connection-pool)
    (let ((pooled* (apply 'connect db-params)))
      (is (not (eq pooled pooled*)))
      (disconnect pooled*))
    (clear-connection-pool)))

(test reconnect
  (with-test-connection
    (disconnect *database*)
    (is (not (connected-p *database*)))
    (reconnect *database*)
    (is (connected-p *database*))))

(test simple-query
  (with-test-connection
    (destructuring-bind (a b c d e f)
        (query (:select 22 (:type 44.5 double-precision) "abcde" t (:type 9/2 (numeric 5 2))
                        (:[] #("A" "B") 2)) :row)
      (is (eql a 22))
      (is (eql b 44.5d0))
      (is (string= c "abcde"))
      (is (eql d t))
      (is (eql e 9/2))
      (is (equal f "B")))))

(test reserved-words
  (with-test-connection
    (is (= (query (:select '* :from (:as (:select (:as 1 'as)) 'where) :where (:= 'where.as 1)) :single!) 1))))

(test time-types
  "Ensure that we are using a readtable that reads into simple-dates."
  (setf cl-postgres:*sql-readtable*
        (cl-postgres:copy-sql-readtable
         simple-date-cl-postgres-glue:*simple-date-sql-readtable*))
  (with-test-connection
    (is (time= (query (:select (:type (encode-date 1980 2 1) date)) :single)
               (encode-date 1980 2 1)))
    (is (time= (query (:select (:type (encode-timestamp 2040 3 19 12 15 0 2) timestamp)) :single)
               (encode-timestamp 2040 3 19 12 15 0 2)))
    (is (time= (query (:select (:type (encode-interval :month -1 :hour 24) interval)) :single)
               (encode-interval :month -1 :hour 24))))
  ;;; Reset readtable to default
  (setf cl-postgres:*sql-readtable*
        (cl-postgres:copy-sql-readtable
         cl-postgres::*default-sql-readtable*)))

(test table-skeleton
  (with-test-connection
    (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
    (execute (:create-table test-data ((a :type integer :primary-key t) (b :type real) (c :type (or text db-null))) (:unique c)))
    (protect
      (is (table-exists-p 'test-data))
      (execute (:insert-into 'test-data :set 'a 1 'b 5.4 'c "foobar"))
      (execute (:insert-into 'test-data :set 'a 2 'b 88 'c :null))
      (is (equal (query (:order-by (:select '* :from 'test-data) 'a))
                 '((1 5.4 "foobar")
                   (2 88.0 :null))))
      (execute (:drop-table 'test-data)))
    (is (not (table-exists-p 'test-data)))))

(test prepare
  (with-test-connection
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
      (defprepared 'select1 "select a from test_data where c = $1" :single)
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
      (is (equal 1 (length (list-postmodern-prepared-statements t))))
      (is (equal 1 (length (list-prepared-statements t))))
      (is (equalp (funcall select-bytes byte-arr) byte-arr))
      (is (equal 2 (length (list-prepared-statements t))))
      (is (not (prepared-statement-exists-p "select1")))
      (is (equal 1 (funcall 'select1 "foobar")))
      (is (prepared-statement-exists-p "select1"))
      (is (equal 3 (length (list-postmodern-prepared-statements t))))
      (is (equal 3 (length (list-prepared-statements t))))
      ;; drop the defprepared statement from postgresql, but not from postmodern`
      (drop-prepared-statement "select1" :location :postgresql)
      (is (equal 3 (length (list-postmodern-prepared-statements t))))
      (is (equal 2 (length (list-prepared-statements t))))
      ;; drop one of the prepared statements from both postgresql and postmodern
      (drop-prepared-statement select-int-internal-name)
      (is (not (prepared-statement-exists-p "select1")))
      (is (equal 1 (length (list-prepared-statements t))))
      (is (equal 2 (length (list-postmodern-prepared-statements t))))
      ;; recreate the defprepared statement into postgresql
      (is (equal 1 (funcall 'select1 "foobar")))
      (is (prepared-statement-exists-p "select1"))
      (is (equal 2 (length (list-prepared-statements t))))
      ;; recreate the first prepared statement back into both postgresql and postmodern
      (is (= (funcall select-int 10) 10))
      (is (equal 3 (length (list-postmodern-prepared-statements t))))
      (is (equal 3 (length (list-prepared-statements t))))
      (is (member '("SELECT1" . "select a from test_data where c = $1")
                  (list-postmodern-prepared-statements) :test 'equal))
      (is (member "SELECT1" (list-postmodern-prepared-statements t) :test 'equal))
      (is (equal "select a from test_data where c = $1" (find-postmodern-prepared-statement "select1")))
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
      (is (equal "select a from test_data where c = $1" (find-postgresql-prepared-statement "select1")))
      (is (equal "select a from test_data where c = $1" (find-postmodern-prepared-statement "select1")))
      ;; funcall now creates the new version
      (is (eq :null (funcall 'select1 2)))
      ;; Test to ensure that we do not recreate the statement each time it is funcalled
      (let ((time1 (query "select prepare_time from pg_prepared_statements where name = 'select1'" :single)))
        (format t "Sleep 1 to allow prepare_time comparison~%")
        (sleep 1)
        (funcall 'select1 2)
        (is (equal time1 (query "select prepare_time from pg_prepared_statements where name = 'select1'" :single))))
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
      (signals error (query "select c from test_data where a = 2" :single))
      (is (eq :null (funcall 'select1 2)))
      (execute (:drop-table 'test-data)))))

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
      (is (equal 1 (length (list-postmodern-prepared-statements t))))
      (is (equal 1 (length (list-prepared-statements t))))
      (is (equalp (funcall select-bytes byte-arr) byte-arr))
      (is (equal 2 (length (list-prepared-statements t))))
      (is (not (prepared-statement-exists-p "select1")))
      (is (equal 1 (funcall 'select1 "foobar")))
      (is (prepared-statement-exists-p "select1"))
      (is (equal 3 (length (list-postmodern-prepared-statements t))))
      (is (equal 3 (length (list-prepared-statements t))))
      ;; drop the defprepared statement from postgresql, but not from postmodern`
      (drop-prepared-statement "select1" :location :postgresql)
      (is (equal 3 (length (list-postmodern-prepared-statements t))))
      (is (equal 2 (length (list-prepared-statements t))))
      ;; drop one of the prepared statements from both postgresql and postmodern
      (drop-prepared-statement select-int-internal-name)
      (is (not (prepared-statement-exists-p "select1")))
      (is (equal 1 (length (list-prepared-statements t))))
      (is (equal 2 (length (list-postmodern-prepared-statements t))))
      ;; recreate the defprepared statement into postgresql
      (is (equal 1 (funcall 'select1 "foobar")))
      (is (prepared-statement-exists-p "select1"))
      (is (equal 2 (length (list-prepared-statements t))))
      ;; recreate the first prepared statement back into both postgresql and postmodern
      (is (= (funcall select-int 10) 10))
      (is (equal 3 (length (list-postmodern-prepared-statements t))))
      (is (equal 3 (length (list-prepared-statements t))))
      (is (member '("SELECT1" . "select a from test_data where c = $1")
                  (list-postmodern-prepared-statements) :test 'equal))
      (is (member "SELECT1" (list-postmodern-prepared-statements t) :test 'equal))
      (is (equal "select a from test_data where c = $1" (find-postmodern-prepared-statement "select1")))
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
      (is (equal "select a from test_data where c = $1" (find-postgresql-prepared-statement "select1")))
      (is (equal "select a from test_data where c = $1" (find-postmodern-prepared-statement "select1")))
      ;; funcall now creates the new version
      (is (eq :null (funcall 'select1 2)))
      ;; Test to ensure that we do not recreate the statement each time it is funcalled
      (let ((time1 (query "select prepare_time from pg_prepared_statements where name = 'select1'" :single)))
        (format t "Sleep 1 to allow prepare_time comparison~%")
        (sleep 1)
        (funcall 'select1 2)
        (is (equal time1 (query "select prepare_time from pg_prepared_statements where name = 'select1'" :single))))
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

(test doquery
  (with-test-connection
    (doquery (:select 55 "foobar") (number string)
      (is (= number 55))
      (is (string= string "foobar")))))

(test doquery-params
  (with-test-connection
    (doquery ("select $1::integer + 10" 20) (answer)
       (is (= answer 30)))))

(test transaction
  (with-test-connection
    (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
    (execute (:create-table test-data ((value :type integer))))
    (protect
      (with-transaction ()
          (execute (:insert-into 'test-data :set 'value 77)))
      (is (equal (query (:select '* :from 'test-data) :single)
                 77))
      (with-transaction (george)
        (execute (:insert-into 'test-data :set 'value 22)))
      (is (equal (query (:select '* :from 'test-data))
                 '((77) (22))))
      (with-transaction (george :read-committed-rw)
        (execute (:insert-into 'test-data :set 'value 33)))
      (is (equal (query (:select '* :from 'test-data))
                 '((77) (22) (33))))
      (with-transaction (:serializable)
        (execute (:insert-into 'test-data :set 'value 44)))
      (is (equal (query (:select '* :from 'test-data))
                 '((77) (22) (33) (44))))
      ;; Reset table -  Now try errors
      (query (:truncate 'test-data))
      (is (= 0 (length (query (:select '* :from 'test-data)))))
      (ignore-errors
          (with-transaction ()
            (execute (:insert-into 'test-data :set 'value 2))
            (error "no wait")))
      (is (= 0 (length (query (:select '* :from 'test-data)))))
      (ignore-errors
        (with-transaction (transaction)
          (execute (:insert-into 'test-data :set 'value 2))
          (commit-transaction transaction)
          (error "no wait!!")))
      (is (= 1 (length (query (:select '* :from 'test-data)))))
      (ignore-errors
        (with-transaction (george :read-committed-rw)
          (execute (:insert-into 'test-data :set 'value 2))
          (error "no wait")))
      (is (= 1 (length (query (:select '* :from 'test-data)))))
      (ignore-errors
        (with-transaction (:serializable)
          (execute (:insert-into 'test-data :set 'value 2))
          (error "no wait")))
      (is (= 1 (length (query (:select '* :from 'test-data)))))
      ;; Now try abort
      (with-transaction (transaction)
        (execute (:insert-into 'test-data :set 'value 44))
        (abort-transaction transaction))
      (is (= 1 (length (query (:select '* :from 'test-data)))))
      (ignore-errors
        (with-transaction (george :read-committed-rw)
          (execute (:insert-into 'test-data :set 'value 23))
          (abort-transaction george)))
      (is (= 1 (length (query (:select '* :from 'test-data)))))
      (execute (:drop-table 'test-data)))))

(test logical-transaction
  (with-test-connection
    (protect
      (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
      (execute (:create-table 'test-data ((value :type integer))))
      (with-logical-transaction ()
        (execute (:insert-into 'test-data :set 'value 1))
        (ignore-errors
          (with-logical-transaction ()
            (execute (:insert-into 'test-data :set 'value 2))
            (error "fail here"))))
      (is-true (query (:select '* :from 'test-data :where (:= 'value 1))))
      (is-false (query (:select '* :from 'test-data :where (:= 'value 2))))
      (with-logical-transaction ()
        (execute (:insert-into 'test-data :set 'value 77)))
      (is-true (query (:select '* :from 'test-data :where (:= 'value 77))))
      (with-logical-transaction ()
        (execute (:insert-into 'test-data :set 'value 22)))
      (is-true (query (:select '* :from 'test-data :where (:= 'value 22))))
      (with-logical-transaction (george :read-committed-rw)
        (declare (ignore george))
        (execute (:insert-into 'test-data :set 'value 33)))
      (is-true (query (:select '* :from 'test-data :where (:= 'value 33))))
      (with-logical-transaction (:serializable)
        (execute (:insert-into 'test-data :set 'value 44)))
      (is-true (query (:select '* :from 'test-data :where (:= 'value 44))))
      (signals database-error
        (with-logical-transaction (:read-committed-ro)
          (execute (:insert-into 'test-data :set 'value 29))))
      (execute (:drop-table 'test-data)))))

(test transaction-commit-hooks
  (with-test-connection
    (protect
      (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
      (execute (:create-table test-data ((value :type integer))))
      (with-logical-transaction (transaction-1)
        (execute (:insert-into 'test-data :set 'value 1))
        (with-logical-transaction (transaction-2)
          (push (lambda () (execute (:insert-into 'test-data :set 'value 3))) (commit-hooks transaction-2))
          (push (lambda () (execute (:insert-into 'test-data :set 'value 4))) (commit-hooks transaction-1))
          (execute (:insert-into 'test-data :set 'value 2))))
      (is (= 4 (length (query (:select '* :from 'test-data)))))
      (execute (:drop-table 'test-data)))))

(test transaction-abort-hooks
  (with-test-connection
    (protect
      (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
      (execute (:create-table test-data ((value :type integer))))
      (with-logical-transaction (transaction-1)
        (execute (:insert-into 'test-data :set 'value 1))
        (ignore-errors
          (with-logical-transaction (transaction-2)
            (push (lambda () (execute (:insert-into 'test-data :set 'value 3))) (abort-hooks transaction-2))
            (push (lambda () (execute (:insert-into 'test-data :set 'value 4))) (abort-hooks transaction-1))
            (error "no wait")
            (execute (:insert-into 'test-data :set 'value 2)))))
      (is (= 2 (length (query (:select '* :from 'test-data)))))
      (execute (:drop-table 'test-data)))))

(test ensure-transaction
  (with-test-connection
    (with-transaction ()
      (ensure-transaction
        (is (eql postmodern::*transaction-level* 1))))
    (is (eql postmodern::*transaction-level* 0))
    (ensure-transaction
      (is (eql postmodern::*transaction-level* 1)))))

(test transaction-nested-warning "Transaction test nested warning"
  (with-test-connection
    (if (pomo:table-exists-p 'test-data)
        (query (:truncate 'test-data))
        (execute (:create-table test-data ((value :type integer)))))
    (with-transaction (george :read-committed-rw)
      (signals cl-postgres:postgresql-warning
        (with-transaction (foo :read-committed-rw)
          (execute (:insert-into 'test-data :set 'value 27)))))
    (execute (:drop-table 'test-data))))

(test transaction-logical-nested "Transaction test logical nested"
  (with-test-connection
    (if (pomo:table-exists-p 'test-data)
        (query (:truncate 'test-data))
        (execute (:create-table test-data ((value :type integer)))))
    (with-logical-transaction (:read-committed-rw)
      (with-logical-transaction (:read-committed-rw)
        (execute (:insert-into 'test-data :set 'value 28))))
    (is (equal (query (:select '* :from 'test-data) :single)
               28))
    (execute (:drop-table 'test-data))))

(test cursor
  (is (equal (with-test-connection
                    (query (:create-table (:temp 'test-data1) ((value :type integer))))
                    (loop for x from 1 to 100 do (execute (:insert-into 'test-data1 :set 'value x)))
                    (with-transaction ()
                      (execute "declare test_data1_values cursor with hold for select * from test_data1")
                      (query "fetch forward 2 from test_data1_values")))
             '((1) (2))))
  (is (equal (with-test-connection
                    (query (:create-table (:temp 'test-data1) ((value :type integer))))
                    (loop for x from 1 to 100 do (execute (:insert-into 'test-data1 :set 'value x)))
                    (with-transaction ()
                      (execute "declare test_data1_values cursor with hold for select * from test_data1")
                      (query "fetch forward 2 from test_data1_values"))
                    (query "fetch next from test_data1_values"))
             '((3))))
  (is (equal (with-test-connection
                    (query (:create-table (:temp 'test-data1) ((value :type integer))))
                    (loop for x from 1 to 100 do (execute (:insert-into 'test-data1 :set 'value x)))
                    (with-transaction ()
                      (execute "declare test_data1_values cursor with hold for select * from test_data1")
                      (query "fetch forward 2 from test_data1_values"))
                    (query "fetch forward 5 from test_data1_values"))
             '((3) (4) (5) (6) (7)))))

(test notification
  (with-test-connection
    (execute (:listen 'foo))
    (with-test-connection
      (execute (:notify 'foo)))
    (is (cl-postgres:wait-for-notification *database*) "foo")))

(test split-fully-qualified-tablename
  (is (equal (split-fully-qualified-tablename 'uniq.george)
             '("george" "uniq" NIL)))
  (is (equal (split-fully-qualified-tablename "george-and-gracie")
             '("george_and_gracie" NIL NIL)))
  (is (equal (split-fully-qualified-tablename "test.uniq.george-and-gracie")
             '("george_and_gracie" "uniq" "test")))
  (is (equal (split-fully-qualified-tablename 'test.uniq.george-and-gracie)
             '("george_and_gracie" "uniq" "test"))))

;; create two tables with the same name in two different
;; namespaces.
(test namespace
  (with-test-connection
    (when (table-exists-p 'test-uniq)
      (execute (:drop-table 'test-uniq)))
    (when (schema-exists-p 'uniq)
      (drop-schema 'uniq :cascade 't))
    (is (schema-exists-p :public))
    (is (not (table-exists-p 'test-uniq)))
    (unless (table-exists-p 'test-uniq)
      (execute (:create-table test-uniq ((value :type integer)))))
    (is (table-exists-p 'test-uniq))
    (is (not (schema-exists-p 'uniq)))
    (with-schema ('uniq :if-not-exist :create) ;; changing the search path
      (is (schema-exists-p 'uniq))
      (is (schema-exists-p "uniq"))
      (is (not (table-exists-p 'test-uniq)))
      (execute (:create-table test-uniq ((value :type integer))))
      (is (table-exists-p 'test-uniq))
      (execute (:drop-table 'test-uniq)))
    (query (:create-table 'uniq.gracie ((id :type integer))))
    (is (equal (list-tables-in-schema "uniq")
        '("gracie")))
    (is (equal (list-tables-in-schema 'uniq)
        '("gracie")))
    (query (:create-table "uniq.george" ((id :type integer))))
    (is (equal (list-tables-in-schema "uniq")
        '("george" "gracie")))
    (is (table-exists-p "test.uniq.george"))
    (is (table-exists-p "uniq.george"))
    (is (table-exists-p "george" "uniq"))
    (is (table-exists-p 'test.uniq.george))
    (is (table-exists-p 'uniq.george))
    (is (table-exists-p 'george 'uniq))
    (is (schema-exists-p 'uniq))
    (drop-schema 'uniq :cascade 't)
    (is (not (schema-exists-p 'uniq)))
    (create-schema 'uniq)
    (is (not (set-difference (list-schemas)
                             '("public" "information_schema" "uniq")
                             :test #'equal)))
    (drop-schema "uniq" :cascade 't)
    (is (not (schema-exists-p "uniq")))
    (create-schema "uniq")
    (is (not (set-difference (list-schemas)
                             '("public" "information_schema" "uniq")
                             :test #'equal)))
    (drop-schema 'uniq :cascade 't)
    (is (equal (get-search-path)
               "\"$user\", public"))
    (execute (:drop-table 'test-uniq))))

(test arrays-skeleton
  (with-test-connection
    (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
    (execute (:create-table test-data ((a :type integer[]))))
    (protect
      (is (table-exists-p 'test-data))
      (execute (:insert-into 'test-data :set 'a (vector 3 4 5)))
      (execute (:insert-into 'test-data :set 'a #()))
      (execute (:drop-table 'test-data)))
    (is (not (table-exists-p 'test-data)))))

(test find-primary-key-info
  "Testing find-primary-key-info function. Given a table name, it returns
 a list of two strings. First the column name of the primary key of the table
and second the string name for the datatype."
  (with-test-connection
    (execute "create temporary table tasks_lists (id integer primary key)")
    (is (equal (postmodern:find-primary-key-info (s-sql:to-sql-name "tasks_lists"))
               '(("id" "integer"))))
    (is (equal (postmodern:find-primary-key-info (s-sql:to-sql-name "tasks_lists") t)
               '("id")))))

(test list-indices
  "Check the sql generated by the list-indices code"
  (is (equal (sql (postmodern::make-list-query "i"))
             "E'((SELECT relname FROM pg_catalog.pg_class INNER JOIN pg_catalog.pg_namespace ON (relnamespace = pg_namespace.oid) WHERE ((relkind = E''i'') and (nspname NOT IN (E''pg_catalog'', E''pg_toast'')) and pg_catalog.pg_table_is_visible(pg_class.oid))) ORDER BY relname)'")))

(test list-indexes-and-constraints
  "Test various index functions"
  (with-test-connection
    (when (table-exists-p 'test-uniq)
      (execute (:drop-table 'test-uniq)))
    (query (:create-table 'test-uniq ((id :type integer))
                          (:primary-key 'id)))
    (let ((idx-symbol (first (list-indices)))
          (idx-string (first (list-indices t))))
      (is (pomo:index-exists-p idx-symbol))
      (is (pomo:index-exists-p idx-string)))
    (is (equal (list-table-indices "test-uniq")
               '(:TEST-UNIQ-PKEY :ID)))
    (is (equal (list-table-indices 'test-uniq)
               '(:TEST-UNIQ-PKEY :ID)))
    (is (equal (list-table-indices "test-uniq" t)
               '("test_uniq_pkey" "id")))
    (is (equal (list-table-indices 'test-uniq t)
               '("test_uniq_pkey" "id")))
    (execute (:drop-table 'test-uniq))
    (query (:create-table 'test-uniq ((id :type integer))
                          (:primary-key "id")))
    (is (equal (list-table-indices "test-uniq")
               '(:TEST-UNIQ-PKEY :ID)))
    (is (equal (list-table-indices 'test-uniq)
               '(:TEST-UNIQ-PKEY :ID)))
    (is (equal (list-table-indices "test-uniq" t)
               '("test_uniq_pkey" "id")))
    (is (equal (list-table-indices 'test-uniq t)
               '("test_uniq_pkey" "id")))
    (is (equal (list-unique-or-primary-constraints "test-uniq")
               '(("test_uniq_pkey"))))
    (is (equal (list-unique-or-primary-constraints 'test-uniq)
               '(("test_uniq_pkey"))))
    (is (equal (length (list-all-constraints 'test-uniq))
               2))
    (is (equal (length (list-all-constraints "test-uniq"))
               2))
    (query (:alter-table 'test-uniq :drop-constraint 'test-uniq-pkey))
    (is (equal (length (list-all-constraints "test-uniq"))
               1))
    (execute (:drop-table 'test-uniq))))

(test drop-indexes
  "Test drop index variations"
  (with-test-connection
    (query (:drop-table :if-exists 'george :cascade))
    (query (:create-table 'george ((id :type integer))))
    (query (:create-index 'george-idx :on 'george :fields 'id))
    (is (equal (list-table-indices 'george)
               '(:GEORGE-IDX :ID)))
    (is (index-exists-p 'george-idx))
    (query (:drop-index :if-exists 'george-idx))
    (is (not (index-exists-p 'george-idx)))
    (is (equal (sql (:drop-index :if-exists 'george-idx :cascade))
               "DROP INDEX IF EXISTS george_idx CASCADE"))
    (is (equal (sql (:drop-index (:if-exists 'george-idx) :cascade))
               "DROP INDEX IF EXISTS george_idx CASCADE"))
    (is (equal (sql (:drop-index :concurrently (:if-exists 'george-idx) :cascade))
               "DROP INDEX CONCURRENTLY IF EXISTS george_idx CASCADE"))
    (is (equal (sql (:drop-index :concurrently (:if-exists 'george-idx)))
               "DROP INDEX CONCURRENTLY IF EXISTS george_idx"))
    (is (equal (sql (:drop-index :concurrently 'george-idx))
               "DROP INDEX CONCURRENTLY george_idx"))
    (is (equal (sql (:drop-index 'george-idx))
               "DROP INDEX george_idx"))))

(test sequence
  "Sequence testing"
  (with-test-connection
    (when (sequence-exists-p 'my-seq)
      (execute (:drop-sequence 'my-seq)))
    (execute (:create-sequence 'my-seq :increment 4 :start 10))
    (protect
      (is (sequence-exists-p 'my-seq))
      (is (= (sequence-next 'my-seq) 10))
      (is (= (sequence-next 'my-seq) 14))
      (execute (:drop-sequence 'my-seq)))
    (is (not (sequence-exists-p 'my-seq)))
    (when (sequence-exists-p :knobo-seq)
      (query (:drop-sequence :knobo-seq)))
    ;; Setup new sequence
    (is (eq
         (query (:create-sequence 'knobo-seq) :single)
         nil))
    ;; test sequence-exists-p with string
    (is (sequence-exists-p (first (list-sequences t))))
    ;; Test that we can set increment
    (is (equal (sql (:alter-sequence :knobo-seq :increment 1))
               "ALTER SEQUENCE knobo_seq INCREMENT BY 1"))
    (is (equal (sql (:alter-sequence 'knobo-seq :increment 1))
               "ALTER SEQUENCE knobo_seq INCREMENT BY 1"))
    (is (eq (query (:alter-sequence 'knobo-seq :increment 1))
            nil))
    ;; Test that currval is not yet set
    (is (equal (sql (:select (:currval 'knobo-seq)))
               "(SELECT currval(E'knobo_seq'))"))
    (is (equal (sql (:select (:currval :knobo-seq)))
               "(SELECT currval(E'knobo_seq'))"))
    (signals error (query (:select (:currval 'knobo-seq)) :single))
    ;; Test next value
    (is (equal (query (:select (:nextval 'knobo-seq)) :single)
               1))
    ;; Test currval
    (is (eq (query (:select (:currval 'knobo-seq)) :single) 1))
    ;; Test that we can set restart at 2
    ;; TODO Test that when we restart, we get 2.
    (is (equal (sql (:alter-sequence 'knobo-seq :start 2))
               "ALTER SEQUENCE knobo_seq START 2"))
    (is (eq (query (:alter-sequence 'knobo-seq :start 2))
            nil))
    ;; Testing that we can set max value
    (is (equal (sql (:alter-sequence 'knobo-seq :max-value 5))
               "ALTER SEQUENCE knobo_seq MAXVALUE 5"))
    (is (eq (query (:alter-sequence 'knobo-seq :max-value 5))
            nil))
    ;; TODO: check here that we don't can do next past max-value
    (is (equal (query (:select (:nextval 'knobo-seq)) :single) 2))
    (is (equal (sql (:alter-sequence 'knobo-seq :start 3))
               "ALTER SEQUENCE knobo_seq START 3"))
    (is (eq (query (:alter-sequence 'knobo-seq :start 3))
            nil))
    ;; Test set min value
    (is (equal (sql (:alter-sequence 'knobo-seq :min-value 2))
               "ALTER SEQUENCE knobo_seq MINVALUE 2"))
    (signals error (query (:alter-sequence 'knobo-seq :min-value 3)))
    (is (equal (query (:alter-sequence 'knobo-seq :min-value 2)) nil))
    ;; test remove max value
    (is (equal (sql (:alter-sequence 'knobo-seq :no-max))
               "ALTER SEQUENCE knobo_seq NO MAXVALUE"))
    (is (eq (query (:alter-sequence 'knobo-seq :no-max))
            nil))
    ;; test remove min value
    (is (equal (sql (:alter-sequence 'knobo-seq :no-min))
               "ALTER SEQUENCE knobo_seq NO MINVALUE"))
    (is (eq (query (:alter-sequence 'knobo-seq :no-min))
            nil))
    (is (eq (query (:alter-sequence 'knobo-seq :cycle))
            nil))
    (is (eq (query (:alter-sequence 'knobo-seq :no-cycle))
            nil))
    (is (eq (query (:alter-sequence 'knobo-seq :cache 1))
            nil))
    (unless (table-exists-p 'seq-test)
      (query (:create-table 'seq-test ((:id :type :int)))))
    (is (eq (query (:alter-sequence 'knobo-seq :owned-by :seq-test.id))
            nil))
    ;; cleanup
    (is (eq (sequence-exists-p 'knobo-seq)
            t))
    (query (:drop-sequence 'knobo-seq))
    (is (eq (sequence-exists-p 'knobo-seq)
            nil))
    (query (:drop-table 'seq-test))))
