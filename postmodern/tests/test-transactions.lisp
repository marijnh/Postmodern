;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(def-suite :postmodern-transactions
  :description "Transaction testing suite for postmodern"
  :in :postmodern)

(in-suite :postmodern-transactions)

(defun transaction-test-table-fixture ()
  (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
  (execute (:create-table test-data ((value :type integer)))))

(defmacro with-transaction-test-table-fixture (&body body)
  `(progn
     (transaction-test-table-fixture)
     (unwind-protect (progn ,@body)
       (execute (:drop-table :if-exists 'test-data :cascade)))))

(test transaction
  (with-test-connection
    (with-transaction-test-table-fixture
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
      (is (= 1 (length (query (:select '* :from 'test-data))))))))

(test logical-transaction
  (with-test-connection
    (with-transaction-test-table-fixture
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
          (execute (:insert-into 'test-data :set 'value 29)))))))

(test transaction-commit-hooks
  (with-test-connection
    (with-transaction-test-table-fixture
        (with-logical-transaction (transaction-1)
          (execute (:insert-into 'test-data :set 'value 1))
          (with-logical-transaction (transaction-2)
            (push (lambda () (execute (:insert-into 'test-data :set 'value 3)))
                  (commit-hooks transaction-2))
            (push (lambda () (execute (:insert-into 'test-data :set 'value 4)))
                  (commit-hooks transaction-1))
            (execute (:insert-into 'test-data :set 'value 2))))
      (is (= 4 (length (query (:select '* :from 'test-data))))))))

(test transaction-abort-hooks
  (with-test-connection
    (with-transaction-test-table-fixture
        (with-logical-transaction (transaction-1)
          (execute (:insert-into 'test-data :set 'value 1))
          (ignore-errors
           (with-logical-transaction (transaction-2)
             (push (lambda () (execute (:insert-into 'test-data :set 'value 3)))
                   (abort-hooks transaction-2))
             (push (lambda () (execute (:insert-into 'test-data :set 'value 4)))
                   (abort-hooks transaction-1))
             (error "no wait")
             (execute (:insert-into 'test-data :set 'value 2)))))
      (is (= 2 (length (query (:select '* :from 'test-data))))))))

(test ensure-transaction
  (with-test-connection
    (with-transaction ()
      (ensure-transaction
        (is (eql postmodern::*transaction-level* 1))))
    (is (eql postmodern::*transaction-level* 0))
    (ensure-transaction
      (is (eql postmodern::*transaction-level* 1)))))

(test transaction-nested-warning
  "Transaction test nested warning"
  (with-test-connection
    (with-transaction-test-table-fixture
        (with-transaction (george :read-committed-rw)
          (signals cl-postgres:postgresql-warning
            (with-transaction (foo :read-committed-rw)
              (execute (:insert-into 'test-data :set 'value 27))))))))

(test transaction-logical-nested
  "Transaction test logical nested"
  (with-test-connection
    (with-transaction-test-table-fixture
        (with-logical-transaction (:read-committed-rw)
          (with-logical-transaction (:read-committed-rw)
            (execute (:insert-into 'test-data :set 'value 28))))
      (is (equal (query (:select '* :from 'test-data) :single)
                 28)))))

(test cursor
  (is (equal
       (with-test-connection
         (query (:create-table (:temp 'test-data1) ((value :type integer))))
         (loop for x from 1 to 100 do (execute (:insert-into 'test-data1 :set 'value x)))
         (with-transaction ()
           (execute "declare test_data1_values cursor with hold for select * from test_data1")
           (query "fetch forward 2 from test_data1_values")))
       '((1) (2))))
  (is (equal
       (with-test-connection
         (query (:create-table (:temp 'test-data1) ((value :type integer))))
         (loop for x from 1 to 100 do (execute (:insert-into 'test-data1 :set 'value x)))
         (with-transaction ()
           (execute "declare test_data1_values cursor with hold for select * from test_data1")
           (query "fetch forward 2 from test_data1_values"))
         (query "fetch next from test_data1_values"))
       '((3))))
  (is (equal
       (with-test-connection
         (query (:create-table (:temp 'test-data1) ((value :type integer))))
         (loop for x from 1 to 100 do (execute (:insert-into 'test-data1 :set 'value x)))
         (with-transaction ()
           (execute "declare test_data1_values cursor with hold for select * from test_data1")
           (query "fetch forward 2 from test_data1_values"))
         (query "fetch forward 5 from test_data1_values"))
       '((3) (4) (5) (6) (7)))))

(test create-savepoint-handle
  (is (typep (make-instance 'pomo::savepoint-handle :name 'sp1) 'pomo::savepoint-handle))
  (is (equal (pomo::savepoint-name (make-instance 'pomo::savepoint-handle :name 'sp1))
             'SP1)))


(test savepoint-rollback
  (let ((x 1))
    (with-test-connection
      (with-transaction-test-table-fixture
          (with-logical-transaction (:read-committed-rw)
            (execute (:insert-into 'test-data :set 'value 0))
            (with-savepoint sp1
              (execute (:insert-into 'test-data :set 'value 1))
              (is (equal (query "select * from test_data")
                         '((0) (1))))
              (when (< x 0)
                (rollback-savepoint sp1))
              (is (equal (query "select * from test_data")
                         '((0) (1)))))
            (with-savepoint sp2
              (execute (:insert-into 'test-data :set 'value 2))
              (is (equal (query "select * from test_data")
                         '((0) (1) (2))))
              (with-savepoint sp3
                (execute (:insert-into 'test-data :set 'value 3))
                (is (equal (query "select * from test_data")
                           '((0) (1) (2) (3))))
                (when (> x 0)
                  (rollback-savepoint sp3))
                (is (equal (query "select * from test_data")
                           '((0) (1) (2)))))
              (when (= x 0)
                (rollback-savepoint sp2))
              (is (equal (query "select * from test_data")
                         '((0) (1) (2))))))
        (is (equal (query "select * from test_data")
                   '((0) (1) (2))))))))

(test savepoint-rollback-from-inside
  (let ((x 1)
        (y t))
    (with-test-connection
      (with-transaction-test-table-fixture
          (with-logical-transaction (:read-committed-rw)
            (execute (:insert-into 'test-data :set 'value 0))
            (with-savepoint sp1
              (execute (:insert-into 'test-data :set 'value 1))
              (is (equal (query "select * from test_data")
                         '((0) (1))))
              (when (< x 0)
                (rollback-savepoint sp1))
              (is (equal (query "select * from test_data")
                         '((0) (1)))))
            (with-savepoint sp2
              (execute (:insert-into 'test-data :set 'value 2))
              (is (equal (query "select * from test_data")
                         '((0) (1) (2))))
              (with-savepoint sp3
                (execute (:insert-into 'test-data :set 'value 3))
                (is (equal (query "select * from test_data")
                           '((0) (1) (2) (3))))
                (when (> x 0)
                  (rollback-savepoint sp3))
                (is (equal (query "select * from test_data")
                           '((0) (1) (2))))
                (when y (rollback-savepoint sp2))
                (is (equal (query "select * from test_data")
                           '((0) (1)))))
              (when (= x 0)
                (rollback-savepoint sp2))
              (is (equal (query "select * from test_data")
                         '((0) (1))))))
        (is (equal (query "select * from test_data")
                   '((0) (1))))))))
