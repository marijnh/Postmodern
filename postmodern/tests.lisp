(defpackage :postmodern-tests
  (:use :common-lisp :fiveam :postmodern :simple-date :cl-postgres-tests))

(in-package :postmodern-tests)

;; Adjust the above to some db/user/pass/host combination that refers
;; to a valid postgresql database in which no table named test_data
;; currently exists. Then after loading the file, run the tests with
;; (fiveam:run! :postmodern)

(def-suite :postmodern)
(in-suite :postmodern)

(defmacro with-test-connection (&body body)
  `(with-connection *test-connection* ,@body))

(defmacro protect (&body body)
  `(unwind-protect (progn ,@(butlast body)) ,(car (last body))))

(test connect-sanely
  (with-test-connection
    (is (not (null *database*)))))

(test connection-pool
  (let ((pooled (apply 'connect (append *test-connection* '(:pooled-p t)))))
    (disconnect pooled)
    (let ((pooled* (apply 'connect (append *test-connection* '(:pooled-p t)))))
      (is (eq pooled pooled*))
      (disconnect pooled*))
    (clear-connection-pool)
    (let ((pooled* (apply 'connect (append *test-connection* '(:pooled-p t)))))
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
  (with-test-connection
    (is (time= (query (:select (:type (encode-date 1980 2 1) date)) :single)
               (encode-date 1980 2 1)))
    (is (time= (query (:select (:type (encode-timestamp 2040 3 19 12 15 0 2) timestamp)) :single)
               (encode-timestamp 2040 3 19 12 15 0 2)))
    (is (time= (query (:select (:type (encode-interval :month -1 :hour 24) interval)) :single)
               (encode-interval :month -1 :hour 24)))))

(test table
  (with-test-connection
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

(test sequence
  (with-test-connection
    (execute (:create-sequence 'my-seq :increment 4 :start 10))
    (protect
      (is (sequence-exists-p 'my-seq))
      (is (= (sequence-next 'my-seq) 10))
      (is (= (sequence-next 'my-seq) 14))
      (execute (:drop-sequence 'my-seq)))
    (is (not (sequence-exists-p 'my-seq)))))

(test prepare
  (with-test-connection
    (let ((select-int (prepare (:select (:type '$1 integer)) :single))
          (byte-arr (make-array 10 :element-type '(unsigned-byte 8) :initial-element 10))
          (select-bytes (prepare (:select (:type '$1 bytea)) :single)))
      (is (= (funcall select-int 10) 10))
      (is (= (funcall select-int -40) -40))
      (is (eq (funcall select-int :null) :null))
      (is (equalp (funcall select-bytes byte-arr) byte-arr)))))

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
    (execute (:create-table test-data ((value :type integer))))
    (protect
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
      (with-transaction (transaction)
        (execute (:insert-into 'test-data :set 'value 44))
        (abort-transaction transaction))
      (is (= 1 (length (query (:select '* :from 'test-data)))))
      (execute (:drop-table 'test-data)))))

(test logical-transaction
  (with-test-connection
    (protect
      (execute (:create-table test-data ((value :type integer))))
      (with-logical-transaction ()
        (execute (:insert-into 'test-data :set 'value 1))
        (ignore-errors
          (with-logical-transaction ()
            (execute (:insert-into 'test-data :set 'value 2))
            (error "fail here"))))
      (is-true (query (:select '* :from 'test-data :where (:= 'value 1))))
      (is-false (query (:select '* :from 'test-data :where (:= 'value 2))))
      (execute (:drop-table 'test-data)))))

(test transaction-commit-hooks
  (with-test-connection
    (protect
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

(defclass test-data ()
  ((id :col-type serial :initarg :id :accessor test-id)
   (a :col-type (or (varchar 100) db-null) :initarg :a :accessor test-a)
   (b :col-type boolean :col-default nil :initarg :b :accessor test-b))
  (:metaclass dao-class)
  (:table-name dao-test)
  (:keys id))

(test dao-class
  (with-test-connection
    (execute (dao-table-definition 'test-data))
    (protect
      (is (member :dao-test (list-tables)))
      (is (null (get-dao 'test-data 1)))
      (let ((dao (make-instance 'test-data :a "quux")))
        (insert-dao dao)
        (is (eql (test-id dao) 1))
        (is (dao-exists-p dao)))
      (let ((dao (get-dao 'test-data 1)))
        (is (not (null dao)))
        (setf (test-b dao) t)
        (update-dao dao))
      (let ((dao (get-dao 'test-data 1)))
        (is (not (null dao)))
        (is (string= (test-a dao) "quux"))
        (is (eq (test-b dao) t))
        (delete-dao dao))
      (is (not (select-dao 'test-data)))
      (execute (:drop-table 'dao-test)))))

(test save-dao
  (with-test-connection
    (execute (dao-table-definition 'test-data))
    (protect
      (let ((dao (make-instance 'test-data :a "quux")))
        (is (save-dao dao))
        (setf (test-a dao) "bar")
        (is (not (save-dao dao)))
        (is (equal (test-a (get-dao 'test-data (test-id dao))) "bar"))
        (signals database-error
          (with-transaction () (save-dao dao)))
        (with-transaction ()
          (is (not (save-dao/transaction dao)))))
      (execute (:drop-table 'dao-test)))))

(defclass test-oid ()
  ((oid :col-type integer :ghost t :accessor test-oid)
   (a :col-type string :initarg :a :accessor test-a)
   (b :col-type string :initarg :b :accessor test-b))
  (:metaclass dao-class)
  (:keys a))

(test dao-class-oid
  (with-test-connection
    (execute (concatenate 'string (dao-table-definition 'test-oid) "with (oids=true)"))
    (protect
      (let ((dao (make-instance 'test-oid :a "a" :b "b")))
        (insert-dao dao)
        (is-true (integerp (test-oid dao)))
        (let ((back (get-dao 'test-oid "a")))
          (is (test-oid dao) (test-oid back))
          (setf (test-b back) "c")
          (update-dao back))
        (is (test-b (get-dao 'test-oid "a")) "c"))
      (execute (:drop-table 'test-oid)))))

(test notification
  (with-test-connection
    (execute (:listen 'foo))
    (with-test-connection
      (execute (:notify 'foo)))
    (is (cl-postgres:wait-for-notification *database*) "foo")))

(defclass test-col-name ()
  ((a :col-type string :col-name aa :initarg :a :accessor test-a)
   (b :col-type string :col-name bb :initarg :b :accessor test-b)
   (c :col-type string              :initarg :c :accessor test-c))
  (:metaclass dao-class)
  (:keys a))

(test dao-class-col-name
  (with-test-connection
    (execute "CREATE TEMPORARY TABLE test_col_name (aa text primary key,  bb text not null, c text not null)")
    (let ((o (make-instance 'test-col-name :a "1" :b "2" :c "3")))
      (save-dao o)
      (let ((oo (get-dao 'test-col-name "1")))
        (is (string= "1" (test-a oo)))
        (is (string= "2" (test-b oo)))
        (is (string= "3" (test-c oo)))))
    (let ((o (get-dao 'test-col-name "1")))
      (setf (test-b o) "b")
      (update-dao o))
    (is (string= "1" (test-a (get-dao 'test-col-name "1"))))
    (is (string= "b" (test-b (get-dao 'test-col-name "1"))))
    (is (string= "3" (test-c (get-dao 'test-col-name "1"))))))

;; create two tables with the same name in two different
;; namespaces.
(test namespace
  (with-test-connection
    (is (not (table-exists-p 'test-uniq)))
    (execute (:create-table test-uniq ((value :type integer))))
    (is (table-exists-p 'test-uniq))
    (is (not (schema-exist-p 'uniq)))
    (with-schema ('uniq :if-not-exist :create)
      (is (schema-exist-p 'uniq))
      (is (not (table-exists-p 'test-uniq)))
      (execute (:create-table test-uniq ((value :type integer))))
      (is (table-exists-p 'test-uniq))
      (execute (:drop-table 'test-uniq)))
    (is (schema-exist-p 'uniq))
    (drop-schema 'uniq)
    (is (not (schema-exist-p 'uniq)))
    (execute (:drop-table 'test-uniq))))

(test arrays
  (with-test-connection
    (execute (:create-table test-data ((a :type integer[]))))
    (protect
      (is (table-exists-p 'test-data))
      (execute (:insert-into 'test-data :set 'a (vector 3 4 5)))
      (execute (:insert-into 'test-data :set 'a #()))
      (execute (:drop-table 'test-data)))
    (is (not (table-exists-p 'test-data)))))
