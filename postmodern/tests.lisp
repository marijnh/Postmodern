(defpackage :postmodern-tests
  (:use :common-lisp :fiveam :postmodern :simple-date))

(in-package :postmodern-tests)

(defvar *test-connection* '("test" "test" "" "localhost"))

;; Adjust the above to some db/user/pass/host combination that refers
;; to a valid postgresql database in which no table named test_data
;; currently exists. Then after loading the file, run the tests with
;; (fiveam:run! :postmodern)

(def-suite :postmodern)
(in-suite :postmodern)

(defmacro with-test-connection (&body body)
  `(with-connection *test-connection* ,@body))

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
    (destructuring-bind (a b c d e)
        (query (:select 22 (:type 44.5 double-precision) "abcde" t (:type 9/2 (numeric 5 2))) :row)
      (is (eql a 22))
      (is (eql b 44.5d0))
      (is (string= c "abcde"))
      (is (eql d t))
      (is (eql e 9/2)))))

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
    (is (table-exists-p 'test-data))
    (execute (:insert-into 'test-data :set 'a 1 'b 5.4 'c "foobar"))
    (execute (:insert-into 'test-data :set 'a 2 'b 88 'c :null))
    (is (equal (query (:order-by (:select '* :from 'test-data) 'a))
               '((1 5.4 "foobar")
                 (2 88.0 :null))))
    (execute (:drop-table 'test-data))
    (is (not (table-exists-p 'test-data)))))

(test sequence
  (with-test-connection
    (execute (:create-sequence 'my-seq :increment 4 :start 10))
    (is (sequence-exists-p 'my-seq))
    (is (= (sequence-next 'my-seq) 10))
    (is (= (sequence-next 'my-seq) 14))
    (execute (:drop-sequence 'my-seq))
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

(test transaction
  (with-test-connection
    (execute (:create-table test-data ((value :type integer))))
    (ignore-errors
      (with-transaction ()
        (execute (:insert-into 'test-data :set 'value 2))
        (error "no wait")))
    (is (length (query (:select '* :from 'test-data))) 0)
    (ignore-errors
      (with-transaction (transaction)
        (execute (:insert-into 'test-data :set 'value 2))
        (commit-transaction transaction)
        (error "no wait!!")))
    (is (length (query (:select '* :from 'test-data))) 1)
    (with-transaction (transaction)
      (execute (:insert-into 'test-data :set 'value 44))
      (abort-transaction transaction))
    (is (length (query (:select '* :from 'test-data))) 1)
    (execute (:drop-table 'test-data))))

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
    (execute (:drop-table 'dao-test))))
