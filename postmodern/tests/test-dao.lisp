(in-package :postmodern-tests)

(fiveam:def-suite :postmodern-daos
    :description "Dao suite for postmodern"
    :in :postmodern)

(fiveam:in-suite :postmodern-daos)

(defclass test-data ()
    ((id :col-type serial :initarg :id :accessor test-id)
     (a :col-type (or (varchar 100) db-null) :initarg :a :accessor test-a)
     (b :col-type boolean :col-default nil :initarg :b :accessor test-b)
     (c :col-type integer :col-default 0 :initarg :c :accessor test-c)
     (d :col-type numeric :col-default 0.0 :initarg :d :accessor test-d))
    (:metaclass dao-class)
    (:table-name dao-test)
    (:keys id))

(defclass test-data-short ()
  ((id :col-type serial :initarg :id :accessor test-id)
   (a :col-type (or (varchar 100) db-null) :initarg :a :accessor test-a))
  (:metaclass dao-class)
  (:table-name dao-test)
  (:keys id))

(defclass test-data-short-wrong-1 ()
  ((id :col-type serial :initarg :id :accessor test-id)
   (a :col-type (or numeric db-null) :initarg :a :accessor test-a))
  (:metaclass dao-class)
  (:table-name dao-test)
  (:keys id))

(defclass test-data-d-string ()
    ((id :col-type serial :initarg :id :accessor test-id)
     (a :col-type (or (varchar 100) db-null) :initarg :a :accessor test-a)
     (b :col-type boolean :col-default nil :initarg :b :accessor test-b)
     (c :col-type integer :col-default 0 :initarg :c :accessor test-c)
     (d :col-type text :col-default "" :initarg :d :accessor test-d))
    (:metaclass dao-class)
    (:table-name dao-test)
    (:keys id))

(test dao-class
  (with-test-connection
    (when (table-exists-p 'dao-test)
      (query (:drop-table :if-exists 'dao-test :cascade)))
    (is (not (table-exists-p 'dao-test)))
    (is (equal (dao-table-definition 'test-data)
               "CREATE TABLE dao_test (id SERIAL NOT NULL, a VARCHAR(100) DEFAULT NULL, b BOOLEAN NOT NULL DEFAULT false, c INTEGER NOT NULL DEFAULT 0, d NUMERIC NOT NULL DEFAULT 0.0, PRIMARY KEY (id))"))
    (is (equal (dao-keys (find-class 'test-data))
               '(ID)))
    (dao-keys (make-instance 'test-data :id 1))
    '(1)
    (execute (dao-table-definition 'test-data))
    (is (table-exists-p 'dao-test))
    (protect
      (is (member :dao-test (list-tables)))
      (is (null (select-dao 'test-data)))
      (let ((dao (make-instance 'test-data :a "quux")))
        (signals error (test-id dao))
        (insert-dao dao)
        (is (dao-exists-p dao))
        (let* ((id (test-id dao))
              (database-dao (get-dao 'test-data id)))
          (is (not (null database-dao)))
          (is (eql (test-id dao) (test-id database-dao)))
          (is (string= (test-a database-dao) "quux"))
          (setf (test-b dao) t)
          (update-dao dao)
          (let ((new-database-dao (get-dao 'test-data id)))
            (is (eq (test-b new-database-dao) t))
            (is (eq (test-b database-dao) nil))
            (delete-dao dao))))
      (is (not (select-dao 'test-data)))
      (execute (:drop-table 'dao-test :cascade)))))

(test save-dao
  (with-test-connection
    (when (table-exists-p 'dao-test)
      (query (:drop-table :if-exists 'dao-test :cascade)))
    (is (not (table-exists-p 'dao-test)))
    (execute (dao-table-definition 'test-data))
    (is (table-exists-p 'dao-test))
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
      (let ((short-dao (make-instance 'test-data-short :a "first short")))
        (save-dao short-dao)
        (is (equalp (query (:select '* :from 'dao-test) :alists)
                    '(((:ID . 1) (:A . "bar") (:B) (:C . 0) (:D . 0))
                      ((:ID . 2) (:A . "first short") (:B) (:C . 0) (:D . 0))))))
      (let ((dao-short-wrong (make-instance 'test-data-short-wrong-1 :a 12.75)))
        (save-dao dao-short-wrong)
        (is (equalp (query (:select '* :from 'dao-test) :alists)
                    '(((:ID . 1) (:A . "bar") (:B) (:C . 0) (:D . 0))
                      ((:ID . 2) (:A . "first short") (:B) (:C . 0) (:D . 0))
                      ((:ID . 3) (:A . "12.75") (:B) (:C . 0) (:D . 0))))))
      (let ((dao-d-string (make-instance 'test-data-d-string :a "D string" :b nil :c 14.37
                            :d 18.78)))
        (save-dao dao-d-string)
        (is (equalp (query (:select '* :from 'dao-test) :alists)
            '(((:ID . 1) (:A . "bar") (:B) (:C . 0) (:D . 0))
              ((:ID . 2) (:A . "first short") (:B) (:C . 0) (:D . 0))
              ((:ID . 3) (:A . "12.75") (:B) (:C . 0) (:D . 0))
              ((:ID . 4) (:A . "D string") (:B) (:C . 14) (:D . 939/50)))))
        (is (equal 939/50 (test-d (get-dao 'test-data-d-string
                                           (test-id dao-d-string)))))
        (is (equal (test-a (first (query (:select '* :from 'dao-test :where (:= 'id 1))
                                   (:dao test-data))))
                   "bar")))
      (is (equal (length (query (:select '* :from 'dao-test) (:dao test-data)))
                 4))
      (is (equal (type-of (first (with-test-connection
                                   (query (:select '* :from 'dao-test)
                                          (:dao test-data)))))
                 'TEST-DATA))
      (is (equal (type-of (first (with-test-connection
                                   (query (:select '* :from 'dao-test)
                                          (:dao test-data-d-string)))))
                 'TEST-DATA-D-STRING))
      (let ((dao (make-instance 'test-data-d-string :a "D string" :b nil :c 14
                            :d "Trying string")))
        (signals error (with-transaction () (save-dao dao))))
      (setf *ignore-unknown-columns* t)
      (is (equal (test-a (get-dao 'test-data-short 3))
                 "12.75"))
      (setf *ignore-unknown-columns* nil)
      (with-test-connection
        (execute (:drop-table 'dao-test :cascade))))))

(test query-drop-table-1
  (with-test-connection
    (unless (pomo:table-exists-p 'dao-test)
      (execute (dao-table-definition 'test-data)))
    (protect
      (is (member :dao-test (with-test-connection (pomo:list-tables))))
      (pomo:query (:drop-table :dao-test))
      (is (not (member :dao-test (with-test-connection (pomo:list-tables))))))))

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

(defclass test-col-name ()
  ((a :col-type string :col-name aa :initarg :a :accessor test-a)
   (b :col-type string :col-name bb :initarg :b :accessor test-b)
   (c :col-type string              :initarg :c :accessor test-c)
   (from :col-type string :col-name from :initarg :d :accessor test-d)
   (to-destination :col-type string :col-name to :initarg :e :accessor test-e))
  (:metaclass dao-class)
  (:keys a))

(test dao-class-col-name
  (with-test-connection
    (execute "CREATE TEMPORARY TABLE test_col_name (aa text primary key, bb text not null, c text not null,
              \"from\" text not null, \"to\" text not null)")
    (let ((o (make-instance 'test-col-name :a "1" :b "2" :c "3" :d "Reykjavík" :e "Garðabær")))
      (save-dao o)
      (let ((oo (get-dao 'test-col-name "1")))
        (is (string= "1" (test-a oo)))
        (is (string= "2" (test-b oo)))
        (is (string= "3" (test-c oo)))
        (is (string= "Reykjavík" (test-d oo)))
        (is (string= "Garðabær" (test-e oo)))))
    (let ((o (get-dao 'test-col-name "1")))
      (setf (test-b o) "b")
      (setf (test-d o) "Vestmannaeyjar")
      (update-dao o))
    (is (string= "1" (test-a (get-dao 'test-col-name "1"))))
    (is (string= "b" (test-b (get-dao 'test-col-name "1"))))
    (is (string= "3" (test-c (get-dao 'test-col-name "1"))))
    (is (string= "Vestmannaeyjar" (test-d (get-dao 'test-col-name "1")))))
  (with-test-connection
    (execute "CREATE TEMPORARY TABLE test_col_name (aa text primary key default md5(random()::text), bb text not null, c text not null,
              \"from\" text not null, \"to\" text not null)")
    (let ((o (make-instance 'test-col-name :b "2" :c "3" :d "Reykjavík" :e "Garðabær")))
      (fiveam:finishes
        (insert-dao o)))))

;;; For threading tests
(defvar *dao-update-lock* (bt:make-lock))


(defun make-class (name) (eval `(defclass ,(intern name) ()
                                  ((id :col-type serial :initarg :id :accessor test-id)
                                   (a :col-type (or (varchar 100) db-null) :initarg :a :accessor test-a)
                                   (b :col-type boolean :col-default nil :initarg :b :accessor test-b)
                                   (c :col-type integer :col-default 0 :initarg :c :accessor test-c))
                                  (:metaclass dao-class)
                                  (:table-name dao-test)
                                  (:keys id))))

(test make-class
  (let ((a (make-class (write-to-string (gensym)))))
    (is (not (equal nil (make-instance a :id 12 :a "six" :b t))))))


(test dao-class-threads
  (with-test-connection
    (unless (pomo:table-exists-p 'dao-test)
      (execute (dao-table-definition 'test-data)))
    (let ((item (make-instance 'test-data :a "test-name" :b t :c 0))
          (threads '()))
      (save-dao item)
      (loop for x from 1 to 5
            do (push (bt:make-thread
                      (lambda ()
                        (with-test-connection
                          (loop repeat 10
                                do (bt:with-lock-held (*dao-update-lock*)
                                     (incf (test-c item) 1))
                                   (save-dao item))
                          (loop repeat 10
                                do (bt:with-lock-held (*dao-update-lock*)
                                     (decf (test-c item) 1))
                                   (save-dao item)))))
                     threads))
      (mapc #'bt:join-thread threads)
      (is (eq 0 (test-c item))))
    (execute (:drop-table 'dao-test :cascade))))

(test reserved-column-names-defclass
  (with-test-connection
    (defclass from-test-data ()
      ((id :col-type serial :initarg :id :accessor id)
       (flight :col-type (or integer db-null) :initarg :flight :accessor flight)
       (from :col-type (or (varchar 100) db-null) :initarg :from :accessor from)
       (to-destination :col-type (or (varchar 100) db-null) :initarg :to-destination :accessor to-destination))
      (:metaclass dao-class)
      (:table-name from-test)
      (:keys id from))

    (loop for x in '(from-test from-test-data1 iceland-cities) do
         (when (pomo:table-exists-p x)
           (execute (:drop-table x :cascade))))
    (is (equal (dao-table-definition 'from-test-data)
               "CREATE TABLE from_test (id SERIAL NOT NULL, flight INTEGER DEFAULT NULL, \"from\" VARCHAR(100) DEFAULT NULL, to_destination VARCHAR(100) DEFAULT NULL, PRIMARY KEY (id, \"from\"))"))
    (execute (dao-table-definition 'from-test-data))
    (is (equal (pomo:list-columns 'from-test)
               '("id" "flight" "from" "to_destination")))
    (is (equal (pomo:find-primary-key-info 'public.from-test)
        '(("id" "integer") ("from" "character varying(100)"))))
    (let*  ((item1 (make-instance 'from-test-data :flight 1 :from "Reykjavík" :to-destination "Seyðisfjörður"))
            (item2 (make-instance 'from-test-data :flight 2 :from "Stykkishólmur" :to-destination "Reykjavík"))
            (item3 (make-instance 'from-test-data :flight 3 :from "Stykkishólmur" :to-destination "Reykjavík")))
      (is (equal "Reykjavík" (from item1)))
      (is (equal "Seyðisfjörður" (to-destination item1)))
      (insert-dao item1)
      (is (equal (query (:select 'from :from 'from-test :where (:= 'flight 1)) :single)
                 "Reykjavík"))
      (setf (to-destination item1) "Bolungarvík")
      (update-dao item1)
      (is (equal (query (:select 'to-destination :from 'from-test :where (:= 'flight 1)) :single)
                 "Bolungarvík"))
      (save-dao item2)
      (is (equal (query (:select 'to-destination :from 'from-test :where (:= 'flight 2)) :single)
                 "Reykjavík"))
      (setf (to-destination item1) "Stykkishólmur")
      (upsert-dao item2)
      (is (equal (query (:select 'from 'to-destination :from 'from-test :where (:= 'flight 1)))
                 '(("Reykjavík" "Bolungarvík"))))
      (upsert-dao item3)
      (is (equal (query (:select 'from 'to-destination :from 'from-test :where (:= 'flight 3)))
                 '(("Stykkishólmur" "Reykjavík"))))
      (is (equal (from (get-dao 'from-test-data 1 "Reykjavík")) "Reykjavík"))
      (pomo:delete-dao item3)
      (is (not (get-dao 'from-test-data 3 "Stykkishólmur")))
      (execute (:drop-table 'from-test :cascade)))))
