;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
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
  (:metaclass postmodern:dao-class)
  (:table-name dao-test)
  (:keys id))

;; This class has fewer slots than the database table has fields
(defclass test-data-short ()
  ((id :col-type serial :initarg :id :accessor test-id)
   (a :col-type (or (varchar 100) db-null) :initarg :a :accessor test-a))
  (:metaclass postmodern:dao-class)
  (:table-name dao-test)
  (:keys id))

;; This class is short and has a slot that has a col-type different than the database table
;; slot a has a numeric col-type, but in the database table it is a string
;; Postmodern col-types do not control the actual parameter type.
;; THIS SHOULD BE REVISITED WHEN WE DO THE BINARY PARAMETERS
(defclass test-data-short-wrong-col-type ()
  ((id :col-type serial :initarg :id :accessor test-id)
   (a :col-type (or numeric db-null) :initarg :a :accessor test-a))
  (:metaclass postmodern:dao-class)
  (:table-name dao-test)
  (:keys id))

;; This class has the same number of slots, but one is the wrong type
;; Postmodern col-types do not control the actual parameter type.
;; THIS SHOULD BE REVISITED WHEN WE DO THE BINARY PARAMETERS
(defclass test-data-d-string ()
  ((id :col-type serial :initarg :id :accessor test-id)
   (a :col-type (or (varchar 100) db-null) :initarg :a :accessor test-a)
   (b :col-type boolean :col-default nil :initarg :b :accessor test-b)
   (c :col-type integer :col-default 0 :initarg :c :accessor test-c)
   (d :col-type text :col-default "" :initarg :d :accessor test-d))
  (:metaclass postmodern:dao-class)
  (:table-name dao-test)
  (:keys id))

(defclass test-data-multicolumn-key ()
  ((id :col-type serial :initarg :id :accessor test-id)
   (a :col-type (or (varchar 100) db-null) :initarg :a :accessor test-a)
   (b :col-type boolean :col-default nil :initarg :b :accessor test-b)
   (c :col-type integer :col-default 0 :initarg :c :accessor test-c)
   (d :col-type numeric :col-default 0.0 :initarg :d :accessor test-d))
  (:metaclass postmodern:dao-class)
  (:table-name dao-test-mk)
  (:keys id a))

(defclass test-data-not-serial-key ()
  ((id :col-type integer :initarg :id :accessor id)
   (username :col-type text :unique t  :initarg :username :accessor username)
   (department-id :col-type integer :initarg :department-id :accessor department-id))
  (:metaclass dao-class)
  (:table-name users1)
  (:keys username))

(defclass test-data-col-identity ()
  ((id :col-type integer :col-identity t :accessor id)
   (username :col-type text :unique t  :initarg :username :accessor username)
   (department-id :col-type integer :initarg :department-id :accessor department-id))
  (:metaclass dao-class)
  (:table-name users1)
  (:keys username))

(defclass test-data-col-primary-key ()
  ((id :col-type integer  :accessor id)
   (username :col-type text :col-primary-key t :unique t  :initarg :username :accessor username)
   (department-id :col-type integer :initarg :department-id :accessor department-id))
  (:metaclass dao-class)
  (:table-name users1))

(defclass test-data-col-identity-no-keys ()
  ((id :col-type integer :col-identity t :accessor id)
   (username :col-type text :unique t  :initarg :username :accessor username)
   (department-id :col-type integer :initarg :department-id :accessor department-id))
  (:metaclass dao-class)
  (:table-name users1))

(defclass test-data-col-identity-with-references ()
  ((id :col-type integer :col-identity t :accessor id)
   (username :col-type text :unique t :initarg :username :accessor username)
   (department-id :col-type integer :col-references ((departments id))
                  :initarg :department-id :accessor department-id))
  (:metaclass dao-class)
  (:table-name usersr))

(defclass test-data-department ()
  ((id :col-type integer :col-identity t :accessor id)
   (department-name :col-type text :unique t :initarg :department-name :accessor department-name))
  (:metaclass dao-class)
  (:table-name departments))

(defclass from-test-data ()
        ((id :col-type serial :initarg :id :accessor id)
         (flight :col-type (or integer db-null) :initarg :flight :accessor flight)
         (from :col-type (or (varchar 100) db-null) :initarg :from :accessor from)
         (to-destination :col-type (or (varchar 100) db-null)
                         :initarg :to-destination :accessor to-destination))
        (:metaclass dao-class)
        (:table-name from-test)
        (:keys id from))

(defun dao-test-table-fixture ()
  "Drops and recreates the dao-test table"
  (when (table-exists-p 'dao-test)
    (query (:drop-table :if-exists 'dao-test :cascade)))
  (execute (dao-table-definition 'test-data)))

(defun dao-test-table-fixture-mk ()
  "Drops and recreates the dao-test-mk table"
  (when (table-exists-p 'dao-test-mk)
    (query (:drop-table :if-exists 'dao-test-mk :cascade)))
  (execute (dao-table-definition 'test-data-multicolumn-key)))

(defun dao-test-table-fixture-references ()
  "Drops and recreates the usersr and departments tables"
  (when (table-exists-p 'usersr)
    (query (:drop-table :if-exists 'usersr :cascade)))
  (when (table-exists-p 'departments)
    (query (:drop-table :if-exists 'departments :cascade)))
  (execute (dao-table-definition 'test-data-department))
  (execute (dao-table-definition 'test-data-col-identity-with-references)))

(defun dao-test-table-fixture-not-serial-key ()
  "Drops and recreates the users1 table"
  (when (table-exists-p 'users1)
    (query (:drop-table :if-exists 'users1 :cascade)))
  (execute (dao-table-definition 'test-data-not-serial-key)))

(test dao-class
  (with-test-connection
    (dao-test-table-fixture)
    (is (equal (dao-table-definition 'test-data)
               "CREATE TABLE dao_test (id SERIAL NOT NULL, a VARCHAR(100) DEFAULT NULL, b BOOLEAN NOT NULL DEFAULT false, c INTEGER NOT NULL DEFAULT 0, d NUMERIC NOT NULL DEFAULT 0.0, PRIMARY KEY (id))"))
    (is (equal (dao-keys (find-class 'test-data))
               '(ID)))
    (is (equal (dao-keys (make-instance 'test-data :id 1))
               '(1)))
    (is (member :dao-test (list-tables)))
    (is (null (select-dao 'test-data)))
    (protect
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
           (delete-dao dao)))
       (is (not (select-dao 'test-data))))
     (execute (:drop-table 'dao-test :cascade)))))

(test dao-keys
  "Explicit keys takes priority over col-identity which takes priority over col-primary-key"
  (is (equal (dao-keys (find-class 'test-data))
             '(ID)))
  (is (equal (dao-keys (find-class 'test-data-col-identity))
             '(USERNAME)))
  (is (equal (dao-keys (find-class 'test-data-col-identity-no-keys))
             '(ID)))
  (is (equal (dao-keys (find-class 'test-data-col-primary-key))
             '(USERNAME))))

(test single-column-primary-keys
  (with-test-connection
    (protect
      (dao-test-table-fixture)
      (let ((dao (make-instance 'test-data :a "quux"))
            (dao-col-identity (make-instance 'test-data-not-serial-key :a "quux-ci")))
        (is (equal (find-primary-key-column 'test-data)
                   nil))
        (is (equal (find-primary-key-column dao-col-identity)
                   nil))
        (is (equal (find-primary-key-column dao)
                   nil))
        (is (equal (find-primary-key-column 'test-data-col-identity)
                   'id))
        (is (equal (find-primary-key-column
                    (class-of (make-instance 'test-data-col-identity)))
                   'id))
        (is (equal (pomo::dao-keys 'test-data)
                   '(id)))
        (is (equal (find-primary-key-info
                    (dao-table-name
                     (class-of (make-instance 'test-data :a "quux"))))
                   '(("id" "integer"))))
        (is (equal (dao-keys 'test-data)
                   '(id))))
      (execute (:drop-table 'dao-test :cascade)))))

(test multi-column-primary-keys
  (with-test-connection
    (protect
      (dao-test-table-fixture-mk)
      (is (equal (find-primary-key-column 'test-data-multicolumn-key)
                 nil))
      (is (equal (find-primary-key-info 'dao-test-mk)
                 '(("id" "integer") ("a" "character varying(100)"))))
      (is (equal (dao-keys 'test-data-multicolumn-key)
                 '(id a)))
      (execute (:drop-table 'dao-test-mk :cascade)))))

(test dao-column-slots
  (is (equal (mapcar #'pomo::slot-definition-name
                     (pomo::dao-column-slots
                      (class-of (make-instance 'test-data-col-identity
                                               :id 1
                                               :username "duck"
                                               :department-id 1))))
             '(ID USERNAME DEPARTMENT-ID)))
  (is (equal (mapcar #'pomo::slot-definition-name
                      (pomo::dao-column-slots (find-class 'test-data-col-identity)))
             '(ID USERNAME DEPARTMENT-ID))))

(test dao-column-fields
  (is (equal (pomo::dao-column-fields (find-class 'test-data-col-identity))
             '(ID USERNAME DEPARTMENT-ID)))
  (is (equal (pomo::dao-column-fields
              (class-of
               (make-instance 'test-data-col-identity
                              :id 1 :username "duck" :department-id 1)))
             '(ID USERNAME DEPARTMENT-ID))))

(test dao-table-name
  (is (equal (dao-table-name 'test-data-col-identity)
             'USERS1)))

(test dao-superclasses
  (is (equal (class-name
              (class-of (first (pomo::dao-superclasses (find-class 'test-data-col-identity)))))
               'dao-class)))

(test insert-dao-base
  (with-test-connection
  (dao-test-table-fixture)
    (protect
      (progn
        (insert-dao (make-instance 'test-data :a "unbound-stuff-here"))
        (is (equal (query "select * from dao_test")
                   '((1 "unbound-stuff-here" NIL 0 0))))
        ;; The following will trigger a duplicate key error
        (signals error (insert-dao (make-instance 'test-data :id 1 :a "unbound-stuff-here")))
        (is (equal (query "select * from dao_test")
                   '((1 "unbound-stuff-here" NIL 0 0))))
        (signals error (insert-dao (make-instance 'test-data :id 1 :a "bar" :b t :c 17 :d 13.2)))
        (is (equal (query "select * from dao_test")
                   '((1 "unbound-stuff-here" NIL 0 0))))
        (insert-dao (make-instance 'test-data :a "bar" :b t :c 17 :d 13.2))
        (is (equal (query "select * from dao_test")
                   '((1 "unbound-stuff-here" NIL 0 0) (2 "bar" T 17 66/5)))))
      (with-test-connection
         (execute (:drop-table 'dao-test :cascade))))))

(test save-dao-base
  (with-test-connection
    (dao-test-table-fixture)
    (protect
     (let ((dao (make-instance 'test-data :a "quux")))
       (is (save-dao dao)) ; returns a boolean to indicate a new row was inserted
       (setf (test-a dao) "bar")
       (is (equal (test-id dao)
                  1))
       (is (equal (test-d dao)
                  0))
       (is (not (save-dao dao))) ; returns boolean nil showing no new row was inserted
       (is (equal (test-a (get-dao 'test-data (test-id dao))) "bar")))
     (with-test-connection
         (execute (:drop-table 'dao-test :cascade))))))

(test save-dao-with-transaction
  (with-test-connection
    (dao-test-table-fixture)
    (protect
      (let ((dao (make-instance 'test-data :a "quux")))
        (with-transaction ()
          (save-dao dao))
        (is (equal (query "select * from dao_test")
                    '((1 "quux" NIL 0 0))))
       (setf (test-a dao) "bar")
       (signals database-error
                (with-transaction ()
                  (save-dao dao)))
       (with-transaction ()
         (is (equal (query "select * from dao_test")
                    '((1 "quux" NIL 0 0))))
         (is (not (save-dao/transaction dao)))
         (is (equal (query "select * from dao_test")
                    '((1 "bar" NIL 0 0))))))
     (with-test-connection
         (execute (:drop-table 'dao-test :cascade))))))

(test save-dao-with-same-key
  (with-test-connection
    (dao-test-table-fixture)
    (protect
     (let ((dao (make-instance 'test-data :a "quux")))
       (save-dao dao)
       (is (equal (test-id dao)
                  1))
       (is (equal (query "select * from dao_test")
                  '((1 "quux" NIL 0 0))))
       (setf (test-a dao) "bar")
       (save-dao dao)
       (is (equal (query "select * from dao_test")
                  '((1 "bar" NIL 0 0)))))
     (with-test-connection
         (execute (:drop-table 'dao-test :cascade))))))

(test save-dao-smaller-than-table
  (with-test-connection
    (dao-test-table-fixture)
    (protect
      (let ((short-dao (make-instance 'test-data-short :a "first short")))
       (save-dao short-dao)
       (is (equalp (query (:select '* :from 'dao-test) :alists)
                   '(((:ID . 1) (:A . "first short") (:B) (:C . 0) (:D . 0)))))
        (setf *ignore-unknown-columns* t)
        (is (equal (test-a (get-dao 'test-data-short 1))
                   "first short"))
        (setf *ignore-unknown-columns* nil))
      (with-test-connection
         (execute (:drop-table 'dao-test :cascade))))))

(test save-short-dao-with-bad-col-type
  (with-test-connection
    (dao-test-table-fixture)
    (protect
     (let ((dao-short-wrong (make-instance 'test-data-short-wrong-col-type :a 12.75)))
       (save-dao dao-short-wrong)
       (is (equalp (query (:select '* :from 'dao-test) :alists)
                   '(((:ID . 1) (:A . "12.75") (:B) (:C . 0) (:D . 0))))))
     (with-test-connection
         (execute (:drop-table 'dao-test :cascade))))))

(test save-dao-with-bad-col-type
  "Tests saving a dao when slot d, accessor test-d has a text col-type and the table is numeric."
  (with-test-connection
    (dao-test-table-fixture)
    (protect
      (progn
        (let ((dao-d-string (make-instance 'test-data-d-string :a "D string" :b nil :c 14
                                                               :d "abcd")))
          (signals error (save-dao dao-d-string)) ; invalid type
          (setf (test-d dao-d-string) "18.75")
          (save-dao dao-d-string)
          (is (equal (query (:select '* :from 'dao-test))
                     '((1 "D string" NIL 14 75/4))))
          (setf (test-d dao-d-string) 18.75)
          (save-dao dao-d-string)
          (is (equal (query (:select '* :from 'dao-test))
                     '((1 "D string" NIL 14 75/4))))))
      (with-test-connection
        (execute (:drop-table 'dao-test :cascade))))))

(test save-dao-with-col-identity
  "Note the difference between make-instance and make-dao"
  (with-test-connection
    (dao-test-table-fixture-references)
    (protect
      (let ((dao (make-dao 'test-data-department :department-name "Math")))
        (is (equal (query "select * from departments")
                   '((1 "Math"))))
        (setf (department-name dao) "German")
        (signals error (save-dao dao)) ; save-dao tries to insert and cannot insert over the id
        (update-dao dao)
        (is (equal (query "select * from departments")
                   '((1 "German"))))
        (insert-dao (make-instance 'test-data-department :department-name "Philosophy"))
        (is (equal (query "select * from departments")
                   '((1 "German") (2 "Philosophy"))))
        (make-dao 'test-data-department :department-name "Economics")
        (is (equal (query "select * from departments")
                   '((1 "German") (2 "Philosophy") (3 "Economics"))))
        (let ((dao (make-instance 'test-data-department :department-name "Geopolitics")))
                    (upsert-dao dao))
        (is (equal (query "select * from departments")
                   '((1 "German") (2 "Philosophy") (3 "Economics") (4 "Geopolitics"))))))))

(test returning-different-dao-types
  "Demonstrates that daos do not enforce slot types. The database will enforce the slot types
so there is a single source of type truth."
  (with-test-connection
    (dao-test-table-fixture)
    (protect
      (progn
        (let ((dao-d-string (make-instance 'test-data-d-string :a "D string" :b nil :c 14
                                                               :d "18.75")))
          (save-dao dao-d-string)
          (is (equal (query (:select '* :from 'dao-test))
                     '((1 "D string" NIL 14 75/4)))))
        (is (equal (type-of (query (:select '* :from 'dao-test)
                                   (:dao test-data :single)))
                   'TEST-DATA))
        (is (equal (type-of (query (:select '* :from 'dao-test)
                                   (:dao test-data-d-string :single)))
                   'TEST-DATA-D-STRING))
        (is (equal (test-d (query (:select '* :from 'dao-test)
                                  (:dao test-data-d-string :single)))
                   75/4)))
      (with-test-connection
        (execute (:drop-table 'dao-test :cascade))))))

(test update-dao
    (with-test-connection
    (dao-test-table-fixture)
    (protect
      (progn
        (save-dao (make-instance 'test-data :id 1 :a "bar"))
        ;; error signaled next line due to trying to update using a dao with unbounded slots
        (signals error (update-dao (make-instance 'test-data :id 1 :a "bar")))
        (update-dao (make-instance 'test-data :id 1 :a "bar" :b t :c 17 :d 13.2))
        (is (equal (query "select * from dao_test")
                   '((1 "bar" T 17 66/5)))))
      (with-test-connection
        (execute (:drop-table 'dao-test :cascade))))))

(test save-upsert-dao-with-serial
  (with-test-connection
    (dao-test-table-fixture)
    (protect
     (let ((dao (make-instance 'test-data :a "quux")))
       (save-dao dao)
       (is (equal (query "select * from dao_test")
                  '((1 "quux" NIL 0 0))))
       (is (equal (test-a (get-dao 'test-data (test-id dao))) "quux"))
       (setf (test-a dao) "bar")
       (upsert-dao dao)
       (is (equal (test-a (get-dao 'test-data (test-id dao))) "bar"))
       (is (equal (query "select * from dao_test")
                  '((1 "bar" NIL 0 0))))
       (signals error (update-dao (make-instance 'test-data :id 1 :a "cover?"))) ; unbound :b
       (signals error (upsert-dao (make-instance 'test-data :id 1 :a "cover?"))) ; duplicate key
       (upsert-dao (make-instance 'test-data :a "cover?"))
       (is (equal (query "select * from dao_test")
                  '((1 "bar" NIL 0 0) (2 "cover?" NIL 0 0))))))))

(test save-upsert-dao-not-serial
  (with-test-connection
    (dao-test-table-fixture-not-serial-key)
    (protect
     (let ((dao (make-instance 'test-data-not-serial-key :id 1 :username "duck")))
       (signals error (save-dao dao)) ; unbound department-id
       (setf (department-id dao) 12)
       (save-dao dao)
       (is (equal (query "select * from users1")
                  '((1 "duck" 12))))
       (is (equal (username (get-dao 'test-data-not-serial-key (username dao)))
                  "duck"))
       (setf (department-id dao) 13)
       (upsert-dao dao)
       (is (equal (query "select * from users1")
                  '((1 "duck" 13))))
       (setf (username dao) "goose")
       (setf (department-id dao) 17)
       (upsert-dao dao)
       (is (equal (query "select * from users1")
            '((1 "duck" 13) (1 "goose" 17))))
       (is (equal (department-id (get-dao 'test-data-not-serial-key (username dao)))
                  17))
       (signals error (update-dao (make-instance 'test-data-not-serial-key
                                   :id 1 :username "turkey" :department-id 43)))
       ;; update row does not exist
       (is (equal (query "select * from users1")
               '((1 "duck" 13) (1 "goose" 17))))
       (upsert-dao (make-instance 'test-data-not-serial-key
                                  :id 1 :username "chicken" :department-id 3))
       (is (equal (query "select * from users1")
                  '((1 "duck" 13) (1 "goose" 17) (1 "chicken" 3))))
       (upsert-dao (make-instance 'test-data-not-serial-key
                                  :id 1 :username "duck" :department-id 3))
       (is (equal (query "select * from users1")
                  '((1 "goose" 17) (1 "chicken" 3) (1 "duck" 3))))
       (update-dao (make-instance 'test-data-not-serial-key
                                  :id 1 :username "chicken" :department-id 3))
       (is (equal (query "select * from users1")
               '((1 "goose" 17) (1 "duck" 3) (1 "chicken" 3))))
       (upsert-dao (make-instance 'test-data-not-serial-key
                                  :id 1 :username "penguin" :department-id 43))
       (is (equal (query "select * from users1")
               '((1 "goose" 17) (1 "duck" 3) (1 "chicken" 3) (1 "penguin" 43))))
       (signals error (update-dao (make-instance 'test-data-not-serial-key
                                                 :id 1 :username "turkey" :department-id 43)))
       ;; still no turkey to update
       (is (equal (query "select * from users1")
               '((1 "goose" 17) (1 "duck" 3) (1 "chicken" 3) (1 "penguin" 43))))))))

(test dao-create-table-with-references
  (is (equal (dao-table-definition 'test-data-col-identity-with-references)
             "CREATE TABLE usersr (id INTEGER NOT NULL PRIMARY KEY generated always as identity, username TEXT NOT NULL, department_id INTEGER NOT NULL REFERENCES departments(id) MATCH SIMPLE ON DELETE RESTRICT ON UPDATE RESTRICT)"))
  (with-test-connection
    (dao-test-table-fixture-references)
    (protect
      (progn
        (signals error (insert-dao (make-instance 'test-data--col-identity-with-references
                                                  :username "user-1" :department-id 1)))
        (format t "Departments ~a~%" (query "select * from departments"))
        (insert-dao (make-instance 'test-data-department :department-name "department 1"
                                                         :department-id 1))
        (format t "Departments ~a~%" (query "select * from departments"))
        (insert-dao (make-instance 'test-data-col-identity-with-references
                                   :username "user-1" :department-id 1))
        (is (equal (query "select * from usersr")
                   '((1 "user-1" 1))))
        (is (equal (username (get-dao 'test-data-col-identity-with-references 1))
                   "user-1")))
      (progn
        (query (:drop-table :if-exists 'usersr :cascade))
        (query (:drop-table :if-exists 'departments :cascade))))))

(test query-drop-table-1
  (with-test-connection
  (dao-test-table-fixture)
    (protect
     (is (member :dao-test (with-test-connection (pomo:list-tables))))
     (pomo:query (:drop-table :dao-test))
     (is (not (member :dao-test (with-test-connection (pomo:list-tables))))))))

(defclass test-col-name ()
  ((a :col-type string :col-name aa :initarg :a :accessor test-a)
   (b :col-type string :col-name bb :initarg :b :accessor test-b)
   (c :col-type string              :initarg :c :accessor test-c)
   (from :col-type string :col-name from :initarg :d :accessor test-d)
   (to-destination :col-type string :col-name to :initarg :e :accessor test-e))
  (:metaclass dao-class)
  (:keys a))

(test dao-class-col-name
  "Test the use of col-name in daos"
  (with-test-connection
    (execute "CREATE TEMPORARY TABLE test_col_name (aa text primary key, bb text not null, c text not null,
              \"from\" text not null, \"to\" text not null)")
    (let ((o (make-instance 'test-col-name :a "1" :b "2" :c "3" :d "Reykjavík" :e "Garðabær")))
      (save-dao o)
      (is (equal (query "select * from test_col_name" :alists)
                 '(((:AA . "1") (:BB . "2") (:C . "3") (:FROM . "Reykjavík") (:TO . "Garðabær")))))
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

(defun make-class (name)
  (eval `(defclass ,(intern name) ()
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
                         (loop repeat 5
                            do (bt:with-lock-held (*dao-update-lock*)
                                 (incf (test-c item) 1)
                                 (upsert-dao item))))
                     (with-test-connection
                         (loop repeat 5
                            do (bt:with-lock-held (*dao-update-lock*)
                                 (decf (test-c item) 1)
                                 (upsert-dao item))))))
                  threads))
      (mapc #'bt:join-thread threads)
      (is (eq 0 (test-c item))))
    (execute (:drop-table 'dao-test :cascade))))

(test reserved-column-names-defclass
  (with-test-connection
    (when (pomo:table-exists-p "from-test")
      (execute (:drop-table "from-test" :cascade)))
    (when (pomo:table-exists-p 'from-test-data1)
      (execute (:drop-table 'from-test-data1 :cascade)))
    (when (pomo:table-exists-p 'iceland-cities)
      (execute (:drop-table 'iceland-cities :cascade)))
    (is (equal (dao-table-definition 'from-test-data)
               "CREATE TABLE from_test (id SERIAL NOT NULL, flight INTEGER DEFAULT NULL, \"from\" VARCHAR(100) DEFAULT NULL, to_destination VARCHAR(100) DEFAULT NULL, PRIMARY KEY (id, \"from\"))"))
    (execute (dao-table-definition 'from-test-data))
    (is (equal (pomo:list-columns 'from-test)
               '("id" "flight" "from" "to_destination")))
    (is (equal (pomo:find-primary-key-info 'public.from-test)
               '(("id" "integer") ("from" "character varying(100)"))))
    (let*  ((item1
              (make-instance 'from-test-data
                             :flight 1 :from "Reykjavík" :to-destination "Seyðisfjörður"))
            (item2
              (make-instance 'from-test-data
                             :flight 2 :from "Stykkishólmur" :to-destination "Reykjavík"))
            (item3
              (make-instance 'from-test-data
                             :flight 3 :from "Stykkishólmur" :to-destination "Reykjavík")))
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
      (delete-dao item3)
      (is (not (get-dao 'from-test-data 3 "Stykkishólmur")))
      (execute (:drop-table 'from-test :cascade)))))

(test fetch-defaults
  (with-test-connection
    (dao-test-table-fixture)
    (protect
      (let ((dao (make-instance 'test-data :a "something"))
            (short-dao (make-instance 'test-data-short)))
        (signals error (test-b  dao))    ; unbound slot b
        (pomo:fetch-defaults dao)
        (is (equal (test-b dao)
                   nil))
        (is (equal (test-c dao)
                  0))
        (signals error (test-id short-dao))
        (pomo:fetch-defaults short-dao)
        (signals error (test-id short-dao)))
      (execute (:drop-table 'dao-test :cascade)))))

(test generate-dao-query
  (is (equal (pomo::generate-dao-query 'test-data)
             '(:SELECT '* :FROM (DAO-TABLE-NAME (FIND-CLASS TEST-DATA)) :WHERE T))))

(test select-dao
  (with-test-connection
    (dao-test-table-fixture)
    (protect
      (progn
        (make-dao 'test-data :a "dao1")
        (make-dao 'test-data :a "dao2")
        (is (equal (query "select * from dao_test")
                   '((1 "dao1" NIL 0 0) (2 "dao2" NIL 0 0))))
        (is (equal (mapcar #'test-a (select-dao 'test-data))
                   '("dao1" "dao2"))))
      (execute (:drop-table 'dao-test :cascade)))))

(test do-select-dao
  (with-test-connection
    (dao-test-table-fixture)
    (protect
      (progn
        (make-dao 'test-data :a "dao1")
        (make-dao 'test-data :a "dao2")
        (is (equal (query "select * from dao_test")
                   '((1 "dao1" NIL 0 0) (2 "dao2" NIL 0 0))))
        (do-select-dao (('test-data dao))
          (progn
            (setf (test-c dao) 2)
            (signals error (update-dao dao))
            (with-test-connection
              ;; without a new connection, this errors because the previous connection is not done
              (update-dao dao))))
        (is (equal (query "select * from dao_test")
                   '((1 "dao1" NIL 2 0) (2 "dao2" NIL 2 0)))))
      (execute (:drop-table 'dao-test :cascade)))))
