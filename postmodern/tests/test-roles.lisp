;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(fiveam:def-suite :postmodern-roles
    :description "Dao suite for postmodern"
    :in :postmodern)

(fiveam:in-suite :postmodern-roles)

;;; Test what happens if role already exists
;;; Test if role name is not allowed
;;; Test if role name has utf8
;;; Test if role name has whitespace
;;; Test if role name has normalization issues
;;; Test that both readonly and editor cannot create tables
;;; Test to ensure tables in different schemas are covered.
;;; Test to ensure that subsequently created databases do not allow user any additional permissions.
;;; Test to ensure that subsequently created schemas do not allow user any additional permissions.
;;; Test to ensure that subsequently created tables do not allow user any additional permissions.
;;; Test to ensure that tables, databases and schemas work whether string or symbol

;;; NOTE THAT LIST-TABLES ONLY LISTS TABLES IN PUBLIC SCHEMA

;;; These tests must be run in order. They also assume that the existing user is a superuser

(defun test-result (result)
  (cond ((not (listp result))
         result)
        ((and (listp result)
              (first result))
         (first result))
        ((and (listp result)
              (not (first result)))
         (if (not (database-error-detail (second result)))
             (database-error-message (second result))
             (database-error-detail  (second result))))
        (t "something wrong ~a " (database-error-message (second result)))))

(defun generate-test-table-row (db role schema table &optional (org-table nil))
  "Makes a hash table of the results of each combination."
  (let* ((lst (multiple-value-list
              (ignore-errors
               (with-connection (list db role role "localhost")
                 (query (format nil "select name from ~a.~a" schema table))))))
        (result (test-result lst)))
    (if org-table (format t "|~a|~a|~a|~a|~a|~%" role db schema table (test-result lst))
        result)))

(defun test-loop (&optional (dbs *test-dbs*) (roles *test-roles*))
  "Loops through roles, databases, schemas and tables to generate a complete union of possibilities."
  (when (stringp dbs) (setf dbs (list dbs)))
  (when (stringp roles) (setf roles (list roles)))
  (loop for name in roles do
    (loop for database in dbs do
      (loop for schema in '("public" "s2") do
        (loop for table in '("t1" "t2" "t3")  do
          (generate-test-table-row database name schema table nil))))))

(defun test-create-role-names ()
  (let ((names nil))
    (loop for database in '("d1" "d2" "all") do
      (loop for schema in '("public" "s2") do
        (loop for table in '("t1" "all")  do
          (let ((name (format nil "readonly_d_~a_s_~a_t_~a" database schema table)))
            (push name names)))))
    (push "standard" names)
    names))

(defun test-create-roles ()
  (loop for database in '("d1" "d2" "all") do
    (loop for schema in '("public" "s2") do
      (loop for table in '("t1" "all") do
        (let ((name (format nil "readonly_d_~a_s_~a_t_~a" database schema table)))
          (create-role name name :tables (list table) :schema (list schema) :databases (if (string= "all" database) :all database))))))
  (create-role "standard" "standard" :base-role :standard))

(defparameter *test-dbs* '("d1" "d1_al" "d2" "d2_al" "d3" "d3_al"))
(defparameter *first-test-dbs* '("d1" "d1_al" "d2" "d2_al"))
(defparameter *subsequent-test-dbs* '("d3"))
(defparameter *public-limited-test-dbs* '("d1_al""d2_al" "d3_al"))
(defparameter *test-roles* (test-create-role-names))

(defun clean-test ()
  "Drops roles and databases created by this test suite."
  (with-test-connection
    (let ((superuser-name (cl-postgres::connection-user *database*))
          (superuser-password (cl-postgres::connection-password *database*))
          (host (cl-postgres::connection-host *database*))
          (dbs (intersection *test-dbs*
                             (list-databases :names-only t) :test #'equal))
          (users (intersection *test-roles*
                               (list-database-users) :test #'equal)))
      (loop for x in dbs do
        (when (database-exists-p x)
          (with-connection (list x superuser-name superuser-password host)
                   (loop for y in users do
                     (query (format nil "reassign owned by ~a to ~a" y superuser-name))
                     (query (format nil "drop owned by ~a" y))))))
      (loop for x in users do (drop-role x))
      (loop for x in dbs do (drop-database x)))))

(defun test-db-creation-helper ()
  "These tables get created for every db created"
  (query (:create-table 't1
                        ((id :type bigint :generated-as-identity-always t)
                         (name :type (varchar 40) :check (:<> 'name "")))))
  (query (:create-table 't2
                        ((id :type bigint :generated-as-identity-always t)
                         (name :type (varchar 40) :check (:<> 'name "")))))
  (query (:insert-into 't1 :set 'name "Oz"))
  (query (:insert-into 't2 :set 'name "Wonderland"))
  (create-schema "s2")
  (query (:create-table 's2.t1
                        ((id :type bigint :generated-as-identity-always t)
                         (name :type (varchar 40) :check (:<> 'name "")))))
  (query (:create-table 's2.t2
                        ((id :type bigint :generated-as-identity-always t)
                         (name :type (varchar 40) :check (:<> 'name "")))))
  (query (:insert-into 's2.t1 :set 'name "Moria"))
  (query (:insert-into 's2.t2 :set 'name "Minas Morgul")))

(test create-db-role-0-1
  (clean-test)
  (with-test-connection
    (let ((dbs *first-test-dbs*))
      (loop for x in dbs do
        (let ((lpa (if (or (equal x "d1")
                           (equal x "d2"))
                       nil
                       t)))
          (create-database x :limit-public-access lpa)))
      (let ((superuser-password (cl-postgres::connection-password *database*))
            (superuser-name (cl-postgres::connection-user *database*))
            (host (cl-postgres::connection-host *database*)))
        (loop for x in dbs counting x into y do
          (with-connection (list x superuser-name superuser-password host)
            (test-db-creation-helper)
            ;; YES CREATE THE ROLES ONLY ONCE, BUT WHAT ABOUT THE PERMISSIONS?

            (when (= y 1) (test-create-roles))
            ;; Create table in same database, but subsequent to creation of the roles
            (query (:create-table 't3
                                  ((id :type bigint :generated-as-identity-always t)
                                   (name :type (varchar 40) :check (:<> 'name ""))
                                   (population :type integer))))
            (query (:insert-into 't3 :set 'name "Crystal Pines" 'population 52))
            (query (:insert-into 't3 :set 'name "Fog Harbour" 'population 57))
            (query (:create-table 's2.t3
                                  ((id :type bigint :generated-as-identity-always t)
                                   (name :type (varchar 40) :check (:<> 'name ""))
                                   (population :type integer))))
            (query (:insert-into 's2.t3 :set 'name "Crystal Pines" 'population 52))
            (query (:insert-into 's2.t3 :set 'name "Fog Harbour" 'population 57))
            (is (table-exists-p "t1"))))
        (create-database "d3" :limit-public-access nil) ;after creation of users
        (create-database "d3_al" :limit-public-access t)
        (loop for x in '("d3" "d3_al") do
          (with-connection (list x superuser-name superuser-password host)
            (test-db-creation-helper))))))) ;after creation of users

(test readonly_d_all_s_s2_t_all-t3
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_all" "public" "t3")
           '(("Crystal Pines") ("Fog Harbour")))))

(test readonly_d_all_s_s2_t_all-1
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_all" "public" "t1")
             "permission denied for table t1")))
(test readonly_d_all_s_s2_t_all-2
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_all" "public" "t2")
             "permission denied for table t2")))
(test readonly_d_all_s_s2_t_all-3
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_all" "s2" "t1")
             '(("Moria")))))
(test readonly_d_all_s_s2_t_all-4
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_all" "s2" "t2")
             '(("Minas Morgul")))))
(test readonly_d_all_s_s2_t_t1-5
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_t1" "public" "t1")
             "permission denied for table t1")))
(test readonly_d_all_s_s2_t_t1-6
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_t1" "public" "t2")
             "permission denied for table t2")))
(test readonly_d_all_s_s2_t_t1-7
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_t1" "s2" "t1")
             '(("Moria")))))
(test readonly_d_all_s_s2_t_t1-8
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_t1" "s2" "t2")
             "permission denied for table t2")))
(test readonly_d_all_s_public_t_all-9
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_all" "public" "t1")
             '(("Oz")))))
(test readonly_d_all_s_public_t_all-10
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_all" "public" "t2")
             '(("Wonderland")))))
(test readonly_d_all_s_public_t_all-11
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_all" "s2" "t1")
             "permission denied for schema s2")))
(test readonly_d_all_s_public_t_all-12
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_all" "s2" "t2")
             "permission denied for schema s2")))
(test readonly_d_all_s_public_t_t1-13
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_t1" "public" "t1")
             '(("Oz")))))
(test readonly_d_all_s_public_t_t1-14
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_t1" "public" "t2")
             "permission denied for table t2")))
(test readonly_d_all_s_public_t_t1-15
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_t1" "s2" "t1")
             "permission denied for schema s2")))
(test readonly_d_all_s_public_t_t1-16
  (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_t1" "s2" "t2")
             "permission denied for schema s2")))
(test readonly_d_d1_s_s2_t_all-17
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_all" "public" "t1")
             "permission denied for table t1")))
(test readonly_d_d1_s_s2_t_all-18
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_all" "public" "t2")
             "permission denied for table t2")))
(test readonly_d_d1_s_s2_t_all-19
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_all" "s2" "t1")
             '(("Moria")))))
(test readonly_d_d1_s_s2_t_all-20
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_all" "s2" "t2")
             '(("Minas Morgul")))))
(test readonly_d_d1_s_s2_t_t1-21
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_t1" "public" "t1")
             "permission denied for table t1")))
(test readonly_d_d1_s_s2_t_t1-22
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_t1" "public" "t2")
             "permission denied for table t2")))
(test readonly_d_d1_s_s2_t_t1-23
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_t1" "s2" "t1")
             '(("Moria")))))
(test readonly_d_d1_s_s2_t_t1-24
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_t1" "s2" "t2")
             "permission denied for table t2")))
(test readonly_d_d1_s_public_t_all-25
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_all" "public" "t1")
             '(("Oz")))))
(test readonly_d_d1_s_public_t_all-26
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_all" "public" "t2")
             '(("Wonderland")))))
(test readonly_d_d1_s_public_t_all-27
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_all" "s2" "t1")
             "permission denied for schema s2")))
(test readonly_d_d1_s_public_t_all-28
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_all" "s2" "t2")
             "permission denied for schema s2")))
(test readonly_d_d1_s_public_t_t1-29
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_t1" "public" "t1")
             '(("Oz")))))
(test readonly_d_d1_s_public_t_t1-30
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_t1" "public" "t2")
             "permission denied for table t2")))
(test readonly_d_d1_s_public_t_t1-31
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_t1" "s2" "t1")
             "permission denied for schema s2")))
(test readonly_d_d1_s_public_t_t1-32
  (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_t1" "s2" "t2")
             "permission denied for schema s2")))
(test readonly_d_d2_s_s2_t_all-33
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_all" "public" "t1")
             "permission denied for table t1")))
(test readonly_d_d2_s_s2_t_all-34
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_all" "public" "t2")
             "permission denied for table t2")))
(test readonly_d_d2_s_s2_t_all-35
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_all" "s2" "t1")
             "permission denied for schema s2")))
(test readonly_d_d2_s_s2_t_all-36
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_all" "s2" "t2")
             "permission denied for schema s2")))
(test readonly_d_d2_s_s2_t_t1-37
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_t1" "public" "t1")
             "permission denied for table t1")))
(test readonly_d_d2_s_s2_t_t1-38
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_t1" "public" "t2")
             "permission denied for table t2")))
(test readonly_d_d2_s_s2_t_t1-39
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_t1" "s2" "t1")
             "permission denied for schema s2")))
(test readonly_d_d2_s_s2_t_t1-40
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_t1" "s2" "t2")
             "permission denied for schema s2")))
(test readonly_d_d2_s_public_t_all-41
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_all" "public" "t1")
             "permission denied for table t1")))
(test readonly_d_d2_s_public_t_all-42
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_all" "public" "t2")
             "permission denied for table t2")))
(test readonly_d_d2_s_public_t_all-43
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_all" "s2" "t1")
             "permission denied for schema s2")))
(test readonly_d_d2_s_public_t_all-44
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_all" "s2" "t2")
             "permission denied for schema s2")))
(test readonly_d_d2_s_public_t_t1-45
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_t1" "public" "t1")
             "permission denied for table t1")))
(test readonly_d_d2_s_public_t_t1-46
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_t1" "public" "t2")
             "permission denied for table t2")))
(test readonly_d_d2_s_public_t_t1-47
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_t1" "s2" "t1")
             "permission denied for schema s2")))
(test readonly_d_d2_s_public_t_t1-48
  (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_t1" "s2" "t2")
             "permission denied for schema s2")))

(test create-db-row-0-2
  (with-test-connection
    (clean-test)
    (is (not (database-exists-p 'd1)))))

(test default-privileges-1
  (with-test-connection
    (when (schema-exists-p "a")
      (drop-schema "a" :cascade t))
    (when (schema-exists-p "b")
      (drop-schema "b" :cascade t))
    (when (role-exists-p "a") (drop-role "a"))
    (when (role-exists-p "b") (drop-role "b"))
    (create-role "a" "a")
    (create-schema "a" "a")
    (alter-role-search-path "a" "a")
    (create-role "b" "b")
    (create-schema "b" "b")
    (alter-role-search-path "b" "b")
    (with-connection '("test" "a" "a" "localhost")
      (query (:create-table 't1 ((a :type integer))))
      (query (:create-table 't2 ((a :type integer))))
      (query (:insert-into 't1 :set 'a 1))
      (query (:insert-into 't2 :set 'a 2))
      (query "grant select on table t1 to b") ; going directly to readonly-permissions would grant default privileges and we want to validate that later
      (query "grant select on table t2 to b"))
    (with-connection '("test" "b" "b" "localhost")
      (signals error (query (:select (:count '*) :from 'a.t1))))
    (with-connection '("test" "a" "a" "localhost")
      (query "grant usage on schema a to b"))
    (with-connection '("test" "b" "b" "localhost")
      (is (equal (query (:select (:count '*) :from 'a.t1) :single)
                 1)))
    (with-connection '("test" "a" "a" "localhost")
      (query "create table t3 as select * from t1"))
    (with-connection '("test" "b" "b" "localhost")
      (signals error (query (:select (:count '*) :from 'a.t3))))
    (with-connection '("test" "a" "a" "localhost")
      (pomo:grant-readonly-permissions "a" "b"))
    (with-connection '("test" "b" "b" "localhost")
      (is (equal (query (:select (:count '*) :from 'a.t3) :single)
                 1)))
    (with-connection '("test" "a" "a" "localhost")
      (query "create table t4 as select * from t1"))
    (with-connection '("test" "b" "b" "localhost")
      (is (equal (query (:select (:count '*) :from 'a.t4) :single)
                 1)))))

(test default-privileges-2
  (with-test-connection
    (when (schema-exists-p "a")
      (drop-schema "a" :cascade t))
    (when (schema-exists-p "b")
      (drop-schema "b" :cascade t))
    (when (role-exists-p "a") (drop-role "a"))
    (when (role-exists-p "b") (drop-role "b"))
    (create-role "a" "a")
    (create-schema "a" "a")
    (alter-role-search-path "a" "a")
    (create-role "b" "b")
    (create-schema "b" "b")
    (alter-role-search-path "b" "b")
    (with-connection '("test" "a" "a" "localhost")
      (query (:create-table 't1 ((a :type integer))))
      (query (:insert-into 't1 :set 'a 1)))
    (with-connection '("test" "b" "b" "localhost")
      (signals error (query (:select (:count '*) :from 'a.t1))))
    (with-connection '("test" "a" "a" "localhost")
      (pomo:grant-readonly-permissions "a" "b"))
    (with-connection '("test" "b" "b" "localhost")
      (is (equal (query (:select (:count '*) :from 'a.t1) :single)
                 1)))
    (with-connection '("test" "a" "a" "localhost")
      (query "create table t3 as select * from t1"))
    (with-connection '("test" "b" "b" "localhost")
      (is (equal (query (:select (:count '*) :from 'a.t1) :single)
                 1)))
    (with-connection '("test" "b" "b" "localhost")
      (signals error (query (:insert-into 'a.t1 :set 'a 2)))
      (signals error (query (:update 'a.t1 :set 'a 3 :where (:= 'a 1)))))
    (with-connection '("test" "a" "a" "localhost")
      (pomo:grant-editor-permissions "a" "b"))
    (with-connection '("test" "b" "b" "localhost")
      (query (:insert-into 'a.t1 :set 'a 2))
      (query (:update 'a.t1 :set 'a 3 :where (:= 'a 1)))
      (is (equal (query (:select 'a :from 'a.t1 :where (:= 'a 3)) :single)
                 3)))))
