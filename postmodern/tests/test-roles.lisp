;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(fiveam:def-suite :postmodern-roles
    :description "Dao suite for postmodern"
    :in :postmodern-utilities)

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
        (loop for table in '("c1" "c2")  do
          (generate-test-table-row database name schema table))))))

(defun test-create-role-names ()
  (let ((names nil))
    (loop for database in '("t1" "t2" "all") do
      (loop for schema in '("public" "s2") do
        (loop for table in '("c1" "all")  do
          (let ((name (format nil "readonly_d_~a_s_~a_t_~a" database schema table)))
            (push name names)))))
    names))

(defun test-create-roles ()
  (loop for database in '("t1" "t2" "all") do
    (loop for schema in '("public" "s2") do
      (loop for table in '("c1" "all") do
        (let ((name (format nil "readonly_d_~a_s_~a_t_~a" database schema table)))
          (create-role name name :tables (list table) :schema (list schema) :databases (if (string= "all" database) :all database)))))))

(defparameter *test-dbs* '("t1" "t1_al" "t2" "t2_al" "t3" "t3_al"))
(defparameter *first-test-dbs* '("t1" "t1_al" "t2" "t2_al"))
(defparameter *subsequent-test-dbs* '("t3"))
(defparameter *public-limited-test-dbs* '("t1_al""t2_al" "t3_al"))
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
        (with-connection (list x superuser-name superuser-password host)
          (loop for y in users do
            (query (format nil "reassign owned by ~a to ~a" y superuser-name))
            (query (format nil "drop owned by ~a" y)))))
      (loop for x in users do (drop-role x))
      (loop for x in dbs do (drop-database x)))))

(defun test-db-creation-helper ()
  "These tables get created for every db created"
  (query (:create-table 'c1
                        ((id :type bigint :generated-as-identity-always t)
                         (name :type (varchar 40) :check (:<> 'name "")))))
  (query (:create-table 'c2
                        ((id :type bigint :generated-as-identity-always t)
                         (name :type (varchar 40) :check (:<> 'name "")))))
  (query (:insert-into 'c1 :set 'name "Oz"))
  (query (:insert-into 'c2 :set 'name "Wonderland"))
  (create-schema "s2")
  (query (:create-table 's2.c1
                        ((id :type bigint :generated-as-identity-always t)
                         (name :type (varchar 40) :check (:<> 'name "")))))
  (query (:create-table 's2.c2
                        ((id :type bigint :generated-as-identity-always t)
                         (name :type (varchar 40) :check (:<> 'name "")))))
  (query (:insert-into 's2.c1 :set 'name "Moria"))
  (query (:insert-into 's2.c2 :set 'name "Minas Morgul")))

(test create-db-role-0-1
  (clean-test)
  (with-test-connection
    (let ((dbs *first-test-dbs*))
      (loop for x in dbs do
        (create-database x
                         :limit-public-access
                         (if (or (equal x "t1")
                                 (equal x "t2"))
                             nil
                             t)))
      (let ((superuser-password (cl-postgres::connection-password *database*))
            (superuser-name (cl-postgres::connection-user *database*))
            (host (cl-postgres::connection-host *database*)))
        (loop for x in dbs counting x into y do
          (with-connection (list x superuser-name superuser-password host)
            (test-db-creation-helper)
            ;; YES CREATE THE ROLES ONLY ONCE, BUT WHAT ABOUT THE PERMISSIONS?

            (when (= y 1) (test-create-roles))
            ;; Create table in same database, but subsequent to creation of the roles
            (query (:create-table 'c3
                                  ((id :type bigint :generated-as-identity-always t)
                                   (name :type (varchar 40) :check (:<> 'name ""))
                                   (population :type integer))))
            (query (:insert-into 'c3 :set 'name "Crystal Pines" 'population 52))
            (query (:insert-into 'c3 :set 'name "Fog Harbour" 'population 57))
            (is (table-exists-p "c1"))))
        (create-database "t3" :limit-public-access nil) ;after creation of users
        (create-database "t3_al" :limit-public-access t)
        (loop for x in '("t3" "t3_al") do
          (with-connection (list x superuser-name superuser-password host)
            (test-db-creation-helper))))))) ;after creation of users

(test readonly_d_all_s_s2_t_all-1
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_s2_t_all" "public" "c1")
             "permission denied for table c1")))
(test readonly_d_all_s_s2_t_all-2
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_s2_t_all" "public" "c2")
             "permission denied for table c2")))
(test readonly_d_all_s_s2_t_all-3
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_s2_t_all" "s2" "c1")
             '(("Moria")))))
(test readonly_d_all_s_s2_t_all-4
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_s2_t_all" "s2" "c2")
             '(("Minas Morgul")))))
(test readonly_d_all_s_s2_t_c1-5
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_s2_t_c1" "public" "c1")
             "permission denied for table c1")))
(test readonly_d_all_s_s2_t_c1-6
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_s2_t_c1" "public" "c2")
             "permission denied for table c2")))
(test readonly_d_all_s_s2_t_c1-7
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_s2_t_c1" "s2" "c1")
             '(("Moria")))))
(test readonly_d_all_s_s2_t_c1-8
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_s2_t_c1" "s2" "c2")
             "permission denied for table c2")))
(test readonly_d_all_s_public_t_all-9
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_public_t_all" "public" "c1")
             '(("Oz")))))
(test readonly_d_all_s_public_t_all-10
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_public_t_all" "public" "c2")
             '(("Wonderland")))))
(test readonly_d_all_s_public_t_all-11
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_public_t_all" "s2" "c1")
             "permission denied for schema s2")))
(test readonly_d_all_s_public_t_all-12
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_public_t_all" "s2" "c2")
             "permission denied for schema s2")))
(test readonly_d_all_s_public_t_c1-13
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_public_t_c1" "public" "c1")
             '(("Oz")))))
(test readonly_d_all_s_public_t_c1-14
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_public_t_c1" "public" "c2")
             "permission denied for table c2")))
(test readonly_d_all_s_public_t_c1-15
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_public_t_c1" "s2" "c1")
             "permission denied for schema s2")))
(test readonly_d_all_s_public_t_c1-16
  (is (equal (generate-test-table-row "t1" "readonly_d_all_s_public_t_c1" "s2" "c2")
             "permission denied for schema s2")))
(test readonly_d_t1_s_s2_t_all-17
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_s2_t_all" "public" "c1")
             "permission denied for table c1")))
(test readonly_d_t1_s_s2_t_all-18
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_s2_t_all" "public" "c2")
             "permission denied for table c2")))
(test readonly_d_t1_s_s2_t_all-19
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_s2_t_all" "s2" "c1")
             '(("Moria")))))
(test readonly_d_t1_s_s2_t_all-20
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_s2_t_all" "s2" "c2")
             '(("Minas Morgul")))))
(test readonly_d_t1_s_s2_t_c1-21
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_s2_t_c1" "public" "c1")
             "permission denied for table c1")))
(test readonly_d_t1_s_s2_t_c1-22
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_s2_t_c1" "public" "c2")
             "permission denied for table c2")))
(test readonly_d_t1_s_s2_t_c1-23
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_s2_t_c1" "s2" "c1")
             '(("Moria")))))
(test readonly_d_t1_s_s2_t_c1-24
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_s2_t_c1" "s2" "c2")
             "permission denied for table c2")))
(test readonly_d_t1_s_public_t_all-25
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_public_t_all" "public" "c1")
             '(("Oz")))))
(test readonly_d_t1_s_public_t_all-26
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_public_t_all" "public" "c2")
             '(("Wonderland")))))
(test readonly_d_t1_s_public_t_all-27
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_public_t_all" "s2" "c1")
             "permission denied for schema s2")))
(test readonly_d_t1_s_public_t_all-28
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_public_t_all" "s2" "c2")
             "permission denied for schema s2")))
(test readonly_d_t1_s_public_t_c1-29
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_public_t_c1" "public" "c1")
             '(("Oz")))))
(test readonly_d_t1_s_public_t_c1-30
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_public_t_c1" "public" "c2")
             "permission denied for table c2")))
(test readonly_d_t1_s_public_t_c1-31
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_public_t_c1" "s2" "c1")
             "permission denied for schema s2")))
(test readonly_d_t1_s_public_t_c1-32
  (is (equal (generate-test-table-row "t1" "readonly_d_t1_s_public_t_c1" "s2" "c2")
             "permission denied for schema s2")))
(test readonly_d_t2_s_s2_t_all-33
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_s2_t_all" "public" "c1")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_s2_t_all-34
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_s2_t_all" "public" "c2")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_s2_t_all-35
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_s2_t_all" "s2" "c1")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_s2_t_all-36
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_s2_t_all" "s2" "c2")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_s2_t_c1-37
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_s2_t_c1" "public" "c1")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_s2_t_c1-38
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_s2_t_c1" "public" "c2")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_s2_t_c1-39
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_s2_t_c1" "s2" "c1")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_s2_t_c1-40
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_s2_t_c1" "s2" "c2")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_public_t_all-41
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_public_t_all" "public" "c1")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_public_t_all-42
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_public_t_all" "public" "c2")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_public_t_all-43
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_public_t_all" "s2" "c1")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_public_t_all-44
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_public_t_all" "s2" "c2")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_public_t_c1-45
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_public_t_c1" "public" "c1")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_public_t_c1-46
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_public_t_c1" "public" "c2")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_public_t_c1-47
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_public_t_c1" "s2" "c1")
             "User does not have CONNECT privilege.")))
(test readonly_d_t2_s_public_t_c1-48
  (is (equal (generate-test-table-row "t1" "readonly_d_t2_s_public_t_c1" "s2" "c2")
             "User does not have CONNECT privilege.")))

(test create-db-row-0-2
  (with-test-connection
    (clean-test)
    (is (not (database-exists-p 't1)))))
