;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

(def-suite :postmodern-roles
    :description "Dao suite for postmodern"
    :in :postmodern)

(in-suite :postmodern-roles)

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

(defun test-create-role-names ()
  (let ((names nil))
    (loop for database in '("d1" "d2" "all") do
      (loop for schema in '("public" "s2") do
        (loop for table in '("t1" "all")  do
          (let ((name (format nil "readonly_d_~a_s_~a_t_~a" database schema table)))
            (push name names)))))
    (push "standard" names)
    names))

(defparameter *test-dbs* '("d1" "d1_al" "d2" "d2_al" "d3" "d3_al"))
(defparameter *first-test-dbs* '("d1" "d1_al" "d2" "d2_al"))
(defparameter *subsequent-test-dbs* '("d3"))
(defparameter *public-limited-test-dbs* '("d1_al""d2_al" "d3_al"))
(defparameter *test-roles* (test-create-role-names))

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

(defun generate-test-table-row (db role schema table port)
  "Makes a hash table of the results of each combination."
  (let* ((lst (multiple-value-list
              (ignore-errors
               (with-connection (list db role role "localhost" :port port)
                 (query (format nil "select name from ~a.~a" schema table))))))
        (result (test-result lst)))
    result))

(defun test-loop (&optional (dbs *test-dbs*) (roles *test-roles*)
                    (port (with-test-connection (cl-postgres::connection-port *database*))))
  "Loops through roles, databases, schemas and tables to generate a complete union of possibilities."
  (when (stringp dbs) (setf dbs (list dbs)))
  (when (stringp roles) (setf roles (list roles)))
  (loop for name in roles do
    (loop for database in dbs do
      (loop for schema in '("public" "s2") do
        (loop for table in '("t1" "t2" "t3")  do
          (generate-test-table-row database name schema table port))))))

(defun test-create-roles ()
  (loop for database in '("d1" "d2" "all") do
    (loop for schema in '("public" "s2") do
      (loop for table in '("t1" "all") do
        (let ((name (format nil "readonly_d_~a_s_~a_t_~a" database schema table)))
          (create-role name name :tables (list table) :schema (list schema)
                                 :databases (if (string= "all" database)
                                                :all
                                                database))))))
  (create-role "standard" "standard" :base-role :standard))

(defun clean-test ()
  "Drops roles and databases created by this test suite."
  (with-test-connection
    (let ((superuser-name (cl-postgres::connection-user *database*))
          (superuser-password (cl-postgres::connection-password *database*))
          (host (cl-postgres::connection-host *database*))
          (dbs *test-dbs*)
          (users (intersection *test-roles*
                               (list-database-users) :test #'equal))
          (port (cl-postgres::connection-port *database*)))
      (format t "dbs ~a ~%" dbs)
      (loop for x in dbs do
        (when (database-exists-p x)
          (with-connection (list x superuser-name superuser-password host :port port)
                   (loop for y in users do
                     (query (format nil "reassign owned by ~a to ~a" y superuser-name))
                     (query (format nil "drop owned by ~a" y))))))
      (loop for x in users do (drop-role x))
      (loop for x in dbs do
        (when (database-exists-p x) (drop-database x))))))

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


(defun create-role-test-dbs ()
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
            (host (cl-postgres::connection-host *database*))
            (port (cl-postgres::connection-port *database*)))
        (loop for x in dbs counting x into y do
          (with-connection (list x superuser-name superuser-password host
                                 :port port)
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
            (query (:insert-into 's2.t3 :set 'name "Fog Harbour" 'population 57))))
        (create-database "d3" :limit-public-access nil) ;after creation of users
        (create-database "d3_al" :limit-public-access t)
        (loop for x in '("d3" "d3_al") do
          (with-connection (list x superuser-name superuser-password host
                                 :port port)
            (test-db-creation-helper)))))))

(test create-db-role-0-1
  (clean-test)
  (create-role-test-dbs)
  (with-test-connection
    (let ((port (cl-postgres::connection-port *database*)))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_all" "public" "t3"
                                          port)
                 '(("Crystal Pines") ("Fog Harbour"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_all" "public" "t1"
                                          port)
                 "permission denied for table t1"))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_all" "public" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_all" "s2" "t1"
                                          port)
                 '(("Moria"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_all" "s2" "t2"
                                          port)
                 '(("Minas Morgul"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_t1" "public" "t1"
                                          port)
                 "permission denied for table t1"))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_t1" "public" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_t1" "s2" "t1"
                                          port)
                 '(("Moria"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_s2_t_t1" "s2" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_all" "public" "t1"
                                          port)
                 '(("Oz"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_all" "public" "t2"
                                          port)
                 '(("Wonderland"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_all" "s2" "t1"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_all" "s2" "t2"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_t1" "public" "t1"
                                          port)
                 '(("Oz"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_t1" "public" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_t1" "s2" "t1"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_all_s_public_t_t1" "s2" "t2"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_all" "public" "t1"
                                          port)
                 "permission denied for table t1"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_all" "public" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_all" "s2" "t1"
                                          port)
                 '(("Moria"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_all" "s2" "t2"
                                          port)
                 '(("Minas Morgul"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_t1" "public" "t1"
                                          port)
                 "permission denied for table t1"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_t1" "public" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_t1" "s2" "t1"
                                          port)
                 '(("Moria"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_s2_t_t1" "s2" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_all" "public" "t1"
                                          port)
                 '(("Oz"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_all" "public" "t2"
                                          port)
                 '(("Wonderland"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_all" "s2" "t1"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_all" "s2" "t2"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_t1" "public" "t1"
                                          port)
                 '(("Oz"))))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_t1" "public" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_t1" "s2" "t1"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d1_s_public_t_t1" "s2" "t2"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_all" "public" "t1"
                                          port)
                 "permission denied for table t1"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_all" "public" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_all" "s2" "t1"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_all" "s2" "t2"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_t1" "public" "t1"
                                          port)
                 "permission denied for table t1"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_t1" "public" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_t1" "s2" "t1"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_s2_t_t1" "s2" "t2"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_all" "public" "t1"
                                          port)
                 "permission denied for table t1"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_all" "public" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_all" "s2" "t1"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_all" "s2" "t2"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_t1" "public" "t1"
                                          port)
                 "permission denied for table t1"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_t1" "public" "t2"
                                          port)
                 "permission denied for table t2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_t1" "s2" "t1"
                                          port)
                 "permission denied for schema s2"))
      (is (equal (generate-test-table-row "d1" "readonly_d_d2_s_public_t_t1" "s2" "t2"
                                          port)
                 "permission denied for schema s2"))
      (clean-test)
      (is (not (database-exists-p 'd1))))))

(test default-privileges-1
  (with-test-connection
    (let ((port (cl-postgres::connection-port *database*)))
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
      (with-connection `("test" "a" "a" "localhost" :port ,port)
        (query (:create-table 't1 ((a :type integer))))
        (query (:create-table 't2 ((a :type integer))))
        (query (:insert-into 't1 :set 'a 1))
        (query (:insert-into 't2 :set 'a 2))
        (query "grant select on table t1 to b") ; going directly to readonly-permissions would grant default privileges and we want to validate that later
        (query "grant select on table t2 to b"))
      (with-connection `("test" "b" "b" "localhost" :port ,port)
        (signals error (query (:select (:count '*) :from 'a.t1))))
      (with-connection `("test" "a" "a" "localhost" :port ,port)
        (query "grant usage on schema a to b"))
      (with-connection `("test" "b" "b" "localhost" :port ,port)
        (is (equal (query (:select (:count '*) :from 'a.t1) :single)
                   1)))
      (with-connection `("test" "a" "a" "localhost" :port ,port)
        (query "create table t3 as select * from t1"))
      (with-connection `("test" "b" "b" "localhost" :port ,port)
        (signals error (query (:select (:count '*) :from 'a.t3))))
      (with-connection `("test" "a" "a" "localhost" :port ,port)
        (pomo:grant-readonly-permissions "a" "b"))
      (with-connection `("test" "b" "b" "localhost" :port ,port)
        (is (equal (query (:select (:count '*) :from 'a.t3) :single)
                   1)))
      (with-connection `("test" "a" "a" "localhost" :port ,port)
        (query "create table t4 as select * from t1"))
      (with-connection `("test" "b" "b" "localhost" :port ,port)
        (is (equal (query (:select (:count '*) :from 'a.t4) :single)
                   1))))))

(test default-privileges-2
  (with-test-connection
    (let ((port (cl-postgres::connection-port *database*)))
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
    (with-connection `("test" "a" "a" "localhost" :port ,port)
      (query (:create-table 't1 ((a :type integer))))
      (query (:insert-into 't1 :set 'a 1)))
    (with-connection `("test" "b" "b" "localhost" :port ,port)
      (signals error (query (:select (:count '*) :from 'a.t1))))
    (with-connection `("test" "a" "a" "localhost" :port ,port)
      (pomo:grant-readonly-permissions "a" "b"))
    (with-connection `("test" "b" "b" "localhost" :port ,port)
      (is (equal (query (:select (:count '*) :from 'a.t1) :single)
                 1)))
    (with-connection `("test" "a" "a" "localhost" :port ,port)
      (query "create table t3 as select * from t1"))
    (with-connection `("test" "b" "b" "localhost" :port ,port)
      (is (equal (query (:select (:count '*) :from 'a.t1) :single)
                 1)))
    (with-connection `("test" "b" "b" "localhost" :port ,port)
      (signals error (query (:insert-into 'a.t1 :set 'a 2)))
      (signals error (query (:update 'a.t1 :set 'a 3 :where (:= 'a 1)))))
    (with-connection `("test" "a" "a" "localhost" :port ,port)
      (pomo:grant-editor-permissions "a" "b"))
    (with-connection `("test" "b" "b" "localhost" :port ,port)
      (query (:insert-into 'a.t1 :set 'a 2))
      (query (:update 'a.t1 :set 'a 3 :where (:= 'a 1)))
      (is (equal (query (:select 'a :from 'a.t1 :where (:= 'a 3)) :single)
                 3))))))
