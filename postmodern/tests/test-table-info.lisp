;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

;; Adjust the above to some db/user/pass/host combination that refers
;; to a valid postgresql database in which no table named test_data
;; currently exists. Then after loading the file, run the tests with
;; (run! :postmodern)

(def-suite :postmodern-table-info
  :description "Test suite for postmodern table information functions"
  :in :postmodern)

(in-suite :postmodern-table-info)

(defun create-products ()
  (drop-table "products" :if-exists t :cascade t)
  (query "CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    price NUMERIC(5,2) default 9.9);"))

(defun create-customers-and-contacts ()
  (drop-table "s1.customers" :if-exists t :cascade t)
  (drop-table "s1.contacts" :if-exists t :cascade t)
  (drop-schema "s1" :if-exists t :cascade t)
  (create-schema "s1")
  (query "CREATE TABLE s1.customers(
   customer_id INT GENERATED ALWAYS AS IDENTITY,
   customer_name VARCHAR(255) NOT NULL,
   PRIMARY KEY(customer_id))")

  (query "CREATE TABLE s1.contacts (
   contact_id INT GENERATED ALWAYS AS IDENTITY,
   customer_id INT,
   contact_name VARCHAR(255) NOT NULL,
   phone VARCHAR(25),
   email VARCHAR(100) UNIQUE,
   PRIMARY KEY(contact_id),
   CONSTRAINT fk_customer
      FOREIGN KEY(customer_id)
	  REFERENCES s1.customers(customer_id))")
  (pomo:add-comment :database "Test" "This is a test database for reporting purposes")
  (pomo:add-comment :schema "s1" "This is a comment about the s1 schema which is external looking")
  (pomo:add-comment :table "s1.customers" "This is a test comment for the s1.customers table for reporting purposes"))

(defun create-employees ()
  (drop-table "s2.employees" :if-exists t :cascade t)
  (drop-schema "s2" :if-exists t :cascade t)
  (create-schema "s2")
  (query "CREATE TABLE s2.employees (
         	id SERIAL PRIMARY KEY,
         	first_name VARCHAR (50),
          last_name VARCHAR (50),
          birth_date DATE CHECK (birth_date > '1900-01-01'),
        	start_date DATE CHECK (start_date > birth_date),
         	salary numeric CHECK(salary > 0));")

  (query "CREATE UNIQUE INDEX CONCURRENTLY employee_idx
        ON s2.employees (id)")

  (query "ALTER TABLE s2.employees
        ADD CONSTRAINT unique_employee_id
        UNIQUE USING INDEX employee_idx")
  (pomo:add-comment :schema "s2" "This is a comment about the s2 schema which is internal looking")
  (pomo:add-comment :table "s2.employees" "This is a test comment for the s2.employees table for reporting purposes")
  (pomo:add-comment :column "s2.employees.birth_date" "This is a test comment for the s2.employees.birth_date column for reporting purposes. There is a check that the start_date needs to occur after the birth_date.")
  (pomo:add-comment :column "s2.employees.start_date" "This is a test comment for the s2.employees.start_date column for reporting purposes. There is a check that the start_date needs to occur after the birth_date.")
  (pomo:add-comment :column "s2.employees.salary" "This is a test comment for the s2.employees.salary column for reporting purposes. There is a check that the salary needs to be greater than 0."))

(test using-employees
  (with-test-connection
    (create-employees)
    (multiple-value-bind (rows overview check-constraints)
        (table-description-menu "s2.employees"
                                :char-max-length nil :data-type-length nil
                                :has-default nil :default-value nil :not-null nil
                                :numeric-precision nil :numeric-scale nil
                                :storage nil :primary nil :primary-key-name nil
                                :unique nil :unique-key-name nil :fkey nil :fkey-name nil
                                :fkey-col-id nil :fkey-table nil :fkey-local-col-id nil
                                :identity nil :generated nil :collation nil
                                :col-comments t :locally-defined nil :inheritance-count nil
                                :stat-collection nil)
      (is (equal (get-schema-comment "s2")
                 "This is a comment about the s2 schema which is internal looking"))
      (is (equal overview "This is a test comment for the s2.employees table for reporting purposes"))
      (is (equal check-constraints
                 '(("employees_birth_date_check" "CHECK (birth_date > '1900-01-01'::date)")
                   ("employees_check" "CHECK (start_date > birth_date)")
                   ("employees_salary_check" "CHECK (salary > 0::numeric)"))))
      (is (equal rows
                  '((:COLUMN-NAME "id" :DATA-TYPE-NAME "int4" :COL-COMMENTS :NULL)
                    (:COLUMN-NAME "id" :DATA-TYPE-NAME "int4" :COL-COMMENTS :NULL)
                    (:COLUMN-NAME "first_name" :DATA-TYPE-NAME "varchar" :COL-COMMENTS :NULL)
                    (:COLUMN-NAME "last_name" :DATA-TYPE-NAME "varchar" :COL-COMMENTS :NULL)
                    (:COLUMN-NAME "birth_date" :DATA-TYPE-NAME "date" :COL-COMMENTS
                     "This is a test comment for the s2.employees.birth_date column for reporting purposes. There is a check that the start_date needs to occur after the birth_date.")
                    (:COLUMN-NAME "birth_date" :DATA-TYPE-NAME "date" :COL-COMMENTS
                     "This is a test comment for the s2.employees.birth_date column for reporting purposes. There is a check that the start_date needs to occur after the birth_date.")
                    (:COLUMN-NAME "start_date" :DATA-TYPE-NAME "date" :COL-COMMENTS
                     "This is a test comment for the s2.employees.start_date column for reporting purposes. There is a check that the start_date needs to occur after the birth_date.")
                    (:COLUMN-NAME "salary" :DATA-TYPE-NAME "numeric" :COL-COMMENTS
                     "This is a test comment for the s2.employees.salary column for reporting purposes. There is a check that the salary needs to be greater than 0.")))))
    (is (equal (list-check-constraints "s2.employees")
               '(("employees_birth_date_check" "CHECK (birth_date > '1900-01-01'::date)")
                 ("employees_check" "CHECK (start_date > birth_date)")
                 ("employees_salary_check" "CHECK (salary > 0::numeric)"))))
    (is (equal (list-columns "s2.employees")
               '("id" "first_name" "last_name" "birth_date" "start_date" "salary")))
    (is (equal (list-columns-with-types "s2.employees")
               '(("id" "int4") ("first_name" "varchar") ("last_name" "varchar")
                 ("birth_date" "date") ("start_date" "date") ("salary" "numeric"))))
    (is (equal (column-exists-p "s2.employees" "id")
               t))
    (is (equal (get-column-comments 's2.employees)
               '(("birth_date"
                  "This is a test comment for the s2.employees.birth_date column for reporting purposes. There is a check that the start_date needs to occur after the birth_date.")
                 ("start_date"
                  "This is a test comment for the s2.employees.start_date column for reporting purposes. There is a check that the start_date needs to occur after the birth_date.")
                 ("salary"
                  "This is a test comment for the s2.employees.salary column for reporting purposes. There is a check that the salary needs to be greater than 0."))))
    (is (equal (find-comments :schema 's2)
               "This is a comment about the s2 schema which is internal looking"))
    (is (equal (find-comments :table 's2.employees)
               "This is a test comment for the s2.employees table for reporting purposes"))
    (is (equal (find-comments :columns 's2.employees)
               '(("birth_date"
                  "This is a test comment for the s2.employees.birth_date column for reporting purposes. There is a check that the start_date needs to occur after the birth_date.")
                 ("start_date"
                  "This is a test comment for the s2.employees.start_date column for reporting purposes. There is a check that the start_date needs to occur after the birth_date.")
                 ("salary"
                  "This is a test comment for the s2.employees.salary column for reporting purposes. There is a check that the salary needs to be greater than 0."))))
    (is (equal (get-column-comment 's2.employees.birth-date)
               "This is a test comment for the s2.employees.birth_date column for reporting purposes. There is a check that the start_date needs to occur after the birth_date."))
    (is (not (get-column-comment 's2.employees.id)))
    (is (not (get-column-comment 's2.employees)))
    (drop-table 's2.employees)
    (drop-schema 's2)))

(test table-schema-names
  (multiple-value-bind (tn sn)
      (pomo::table-schema-names "t1" nil)
    (is (equal tn "t1"))
    (is (equal sn "public")))
  (multiple-value-bind (tn sn)
      (pomo::table-schema-names "t1" "s2")
    (is (equal tn "t1"))
    (is (equal sn "s2")))
  (multiple-value-bind (tn sn)
      (pomo::table-schema-names "s1.t1" nil)
    (is (equal tn "t1"))
    (is (equal sn "s1")))
  (signals error (pomo::table-schema-names "s1.t1" "s2")))

(test table-inheritance
  (with-test-connection
    (drop-table "cities" :if-exists t :cascade t)
    (query "CREATE TABLE cities (
    name            text,
    population      float,
    altitude        int  );")
    (query "CREATE TABLE capitals (
    state           char(2))
    INHERITS (cities);")
    (is (equalp (table-description-menu
                 "capitals" :char-max-length nil :data-type-length nil
                 :has-default nil :default-value nil :not-null nil
                 :numeric-precision nil :numeric-scale nil
                 :storage nil :primary nil :primary-key-name nil
                 :unique nil :unique-key-name nil :fkey nil :fkey-name nil
                 :fkey-col-id nil :fkey-table nil :fkey-local-col-id nil
                 :identity nil :generated nil :collation nil
                 :col-comments nil
                 :stat-collection nil :inheritance-count t
                 :locally-defined t)
                '((:COLUMN-NAME "name" :DATA-TYPE-NAME "text" :LOCALLY-DEFINED :NULL
                   :INHERITANCE-COUNT 1)
                  (:COLUMN-NAME "population" :DATA-TYPE-NAME "float8" :LOCALLY-DEFINED :NULL
                   :INHERITANCE-COUNT 1)
                  (:COLUMN-NAME "altitude" :DATA-TYPE-NAME "int4" :LOCALLY-DEFINED :NULL
                   :INHERITANCE-COUNT 1)
                  (:COLUMN-NAME "state" :DATA-TYPE-NAME "bpchar" :LOCALLY-DEFINED T
                   :INHERITANCE-COUNT 0))))
    (drop-table 'cities :cascade t)
    (drop-table 'capitals :cascade t)))

(test using-products
  (with-test-connection
    (create-products)
    (multiple-value-bind (rows overview)
        (table-description-menu "products"
                                :char-max-length nil :data-type-length nil
                                :has-default nil :default-value nil :not-null nil
                                :numeric-precision nil :numeric-scale nil
                                :storage t :primary nil :primary-key-name nil
                                :unique nil :unique-key-name nil :fkey nil :fkey-name nil
                                :fkey-col-id nil :fkey-table nil :fkey-local-col-id nil
                                :identity nil :generated nil :collation nil
                                :col-comments nil :locally-defined nil :inheritance-count nil
                                :stat-collection nil)
      (declare (ignore overview))
      (is (equalp rows
                  '((:COLUMN-NAME "id" :DATA-TYPE-NAME "int4" :STORAGE "plain")
                    (:COLUMN-NAME "name" :DATA-TYPE-NAME "varchar" :STORAGE "extended")
                    (:COLUMN-NAME "price" :DATA-TYPE-NAME "numeric" :STORAGE "main")))))
    (multiple-value-bind (rows overview)
        (table-description-menu "products"
                                :char-max-length nil :data-type-length nil
                                :has-default nil :default-value nil :not-null nil
                                :numeric-precision nil :numeric-scale nil
                                :storage nil :primary t :primary-key-name t
                                :unique nil :unique-key-name nil :fkey nil :fkey-name nil
                                :fkey-col-id nil :fkey-table nil :fkey-local-col-id nil
                                :identity nil :generated nil :collation nil
                                :col-comments nil :locally-defined nil :inheritance-count nil
                                :stat-collection nil)
      (declare (ignore overview))
      (is (equalp rows
                  '((:COLUMN-NAME "id" :DATA-TYPE-NAME "int4" :PRIMARY "Primary" :PRIMARY-KEY-NAME
                     "products_pkey")
                    (:COLUMN-NAME "name" :DATA-TYPE-NAME "varchar" :PRIMARY "" :PRIMARY-KEY-NAME
                     :NULL)
                    (:COLUMN-NAME "price" :DATA-TYPE-NAME "numeric" :PRIMARY "" :PRIMARY-KEY-NAME
                     :NULL)))))
    (multiple-value-bind (rows overview)
        (table-description-menu "products"
                                :char-max-length t :data-type-length t
                                :has-default t :default-value t :not-null nil
                                :numeric-precision nil :numeric-scale nil
                                :storage nil :primary nil :primary-key-name nil
                                :unique nil :unique-key-name nil :fkey nil :fkey-name nil
                                :fkey-col-id nil :fkey-table nil :fkey-local-col-id nil
                                :identity nil :generated nil :collation nil
                                :col-comments nil :locally-defined nil :inheritance-count nil
                                :stat-collection nil)
      (declare (ignore overview))
      (is (equalp rows
                  '((:COLUMN-NAME "id" :DATA-TYPE-NAME "int4" :CHARACTER-MAXIMUM-LENGTH :NULL
                     :DATA-TYPE-LENGTH 4 :HAS-DEFAULT T :DEFAULT-VALUE
                     "nextval('products_id_seq'::regclass)")
                    (:COLUMN-NAME "name" :DATA-TYPE-NAME "varchar" :CHARACTER-MAXIMUM-LENGTH 100
                     :DATA-TYPE-LENGTH -1 :HAS-DEFAULT :NULL :DEFAULT-VALUE :NULL)
                    (:COLUMN-NAME "price" :DATA-TYPE-NAME "numeric" :CHARACTER-MAXIMUM-LENGTH
                     :NULL :DATA-TYPE-LENGTH -1 :HAS-DEFAULT T :DEFAULT-VALUE "9.9")))))
    (multiple-value-bind (rows overview)
        (table-description-menu "products"
                                :char-max-length nil :data-type-length nil
                                :has-default nil :default-value nil :not-null t
                                :numeric-precision nil :numeric-scale nil
                                :storage nil :primary nil :primary-key-name nil
                                :unique nil :unique-key-name nil :fkey nil :fkey-name nil
                                :fkey-col-id nil :fkey-table nil :fkey-local-col-id nil
                                :identity nil :generated nil :collation nil
                                :col-comments nil :locally-defined nil :inheritance-count nil
                                :stat-collection nil)
      (declare (ignore overview))
      (is (equalp
           rows
           '((:COLUMN-NAME "id" :DATA-TYPE-NAME "int4" :NOT-NULL T)
             (:COLUMN-NAME "name" :DATA-TYPE-NAME "varchar" :NOT-NULL T)
             (:COLUMN-NAME "price" :DATA-TYPE-NAME "numeric" :NOT-NULL :NULL)))))
    (multiple-value-bind (rows overview)
        (table-description-menu "products"
                                :char-max-length nil :data-type-length nil
                                :has-default nil :default-value nil :not-null nil
                                :numeric-precision t :numeric-scale t
                                :storage nil :primary nil :primary-key-name nil
                                :unique nil :unique-key-name nil :fkey nil :fkey-name nil
                                :fkey-col-id nil :fkey-table nil :fkey-local-col-id nil
                                :identity nil :generated nil :collation nil
                                :col-comments nil :locally-defined nil :inheritance-count nil
                                :stat-collection nil)
      (declare (ignore overview))
      (is (equalp rows
                  '((:COLUMN-NAME "id" :DATA-TYPE-NAME "int4" :NUMERIC-PRECISION :NULL
                     :NUMERIC-SCALE :NULL)
                    (:COLUMN-NAME "name" :DATA-TYPE-NAME "varchar" :NUMERIC-PRECISION :NULL
                     :NUMERIC-SCALE :NULL)
                    (:COLUMN-NAME "price" :DATA-TYPE-NAME "numeric" :NUMERIC-PRECISION 5
                     :NUMERIC-SCALE 2)))))
    (drop-table 'products :cascade t)))

(test using-customer-contacts
  (with-test-connection
    (create-customers-and-contacts)
    (is (equalp (table-description-menu "s1.contacts"
                                        :char-max-length nil :data-type-length nil
                                        :has-default nil :default-value nil :not-null nil
                                        :numeric-precision nil :numeric-scale nil
                                        :storage nil :primary nil :primary-key-name nil
                                        :unique t :unique-key-name t :fkey nil :fkey-name nil
                                        :fkey-col-id nil :fkey-table nil :fkey-local-col-id nil
                                        :identity nil :generated nil :collation nil
                                        :col-comments nil :locally-defined nil :inheritance-count nil
                                        :stat-collection nil)
                '((:COLUMN-NAME "contact_id" :DATA-TYPE-NAME "int4" :UNIQUE :NULL
                   :UNIQUE-KEY-NAME :NULL)
                  (:COLUMN-NAME "customer_id" :DATA-TYPE-NAME "int4" :UNIQUE :NULL
                   :UNIQUE-KEY-NAME :NULL)
                  (:COLUMN-NAME "contact_name" :DATA-TYPE-NAME "varchar" :UNIQUE :NULL
                   :UNIQUE-KEY-NAME :NULL)
                  (:COLUMN-NAME "phone" :DATA-TYPE-NAME "varchar" :UNIQUE :NULL :UNIQUE-KEY-NAME
                                                                          :NULL)
                  (:COLUMN-NAME "email" :DATA-TYPE-NAME "varchar" :UNIQUE T :UNIQUE-KEY-NAME
                   "contacts_email_key"))))
    (is (equalp (table-description-menu
                 "s1.contacts" :char-max-length nil :data-type-length nil
                 :has-default nil :default-value nil :not-null nil
                 :numeric-precision nil :numeric-scale nil
                 :storage nil :primary nil :primary-key-name nil
                 :unique nil :unique-key-name nil :fkey t :fkey-name t
                 :fkey-col-id t :fkey-table t :fkey-local-col-id t
                 :identity nil :generated nil :collation nil
                 :col-comments nil :locally-defined nil :inheritance-count nil
                 :stat-collection nil)
                '((:COLUMN-NAME "contact_id" :DATA-TYPE-NAME "int4" :FKEY :NULL :FKEY-NAME :NULL
                   :FKEY-COL-ID :NULL :FKEY-TABLE :NULL :FKEY-LOCAL-COL-ID :NULL)
                  (:COLUMN-NAME "customer_id" :DATA-TYPE-NAME "int4" :FKEY T :FKEY-NAME
                   "fk_customer" :FKEY-COL-ID #(1) :FKEY-TABLE "customers" :FKEY-LOCAL-COL-ID
                   #(2))
                  (:COLUMN-NAME "contact_name" :DATA-TYPE-NAME "varchar"
                   :FKEY :NULL :FKEY-NAME
                         :NULL :FKEY-COL-ID :NULL :FKEY-TABLE :NULL :FKEY-LOCAL-COL-ID :NULL)
                  (:COLUMN-NAME "phone" :DATA-TYPE-NAME "varchar" :FKEY :NULL :FKEY-NAME :NULL
                   :FKEY-COL-ID :NULL :FKEY-TABLE :NULL :FKEY-LOCAL-COL-ID :NULL)
                  (:COLUMN-NAME "email" :DATA-TYPE-NAME "varchar" :FKEY :NULL :FKEY-NAME :NULL
                   :FKEY-COL-ID :NULL :FKEY-TABLE :NULL :FKEY-LOCAL-COL-ID :NULL))))
    (is (equalp (table-description-menu
                 "s1.contacts"
                 :char-max-length nil :data-type-length nil
                 :has-default nil :default-value nil :not-null nil
                 :numeric-precision nil :numeric-scale nil
                 :storage nil :primary nil :primary-key-name nil
                 :unique nil :unique-key-name nil :fkey nil :fkey-name nil
                 :fkey-col-id nil :fkey-table nil :fkey-local-col-id nil
                 :identity nil :generated nil :collation nil
                 :col-comments nil :locally-defined nil :inheritance-count nil
                 :stat-collection t)
                '((:COLUMN-NAME "contact_id" :DATA-TYPE-NAME "int4" :STAT-COLLECTION :NULL)
                  (:COLUMN-NAME "customer_id" :DATA-TYPE-NAME "int4" :STAT-COLLECTION :NULL)
                  (:COLUMN-NAME "contact_name" :DATA-TYPE-NAME "varchar" :STAT-COLLECTION :NULL)
                  (:COLUMN-NAME "phone" :DATA-TYPE-NAME "varchar" :STAT-COLLECTION :NULL)
                  (:COLUMN-NAME "email" :DATA-TYPE-NAME "varchar" :STAT-COLLECTION :NULL))))
    (is (equal (get-schema-comment "s1")
               "This is a comment about the s1 schema which is external looking"))
    (drop-table 's1.customers :cascade t)
    (drop-table 's1.contacts :cascade t)
    (drop-schema 's1 :cascade t)))

(test identity-and-generated
  (with-test-connection
    (drop-table "people" :if-exists t :cascade t)
    (query "CREATE TABLE people (
            height_cm numeric,
            height_in numeric GENERATED ALWAYS AS (height_cm / 2.54) STORED);")
    (is (equalp
         (table-description-menu "people"
                                 :char-max-length nil :data-type-length nil
                                 :has-default nil :default-value nil :not-null nil
                                 :numeric-precision nil :numeric-scale nil
                                 :storage nil :primary nil :primary-key-name nil
                                 :unique nil :unique-key-name nil :fkey nil :fkey-name nil
                                 :fkey-col-id nil :fkey-table nil :fkey-local-col-id nil
                                 :identity t :generated t :collation nil
                                 :col-comments nil :locally-defined nil :inheritance-count nil
                                 :stat-collection nil)
         '((:COLUMN-NAME "height_cm" :DATA-TYPE-NAME "numeric"
            :IDENTITY :NULL :GENERATED :NULL)
           (:COLUMN-NAME "height_in" :DATA-TYPE-NAME "numeric"
            :IDENTITY :NULL :GENERATED "stored"))))
    (drop-table 'people :cascade t)))

(test table-collation
  (with-test-connection
    (drop-table "test_collations" :if-exists t :cascade t)
    (query "CREATE TABLE test_collations (
    a text COLLATE \"C\",
    b text COLLATE \"POSIX\");")

    (is (equalp
         (table-description-menu "test_collations"
                                 :char-max-length nil :data-type-length nil
                                 :has-default nil :default-value nil :not-null nil
                                 :numeric-precision nil :numeric-scale nil
                                 :storage nil :primary nil :primary-key-name nil
                                 :unique nil :unique-key-name nil :fkey nil :fkey-name nil
                                 :fkey-col-id nil :fkey-table nil :fkey-local-col-id nil
                                 :identity nil :generated nil :collation t
                                 :col-comments nil :locally-defined nil :inheritance-count nil
                                 :stat-collection nil)
         '((:COLUMN-NAME "a" :DATA-TYPE-NAME "text" :COLLATION "C")
           (:COLUMN-NAME "b" :DATA-TYPE-NAME "text" :COLLATION "POSIX"))))
    (drop-table 'test-collations :cascade t)))
