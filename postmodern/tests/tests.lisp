;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN-TESTS; -*-
(in-package :postmodern-tests)

;; Adjust the above to some db/user/pass/host combination that refers
;; to a valid postgresql database in which no table named test_data
;; currently exists. Then after loading the file, run the tests with
;; (run! :postmodern)

(def-suite :postmodern
  :description "Test suite for postmodern subdirectory files")

(def-suite :postmodern-base
    :description "Base Test suite for postmodern subdirectory files")

(in-suite :postmodern-base)

(def-suite :postmodern-base
  :description "Base test suite for postmodern"
  :in :postmodern)

(in-suite :postmodern-base)

(defmacro with-binary (&body body)
  `(let ((old-use-binary-parameters (connection-use-binary *database*)))
     (use-binary-parameters *database* t)
     (unwind-protect (progn ,@body)
       (use-binary-parameters *database* old-use-binary-parameters))))

(defmacro without-binary (&body body)
  `(let ((old-use-binary-parameters (connection-use-binary *database*)))
     (use-binary-parameters *database* nil)
     (unwind-protect (progn ,@body)
       (use-binary-parameters *database* old-use-binary-parameters))))

(defun prompt-connection-to-postmodern-db-spec (param-lst)
  "Takes the 6 item parameter list from prompt-connection and restates it for pomo:with-connection. Note that cl-postgres does not provide the pooled connection - that is only in postmodern - so that parameter is not passed."
  (when (and (listp param-lst)
             (= 6 (length param-lst)))
    (destructuring-bind (db user password host port use-ssl) param-lst
      (list db user password host :port port :use-ssl use-ssl))))

(defmacro with-test-connection (&body body)
  `(with-connection (prompt-connection-to-postmodern-db-spec
                     (cl-postgres-tests:prompt-connection))
     ,@body))

(defmacro with-binary-test-connection (&body body)
  `(with-connection (append (prompt-connection-to-postmodern-db-spec
                             (cl-postgres-tests:prompt-connection))
                            '(:use-binary t))
     ,@body))

(defmacro with-pooled-test-connection (&body body)
  `(with-connection (append (prompt-connection-to-postmodern-db-spec
                             (cl-postgres-tests:prompt-connection))
                            '(:pooled-p t))
     ,@body))

(defmacro with-binary-pooled-test-connection (&body body)
  `(with-connection (append (prompt-connection-to-postmodern-db-spec
                             (cl-postgres-tests:prompt-connection))
                            '(:pooled-p t :use-binary t))
     ,@body))

(defmacro protect (&body body)
  `(unwind-protect (progn ,@(butlast body)) ,(car (last body))))

(test connect-sanely
      "Check that with-test-connection and with-binary-test-connection actually connect"
      (with-test-connection
          (is (not (null *database*))))
      (with-binary-test-connection
          (is (not (null *database*)))
        (is (connection-use-binary *database*))))

(defmacro with-application-connection (&body body)
  `(with-connection
       (append
        (prompt-connection-to-postmodern-db-spec (cl-postgres-tests:prompt-connection))
        '(:application-name "george"))
     ,@body))

(test application-name
      (with-application-connection
          (is (equal
               (query "select distinct application_name from pg_stat_activity where application_name = 'george'"
                      :single)
               "george"))))

(test connection-pool
   (let* ((db-params (append (prompt-connection-to-postmodern-db-spec
                                 (cl-postgres-tests:prompt-connection)) '(:pooled-p t)))
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
    (is (connected-p *database*))
    (is (equal (get-search-path)
               "\"$user\", public"))
    (is (equal
         (with-schema ("a")
           (disconnect *database*)
           (reconnect *database*)
           (get-search-path))
         "a"))
    (is (equal (get-search-path)
               "\"$user\", public"))))

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
    (is (= (query (:select '*
                   :from (:as (:select (:as 1 'as)) 'where)
                   :where (:= 'where.as 1)) :single!)
           1))))

(test time-types
  "Ensure that we are using a readtable that reads into simple-dates."
  (setf cl-postgres:*sql-readtable*
        (cl-postgres:copy-sql-readtable
         simple-date-cl-postgres-glue:*simple-date-sql-readtable*))
  (with-test-connection
    (is (time= (query (:select (:type (simple-date:encode-date 1980 2 1) date)) :single)
               (encode-date 1980 2 1)))
    (is (time= (query (:select (:type (simple-date:encode-timestamp 2040 3 19 12 15 0 2) timestamp))
                      :single)
               (encode-timestamp 2040 3 19 12 15 0 2)))
    (is (time= (query (:select (:type (simple-date:encode-interval :month -1 :hour 24) interval))
                      :single)
               (encode-interval :month -1 :hour 24))))
  ;;; Reset readtable to default
  (setf cl-postgres:*sql-readtable*
        (cl-postgres:copy-sql-readtable
         cl-postgres::*default-sql-readtable*)))

(test table-skeleton
  (with-test-connection
    (when (table-exists-p 'test-data) (execute (:drop-table 'test-data)))
    (execute (:create-table test-data ((a :type integer :primary-key t) (b :type real)
                                       (c :type (or text db-null))) (:unique c)))
    (protect
      (is (table-exists-p 'test-data))
      (execute (:insert-into 'test-data :set 'a 1 'b 5.4 'c "foobar"))
      (execute (:insert-into 'test-data :set 'a 2 'b 88 'c :null))
      (is (equal (query (:order-by (:select '* :from 'test-data) 'a))
                 '((1 5.4 "foobar")
                   (2 88.0 :null))))
      (execute (:drop-table 'test-data)))
    (is (not (table-exists-p 'test-data)))))

(test doquery
  (with-test-connection
    (doquery (:select 55 "foobar") (number string)
      (is (= number 55))
      (is (string= string "foobar")))))

(test doquery-params
  (with-test-connection
    (doquery ("select $1::integer + 10" 20) (answer)
      (is (= answer 30)))))


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
             '("george_and_gracie" "public" NIL)))
  (is (equal (split-fully-qualified-tablename "test.uniq.george-and-gracie")
             '("george_and_gracie" "uniq" "test")))
  (is (equal (split-fully-qualified-tablename 'test.uniq.george-and-gracie)
             '("george_and_gracie" "uniq" "test"))))

;; create two tables with the same name in two different
;; namespaces.
(test namespace
  (with-test-connection
    (let ((excess-schemas
            (set-difference (list-schemas)
                            '("public" "information_schema" "uniq")
                            :test #'equal)))
      (when excess-schemas (loop for x in excess-schemas do
        (drop-schema x :cascade 't))))
    (when (table-exists-p 'test-uniq)
      (execute (:drop-table 'test-uniq)))
    (is (schema-exists-p :public))
    (is (not (table-exists-p 'test-uniq)))
    (unless (table-exists-p 'test-uniq)
      (execute (:create-table test-uniq ((value :type integer)))))
    (is (table-exists-p 'test-uniq))
    (is (not (schema-exists-p 'uniq)))
    (is (eq (column-exists-p 'public.test-uniq 'value) t))
    (is (not (eq (column-exists-p 'public.test-uniq 'valuea) t)))
    (is (eq (column-exists-p 'test-uniq 'value 'public) t))
    (is (not (eq (column-exists-p 'test-uniq 'valuea 'public) t)))
    (with-schema ('uniq :if-not-exist :create) ;; changing the search path
      (is (schema-exists-p 'uniq))
      (is (schema-exists-p "uniq"))
      (is (not (table-exists-p 'test-uniq 'uniq)))
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
    (format t "List-schemas ~a~%" (list-schemas))
    (is (equal "uniq" (find "uniq" (list-schemas) :test #'equal)))
    (drop-schema "uniq" :cascade 't)
    (is (not (schema-exists-p "uniq")))
    (create-schema "uniq")
    (is (equal "uniq" (find "uniq" (list-schemas) :test #'equal)))
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

(test list-indices-and-constraints
  "Test various index functions"
  (with-test-connection
    (pomo:drop-table 'people :if-exists t :cascade t)
    (query (:create-table 'people ((id :type (or integer db-null) :primary-key :identity-by-default)
                                   (first-name :type (or (varchar 50) db-null))
                                   (last-name :type (or (varchar 50) db-null)))))
    (query (:create-index 'idx-people-names :on 'people :fields 'last-name 'first-name))
    (query (:create-index 'idx-people-first-names :on 'people :fields 'first-name))
    (query (:insert-rows-into 'people
            :columns 'first-name 'last-name
            :values '(("Eliza" "Gregory")  ("Dean" "Rodgers")  ("Christine" "Alvarez")
                      ("Dennis" "Peterson") ("Ernest" "Roberts") ("Jorge" "Wood")
                      ("Harvey" "Strickland") ("Eugene" "Rivera")
                      ("Tillie" "Bell")  ("Marie" "Lloyd")  ("John" "Lyons")
                      ("Lucas" "Gray")  ("Edward" "May")
                      ("Randy" "Fields")  ("Nell" "Malone")  ("Jacob" "Maxwell")
                      ("Vincent" "Adams") ("Henrietta" "Schneider")
                      ("Ernest" "Mendez")  ("Jean" "Adams")  ("Olivia" "Adams"))))
    (let ((idx-symbol (first (list-indices)))
          (idx-string (first (list-indices t))))
      (is (pomo:index-exists-p idx-symbol))
      (is (pomo:index-exists-p idx-string)))
    (is (equal (list-table-indices 'people)
               '((:IDX-PEOPLE-FIRST-NAMES :FIRST-NAME) (:IDX-PEOPLE-NAMES :FIRST-NAME)
                 (:IDX-PEOPLE-NAMES :LAST-NAME) (:PEOPLE-PKEY :ID))))
    (is (equal (list-table-indices 'people t)
               '(("idx_people_first_names" "first_name") ("idx_people_names" "first_name")
                 ("idx_people_names" "last_name") ("people_pkey" "id"))))
    (is (equal (list-table-indices "people")
               '((:IDX-PEOPLE-FIRST-NAMES :FIRST-NAME) (:IDX-PEOPLE-NAMES :FIRST-NAME)
                 (:IDX-PEOPLE-NAMES :LAST-NAME) (:PEOPLE-PKEY :ID))))
    (is (equal (list-table-indices "people" t)
               '(("idx_people_first_names" "first_name") ("idx_people_names" "first_name")
                 ("idx_people_names" "last_name") ("people_pkey" "id"))))
    (is (equal (list-unique-or-primary-constraints "people" t)
               '(("people_pkey"))))
    (is (equal (list-unique-or-primary-constraints "people")
               '((:PEOPLE-PKEY))))
    (is (equal (length (list-all-constraints 'people))
               2))
    (is (equal (length (list-all-constraints "people"))
               2))
    (query (:alter-table 'people :drop-constraint 'people-pkey))
    (is (equal (length (list-all-constraints "people"))
               1))
    (execute (:drop-table 'people))))

(test drop-indices
  "Test drop index variations"
  (with-test-connection
    (query (:drop-table :if-exists 'george :cascade))
    (query (:create-table 'george ((id :type integer))))
    (query (:create-index 'george-idx :on 'george :fields 'id))
    (is (equal (list-table-indices 'george)
               '((:GEORGE-IDX :ID))))
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
               "DROP INDEX george_idx"))
    (query (:drop-table :if-exists 'george :cascade))))

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

(test schema-comments
  (with-test-connection
    (when (schema-exists-p "schema_1")
      (drop-schema "schema_1" :cascade t))
    (create-schema "schema_1")
    (when (table-exists-p "p1")
      (drop-table "p1" :cascade t))
    (query (:create-table "p1"
                          ((id :type integer :generated-as-identity-always t)
                           (text :type text))))
    (query (:create-table "schema_1.s1"
                          ((id :type integer :generated-as-identity-always t)
                           (text :type text))))
    (add-comment :table 'p1 "a comment on recipes")
    (add-comment :table 'schema-1.s1 "a comment on schema-1 s1")
    (is (equal (get-table-comment 'p1)
               "a comment on recipes"))
    (is (equal (integerp (get-table-oid 'schema-1.s1)) t))
    (is (equal (integerp (get-table-oid 'information-schema.columns)) t))
    (is (equal (get-table-comment 'schema-1.s1)
               "a comment on schema-1 s1"))
    (drop-schema "schema_1" :cascade t)
    (drop-table "p1" :cascade t)))

(test valid-sql-identifier-p
  (is (equal (valid-sql-identifier-p "abc") T))
  (is (equal (valid-sql-identifier-p "_abc3") T))
  (is (equal (valid-sql-identifier-p "abc-3") NIL))
  (is (equal (valid-sql-identifier-p "abc$q7") T))
  (is (equal (valid-sql-identifier-p "_abc;'sd") NIL))
  (is (equal (valid-sql-identifier-p "haček") T))
  (is (equal (valid-sql-identifier-p "Tuđman") T))
  (is (equal (valid-sql-identifier-p "Åland") T))
  (is (equal (valid-sql-identifier-p "hôtel") T))
  (is (equal (valid-sql-identifier-p "piñata") T))
  (is (equal (valid-sql-identifier-p "frappé") T))
  (is (equal (valid-sql-identifier-p "تَشْكِيل") NIL))
  (is (equal (valid-sql-identifier-p "تسير") T))
  (is (equal (valid-sql-identifier-p "إمرأة") T))
  (is (equal (valid-sql-identifier-p "أهلا") T))
  (is (equal (valid-sql-identifier-p "أداة") T))
  (is (equal (valid-sql-identifier-p "شخصا") T))
  (is (equal (valid-sql-identifier-p "새우") T))
  (is (equal (valid-sql-identifier-p "이해") T))
  (is (equal (valid-sql-identifier-p "죄") T))
  (is (equal (valid-sql-identifier-p "国") T))
  (is (equal (valid-sql-identifier-p "高さ") T))
  (is (equal (valid-sql-identifier-p "たかさ") T))
  (is (equal (valid-sql-identifier-p "住所") T))
  (is (equal (valid-sql-identifier-p "会议") T))
  (is (equal (valid-sql-identifier-p "kusipää") T)))

(test rename-table-and-columns
  (with-test-connection
    (when (schema-exists-p 'test-schema)
      (drop-schema 'test-schema :cascade t :if-exists t))
    (create-schema 'test-schema)
    (when (table-exists-p 'test-schema.t1)
      (drop-table 'test-schema.t1 :if-exists t :cascade t))
    (when (table-exists-p 'test-rename-t1)
      (drop-table 'test-rename-t1 :if-exists t :cascade t))
    (when (table-exists-p 'test-rename-t2)
      (drop-table 'test-rename-t2 :if-exists t :cascade t))
    (when (table-exists-p 'test-rename-t3)
      (drop-table 'test-rename-t3 :if-exists t :cascade t))
    (query (:create-table 'test-schema.t1 ((id :type (or integer db-null)))))
    (query (:create-table 'test-rename-t1 ((id :type (or integer db-null)))))
    (is (table-exists-p 'test-schema.t1))
    (is (rename-table 'test-schema.t1 'test-schema.t2))
    (is (table-exists-p 'test-schema.t2))
    (is (rename-table 'test-schema.t2 't3))
    (is (table-exists-p 'test-schema.t3))
    (is (column-exists-p 'test-schema.t3 'id))
    (is (rename-column 'test-schema.t3 'id 'new-id))
    (is (column-exists-p 'test-schema.t3 'new-id))
    (is (rename-column 'test-schema.t3 "new_id" "id"))
    (is (column-exists-p "test-schema.t3" "id"))
    (is (rename-table "test-schema.t3" "t2"))
    (is (table-exists-p "test-schema.t2"))
    (is (table-exists-p 'test-rename-t1))
    (is (rename-table 'test-rename-t1 'test-rename-t2))
    (is (table-exists-p 'test-rename-t2))
    (is (rename-table 'test-rename-t2 'test-rename-t3))
    (is (table-exists-p 'test-rename-t3))
    (is (column-exists-p 'test-rename-t3 'id))
    (is (rename-column 'test-rename-t3 'id 'new-id))
    (is (column-exists-p 'test-rename-t3 'new-id))
    (is (rename-column 'test-rename-t3 "new_id" "id"))
    (is (column-exists-p "test-rename-t3" "id"))
    (is (rename-table "test-rename-t3" "test-rename-t2"))
    (is (table-exists-p "test-rename-t2"))
    (drop-table 'test-rename-t2 :if-exists t :cascade t)
    (drop-table 'test-schema.t2 :if-exists t :cascade t)
    (drop-schema 'test-schema :cascade t :if-exists t)))
