;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

(defun to-identifier (name)
  "Used to allow both strings and symbols as identifier - converts
symbols to string with the S-SQL rules."
  (if (stringp name)
      name
      (to-sql-name name)))

(defun coalesce (&rest args)
  "Returns the first non-NIL, non-NULL (as in :null) argument, or NIL if none are present. Useful for providing a fall-back value for the result of a query, or, when given only one argument, for transforming :nulls to NIL."
  (some (lambda (x) (if (eq x :null) nil x)) args))

(defun database-version ()
  "Returns the version string provided by postgresql of the current postgresql server. E.g. 'PostgreSQL 12.2 on x86_64-pc-linux-gnu, compiled by gcc (Arch Linux 9.3.0-1) 9.3.0, 64-bit'"
  (query (:select (:version)) :single))

(defmacro make-list-query (relkind)
  "Helper macro for the functions that list tables, sequences, and
views."
  `(sql (:order-by (:select 'relname :from 'pg-catalog.pg-class
                            :inner-join 'pg-catalog.pg-namespace :on (:= 'relnamespace 'pg-namespace.oid)
                            :where (:and (:= 'relkind ,relkind)
                                         (:not-in 'nspname (:set "pg_catalog" "pg_toast"))
                                         (:pg-catalog.pg-table-is-visible 'pg-class.oid)))
                   'relname)))

(defmacro make-exists-query (relkind name)
  "Helper macro for the functions that check whether an object
exists."
  `(sql (:select (:exists (:select 'relname :from 'pg_catalog.pg_class :inner-join 'pg_catalog.pg_namespace :on
                                   (:= 'pg_class.relnamespace 'pg_namespace.oid)
                                   :where (:and (:= 'pg_class.relkind ,relkind)
                                                (:= 'pg_namespace.nspname (:any* (:current_schemas "true")))
                                                (:= 'pg_class.relname (to-identifier ,name))))))))

(defun split-fully-qualified-tablename (name)
  "Take a tablename of the form database.schema.table or schema.table or table and return the tablename and the schema name. The name can be a symbol or a string. Returns a list of form '(table schema database. If the tablename is not fully qualified, it will assume that the schema should be \"public\"."
  (destructuring-bind (table &optional schema database)
      (nreverse (split-sequence:split-sequence #\. (to-sql-name name)
                                               :test 'equal))
    (when (not schema) (setf schema "public"))
    (list table schema database)))

;;; Databases

(defun num-records-in-database ()
  "Returns a list of lists with schema, table name and approximate number of records
in the currently connected database."
  (query (:order-by (:select 'schemaname 'relname 'n_live_tup
			     :from 'pg_stat_user_tables)
		    (:desc 'n_live_tup))))

(defun current-database ()
  "Returns the string name of the current database."
  (query (:select (:current-database)) :single))

(defun database-exists-p (database-name)
  "Checks to see if a particular database exists. Returns T if true, nil if not."
  (setf database-name (to-sql-name database-name))
  (if (member database-name (list-databases :size nil) :test 'equal) t nil))

(defun database-size (&optional (name nil))
  "Given the name of a database, will return the name, a pretty-print string of
the size of the database and the size in bytes. If a database name is not provided,
it will return the result for the currently connected database."
  (unless name
    (setf name (current-database)))
  (first (query
          (:select 'datname
                   (:pg-size-pretty
                    (:pg-database-size 'pg-database.oid))
                   (:pg-database-size 'pg-database.oid)
                   :from 'pg-database
                   :where (:= 'datname '$1))
          (to-sql-name name))))

(defun list-databases (&key (order-by-size nil) (size t))
  "Returns a list of lists where each sub-list contains the name of the
database, a pretty-print string of the size of that database and the size in bytes.
The default order is by database name. Pass t as a parameter to :order-by-size for order by size.
Setting size to nil will return just the database names in a single list
ordered by name. This function excludes the template databases."
  (if order-by-size
      (setf order-by-size (sql (:desc (:pg-database-size 'pg-database.oid))))
      (setf order-by-size " datname"))
  (cond (size
         (query
          (:order-by
           (:select 'datname
                    (:pg-size-pretty
                     (:pg-database-size 'pg-database.oid))
                    (:pg-database-size 'pg-database.oid)
                    :from 'pg-database
                    :where (:not (:like 'datname "template%")))
           (:raw order-by-size))))
        (t
         (loop for x in (query
                         (:order-by
                          (:select 'datname
                                   :from 'pg-database
                                   :where (:not (:like 'datname "template%")))
                          (:raw order-by-size)))
            collect (first x)))))

;;;; Schemas
;;;; See namespace.lisp

;;; Sequences
(defun sequence-next (sequence)
  "Shortcut for getting the next value from a sequence. The sequence identifier can be either a string or a symbol, in the latter case it will be converted to a string according to S-SQL rules."
  (query (:select (:nextval (to-identifier sequence))) :single))

(defun create-sequence (name &key temp if-not-exists increment min-value max-value start cache)
  "Create a sequence. Available additional key parameters are
:temp :if-not-exists :increment :min-value :max-value :start and :cache. See
https://www.postgresql.org/docs/current/static/sql-createsequence.html for details on usage."
  (let ((query-string
         (concatenate 'string
                      "CREATE "
                      (if temp "TEMP " "")
                      "SEQUENCE "
                      (if if-not-exists "IF NOT EXISTS " "")
                      (to-sql-name name)
                      (if increment (concatenate 'string " INCREMENT BY " (format nil "~a" increment)) "")
                      (if min-value (concatenate 'string " MINVALUE " (format nil "~a" min-value)) "")
                      (if max-value (concatenate 'string " MAXVALUE " (format nil "~a" max-value)) "")
                      (if start (concatenate 'string " START " (format nil "~a" start)) "")
                      (if cache (concatenate 'string " CACHE " (format nil "~a" cache)) ""))))
    (query query-string)))

(defun drop-sequence (name &key if-exists cascade)
  "Drop a sequence. Name should be quoted. Available key parameters are :if-exists and :cascade"
  (let ((query-string
         (concatenate 'string
                      "DROP "
                      "SEQUENCE "
                      (if if-exists "IF EXISTS " "")
                      (to-sql-name name)
                      (if cascade " CASCADE" ""))))
    (query query-string)))

(defun list-sequences (&optional strings-p)
  "Return a list of the sequences in a database. Turn them into
keywords if strings-p is not true."
  (let ((result (query (make-list-query "S") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun sequence-exists-p (sequence)
  "Tests whether a sequence with the given name exists. The name can be either a string or a symbol."
  (query (make-exists-query "S" (to-sql-name sequence)) :single))


;;;; Tablespaces
(defun list-tablespaces ()
  "Lists the tablespaces in the currently connected database. What are tablespace you ask? Per the Postgresql documentation https://www.postgresql.org/docs/current/manage-ag-tablespaces.html: Tablespaces in PostgreSQL allow database administrators to define locations in the file system where the files representing database objects can be stored. Once created, a tablespace can be referred to by name when creating database objects.

By using tablespaces, an administrator can control the disk layout of a PostgreSQL installation. This is useful in at least two ways. First, if the partition or volume on which the cluster was initialized runs out of space and cannot be extended, a tablespace can be created on a different partition and used until the system can be reconfigured.

Second, tablespaces allow an administrator to use knowledge of the usage pattern of database objects to optimize performance. For example, an index which is very heavily used can be placed on a very fast, highly available disk, such as an expensive solid state device. At the same time a table storing archived data which is rarely used or not performance critical could be stored on a less expensive, slower disk system."
  (loop for x in (query (:order-by (:select (:as 'spcname 'name)
                                            :from 'pg_tablespace)
                                   'spcname))
       collect (first x)))

(defun list-available-types ()
  "List the available data types in the connected postgresql version, It returns a list of lists, each sublist containing the oid (object identifier number) and the name of the data types. E.g. (21 "smallint")"
  (query (:select 'oid (:as (:format-type :oid :NULL) 'typename)
                  :from 'pg-type
                  :where (:= 'typtype "b"))))


;;; Tables
;;; create table can only be done either using a deftable approach or s-sql

(defun list-tables-in-schema (&optional (schema-name "public") (strings-p nil))
  "Returns a list of tables in a particular schema, defaulting to public. If schema-name is :all, it will return all the non-system tables in the database in fully qualified form: e.g. 'public.test_table'. If string-p is t, the names will be returned as strings with underscores converted to hyphens."
  (let ((result (cond
                  ((or (equal schema-name "all")
                       (eq schema-name :all))
                   (query "select table_schema||'.'||table_name
                               from information_schema.tables
                               where table_schema not in ('pg_catalog', 'information_schema')
                               order by table_schema, table_name"))
                  (t
                   (query "((SELECT table_name
                                 FROM information_schema.tables
                                 WHERE (table_schema = $1))
                                 ORDER BY table_name)"
                          (to-sql-name schema-name))))))
    (if strings-p (mapcar 'from-sql-name result) result )))

(defun list-tables (&optional (strings-p nil))
  "Return a list of the tables in the public schema of a database. By default the table names are returned as keywords. They will be returned as lowercase strings if strings-p is true."
  (let ((result (query (make-list-query "r") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun table-exists-p (table-name &optional (schema-name nil))
  "Check whether a table exists in a particular schema. Defaults to the search path. Takes either a string or a symbol for the table name. The table-name can be fully qualified in the form of schema.table-name or database.schema.table-name. If the schema is specified either in a qualified table-name or in the optional schema-name parameter, we look directly to the information schema tables. Otherwise we use the search path which can be controlled by being within a with-schema form."
  (let* ((destructured-table-name (split-fully-qualified-tablename table-name))
         (schema (if schema-name schema-name (second destructured-table-name)))
         (table (or (first destructured-table-name) table-name))
         (result (if schema
                     (member (to-sql-name table)
                             (alexandria:flatten
                              (query (:order-by
                                      (:select 'table-name
                                               :from 'information-schema.tables
                                               :where (:= 'table-schema '$1))
                                      'table-name)
                                     (to-sql-name schema)))
                             :test 'equal)
                     (query (make-exists-query "r" table) :single))))
    (if result t nil)))

(defun drop-table (table-name &key if-exists cascade)
  "Drop a table. Available additional key parameters are :if-exists and :cascade."
  (when (table-exists-p table-name)
    (let ((query-string
           (concatenate 'string
                        "DROP "
                        "TABLE "
                        (if if-exists "IF EXISTS " "")
                        (to-sql-name table-name)
                        (if cascade " CASCADE" ""))))
      (query query-string))))

(defun list-table-sizes (&key (schema "public") (order-by-size nil) (size t))
  "Returns a list of lists (table-name, size in 8k pages) of tables in the current database.
Providing a name to the schema parameter will return just the information for tables in that schema.
It defaults to just the tables in the public schema. Setting schema to nil will return all tables, indexes etc
in the database in descending order of size. This would include system tables, so there
are a lot more than you would expect. If :size is set to nil, it returns only a flat list of table names.
Setting order-by-size to t will return the result in order of size instead of by table name."
  (setf schema (to-sql-name schema))
  (if order-by-size
      (setf order-by-size (sql (:desc 'relpages)))
      (setf order-by-size " relname"))
  (cond ((and size schema)
         (query (:order-by (:select 'relname 'relpages
                                    :from 'pg_class
                                    :where (:in 'relname
                                                (:set (:select 'table-name
                                                               :from 'information-schema.tables
                                                               :where (:= 'table-schema '$1)))))
                           (:raw order-by-size))
                schema))
         (size (query (:order-by (:select 'relname 'relpages
                                       :from 'pg_class)
                                 (:raw order-by-size))))
         (schema (query (:order-by (:select 'relname
                                    :from 'pg_class
                                    :where (:in 'relname
                                                (:set (:select 'table-name
                                                               :from 'information-schema.tables
                                                               :where (:= 'table-schema '$1)))))
                                   'relname)
                        schema))
         (t (loop for x in (query (:order-by (:select 'relname
                                        :from 'pg_class)
                                             'relname))
               collect (first x)))))


(defun table-size (table-name)
  "Return the size of a given postgresql table in k or m. Table-name can be either a string or quoted."
  (query (:select (:pg_size_pretty (:pg_total_relation_size '$1)))
         :single
         (to-sql-name table-name)))

(defun table-description (table-name &optional schema-name)
  "Returns a list of the fields in the named table. Each field is represented by a list of three elements: the field name, the type, and a boolean indicating whether the field may be NULL. Optionally, schema-name can be specified to restrict the result to fields of a table from the named schema. The table and schema names can be either strings or quoted."
  (setf table-name (to-sql-name table-name))
  (when schema-name (setf schema-name (to-sql-name schema-name)))
  (let ((schema-test (if (and schema-name (schema-exists-p schema-name) (table-exists-p table-name))
                         (sql (:= 'pg-namespace.nspname schema-name))
                         "true")))
    (mapcar #'butlast
            (query (:order-by (:select 'attname 'typname (:not 'attnotnull) 'attnum :distinct
                                       :from 'pg-catalog.pg-attribute
                                       :inner-join 'pg-catalog.pg-type :on (:= 'pg-type.oid 'atttypid)
                                       :inner-join 'pg-catalog.pg-class :on (:and (:= 'pg-class.oid 'attrelid)
                                                                                  (:= 'pg-class.relname (to-identifier table-name)))
                                       :inner-join 'pg-catalog.pg-namespace :on (:= 'pg-namespace.oid 'pg-class.relnamespace)
                                       :where (:and (:> 'attnum 0) (:raw schema-test)))
                              'attnum)))))

(defun table-description-plus (table-name)
  "Returns more table info than table-description. Table can be either a string or quoted.
Specifically returns ordinal-position, column-name, data-type, character-maximum-length,
modifier, whether it is not-null and the default value. "
  (query (:order-by (:select (:as 'a.attnum 'ordinal-position)
                             (:as 'a.attname 'column-name)
                             (:as 'tn.typname 'data-type)
                             (:as 'a.attlen  'character-maximum-length)
                             (:as 'a.atttypmod 'modifier)
                             (:as 'a.attnotnull 'notnull)
                             (:as 'a.atthasdef 'hasdefault)
                             :from (:as 'pg_class 'c)
                             (:as 'pg_attribute 'a)
                             (:as 'pg_type 'tn)
                             :where (:and
                                     (:= 'c.relname '$1)
                                     (:> 'a.attnum 0)
                                     (:= 'a.attrelid 'c.oid)
                                     (:= 'a.atttypid 'tn.oid)))
                    'a.attnum)
         (to-sql-name table-name)))

;; Columns
(defun list-columns (table-name)
  "Returns a list of strings of just the column names in a table.
Pulls info from the postmodern table-description function
rather than directly."
  (when (table-exists-p table-name)
    (loop for x in (table-description table-name)
       collect (first x))))

(defun list-columns-with-types (table-name)
  "Return a list of (name type) lists for the fields of a table. Goes directly to the pg-catalog tables."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (query
     (:select (:as 'a.attname 'column)
              (:as (:pg-catalog.format_type 'a.atttypid  'a.atttypmod)
                   'datatype)
              :from (:as 'pg-catalog.pg-attribute 'a)
              :where (:and
                      (:> 'a.attnum 0)
                      (:not 'a.attisdropped)
                      (:= 'a.attrelid
                          (:select
                           'c.oid
                           :from (:as 'pg-catalog.pg-class 'c)
                           :left-join (:as 'pg-catalog.pg-namespace 'n)
                           :on (:= 'n.oid 'c.relnamespace)
                           :where (:and
                                   (:= 'c.relname '$1)
                                   (:pg-catalog.pg-table-is-visible 'c.oid))))))
     table-name)))

(defun column-exists-p (table-name column-name)
  "Determine if a particular column exists. Table name and column-name can be either strings or symbols."
  (query (:select 'attname :from 'pg_attribute
		  :where (:= 'attrelid
			     (:select 'oid :from 'pg-class
				      :where (:and (:= 'relname '$1)
						   (:= 'attname '$2)))))
         (to-sql-name table-name) (to-sql-name column-name)
         :single))

;;; Views
(defun list-views (&optional strings-p)
  "Returns list of the user defined views in the current database. When strings-p is T, the names will be returned as strings, otherwise as keywords."
  (let ((result (query (make-list-query "v") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun view-exists-p (view)
  "Tests whether a view with the given name exists. Takes either a string or a symbol for the view name."
  (query (make-exists-query "v" view) :single))

(defun describe-views (&optional (schema "public"))
  "Describe the current views in the specified schema. Includes the select statements used to create the view. Takes an optional schema
name but defaults to public schema."
  (setf schema (to-sql-name schema))
  (query
   (:order-by
    (:select 'c.oid 'c.xmin 'c.relname
	     (:as (:pg_get_userbyid 'c.relowner) 'viewowner)
	     'c.relacl 'description
	     (:as (:pg_get-viewdef 'c.oid 't) 'code)
	     :from (:as 'pg_class 'c)
	     :left-join (:as 'pg_description 'des)
	     :on (:and (:= 'des.objoid 'c.oid)
		       (:= 0 'des.objsubid))
	     :left-join (:as 'pg_catalog.pg_namespace 'n)
	     :on (:= 'n.oid 'c.relnamespace)
	     :where
             (:and
              (:or
               (:and 'c.relhasrules
                     (:exists
                      (:select 'r.rulename
                               :from (:as 'pg_rewrite 'r)
                               :where (:and (:= 'r.ev_class 'c.oid)
                                            (:= (:bpchar 'r.ev_type)
                                                (:type "I" bpchar))))))
               (:= 'c.relkind (:type "v" char)))
              (:= 'n.nspname '$1)))
    'relname)
   schema))

;;;; Functions
(defun list-database-functions ()
  "Returns a list of the functions in the database from the information_schema."
  (query (:select 'routine-name :from 'information-schema.routines
                  :where
                  (:and
                   (:not-in 'specific-schema
                            (:set "pg_catalog" "information-schema"))
                   (:!= 'type-udt-name "trigger")))))

;;;; Indices
(defun index-exists-p (index-name)
  "Tests whether an index with the given name exists. The name can be either a string or a symbol."
  (query (make-exists-query "i" (to-sql-name index-name)) :single))

(defun create-index (name  &key unique if-not-exists concurrently on using fields)
  "Create an index. Slightly less sophisticated than the query version because it does not have a where clause capability."
  (let ((query-string
         (concatenate 'string
                      "CREATE "
                      (if unique "UNIQUE " "")
                      "INDEX "
                      (if concurrently "CONCURRENTLY " "")
                      (if if-not-exists "IF NOT EXISTS " "")
                      (to-sql-name name)
                      " ON "
                      (to-sql-name on)
                      " "
                      (if using (to-sql-name using) "")
                      " ("
                      (format nil "~{ ~a~^, ~}" (mapcar #'to-sql-name fields))
                      ") "
                      )))
    (query query-string)))

(defun drop-index (name &key concurrently if-exists cascade)
  "Drop an index. Available keys are :concurrently, :if-exists, and :cascade."
  (let ((query-string
         (concatenate 'string
                      "DROP "
                      "INDEX "
                      (if concurrently "CONCURRENTLY " "")
                      (if if-exists "IF EXISTS " "")
                      (to-sql-name name)
                      (if cascade " CASCADE" ""))))
    (query query-string)))


(defun list-indices (&optional strings-p)
  "Return a list of the indexs in a database. Turn them into keywords if strings-p is not true."
  (let ((result (query (make-list-query "i") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun list-table-indices (table-name &optional strings-p)
  "List the index names and the related columns in a single table. Each index will be in a separate sublist."
  (when (table-exists-p (to-sql-name table-name))
    (let ((result (query
                   (:order-by
                    (:select
                     (:as 'i.relname 'index-name) (:as 'a.attname 'column-name)
                     :from (:as 'pg-class 't1) (:as 'pg-class 'i) (:as 'pg-index 'ix)
                     (:as 'pg-attribute 'a)
                     :where
                     (:and (:= 't1.oid 'ix.indrelid)
                           (:= 'i.oid 'ix.indexrelid)
                           (:= 'a.attrelid 't1.oid)
                           (:= 'a.attnum (:any* 'ix.indkey))
                           (:= 't1.relkind "r")
                           (:= 't1.relname '$1)))
                    'i.relname)
                   (to-sql-name table-name))))
      (if strings-p result (loop for x in result collect (mapcar 'from-sql-name x))))))

(defun list-indexed-column-and-attributes (table-name)
  "List the indexed columns and their attributes in a table. Includes primary key."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
   (query
    (:select 'pg_attribute.attname
             (:format_type 'pg_attribute.atttypid 'pg_attribute.atttypmod)
             :from 'pg_index 'pg_class 'pg_attribute
             :where (:and (:= 'pg_class.oid (:type '$1 :regclass))
                          (:= 'indrelid 'pg_class.oid)
                          (:= 'pg_attribute.attrelid 'pg_class.oid)
                          (:= 'pg_attribute.attnum
                              (:any* 'pg_index.indkey))))
    table-name)))

(defun list-index-definitions (table-name)
  "Returns a list of the definitions used to create the current indexes for the table."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (query (:select (:pg_get_indexdef 'indexrelid)
                    :from 'pg_index
                    :where (:= 'indrelid (:type '$1 :regclass)))
	   table-name)))

;;;; Keys
(defun find-primary-key-info (table &optional (just-key nil))
  "Returns a list of sublists where the sublist contains two strings. If a table primary key consists of only one column, such as 'id' there will be a single sublist where the first string is the name of the column and the second string is the string name for the datatype for that column. If the primary key for the table consists of more than one column, there will be a sublist for each column subpart of the key. The sublists will be in the order they are used in the key, not in the order they appear in the table. If just-key is set to t, the list being returned will contain just the column names in the primary key as string names with no sublists. If the table is not in the public schema, provide the fully qualified table name e.g. schema-name.table-name."
  (when (symbolp table) (setf table (s-sql:to-sql-name table)))
  (let ((info (query (:order-by
         (:select
          'a.attname
          (:format-type 'a.atttypid 'a.atttypmod)
          :from
          (:as 'pg-attribute 'a)
          :inner-join (:as (:select '*
                                   (:as (:generate-subscripts 'indkey 1) 'indkey-subscript)
                                   :from 'pg-index)
                          'i)
          :on
          (:and 'i.indisprimary
                (:= 'i.indrelid  'a.attrelid)
                (:= 'a.attnum  (:[] 'i.indkey 'i.indkey-subscript)))
          :where
          (:= 'a.attrelid  (:type '$1 regclass)))
         'i.indkey-subscript)
                     table)))
    (if just-key (loop for x in info collect (first x)) info)))

(defun list-foreign-keys (table schema)
  "Returns a list of sublists of foreign key info in the form of
   '((constraint-name local-table local-table-column
     foreign-table-name foreign-column-name))"
  (setf table (s-sql:to-sql-name table))
  (query
   (:select
    (:as 'conname 'constraint-name)
    table
    (:as 'att2.attname 'local-column)
    (:as 'cl.relname 'foreign-table-name)
    (:as 'att.attname 'foreign-table-column)
    :from
    (:as (:select
          (:as (:unnest 'con1.conkey) 'parent)
          (:as (:unnest 'con1.confkey) 'child)
          'con1.confrelid
          'con1.conrelid
          'con1.conname
          :from
          (:as 'pg-class 'cl)
          :inner-join (:as 'pg-namespace 'ns)
          :on (:= 'cl.relnamespace 'ns.oid)
          :inner-join (:as 'pg-constraint 'con1)
          :on (:= 'con1.conrelid 'cl.oid)
          :where
          (:and (:= 'cl.relname '$1)
                (:= 'ns.nspname '$2)
                (:= 'con1.contype "f")))
         'con)
    :inner-join (:as 'pg-attribute 'att)
    :on
    (:and (:= 'att.attrelid 'con.confrelid)
          (:= 'att.attnum 'con.child))
    :inner-join (:as 'pg-class 'cl)
    :on
    (:= 'cl.oid 'con.confrelid)
    :inner-join (:as 'pg-attribute 'att2)
    :on
    (:and (:= 'att2.attrelid 'con.conrelid)
          (:= 'att2.attnum 'con.parent)))
   table schema))

;;;; Constraints
(defun list-unique-or-primary-constraints (table-name &optional (strings-p))
  "List constraints on a table. Table-name
can be either a string or quoted. Turns constraints into keywords if strings-p is not true."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (let ((result (query (:select 'relname
                          :from 'pg-class
                          :where
                          (:in 'oid (:select 'indexrelid
                                             :from 'pg-index 'pg-class
                                             :where (:and
                                                     (:= 'pg-class.relname '$1)
                                                     (:= 'pg-class.oid 'pg-index.indrelid)
                                                     (:or (:= 'indisunique "t")
                                                          (:= 'indisprimary "t"))))))
                         table-name)))
      (if strings-p result (loop for x in result collect (mapcar 'from-sql-name x))))))

(defun list-all-constraints (table-name &optional (strings-p))
  "Uses information_schema to list all the constraints in a table. Table-name
can be either a string or quoted. Turns constraints into keywords if strings-p is not true."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (let ((result (query (:select 'constraint-name 'constraint-type
		                       :from 'information-schema.table-constraints
		                       :where (:= 'table-name '$1))
                         table-name)))
      (if strings-p result (loop for x in result collect (mapcar 'from-sql-name x))))))

(defun describe-constraint (table-name constraint-name)
  "Return a list of alists of the descriptions a particular constraint given
the table-name and the  constraint name using the information_schema
table."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (first
     (query
      (:select 'tc.constraint-name
               'tc.constraint-type
               'tc.table-name
               'kcu.column-name
               'tc.is-deferrable
               'tc.initially-deferred
               (:as 'rc.match-option 'match-type)
               (:as 'rc.update-rule 'on-update)
               (:as 'rc.delete-rule 'on-delete)
               (:as 'ccu.table-name 'references-table)
               (:as 'ccu.column-name 'references-field)
               :from (:as 'information-schema.table-constraints 'tc)
               :left-join (:as 'information-schema.key-column-usage 'kcu)
               :on (:and (:= 'tc.constraint-catalog 'kcu.constraint-catalog)
                         (:= 'tc.constraint-schema 'kcu.constraint-schema)
                         (:= 'tc.constraint-name 'kcu.constraint-name))
               :left-join (:as 'information-schema.referential-constraints
                               'rc)
               :on (:and (:= 'tc.constraint-catalog 'rc.constraint-catalog)
                         (:= 'tc.constraint-schema 'rc.constraint-schema)
                         (:= 'tc.constraint-name 'rc.constraint-name))
               :left-join (:as 'information-schema.constraint-column-usage
                               'ccu)
               :on (:and (:= 'rc.unique-constraint-catalog
                             'ccu.constraint-catalog)
                         (:= 'rc.unique-constraint-schema
                             'ccu.constraint-schema)
                         (:= 'rc.unique-constraint-name 'ccu.constraint-name))
               :where (:and (:= 'tc.table-name '$1)
                            (:= 'tc.constraint-name
                                (to-sql-name constraint-name))))
      table-name :alists))))

(defun describe-foreign-key-constraints ()
  "Generates a list of lists of information on the foreign key constraints"
  (query (:order-by (:select 'conname
                             (:as 'conrelid 'table)
                             (:as 'pgc.relname 'tabname)
                             (:as 'a.attname 'columns)
                             (:as 'confrelid 'foreign-table)
                             (:as 'pgf.relname 'ftabname)
                             (:as 'af.attname 'fcolumn)
                             :from
                             (:as 'pg_attribute 'af)
                             (:as 'pg_attribute 'a)
                             (:as 'pg_class 'pgc)
                             (:as 'pg_class 'pgf)
                             (:as
                              (:select 'conname 'conrelid 'confrelid
                                       (:as (:[] 'conkey 'i) 'conkey)
                                       (:as (:[] 'confkey 'i) 'confkey)
                                       :from (:as
                                              (:select
                                               'conname
                                               'conrelid 'confrelid
                                               'conkey 'confkey
                                               (:as
                                                (:generate-series
                                                 '1
                                                 (:array-upper 'conkey 1))
                                                'i)
                                               :from 'pg_constraint
                                               :where (:= 'contype "f" ))
                                              'ss)
                                       ) 'ss2)
                             :where (:and (:= 'af.attnum 'confkey)
                                          (:= 'af.attrelid 'confrelid)
                                          (:= 'a.attnum 'conkey)
                                          (:= 'a.attrelid 'conrelid)
                                          (:= 'pgf.relfilenode 'confrelid)
                                          (:= 'pgc.relfilenode 'conrelid)))
                    'ftabname 'fcolumn 'tabname 'columns)))


;;;; Triggers
(defun describe-triggers ()
  "List detailed information on the triggers from the information_schema table."
  (query
   (:select '*
            :from 'information-schema.triggers
            :where
            (:not-in 'trigger-schema
                     (:set "pg_catalog" "information_schema")))))

(defun list-triggers (&optional table-name)
  "List distinct trigger names from the information_schema table. Table-name can be either quoted or string. (A trigger is a specification that the database should automatically execute a particular function whenever a certain type of operation is performed. Triggers can be attached to tables (partitioned or not), views, and foreign tables. See https://www.postgresql.org/docs/current/trigger-definition.html)"
  (if table-name
      (progn
        (setf table-name (to-sql-name table-name))
        (when (table-exists-p table-name)
          (loop for x in (query
                          (:select (:as 'trg.tgname 'trigger-name)
                                   :from (:as 'pg-trigger 'trg) (:as 'pg-class 'tbl)
                                   :where (:and (:= 'trg.tgrelid 'tbl.oid)
                                                (:= 'tbl.relname '$1)))
                          table-name)
               collect (first x))))
      (loop for x in (query
                      (:select 'trigger-name :distinct
                               :from 'information-schema.triggers
                               :where
                               (:not-in 'trigger-schema
                                        (:set "pg-catalog" "information-schema"))))
           collect (first x))))

(defun list-detailed-triggers ()
  "DEPRECATED FOR DESCRIBE-TRIGGERS.List detailed information on the triggers from the information_schema table."
  (query
   (:select '*
            :from 'information-schema.triggers
            :where
            (:not-in 'trigger-schema
                     (:set "pg_catalog" "information_schema")))))



;;; Roles
(defun list-database-users ()
  "List database users (actually 'roles' in Postgresql terminology)."
  (loop for x in (query (:order-by
                         (:select 'usename :from 'pg_user)
                         'usename))
     collect (first x)))

(defun list-roles (&optional (lt nil))
  "Returns a list of alists of rolenames, role attributes and membership in roles.
See https://www.postgresql.org/docs/current/role-membership.html for an explanation.
The optional parameter can be used to set the return list types to :alists or :plists."
  (let ((sql-query "SELECT r.rolname, r.rolsuper, r.rolinherit,
  r.rolcreaterole, r.rolcreatedb, r.rolcanlogin,
  r.rolconnlimit, r.rolvaliduntil,
  ARRAY(SELECT b.rolname
        FROM pg_catalog.pg_auth_members m
        JOIN pg_catalog.pg_roles b ON (m.roleid = b.oid)
        WHERE m.member = r.oid) as memberof
  , r.rolreplication
  , r.rolbypassrls
  FROM pg_catalog.pg_roles r
  WHERE r.rolname !~ '^pg_'
  ORDER BY 1;"))
    (cond ((equal lt :alists)
           (query  sql-query :alists))
          ((equal lt :plists)
           (query  sql-query :plists))
          (t (query sql-query)))))


;;;; Misc that need to be reorganized

(defun change-toplevel-database (new-database user password host)
  "Just changes the database assuming you are using a toplevel connection.
Recommended only for development work. Returns the name of the newly
connected database as a string."
  (disconnect-toplevel)
  (connect-toplevel (to-sql-name new-database) user password host)
  (current-database))

(defun list-connections ()
  "List the current postgresql connections to the currently connected database. It does this by returningo info from pg_stat_activity on open connections."
  (query (:select '* :from 'pg-stat-activity)))

(defun list-available-extensions ()
  "List the postgresql extensions which are available in the system to the currently connected database. The extensions may or may not be installed."
  (loop for x in (query (:order-by (:select 'name :from 'pg-available-extensions)
                                   'name))
     collect (first x)))

(defun list-installed-extensions ()
  "List the postgresql extensions which are installed in the currently connected database."
  (loop for x in (query (:order-by (:select 'extname :from 'pg-extension)
                                   'extname))
       collect (first x)))

(defun replace-non-alphanumeric-chars (str &optional (replacement #\_))
  "Takes a string and a replacement char and replaces any character which is not alphanumeric or an asterisk
with a specified character - by default an underscore and returns the modified string."
  (let ((str1 str))
        (with-output-to-string (*standard-output*)
                               (loop :for ch :of-type character :across str1
                                     :do (if (or (eq ch #\*)
                                                 (alphanumericp ch))
                                             (write-char ch)
                                             (write-char replacement))))))

(defun cache-hit-ratio ()
  "The cache hit ratio shows data on serving the data from memory compared to how often you have to go to disk.
This function returns a list of heapblocks read from disk, heapblocks hit from memory and the ratio of
heapblocks hit from memory / total heapblocks hit.
Borrowed from: https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/"
 (query "SELECT
           sum(heap_blks_read) as heap_read,
           sum(heap_blks_hit)  as heap_hit,
           sum(heap_blks_hit) / (sum(heap_blks_hit) + sum(heap_blks_read)) as ratio
         FROM
           pg_statio_user_tables;"))

(defun bloat-measurement ()
  "Bloat measurement of unvacuumed dead tuples. Borrowed from: https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/ who
borrowed it from https://github.com/heroku/heroku-pg-extras/tree/master/commands."
  (query "WITH constants AS (
  SELECT current_setting('block_size')::numeric AS bs, 23 AS hdr, 4 AS ma
), bloat_info AS (
  SELECT
    ma,bs,schemaname,tablename,
    (datawidth+(hdr+ma-(case when hdr%ma=0 THEN ma ELSE hdr%ma END)))::numeric AS datahdr,
    (maxfracsum*(nullhdr+ma-(case when nullhdr%ma=0 THEN ma ELSE nullhdr%ma END))) AS nullhdr2
  FROM (
    SELECT
      schemaname, tablename, hdr, ma, bs,
      SUM((1-null_frac)*avg_width) AS datawidth,
      MAX(null_frac) AS maxfracsum,
      hdr+(
        SELECT 1+count(*)/8
        FROM pg_stats s2
        WHERE null_frac<>0 AND s2.schemaname = s.schemaname AND s2.tablename = s.tablename
      ) AS nullhdr
    FROM pg_stats s, constants
    GROUP BY 1,2,3,4,5
  ) AS foo
), table_bloat AS (
  SELECT
    schemaname, tablename, cc.relpages, bs,
    CEIL((cc.reltuples*((datahdr+ma-
      (CASE WHEN datahdr%ma=0 THEN ma ELSE datahdr%ma END))+nullhdr2+4))/(bs-20::float)) AS otta
  FROM bloat_info
  JOIN pg_class cc ON cc.relname = bloat_info.tablename
  JOIN pg_namespace nn ON cc.relnamespace = nn.oid AND nn.nspname = bloat_info.schemaname AND nn.nspname <> 'information_schema'
), index_bloat AS (
  SELECT
    schemaname, tablename, bs,
    COALESCE(c2.relname,'?') AS iname, COALESCE(c2.reltuples,0) AS ituples, COALESCE(c2.relpages,0) AS ipages,
    COALESCE(CEIL((c2.reltuples*(datahdr-12))/(bs-20::float)),0) AS iotta -- very rough approximation, assumes all cols
  FROM bloat_info
  JOIN pg_class cc ON cc.relname = bloat_info.tablename
  JOIN pg_namespace nn ON cc.relnamespace = nn.oid AND nn.nspname = bloat_info.schemaname AND nn.nspname <> 'information_schema'
  JOIN pg_index i ON indrelid = cc.oid
  JOIN pg_class c2 ON c2.oid = i.indexrelid
)
SELECT
  type, schemaname, object_name, bloat, pg_size_pretty(raw_waste) as waste
FROM
(SELECT
  'table' as type,
  schemaname,
  tablename as object_name,
  ROUND(CASE WHEN otta=0 THEN 0.0 ELSE table_bloat.relpages/otta::numeric END,1) AS bloat,
  CASE WHEN relpages < otta THEN '0' ELSE (bs*(table_bloat.relpages-otta)::bigint)::bigint END AS raw_waste
FROM
  table_bloat
    UNION
SELECT
  'index' as type,
  schemaname,
  tablename || '::' || iname as object_name,
  ROUND(CASE WHEN iotta=0 OR ipages=0 THEN 0.0 ELSE ipages/iotta::numeric END,1) AS bloat,
  CASE WHEN ipages < iotta THEN '0' ELSE (bs*(ipages-iotta))::bigint END AS raw_waste
FROM
  index_bloat) bloat_summary
ORDER BY raw_waste DESC, bloat DESC"))

(defun unused-indexes ()
  "Returns a list of lists showing schema.table, indexname, index_size and number of scans. The code was borrowed from: https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/"
  (query "SELECT
            schemaname || '.' || relname AS table,
            indexrelname AS index,
            pg_size_pretty(pg_relation_size(i.indexrelid)) AS index_size,
            idx_scan as index_scans
         FROM pg_stat_user_indexes ui
         JOIN pg_index i ON ui.indexrelid = i.indexrelid
         WHERE NOT indisunique AND idx_scan < 50 AND pg_relation_size(relid) > 5 * 8192
         ORDER BY pg_relation_size(i.indexrelid) / nullif(idx_scan, 0) DESC NULLS FIRST,
          pg_relation_size(i.indexrelid) DESC;"))

(defun check-query-performance (&optional (ob nil) (num-calls 100) (limit 20))
  "This function requires that postgresql extension pg_stat_statements must be loaded via shared_preload_libraries.
It is borrowed from https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/.
Optional parameters OB allow order-by to be 'calls', 'total-time', 'rows-per' or 'time-per', defaulting to time-per.
num-calls to require that the number of calls exceeds a certain threshold, and limit to limit the number of rows returned.
It returns a list of lists, each row containing the query, number of calls, total_time, total_time/calls, stddev_time, rows,
rows/calls and the cache hit percentage."
  (unless (or (eql ob "calls")
              (eql ob "total-time")
              (eql ob "rows-per")
              (eql ob "time-per"))
    (setf ob "time-per"))
  (setf ob (with-output-to-string (*standard-output*)
             (loop :for ch :of-type character :across ob
                :do (if (or (eq ch #\*)
                            (alphanumericp ch))
                        (write-char ch)
                        (write-char #\_)))))
  (let ((sql-statement (format nil
                               "SELECT query,
       calls,
       total_time,
       total_time / calls as time_per,
       stddev_time,
       rows,
       rows / calls as rows_per,
       100.0 * shared_blks_hit / nullif(shared_blks_hit + shared_blks_read, 0) AS hit_percent
FROM pg_stat_statements
WHERE query not similar to '%pg_%'
and calls > ~a
ORDER BY ~a
DESC LIMIT ~a;" num-calls ob limit)))
    (query sql-statement)))
