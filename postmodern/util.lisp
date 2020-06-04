;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

(defun to-identifier (name)
  "Used to allow both strings and symbols as identifier - converts
symbols to string with the S-SQL rules."
  (if (stringp name)
      name
      (to-sql-name name)))

(defun coalesce (&rest args)
  "Returns the first non-NIL, non-NULL (as in :null) argument, or NIL if none
are present. Useful for providing a fall-back value for the result of a query,
or, when given only one argument, for transforming :nulls to NIL."
  (some (lambda (x) (if (eq x :null) nil x)) args))

(defun database-version ()
  "Returns the version string provided by postgresql of the current postgresql
server. E.g. 'PostgreSQL 12.2 on x86_64-pc-linux-gnu, compiled by
gcc (Arch Linux 9.3.0-1) 9.3.0, 64-bit'"
  (query (:select (:version)) :single))

(defmacro make-list-query (relkind)
  "Helper macro for the functions that list tables, sequences, and views."
  `(sql (:order-by (:select 'relname :from 'pg-catalog.pg-class
                    :inner-join 'pg-catalog.pg-namespace
                    :on (:= 'relnamespace 'pg-namespace.oid)
                    :where (:and (:= 'relkind ,relkind)
                                 (:not-in 'nspname
                                          (:set "pg_catalog" "pg_toast"))
                                 (:pg-catalog.pg-table-is-visible
                                  'pg-class.oid)))
                   'relname)))

(defmacro make-exists-query (relkind name)
  "Helper macro for the functions that check whether an object exists. Only
works for public schema"
  `(sql (:select (:exists (:select 'relname
                           :from 'pg_catalog.pg_class
                           :inner-join 'pg_catalog.pg_namespace :on
                           (:= 'pg_class.relnamespace 'pg_namespace.oid)
                           :where (:and (:= 'pg_class.relkind ,relkind)
                                        (:= 'pg_namespace.nspname
                                            (:any* (:current_schemas "true")))
                                        (:= 'pg_class.relname
                                            (to-identifier ,name))))))))

(defun split-fully-qualified-tablename (name)
  "Take a tablename of the form database.schema.table or schema.table or table
and return the tablename and the schema name. The name can be a symbol or a
string. Returns a list of form '(table schema database. If the tablename is not
fully qualified, it will assume that the schema should be \"public\"."
  (destructuring-bind (table &optional schema database)
      (nreverse (split-sequence:split-sequence #\. (to-sql-name name)
                                               :test 'equal))
    (when (not schema) (setf schema "public"))
    (list table schema database)))

(defun postgresql-version ()
  "Returns the version of the current postgresql instance from postgresql itself
instead of from the connection."
  (query (:select (:version)) :single))

(defun add-comment (type name comment &optional (second-name ""))
  "Attempts to add a comment to a particular database object. The first
parameter is a keyword for the type of database object. The second parameter is
the name of the object. The third parameter is the comment itself. Some objects
require an additional identifier. The names can be strings or symbols.

Example usage would be:
 (add-comment :column 'country-locations.name \"Is what it looks like - the name
of a country\".)

 (add-comment :column \"country_locations.name\" \"Is what it looks like - the
name of a country\".)

Example usage where two identifiers are required would be constraints:

 (add-comment :constraint 'constraint1  \"Some kind of constraint descriptions
here\". 'country-locations)"

  (setf name (to-sql-name name))
  (setf second-name (to-sql-name second-name))
  (case type
    (:access-method (query (format nil "comment on ACCESS METHOD ~a is '~a'"
                                   name comment)))
    (:aggregate (query (format nil "comment on AGGREGATE ~a is '~a'"
                               name comment)))
    (:cast (query (format nil "comment on CAST (~a as ~a) is '~a'"
                          name second-name comment)))
    (:column (query (format nil "comment on COLUMN ~a is '~a'"
                            name comment)))
    (:conversion (query (format nil "comment on CONVERSION m~a is '~a'"
                                name comment)))
    (:constraint (query (format nil "comment on CONSTRAINT ~a on ~a is '~a'"
                                name second-name comment)))
    (:domain-constraint (query (format nil "comment on CONSTRAINT ~a on DOMAIN
~a is '~a'"  name second-name comment)))
    (:database (query (format nil "comment on DATABASE ~a is '~a'"
                              name comment)))
    (:domain (query (format nil "comment on DOMAIN ~a is '~a'"  name comment)))
    (:extension (query (format nil "comment on EXTENSION ~a is '~a'"
                               name comment)))
    (:foreign-data-wrapper (query (format nil "comment on FOREIGN DATA WRAPPER
~a is '~a'"  name comment)))
    (:foreign-table (query (format nil "comment on FOREIGN TABLE ~a is '~a'"
                                   name comment)))
    (:function (query (format nil "comment on FUNCTION ~a is '~a'"
                              name comment)))
    (:index (query (format nil "comment on INDEX ~a is '~a'"
                           name comment)))
    (:language (query (format nil "comment on LANGUAGE ~a is '~a'"
                              name comment)))
    (:large-object (query (format nil "comment on LARGE OBJECT ~a is '~a'"
                                  name comment)))
    (:materialized-view (query (format nil "comment on MATERIALIZED VIEW ~a is
'~a'"  name comment)))
    (:operator (query (format nil "comment on OPERATOR ~a is '~a'"
                              name  comment)))
    (:operator-class (query (format nil "comment on OPERATOR CLASS ~a USING ~a
is '~a'"  name second-name comment)))
    (:operator-family (query (format nil "comment on OPERATOR FAMILY ~a USING
~a is '~a'"  name second-name comment)))
    (:policy (query (format nil "comment on POLICY ~a on ~a is '~a'"
                            name second-name comment)))
    (:procedure (query (format nil "comment on PROCEDURE ~a is '~a'"
                               name comment)))
    (:role (query (format nil "comment on ROLE ~a is '~a'"  name comment)))
    (:rule (query (format nil "comment on RULE ~a on ~a is '~a'"
                          name second-name comment)))
    (:schema (query (format nil "comment on SCHEMA ~a is '~a'"  name comment)))
    (:sequence (query (format nil "comment on SEQUENCE ~a is '~a'"
                              name comment)))
    (:server (query (format nil "comment on SERVER ~a is '~a'"  name comment)))
    (:statistics (query (format nil "comment on STATisTICS ~a is '~a'"
                                name comment)))
    (:table (query (format nil "comment on TABLE ~a is '~a'"  name comment)))
    (:tablespace (query (format nil "comment on TABLESPACE ~a is '~a'"
                                name comment)))
    (:text-search-configuration (query (format nil "comment on TEXT SEARCH
CONFIGURATIon ~a is '~a'"  name comment)))
    (:text-search-dictionary (query (format nil "comment on TEXT SEARCH
DICTIONARY ~a is '~a'"  name comment)))
    (:text-search-parser (query (format nil "comment on TEXT SEARCH PARSER ~a
is '~a'"  name comment)))
    (:text-search-template (query (format nil "comment on TEXT SEARCH TEMPLATE
~a is '~a'"  name comment)))
    (:trigger (query (format nil "comment on TRIGGER ~a on ~a is '~a'"
                             name second-name comment)))
    (:type (query (format nil "comment on TYPE ~a is '~a'"  name comment)))
    (:view (query (format nil "comment on VIEW ~a is '~a'"  name comment)))))

;;; Databases
(define-condition invalid-database-name (error)
  ((text :initarg :text :reader text))
  (:documentation "Invalid-database-name indicates that this database does not
exist in this cluster or the user does not have the permissions necessary to
access this database."))

(defun database-exists-p (database)
  "Returns database name string if the database parameter is actually an
available database"
  (query
       (:select
        (:exists
         (:select 'datname
          :from 'pg-database
          :where (:= 'datname '$1))))
       (to-sql-name database)
       :single))

(defparameter *character-sets*
  '("EUC_CN" "EUC_JP" "EUC_JIS_2004" "EUC_KR" "EUC_TW" "ISO_8859_5" "ISO_8859_6"
    "ISO_8859_7" "ISO_8859_8" "KOI8R" "KOI8U" "LATIN1" "LATIN2" "LATIN3" "LATIN4"
    "LATIN5" "LATIN6" "LATIN7" "LATIN8" "LATIN9" "LATIN10" "MULE_INTERNAL"
    "SQL_ASCII" "UTF8" "WIN866" "WIN874" "WIN1250" "WIN1251" "WIN1252" "WIN1253"
    "WIN1254" "WIN1255" "WIN1256" "WIN1257" "WIN1258"))

(defparameter *collations* nil)

(defun list-available-collations ()
  "Get a list of the collations available from the current database cluster.
Collations are a mess as different operating systems provide different
collations. We might get some sanity if Postgresql can use ICU as the default.
See https://wiki.postgresql.org/wiki/Collations."
  (setf *collation-support* (query "select collname from pg_collation")))

(defun collation-exists-p (collation)
  "This function does require the parameter to be a string and properly upper
and lower cased."
  (query "select collname from pg_collation where collname = $1"
         collation :single))

(defun character-set-exists-p (char-support)
  "There is no good way that I know to determine the availble character sets
on a remote server so we just assume any postgresql usable set is available."
  (member char-support *character-sets* :test 'equalp))

(defun create-database (database-name &key (encoding "UTF8") (owner "")
                                        (connection-limit -1)
                                        (limit-public-access nil)
                                        (comment nil)
                                        (collation nil)
                                        (template "template1"))
  "Creates a basic database. Besides the obvious database-name parameter, you
can also use key parameters to set encoding (defaults to UTF8), owner,
connection-limit (defaults to no limit)). If limit-public-access is set to t,
then only superuser roles or roles with explicit access to this database will
be able to access it. If collation is set, the assumption is that template0
needs to be used rather than template1 which may contain encoding specific or
locale specific data."
  (setf database-name (to-sql-name database-name))
  (cond ((equal owner "")
         (setf owner (cl-postgres::connection-user *database*)))
        ((stringp owner)
         nil)
        (t (setf owner (cl-postgres::connection-user *database*))))
  (if collation (progn (setf template "template template0")
                       (setf collation (format nil " lc_collate '~a' lc_ctype
'~a'" collation collation) "")))
  (when (and (character-set-exists-p encoding)
             (integerp connection-limit))
    (query (format nil "create database ~a owner ~a ~a encoding ~a ~a
connection limit = ~a"
                   database-name owner template encoding collation
                   connection-limit))
    (when limit-public-access
      (query (format nil "revoke all privileges on database ~a from public;"
                     database-name)))
    (when comment (query (format nil "comment on database ~a is '~a'"
                                 database-name comment)))))

(defun drop-database (database)
  "Drop the specified database. Note: This cannot be the database you are
already connected to."
  (setf database (to-sql-name database))
  (if (database-exists-p database)
      (query (format nil "drop database ~a" database))
      (cerror "invalid database name provided" 'invalid-database-name)))

(defun num-records-in-database ()
  "Returns a list of lists with schema, table name and approximate number of
records in the currently connected database."
  (query (:order-by (:select 'schemaname 'relname 'n_live_tup
                     :from 'pg_stat_user_tables)
                    (:desc 'n_live_tup))))

(defun current-database ()
  "Returns the string name of the current database."
  (query (:select (:current-database)) :single))

(defun database-size (&optional (name nil))
  "Given the name of a database, will return the name, a pretty-print string of
the size of the database and the size in bytes. If a database name is not
provided, it will return the result for the currently connected database."
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

(defun get-database-comment (database-name)
  "Returns the comment, if any, attached to a database"
  (setf database-name (to-sql-name database-name))
  (when (database-exists-p database-name)
    (query "SELECT pg_catalog.shobj_description(d.oid, 'pg_database')
                      FROM   pg_catalog.pg_database d
                      WHERE  datname = $1" database-name
                  :single)))

(defun list-databases (&key (order-by-size nil) (size t) (names-only nil))
  "Returns a list of lists where each sub-list contains the name of the
database, a pretty-print string of the size of that database and the size in
bytes. The default order is by database name. Pass t as a parameter
to :order-by-size for order by size. Setting size to nil will return just the
database names in a single list ordered by name. This function excludes the
template databases."
  (if order-by-size
      (setf order-by-size (sql (:desc (:pg-database-size 'pg-database.oid))))
      (setf order-by-size " datname"))
  (cond
    (names-only
     (alexandria:flatten
      (query
       (:order-by
        (:select 'datname
         :from 'pg-database
         :where (:not (:like 'datname "template%")))
        'datname))))
    (size
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
  "Shortcut for getting the next value from a sequence. The sequence identifier
can be either a string or a symbol, in the latter case it will be converted to
a string according to S-SQL rules."
  (query (:select (:nextval (to-identifier sequence))) :single))

(defun create-sequence (name &key temp if-not-exists increment min-value
                               max-value start cache)
  "Create a sequence. Available additional key parameters are :temp
:if-not-exists :increment :min-value :max-value :start and :cache. See
https://www.postgresql.org/docs/current/static/sql-createsequence.html
for details on usage."
  (let ((query-string
          (concatenate 'string
                       "CREATE "
                       (if temp "TEMP " "")
                       "SEQUENCE "
                       (if if-not-exists "IF NOT EXISTS " "")
                       (to-sql-name name)
                       (if increment (concatenate 'string " INCREMENT BY "
                                                  (format nil "~a" increment))
                           "")
                       (if min-value (concatenate 'string " MINVALUE "
                                                  (format nil "~a" min-value))
                           "")
                       (if max-value (concatenate 'string " MAXVALUE "
                                                  (format nil "~a" max-value))
                           "")
                       (if start (concatenate 'string " START "
                                              (format nil "~a" start))
                           "")
                       (if cache (concatenate 'string " CACHE "
                                              (format nil "~a" cache))
                           ""))))
    (query query-string)))

(defun drop-sequence (name &key if-exists cascade)
  "Drop a sequence. Name should be quoted. Available key parameters are
:if-exists and :cascade"
  (let ((query-string
          (concatenate 'string
                       "DROP SEQUENCE "
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
  "Tests whether a sequence with the given name exists. The name can be either a
string or a symbol."
  (query (make-exists-query "S" (to-sql-name sequence)) :single))

;;;; Tablespaces
(defun list-tablespaces ()
  "Lists the tablespaces in the currently connected database. What are tablespace
you ask? Per the Postgresql documentation
https://www.postgresql.org/docs/current/manage-ag-tablespaces.html: Tablespaces
in PostgreSQL allow database administrators to define locations in the file
system where the files representing database objects can be stored. Once created,
a tablespace can be referred to by name when creating database objects.

By using tablespaces, an administrator can control the disk layout of a
PostgreSQL installation. This is useful in at least two ways. First, if the
partition or volume on which the cluster was initialized runs out of space and
cannot be extended, a tablespace can be created on a different partition and
used until the system can be reconfigured.

Second, tablespaces allow an administrator to use knowledge of the usage
pattern of database objects to optimize performance. For example, an index
which is very heavily used can be placed on a very fast, highly available
disk, such as an expensive solid state device. At the same time a table
storing archived data which is rarely used or not performance critical could
be stored on a less expensive, slower disk system."
  (loop for x in (query (:order-by (:select (:as 'spcname 'name)
                                    :from 'pg_tablespace)
                                   'spcname))
        collect (first x)))

;;;; Types
(defun list-available-types ()
  "List the available data types in the connected postgresql version, It returns
a list of lists, each sublist containing the oid (object identifier number) and
the name of the data types. E.g. (21 \"smallint\")"
  (query (:select 'oid (:as (:format-type :oid :NULL) 'typename)
                  :from 'pg-type
                  :where (:= 'typtype "b"))))

;;; Tables
(defun table-schema-names (table-name schema-name)
  (let ((split-name (split-fully-qualified-tablename table-name)))
    (setf table-name (first split-name))
    (if schema-name (setf schema-name (to-sql-name schema-name))
        (setf schema-name (second split-name))))
  (values table-name schema-name))

;;; create table can only be done either using a deftable approach or s-sql
(defun get-table-comment (table-name &optional schema-name)
  "Get table comment retrieves the comment attached to a table, if any."
  (multiple-value-bind (tn sn)
      (table-schema-names table-name schema-name)
    (query (:select 'description
          :from 'pg_description
          :inner-join 'pg_class
          :on (:= 'pg_description.objoid 'pg_class.oid)
          :inner-join 'pg-namespace
          :on (:= 'pg-namespace.oid 'pg-class.relnamespace)
          :where (:and (:= 'pg-class.relname '$1)
                       (:= 'pg-namespace.nspname '$2)))
         tn sn :single)))

(defun get-all-table-comments ()
  "Returns a list of lists, each list showing the schema, table and comment
of all tables with comments."
  (query "select pg_namespace.nspname as schema,
                 relname as table,
                 description
          from pg_description
          inner join pg_class
          on objoid = oid
          inner join pg_namespace
          on pg_namespace.oid = pg_class.relnamespace
          where relkind = 'r'"))

(defun get-table-oid (table-name &optional schema-name)
  "Retrieves the oid identifier for a particular table from postgresql. Works
for tables in all schemas."
  (multiple-value-bind (tn sn)
      (table-schema-names table-name schema-name)
    (query (:select 'pg-class.oid
            :from 'pg-class
            :inner-join 'pg-namespace
            :on (:= 'pg-namespace.oid 'pg-class.relnamespace)
            :where (:and (:= 'pg-class.relname '$1)
                         (:= 'pg-namespace.nspname '$2)))
           tn sn :single)))

(defun get-table-comment (table-name &optional schema-name)
  "Retrieves the comment, if any attached to the table"
  (multiple-value-bind (tn sn)
      (table-schema-names table-name schema-name)
    (query (:select 'description
            :from 'pg-description
            :inner-join 'pg-class
            :on (:= 'objoid 'oid)
            :inner-join 'pg-namespace
            :on (:= 'pg-namespace.oid 'pg-class.relnamespace)
            :where (:and (:= 'pg-class.relname '$1)
                         (:= 'pg-namespace.nspname '$2)))
           tn sn :single)))

(defun table-description (table-name &optional schema-name)
  "Returns a list of the fields in the named table. Each field is represented
by a list of three elements: the field name, the type, and a boolean indicating
whether the field may be NULL.

Table can be either a string or quoted. Table-names can be fully qualified with
the schema or not. If the table-name is not fully qualified and a schema name
is not provided, the table will be assumed to be in the public schema."
  (multiple-value-bind (tn sn)
      (table-schema-names table-name schema-name)
    (mapcar #'butlast
            (query (:order-by
                    (:select 'attname 'typname (:not 'attnotnull) 'attnum
                             :distinct
                     :from 'pg-attribute
                     :inner-join 'pg-type
                     :on (:= 'pg-type.oid 'atttypid)
                     :inner-join 'pg-class
                     :on (:and (:= 'pg-class.oid 'attrelid)
                               (:= 'pg-class.relname '$1))
                     :inner-join 'pg-namespace
                     :on (:= 'pg-namespace.oid 'pg-class.relnamespace)
                     :where (:and (:> 'attnum 0)
                                  (:= 'pg-namespace.nspname '$2)))
                    'attnum)
                   tn sn))))


#|
column_name data_type column_default is_nullable collation_name is_identity |  character_maximum_length | character_octet_length | numeric_precision | numeric_precision_radix | numeric_scale | datetime_precision | interval_type | interval_precision | character_set_catalog | character_set_schema | character_set_name | collation_catalog | collation_schema |  | domain_catalog | domain_schema | domain_name | udt_catalog | udt_schema | udt_name | scope_catalog | scope_schema | scope_name | maximum_cardinality | dtd_identifier | is_self_referencing | | identity_generation | identity_start | identity_increment | identity_maximum | identity_minimum | identity_cycle | is_generated | generation_expression | is_updatable
|#


(defun table-description-plus (table-name &optional schema-name)
  "Returns more table info than table-description. Specifically returns
column-name, data-type, character-maximum-length, modifier,
whether it is not-null and the default value.

Table can be either a string or quoted. Table-names can be fully qualified with
the schema or not. If the table-name is not fully qualified and a schema name
is not provided, the table will be assumed to be in the public schema."
  (multiple-value-bind (tn sn)
      (table-schema-names table-name schema-name)
    (mapcar #'butlast
          (query (:order-by
                  (:select
                   (:as 'a.attname 'column-name)
                   (:as 'tn.typname 'data-type)
                   (:as 'a.attlen  'character-maximum-length)
                   (:as 'a.atttypmod 'modifier)
                   (:as 'a.attnotnull 'notnull)
                   (:as 'a.atthasdef 'hasdefault)
                   (:as 'a.attnum 'ordinal-position)
                   :distinct
                   :from (:as 'pg-attribute 'a)
                   :inner-join (:as 'pg-type 'tn)
                   :on (:= 'tn.oid 'a.atttypid)
                   :inner-join 'pg-class
                   :on (:and (:= 'pg-class.oid 'attrelid)
                             (:= 'pg-class.relname '$1))
                   :inner-join 'pg-namespace
                   :on (:= 'pg-namespace.oid 'pg-class.relnamespace)
                   :where (:and (:> 'attnum 0)
                                (:= 'pg-namespace.nspname '$2)))
                  'ordinal-position)
                 tn sn))))

(defun list-all-tables (&optional (fully-qualified-names-only nil))
  "If fully-qualified-names-only is set to t, returns all schema.table names
other than pg_catalog or the information_schema. Otherwise returns the
following info:

schema-name, table-name, table-owner, tablespace, hasindexes, hasrules,
hastriggers and rowsecurity"
  (if fully-qualified-names-only
      (alexandria:flatten
       (query "select schemaname ||'.'|| tablename as name
               from pg_catalog.pg_tables
               where schemaname != 'pg_catalog'
               and schemaname != 'information_schema'
               order by name"))
       (query "select *
               from pg_catalog.pg_tables
               where schemaname != 'pg_catalog'
               and schemaname != 'information_schema'
               order by schemaname, tablename")))

(defun list-tables-in-schema (&optional (schema-name "public") (strings-p nil))
  "Returns a list of tables in a particular schema, defaulting to public.
If schema-name is :all or \"all\", it will return all the non-system tables in
the database in fully qualified form: e.g. 'public.test_table'. If string-p is t,
the names will be returned as strings with underscores converted to hyphens."
  (let ((result
          (cond
            ((or (equal schema-name "all")
                 (eq schema-name :all))
             (query "select table_schema||'.'||table_name
                               from information_schema.tables
                               where table_schema not in
                                     ('pg_catalog', 'information_schema')
                               order by table_schema, table_name"))
            (t
             (query "((SELECT table_name
                                 FROM information_schema.tables
                                 WHERE (table_schema = $1))
                                 ORDER BY table_name)"
                    (to-sql-name schema-name))))))
    (if strings-p (mapcar 'from-sql-name result) result )))

(defun list-tables (&optional (strings-p nil))
  "DEPRECATED FOR LIST-ALL-TABLES. Return a list of the tables in the public
schema of a database. By default the table names are returned as keywords.
They will be returned as lowercase strings if strings-p is true."
  (let ((result (query (make-list-query "r") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun table-exists-p (table-name &optional schema-name)
  "Check whether a table exists in a particular schema. Defaults to the search
path. Takes either a string or a symbol for the table name. The table-name can
be fully qualified in the form of schema.table-name or database.schema.table-name.
If the schema is specified either in a qualified table-name or in the optional
schema-name parameter, we look directly to the information schema tables.
Otherwise we use the search path which can be controlled by being within a
with-schema form."
  (multiple-value-bind (tn sn)
      (table-schema-names table-name schema-name)
    (query (:select (:exists
                     (:select 'table-name
                      :from 'information-schema.tables
                      :where (:and (:= 'table-schema '$1) (:= 'table-name '$2)))))
           sn tn :single)))

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
  "Returns a list of lists (table-name, size in 8k pages) of tables in the
current database. Providing a name to the schema parameter will return just
the information for tables in that schema. It defaults to just the tables in
the public schema. Setting schema to nil will return all tables, indexes etc
in the database in descending order of size. This would include system tables,
so there are a lot more than you would expect. If :size is set to nil, it
returns only a flat list of table names. Setting order-by-size to t will return
the result in order of size instead of by table name."
  (setf schema (to-sql-name schema))
  (if order-by-size
      (setf order-by-size (sql (:desc 'relpages)))
      (setf order-by-size " relname"))
  (cond ((and size schema)
         (query (:order-by
                 (:select 'relname 'relpages
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
        (schema (query (:order-by
                        (:select 'relname
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
  "Return the size of a given postgresql table in k or m. Table-name can be
either a string or quoted."
  (query (:select (:pg_size_pretty (:pg_total_relation_size '$1)))
         :single
         (to-sql-name table-name)))

;; Columns
(defun get-column-comments (database schema table)
 "Retrieves a list of lists of column names and comments, if any, from a table "
  (query (format nil "SELECT
    cols.column_name,
    (
        SELECT
            pg_catalog.col_description(c.oid, cols.ordinal_position::int)
        FROM pg_catalog.pg_class c
        WHERE
            c.oid     = (SELECT cols.table_name::regclass::oid) AND
            c.relname = cols.table_name
    ) as column_comment

    FROM information_schema.columns cols
    WHERE
    cols.table_catalog = '~a' AND
    cols.table_schema  = '~a' AND
    cols.table_name    = '~a';" database schema table)))

(defun list-columns (table-name)
  "Returns a list of strings of just the column names in a table.
Pulls info from the postmodern table-description function rather than directly.
The table-name can be a string or quoted. Any table-name that is not fully
qualified with the schema will be assumed to be in the public schema."
  (when (table-exists-p table-name)
    (loop for x in (table-description table-name)
          collect (first x))))

(defun list-columns-with-types (table-name)
  "Returns a list of (name type) lists for the fields of a table. Returns a list
of strings of just the column names and their sql data types in a table. Pulls
info from the postmodern table-description function rather than directly. The
table-name can be a string or quoted. Any table-name that is not fully qualified
with the schema will be assumed to be in the public schema."
  (when (table-exists-p table-name)
    (loop for x in (table-description table-name)
          collect (list (first x) (second x)))))

(defun column-exists-p (table-name column-name &optional schema-name)
  "Determine if a particular column exists. Table name and column-name can be
either strings or symbols. If the optional schema name is not given or the
table-name is not fully qualified with a schema name, the schema will be assumed
to be the public schema."
    (let ((split-name (split-fully-qualified-tablename table-name)))
    (setf table-name (first split-name))
    (if schema-name (setf schema-name (to-sql-name schema-name))
        (setf schema-name (second split-name))))
  (let ((schema-test (if (and schema-name
                              (schema-exists-p schema-name)
                              (table-exists-p table-name))
                         (sql (:= 'pg-namespace.nspname schema-name))
                         "true")))
    (query (:select
            (:as 'a.attname 'column-name)
            :distinct
            :from (:as 'pg-attribute 'a)
            :inner-join 'pg-class
            :on (:and (:= 'pg-class.oid 'attrelid)
                      (:= 'pg-class.relname (to-identifier table-name)))
            :inner-join 'pg-namespace
            :on (:= 'pg-namespace.oid 'pg-class.relnamespace)
            :where (:and (:> 'attnum 0) (:raw schema-test)
                         (:= '$1 'a.attname)))
           (to-sql-name column-name)
           :single)))

;;; Views
(defun list-views (&optional strings-p)
  "Returns list of the user defined views in the current database. When
strings-p is T, the names will be returned as strings, otherwise as keywords."
  (let ((result (query (make-list-query "v") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun view-exists-p (view)
  "Tests whether a view with the given name exists. Takes either a string or
a symbol for the view name."
  (query (make-exists-query "v" view) :single))

(defun describe-views (&optional (schema "public"))
  "Describe the current views in the specified schema. Includes the select
statements used to create the view. Takes an optional schema
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
  "Tests whether an index with the given name exists. The name can be either a
string or a symbol."
  (query (make-exists-query "i" (to-sql-name index-name)) :single))

(defun create-index (name  &key unique if-not-exists concurrently on using fields)
  "Create an index. Slightly less sophisticated than the query version because
it does not have a where clause capability."
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
  "Return a list of the indexs in a database. Turn them into keywords if
strings-p is not true."
  (let ((result (query (make-list-query "i") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun list-table-indices (table-name &optional strings-p)
  "List the index names and the related columns in a single table. Each
index will be in a separate sublist."
  (when (table-exists-p (to-sql-name table-name))
    (let ((result (query
                   (:order-by
                    (:select
                     (:as 'i.relname 'index-name)
                     (:as 'a.attname 'column-name)
                     :from (:as 'pg-class 't1)
                     (:as 'pg-class 'i)
                     (:as 'pg-index 'ix)
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
      (if strings-p
          result
          (loop for x in result collect (mapcar 'from-sql-name x))))))

(defun list-indexed-column-and-attributes (table-name)
  "List the indexed columns and their attributes in a table. Includes primary
key."
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
  "Returns a list of the definitions used to create the current indexes for
the table."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (query (:select (:pg_get_indexdef 'indexrelid)
            :from 'pg_index
            :where (:= 'indrelid (:type '$1 :regclass)))
           table-name)))

;;;; Keys
(defun find-primary-key-info (table &optional (just-key nil))
  "Returns a list of sublists where the sublist contains two strings. If a table
primary key consists of only one column, such as 'id' there will be a single
sublist where the first string is the name of the column and the second string
is the string name for the datatype for that column. If the primary key for the
table consists of more than one column, there will be a sublist for each column
subpart of the key. The sublists will be in the order they are used in the key,
not in the order they appear in the table. If just-key is set to t, the list
being returned will contain just the column names in the primary key as string
names with no sublists. If the table is not in the public schema, provide the
fully qualified table name e.g. schema-name.table-name."
  (when (symbolp table) (setf table (s-sql:to-sql-name table)))
  (let ((info (query (:order-by
                      (:select
                       'a.attname
                       (:format-type 'a.atttypid 'a.atttypmod)
                       :from
                       (:as 'pg-attribute 'a)
                       :inner-join (:as
                                    (:select '*
                                             (:as (:generate-subscripts 'indkey 1)
                                                  'indkey-subscript)
                                             :from 'pg-index)
                                    'i)
                       :on
                       (:and 'i.indisprimary
                             (:= 'i.indrelid  'a.attrelid)
                             (:= 'a.attnum
                                 (:[] 'i.indkey 'i.indkey-subscript)))
                       :where
                       (:= 'a.attrelid  (:type '$1 regclass)))
                      'i.indkey-subscript)
                     table)))
    (if just-key (loop for x in info collect (first x)) info)))

(defun list-foreign-keys (table schema)
  "Returns a list of sublists of foreign key info in the form of
   '((constraint-name local-table local-table-column
     foreign-table-name foreign-column-name))"
  (setf table (to-sql-name table))
  (setf schema (to-sql-name schema))
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
  "List constraints on a table. Table-name can be either a string or quoted.
Turns constraints into keywords if strings-p is not true."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (let ((result
            (query (:select 'relname
                    :from 'pg-class
                    :where
                    (:in 'oid
                         (:select 'indexrelid
                          :from 'pg-index 'pg-class
                                  :where (:and
                                          (:= 'pg-class.relname '$1)
                                          (:= 'pg-class.oid 'pg-index.indrelid)
                                          (:or (:= 'indisunique "t")
                                               (:= 'indisprimary "t"))))))
                   table-name)))
      (if strings-p result (loop for x in result
                                 collect
                                 (mapcar 'from-sql-name x))))))

(defun list-all-constraints (table-name &optional (strings-p))
  "Uses information_schema to list all the constraints in a table. Table-name
can be either a string or quoted. Turns constraints into keywords if strings-p
is not true."
  (setf table-name (to-sql-name table-name))
  (when (table-exists-p table-name)
    (let ((result (query (:select 'constraint-name 'constraint-type
                                  :from 'information-schema.table-constraints
                                  :where (:= 'table-name '$1))
                         table-name)))
      (if strings-p result (loop for x in result
                                 collect (mapcar 'from-sql-name x))))))

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
                                              'ss))
                              'ss2)
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
  "List distinct trigger names from the information_schema table. Table-name can
be either quoted or string. (A trigger is a specification that the database
should automatically execute a particular function whenever a certain type of
operation is performed. Triggers can be attached to tables (partitioned or
not), views, and foreign tables.
See https://www.postgresql.org/docs/current/trigger-definition.html)"
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
                                        (:set "pg-catalog"
                                              "information-schema"))))
            collect (first x))))

(defun list-detailed-triggers ()
  "DEPRECATED FOR DESCRIBE-TRIGGERS.List detailed information on the triggers
from the information_schema table."
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
  "Returns a list of alists of rolenames, role attributes and membership in
roles. See https://www.postgresql.org/docs/current/role-membership.html
for an explanation.

The optional parameter can be used to set the return list types
to :alists or :plists."
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
  "List the current postgresql connections to the currently connected database.
It does this by returningo info from pg_stat_activity on open connections."
  (query (:select '* :from 'pg-stat-activity)))

(defun list-available-extensions ()
  "List the postgresql extensions which are available in the system to the
currently connected database. The extensions may or may not be installed."
  (loop for x in (query (:order-by (:select 'name :from 'pg-available-extensions)
                                   'name))
        collect (first x)))

(defun list-installed-extensions ()
  "List the postgresql extensions which are installed in the currently
connected database."
  (loop for x in (query (:order-by (:select 'extname :from 'pg-extension)
                                   'extname))
        collect (first x)))

(defun replace-non-alphanumeric-chars (str &optional (replacement #\_))
  "Takes a string and a replacement char and replaces any character which is
not alphanumeric or an asterisk with a specified character - by default an
underscore and returns the modified string."
  (let ((str1 str))
    (with-output-to-string (*standard-output*)
      (loop :for ch :of-type character :across str1
            :do (if (or (eq ch #\*)
                        (alphanumericp ch))
                    (write-char ch)
                    (write-char replacement))))))

(defun cache-hit-ratio ()
  "The cache hit ratio shows data on serving the data from memory compared to
how often you have to go to disk.

This function returns a list of heapblocks read from disk, heapblocks hit from
memory and the ratio of heapblocks hit from memory / total heapblocks hit.
Borrowed from:
https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/"
  (query "SELECT sum(heap_blks_read) as heap_read,
                sum(heap_blks_hit) as heap_hit,
                sum(heap_blks_hit) / (sum(heap_blks_hit) + sum(heap_blks_read))
                as ratio
         FROM pg_statio_user_tables;"))

(defun bloat-measurement ()
  "Bloat measurement of unvacuumed dead tuples. Borrowed from:
https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/
who borrowed it from:
 https://github.com/heroku/heroku-pg-extras/tree/master/commands."
  (query "WITH constants AS
            (SELECT current_setting('block_size')::numeric AS bs,
                    23 AS hdr,
                    4 AS ma),
               bloat_info AS
            (SELECT ma,
                    bs,
                    schemaname,
                    tablename,
                    (datawidth+(hdr+ma-(case
                                            when hdr%ma=0 THEN ma
                                            ELSE hdr%ma
                                        END)))::numeric AS datahdr,
                    (maxfracsum*(nullhdr+ma-(case
                                                 when nullhdr%ma=0 THEN ma
                                                 ELSE nullhdr%ma
                                             END))) AS nullhdr2
             FROM
               (SELECT schemaname,
                       tablename,
                       hdr,
                       ma,
                       bs,
                       SUM((1-null_frac)*avg_width) AS datawidth,
                       MAX(null_frac) AS maxfracsum,
                       hdr+
                  (SELECT 1+count(*)/8
                   FROM pg_stats s2
                   WHERE null_frac<>0
                     AND s2.schemaname = s.schemaname
                     AND s2.tablename = s.tablename ) AS nullhdr
                FROM pg_stats s,
                     constants
                GROUP BY 1,
                         2,
                         3,
                         4,
                         5) AS foo),
               table_bloat AS
            (SELECT schemaname,
                    tablename,
                    cc.relpages,
                    bs,
                    CEIL((cc.reltuples*((datahdr+ma- (CASE
                                                          WHEN datahdr%ma=0 THEN ma
                                                          ELSE datahdr%ma
                                                      END))+nullhdr2+4))/(bs-20::float))
                    AS otta
             FROM bloat_info
             JOIN pg_class cc ON cc.relname = bloat_info.tablename
             JOIN pg_namespace nn ON cc.relnamespace = nn.oid
             AND nn.nspname = bloat_info.schemaname
             AND nn.nspname <> 'information_schema'),
               index_bloat AS
            (SELECT schemaname,
                    tablename,
                    bs,
                    COALESCE(c2.relname, '?') AS iname,
                    COALESCE(c2.reltuples, 0) AS ituples,
                    COALESCE(c2.relpages, 0) AS ipages,
                    COALESCE(CEIL((c2.reltuples*(datahdr-12))/(bs-20::float)), 0)
                    AS iotta -- very rough approximation, assumes all cols

             FROM bloat_info
             JOIN pg_class cc ON cc.relname = bloat_info.tablename
             JOIN pg_namespace nn ON cc.relnamespace = nn.oid
             AND nn.nspname = bloat_info.schemaname
             AND nn.nspname <> 'information_schema'
             JOIN pg_index i ON indrelid = cc.oid
             JOIN pg_class c2 ON c2.oid = i.indexrelid)
          SELECT type,
                 schemaname,
                 object_name,
                 bloat,
                 pg_size_pretty(raw_waste) as waste
          FROM
            (SELECT 'table' as type,
                    schemaname,
                    tablename as object_name,
                    ROUND(CASE
                              WHEN otta=0 THEN 0.0
                              ELSE table_bloat.relpages/otta::numeric
                          END, 1) AS bloat,
                    CASE
                        WHEN relpages < otta THEN '0'
                        ELSE (bs*(table_bloat.relpages-otta)::bigint)::bigint
                    END AS raw_waste
             FROM table_bloat
             UNION SELECT 'index' as type,
                          schemaname,
                          tablename || '::' || iname as object_name,
                          ROUND(CASE
                                    WHEN iotta=0
                                         OR ipages=0 THEN 0.0
                                    ELSE ipages/iotta::numeric
                                END, 1) AS bloat,
                          CASE
                              WHEN ipages < iotta THEN '0'
                              ELSE (bs*(ipages-iotta))::bigint
                          END AS raw_waste
             FROM index_bloat) bloat_summary
          ORDER BY raw_waste DESC,
                   bloat DESC

          SELECT schemaname || '.' || relname AS table,
                 indexrelname AS index,
                 pg_size_pretty(pg_relation_size(i.indexrelid)) AS index_size,
                 idx_scan as index_scans
          FROM pg_stat_user_indexes ui
          JOIN pg_index i ON ui.indexrelid = i.indexrelid
          WHERE NOT indisunique
            AND idx_scan < 50
            AND pg_relation_size(relid) > 5 * 8192
          ORDER BY pg_relation_size(i.indexrelid) / nullif(idx_scan, 0)
                   DESC NULLS FIRST, pg_relation_size(i.indexrelid) DESC;"))

(defun unused-indexes ()
  "Returns a list of lists showing schema.table, indexname, index_size and
number of scans. The code was borrowed from:
https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/"
  (query "SELECT schemaname || '.' || relname AS table,
                 indexrelname AS index,
                 pg_size_pretty(pg_relation_size(i.indexrelid)) AS index_size,
                 idx_scan as index_scans
          FROM pg_stat_user_indexes ui
          JOIN pg_index i ON ui.indexrelid = i.indexrelid
          WHERE NOT indisunique
            AND idx_scan < 50
            AND pg_relation_size(relid) > 5 * 8192
          ORDER BY pg_relation_size(i.indexrelid) / nullif(idx_scan, 0)
                   DESC NULLS FIRST, pg_relation_size(i.indexrelid) DESC;"))

(defun check-query-performance (&optional (ob nil) (num-calls 100) (limit 20))
  "This function requires that postgresql extension pg_stat_statements must be
loaded via shared_preload_libraries. It is borrowed from:
 https://www.citusdata.com/blog/2019/03/29/health-checks-for-your-postgres-database/.

Optional parameters OB allow order-by to be 'calls', 'total-time', 'rows-per'
or 'time-per', defaulting to time-per.num-calls to require that the number of calls
exceeds a certain threshold, and limit to limit the number of rows returned.
It returns a list of lists, each row containing the query, number of calls,
total_time, total_time/calls, stddev_time, rows, rows/calls and the cache hit
percentage."
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
  (let ((sql-statement
          (format nil
                  "SELECT query,
                         calls,
                         total_time,
                         total_time / calls as time_per,
                         stddev_time,
                         rows,
                         rows / calls as rows_per,
                                100.0 * shared_blks_hit / nullif(shared_blks_hit
                                + shared_blks_read, 0)
                         AS hit_percent
                  FROM pg_stat_statements
                  WHERE query not similar to '%pg_%'
                    and calls > ~a
                  ORDER BY ~a DESC
                    LIMIT ~a;" num-calls ob limit)))
    (query sql-statement)))

;;; Document Database Functions
(defun document-schema (schema out)
  "Outputs some schema description info to out"
  (format out "~%* Schema ~a~%" schema)
  (format out "~%** Table Sizes~%")
  (format out "Pretty size is bytes, KB, MB, etc. ~% Table+Toast is the size of the table not counting indexes plus TOAST (parts of a table stored out of line due to physical rows crossing block boundaries.~% Table+Indices is total size in bytes~% Table Alone is just the size of the main table without the TOAST parts stored outside the main table.~% Index size is the size of the associated indices in bytes. ~% Num 8k pages are the number of pages used assuming postgresql was compiled normally with 8k pages.~% More information is available at [[https://www.postgresql.org/docs/current/disk-usage.html]]~%")
  (format out "~%| Schema | Name | Rows | Type | Owner | Pretty Size | Table+Toast Size | Table+Indices | Table Alone | Index Size | Num 8K Pages | Description |~%")
  (loop for y in (list-table-sizes schema) do
    (format out "|~{ ~a |~}~%" y))
  (format out "~%"))

(defun document-table (schema table out)
  "Outputs some table description info to out"
  (format out "~%** ~a.~a~%" schema table)
  ;; Table Description
  (format out "~%The following information for this table is shown in the following table: (column-name, data type, substring, whether it is not nullable, whether it has a default value, collation, whether it is an identity column, whether it is a generated column, the storage type, attstattarget and a column description/comment if any. attstattarget controls the level of detail of statistics accumulated for this column by ANALYZE. A zero value indicates that no statistics should be collected. A negative value says to use the system default statistics target. The exact meaning of positive values is data type-dependent. For scalar data types, attstattarget is both the target number of most common values to collect, and the target number of histogram bins to create. Attstorage is normally a copy of pg_type.typstorage of this column's type. For TOAST-able data types, this can be altered after column creation to control storage policy. ~%")
  (format out "~%~%| Col. Name | Data Type | Substring | Not Null? | Has Default? | Collation | Attstattarget | Attstorage | Identity Col? | Generated Col? |Col-Description |~%")
  (loop for z in (table-description (format nil "~a.~a" (to-sql-name schema) (to-sql-name table))) do
          (format out "|~{ ~a |~}~%" z)))

(defun document-database (output-file)
  "Writes a description of the currently connected database to a file"
  (with-open-file (out output-file :direction :output :if-exists :supersede)
    (format out "#+TITLE:    Documentation for ~a~%
#+DESCRIPTION:~a%~%"
            (current-database) "Insert current description here")
    (format out "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"css/htmlize.css\"/>
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"css/readtheorg.css\"/>
#+HTML_HEAD: <script type=\"text/javascript\" src=\"js/jquery.min.js\"></script>
#+HTML_HEAD: <script type=\"text/javascript\" src=\"js/bootstrap.min.js\"></script>
#+HTML_HEAD: <script type=\"text/javascript\" src=\"js/jquery.stickytableheaders.min.js\"></script>
#+HTML_HEAD: <script type=\"text/javascript\" src=\"js/readtheorg.js\"></script>
# Change the background of source block.
#+HTML_HEAD: <style>pre.src{background:#343131;color:white;} </style>
#+LANGUAGE:  en
#+OPTIONS:   H:4
#+OPTIONS:   num:nil
#+OPTIONS:   toc:2
#+OPTIONS:   p:t
#+OPTIONS: ^:nil")
    (loop for x in (list-schemas) do
      (document-schema x out)
      (loop for y in (list-tables-in-schema x) do
        (document-table x y out)))))
