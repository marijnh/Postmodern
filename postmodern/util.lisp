(in-package :postmodern)

(defun to-identifier (name)
  "Used to allow both strings and symbols as identifier - converts
symbols to string with the S-SQL rules."
  (if (stringp name)
      name
      (to-sql-name name)))

(defun sequence-next (sequence)
  "Shortcut for getting the next value from a sequence."
  (query (:select (:nextval (to-identifier sequence))) :single))

(defmacro make-list-query (relkind)
  "Helper macro for the functions that list tables, sequences, and
views."
  `(sql (:select 'relname :from 'pg-catalog.pg-class
         :inner-join 'pg-catalog.pg-namespace :on (:= 'relnamespace 'pg-namespace.oid)
         :where (:and (:= 'relkind ,relkind)
                 (:not-in 'nspname (:set "pg_catalog" "pg_toast"))
                 (:pg-catalog.pg-table-is-visible 'pg-class.oid)))))

(defmacro make-exists-query (relkind name)
  "Helper macro for the functions that check whether an object
exists."
  `(sql (:select (:exists (:select 'relname :from 'pg_catalog.pg_class :inner-join 'pg_catalog.pg_namespace :on
                                   (:= 'pg_class.relnamespace 'pg_namespace.oid)
                                   :where (:and (:= 'pg_class.relkind ,relkind)
                                                (:= 'pg_namespace.nspname (:any* (:current_schemas nil)))
                                                (:= 'pg_class.relname (to-identifier ,name))))))))

(defun list-tables (&optional strings-p)
  "Return a list of the tables in a database. Turn them into keywords
if strings-p is not true."
  (let ((result (query (make-list-query "r") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))

(defun table-exists-p (table)
  "Check whether a table exists. Takes either a string or a symbol for
the table name."
  (query (make-exists-query "r" table) :single))

(defun list-sequences (&optional strings-p)
  "Return a list of the sequences in a database. Turn them into
keywords if strings-p is not true."
  (let ((result (query (make-list-query "S") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))
(defun sequence-exists-p (sequence)
  "Check whether a sequence exists. Takes either a string or a symbol
for the sequence name."
  (query (make-exists-query "S" sequence) :single))

(defun list-views (&optional strings-p)
  "Return a list of the views in a database. Turn them into keywords
if strings-p is not true."
  (let ((result (query (make-list-query "v") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))
(defun view-exists-p (view)
  "Check whether a view exists. Takes either a string or a symbol for
the view name."
  (query (make-exists-query "v" view) :single))

(defun table-description (table &optional schema-name)
  "Return a list of (name type null-allowed) lists for the fields of a
table.  If SCHEMA-NAME is specified, only fields from that schema are
returned."
  (let ((schema-test (if schema-name (sql (:= 'pg-namespace.nspname schema-name)) "true")))
    (mapcar #'butlast
            (query (:order-by (:select 'attname 'typname (:not 'attnotnull) 'attnum :distinct
                               :from 'pg-catalog.pg-attribute
                               :inner-join 'pg-catalog.pg-type :on (:= 'pg-type.oid 'atttypid)
                               :inner-join 'pg-catalog.pg-class :on (:and (:= 'pg-class.oid 'attrelid)
                                                                          (:= 'pg-class.relname (to-identifier table)))
                               :inner-join 'pg-catalog.pg-namespace :on (:= 'pg-namespace.oid 'pg-class.relnamespace)
                               :where (:and (:> 'attnum 0) (:raw schema-test)))
                              'attnum)))))

(defun coalesce (&rest args)
  (some (lambda (x) (if (eq x :null) nil x)) args))
