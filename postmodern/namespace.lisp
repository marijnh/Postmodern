;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

(defmacro with-schema ((schema &key (strict t) (if-not-exist :create)
                                 (drop-after nil))
                       &body form)
  "A macro to set the schema search path (namespace) of the postgresql database
to include as first entry a specified schema and then executes the body.
Before executing body the PostgreSQL's session variable search_path is set to
the given namespace. After executing body the search_path variable is restored
to the original value.

   Calling with :strict 't only the specified schema is set as current search
path. All other schema are then not searched any more. If strict is nil, the
namespace is just first schema on the search path upon the the body execution.

   Calling with :if-not-exist set to :create the schema is created if this
   schema did not exist.
   Calling with :if-not-exist set to nil, an error is signaled.

   calling with drop-after set to 't the schema is removed after the execution
   of the body form.

   example :

     (with-schema (:schema-name :strict nil :drop-after nil :if-not-exist :error)
            (foo 1)
            (foo 2))

   example :

     (with-schema ('uniq :if-not-exist :create) ;; changing the search path
            (schema-exists-p 'uniq))"

  `(do-with-schema ,schema (lambda () ,@form)
     :strict ,strict :if-not-exist ,if-not-exist :drop-after ,drop-after))

(defun do-with-schema (schema thunk &key strict if-not-exist drop-after)
  (let ((old-search-path (get-search-path)))
    (unwind-protect
         (progn
           (unless (schema-exists-p schema)
             (if (eq if-not-exist :create)
                 (create-schema schema)
                 (error 'database-error :message
                        (format nil "Schema '~a' does not exist." schema))))
           (set-search-path (if strict (to-sql-name schema t)
                                (concatenate 'string (to-sql-name schema t)
                                             ","
                                             old-search-path)))
           (setf *schema-path* (get-search-path))
           (funcall thunk))
      (set-search-path old-search-path)
      (setf *schema-path* old-search-path)
      (when drop-after (drop-schema schema :cascade 't)))))

(defun get-search-path ()
  "Returns the default schema search path for the current session."
  (query "SHOW search_path" :single))

(defun set-search-path (path)
  "This changes the postgresql runtime parameter controlling what order schemas
are searched. You can always use fully qualified names [schema.table]. By
default, this function only changes the search path for the current
session. This function is used by with-schema."
  (execute (format nil "SET search_path TO ~a" path)))

(defun list-schemas ()
  "List schemas in the current database, excluding the pg_* system schemas.
Should have the same result as list-schemata even though it uses different
system tables."
  (alexandria:flatten
   (query (:order-by
           (:select 'nspname
            :from 'pg_namespace
            :where (:!~* 'nspname "^pg_.*|information_schema"))
           'nspname))))

(defun schema-exists-p (name)
  "Tests for the existence of a given schema. Returns T if the schema exists or
nil otherwise. The name provided can be either a string or quoted symbol."
  (query (:select (:exists (:select 'schema_name
                            :from 'information_schema.schemata
                            :where (:= 'schema_name '$1))))
         (to-sql-name name)
         :single))

(defun create-schema (schema &optional authorization)
  "Create a new schema. Raises an error if the schema already exists. If the
optional authorization parameter is provided, the schema will be owned by that
role."
  (if authorization
      (execute (format nil "create schema ~a authorization ~a"
                       (to-sql-name schema t) authorization))
      (execute (format nil "create schema ~a" (to-sql-name schema t)))))

(defun drop-schema (schema &key (if-exists nil) (cascade nil))
  "Drops an existing database schema 'schema' Accepts :if-exists and/or :cascade
arguments like :drop-table. A notice instead of an error is raised with the
is-exists parameter."
  (execute (format nil "DROP SCHEMA ~:[~;IF EXISTS~] ~a ~:[~;CASCADE~]"
                   if-exists (to-sql-name schema t) cascade)))
