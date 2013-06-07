(in-package :postmodern)

(defmacro with-schema ((schema &key (strict t) (if-not-exist :create) (drop-after nil))
                        &body form)
  "Simple macro to set the schema search path of the postgresql
   database to include as first entry a specified schema.

   calling with strict 't only the specified schema is set as current
   search path. All other schema are then not searched any more.

   calling with if-not-exist set to :create the schema is created if
   this schema did not exist.

   calling with drop-after set to 't the schema is removed after the
   execution of the body form.

   example :
     (with-schema (:schema-name :strict nil :drop-after nil :if-not-exist :error)
            (foo 1)
            (foo 2))"
  (alexandria:with-gensyms (search-path schema-on schema-off sschema)
    `(let* ((,sschema (string ,schema))
            (,search-path (query (make-show-search-path-query) :single))
            (,schema-on (if ,strict
                            (make-set-search-path-query ,sschema)
                            (make-set-search-path-query (concatenate 'string
                                                                     ,sschema
                                                                     ", "
                                                                     ,search-path))))
            (,schema-off (make-set-search-path-query ,search-path)))
       (unwind-protect
            (progn
              (unless (schema-exist-p ,sschema)
                (if (eq ,if-not-exist :create)
                    (create-schema ,sschema)
                    (error "schema does not exist!")))
              (postmodern:execute ,schema-on)
              ,@form)
         (progn
           (postmodern:execute ,schema-off)
           (if (eq ,drop-after t)
               (drop-schema ,sschema)))))))


(defmacro make-set-search-path-query (path)
  "Sets the run-time parameter search_path to path"
  `(sql (:update 'pg_catalog.pg_settings
                 :set 'name ,path
                 :where (:= 'name "search_path"))))


(defmacro make-show-search-path-query ()
  "Returns the current value of the run-time parameter search_path"
  `(sql (:select 'setting
                 :from 'pg_catalog.pg_settings
                 :where (:= 'name "search_path"))))

(defun list-schemata ()
  "List all existing user defined schemata.

  Note: The query uses the portable information_schema relations instead of pg_tables relations
  SELECT schema_name FROM information_schema.schemata where schema_name !~ '(pg_*)|information_schema' ORDER BY schema_name ;"
  (query (sql (:select 'schema_name
                       :from (:dot 'information_schema 'schemata)
                       :where (:!~ 'schema_name "(pg_*)|information_schema")))
         :column))

(defun schema-exist-p (name)
  "Predicate for schema existence"
  (not (not (or (find (string name)
                      (list-schemata)
                      :test #'string=
                      :key #'string-upcase)))))

(defun set-search-path (name)
  "Sets the namespace path permanently"
  (execute (make-set-search-path-query (string name))))

(defun create-schema (schema)
  "Creating a non existing schema.
   If the schema exists an error is raised."
  ;;(format t "creating schema: ~a" schema)
  (execute (format nil "CREATE SCHEMA ~a" (s-sql:to-sql-name schema))))

;; TODO: optional delete all tables in schema
(defun drop-schema (schema)
  "Drops an existing database schema 'schema'"
  (execute (format nil "DROP SCHEMA ~a" (s-sql:to-sql-name schema))))
