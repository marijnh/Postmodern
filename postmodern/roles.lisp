;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)
#|
Every connection is specific to a particular database. However, creating roles
or users is global to the entire cluster (the running postgresql server). You
can create policies for any individual database, schema or table, but you need
to ensure that those policies also apply to any subsequently created database,
schema or table. Note that each user is automatically a member of the public
group, so you need to change those policies for public as well.

Per the Postgresql Documentation, CREATE ROLE adds a new role to a PostgreSQL
database cluster. A role is an entity that can own database objects and have
database privileges; a role can be considered a “user”, a “group”, or both
depending on how it is used.
https://www.postgresql.org/docs/current/sql-createrole.html. The only real
difference between "create role" and "create user" is that create user
defaults to having a login attribute and create role defaults to not having
a login attribute.

Often applications will have their own concept of users and the application
will itself have one or more types of roles to which the application user is
assigned. So, for example, the application may have two roles - reader and
editor with which it interacts with postgresql and then there are many
application users registered with the application and probably listed in some
type of user table in postgresql that the application manages. When users 1,2
or 3 log in to the application, the application might connect to the postgresql
cluster using a role that only has read (select) permissions. When users 4 or 5
log in to the application, the applicatin might connect to the postgresql cluster
using a role that has read, insert, update and delete permission. Postmodern
provides a simplified create-role system allowing easy creation of roles that
have readonly, editor or superuser type permissions. Further, those
permissions can be limited to individual databases, schemas or tables.

We suggest that you separate application users from roles. Make it easy to
drop application users. Dropping roles requires going through every database,
reassigning ownership of any objects that role might own or have privileges
on, then dropping ownership of objects, then dropping the role itself.

In PostgreSQL*, you cannot drop a database while clients are connected to it.
|#

(defun list-database-users ()
  "List database users (actually 'roles' in Postgresql terminology)."
  (alexandria:flatten (query (:order-by
                              (:select 'usename :from 'pg_user)
                              'usename))))

(defun list-roles (&optional (lt nil))
  "Returns a list of alists of rolenames, role attributes and membership in
roles. See https://www.postgresql.org/docs/current/role-membership.html for an
explanation. The optional parameter can be used to set the return list types
to :alists or :plists."
  (let ((sql-query
          "SELECT r.rolname, r.rolsuper, r.rolinherit,
            r.rolcreaterole, r.rolcreatedb, r.rolcanlogin,
            r.rolconnlimit, r.rolvaliduntil,
            ARRAY(SELECT b.rolname
                  FROM pg_catalog.pg_auth_members m
                  JOIN pg_catalog.pg_roles b ON (m.roleid = b.oid)
                  WHERE m.member = r.oid) as memberof,
            r.rolreplication,
            r.rolbypassrls
            FROM pg_catalog.pg_roles r
            WHERE r.rolname !~ '^pg_'
            ORDER BY 1;"))
    (cond ((equal lt :alists)
           (query  sql-query :alists))
          ((equal lt :plists)
           (query  sql-query :plists))
          (t (query sql-query)))))

(defun role-exists-p (role-name)
  "Does the named role exist in this database cluster? Returns t or nil"
  (query "select exists (SELECT r.rolname
            FROM pg_catalog.pg_roles r
            WHERE r.rolname = $1)"
       role-name :single))

(defun remove-whitespace (str)
  "Removes whitespace from strings. "
  (let ((scanner-w (cl-ppcre:create-scanner :whitespace-char-class)))
    (cl-ppcre:regex-replace-all scanner-w str "")))

(defun whitespace-in-string (str)
  "Returns location of whitespace in string. You probably do not want to allow
whitespace in either user names or passwords."
  (let ((scanner-w (cl-ppcre:create-scanner :whitespace-char-class)))
    (cl-ppcre:scan scanner-w str)))

(defun revoke-all-on-table (table-name role-name)
  "Takes a table-name which could be a string, symbol or list of strings or
symbols of tables names, a role name and revokes all privileges that
role-name may have with that/those tables. This is limited to the currently
connected database and can only revoke the privileges granted by the caller
of the function."
  (cond ((stringp table-name)
         (query (format nil "revoke all privileges on ~a from ~a"
                        (to-sql-name table-name) (to-sql-name role-name))))
        ((symbolp table-name)
         (query (format nil "revoke all privileges on ~a from ~a"
                        (to-sql-name table-name) (to-sql-name role-name))))
        ((listp table-name)
         (loop for x in table-name do
                        (query (format nil "revoke all privileges on ~a from ~a"
                                       (to-sql-name x) (to-sql-name role-name)))))))

(defun schema-parameters-to-list (&optional (schema :public))
  "Returns a list of schemas in the current for which a role will be granted
privileges."
  (cond ((eq schema :public)
         (list "public"))
        ((or (eq schema :all)
             (equalp schema "all"))
         (list-schemas))
        ((listp schema)
         (intersection (mapcar #'to-sql-name schema)
                       (list-schemas)
                       :test #'equal))
        ((stringp schema)
         (list (to-sql-name schema)))
        (t (list "public"))))

(defun database-parameters-to-list (databases)
  "Returns a list of databases where the parameter may be a list of databases,
a single string name or :current, :all or \"all\"."
  (cond ((eq databases :current)
         (list (current-database)))
        ((or (eq databases :all)
             (equalp databases "all"))
         (mapcar #'to-sql-name (list-databases :names-only t)))
        ((listp databases)
         (intersection (mapcar #'to-sql-name databases)
                       (list-databases :names-only t)
                       :test #'equal))
        ((stringp databases)
         (if (database-exists-p databases)
             (list (to-sql-name databases))
             nil))
        (t (list (to-sql-name databases)))))

(defun grant-admin-permissions (schema-name role-name &optional (table-name nil))
  "Grants all privileges to a role for the named schema. If the optional table-name
parameter is provided, the privileges are only granted with respect to that table."
  (cond ((not table-name)
         (query (format nil "grant all on all tables in schema ~a to ~a"
                        schema-name role-name))
         (query (format nil "grant all on all sequences in schema ~a to ~a"
                        schema-name role-name))
         (query (format nil "alter default privileges in schema ~a grant all on tables to ~a"
                        schema-name role-name))
         (query (format nil "alter default privileges in schema ~a grant all on sequences to ~a"
                        schema-name role-name))
         (loop for x in *execute-privileges-list* do
           (query (format nil x schema-name role-name))))
        (t
         (query (format nil "grant all on table ~a.~a to ~a"
                        schema-name table-name role-name)))))

(defun grant-editor-permissions (schema-name role-name &optional (table-name nil))
  "Grants select, insert, update and delete privileges to a role for the named
schema. If the optional table-name parameter is provided, the privileges are only
granted with respect to that table. Note that we are giving some function execute
permissions if table-name is nil, but if the table-name is specified, those are
not provided. Your mileage may vary on how many privileges you want to provide
to a editor role with access to only a limited number of tables."
  (cond ((not table-name)
         (loop for x in *alter-all-default-editor-privileges* do
           (query (format nil x schema-name role-name)))
         (loop for x in *execute-privileges-list* do
           (query (format nil x schema-name role-name))))
        (t
         (query (format nil "grant select, insert, update, delete on table ~a.~a to ~a"
                        schema-name table-name role-name)))))

(defun grant-readonly-permissions (schema-name role-name &optional (table-name nil))
  "Grants select privileges to a role for the named schema. If the optional
table-name parameter is provided, the privileges are only granted with respect
to that table. Note that we are giving some function execute permissions if
table-name is nil, but if the table-name is specified, those are not provided.
Your mileage may vary on how many privileges you want to provide to a
read-only role with access to only a limited number of tables."
  (cond ((not table-name)
         (loop for x in *alter-all-default-select-privileges* do
           (let ((query-string (format nil x schema-name role-name)))
             (query query-string)))
         (loop for x in *execute-privileges-list* do
           (let ((query-string (format nil x schema-name role-name)))
             (query query-string))))
        (t
         (query (format nil "grant select on table ~a.~a to ~a"
                        schema-name table-name role-name)))))

(defun grant-role-permissions-helper (role-type name &key (schema :public)
                                                (tables :all))
  (let ((existing-schemas (list-schemas)))
      (loop for schema-x in (mapcar #'to-sql-name schema) do
        (when (member schema-x existing-schemas :test #'equal)
          (query (format nil "revoke all on all tables in schema ~a from ~a"
                         schema-x name))
          (query (format nil "grant usage on schema ~a to ~a" schema-x name))
          (query (format nil "grant select on all sequences in schema ~a to ~a"
                         schema-x name))
          (cond ((or (eq tables :all)
                     (and (listp tables)
                          (equalp (first tables) "all"))) ; Grant access to existing tables
                 (case role-type
                   (:admin
                    (grant-admin-permissions schema-x name))
                   (:editor
                    (grant-editor-permissions schema-x name))
                   (:readonly
                    (grant-readonly-permissions schema-x name))
                   (t (query (grant-readonly-permissions schema-x name)))))
                ((listp tables)
                 (let ((existing-tables (list-tables t))) ;  Grant access to existing tables
                   (when (or (eq (first tables) :all)
                             (equalp (first tables) "all"))
                     (setf tables existing-tables))
                   (loop for table-y in (mapcar #'to-sql-name tables) do
                     (when (or (eq tables :all)
                               (member table-y existing-tables :test #'equal))
                       (case role-type
                         (:admin
                          (grant-admin-permissions schema-x name table-y))
                         (:editor
                          (grant-editor-permissions schema-x name table-y))
                         (:readonly
                          (grant-readonly-permissions schema-x name table-y))
                         (t (grant-readonly-permissions schema-x name table-y)))))))
                (t nil))))))

(defun grant-role-permissions (role-type name &key (schema :public)
                                                (tables :all)
                                                (databases :all))
  "Grant-role-permissions assumes that a role has already been created, but
permissions need to be granted or revoked on a particular database.

   A  :superuser can create databases, roles, replication, etc. Returns nil.
   A  :standard user has no particular privileges or restrictions. Returns nil.
   An :admin user can edit existing data, insert new data and create new tables
in the specified databases/schemas/tables.
   An :editor user can update fields or insert new records but cannot create new
tables in the specified tables or databases.
   A  :readonly role can only read existing data in the specified schemas,
tables or databases. Schema, tables or databases can be :all or a list of
schemas, tables or databases to be granted permission.

  Granting :all provides access to all future items of that type as well.

  Note that the schema and table rights and revocations granted are limited to
the connected database at the time of execution of this function."
  (when (not (member role-type '(:superuser :standard)))
    (when (equalp tables "all") (setf tables :all))
    (setf schema (schema-parameters-to-list schema))
    (setf databases (database-parameters-to-list databases))
    (when (not databases)
      (cerror "invalid database name provided" 'invalid-database-name))
    (loop for x in databases do
      (query (format nil "grant connect on database ~a to ~a"
                     x name)))
    (restart-case
        (loop for x in databases do
              (with-connection (list x (cl-postgres::connection-user *database*)
                             (cl-postgres::connection-password *database*)
                             (cl-postgres::connection-host *database*)
                             :port (cl-postgres::connection-port *database*)
                             :use-ssl (cl-postgres::connection-use-ssl *database*))
                (grant-role-permissions-helper role-type name :schema schema
                                                              :tables tables)))
      (apply-just-to-current (role-type name schema tables databases)
        :report "Use currently connected database only"
        (when (member (current-database) databases :test #'equal)
            (grant-role-permissions-helper role-type name :schema schema
                                                       :tables tables))))))

(defun create-role-helper (role-type name password &key (schema :public)
                                                     (tables :all)
                                                     (databases :all))
  "Create-role-helper creates a user, then calls grant-role-permission

   A  :superuser can create databases, roles, replication, etc.
   An :admin user can edit existing data, insert new data and create new tables
in the specified databases/schemas/tables.
   An :editor user can update fields or insert new records but cannot create new
tables in the specified tables or databases.
   A  :readonly user can only read existing data in the specified schemas,
tables or databases. Schema, tables or databases can be :all or a list of
schemas, tables or databases to be granted permission.

  Granting :all provides access to all future items of that type as well.

  Note that the schema and table rights and revocations granted are limited to
the connected database at the time of execution of this function."
  (case role-type
    (:superuser
     (query (format nil "create role ~a with login password '~a' superuser
inherit createdb createrole replication"
                    name password)))
    (t
     (query (format nil "create role ~a with login password '~a' nosuperuser
inherit nocreatedb nocreaterole noreplication"
                    name password))))
  (grant-role-permissions role-type name :schema schema :tables tables
                                         :databases databases))

(defun create-role (name password &key (base-role :readonly) (schema :public)
                                    (tables :all) (databases :current)
                                    (allow-whitespace nil)
                                    (allow-utf8 nil)
                                    (allow-disallowed-names nil) (comment nil))
  "Keyword parameters: Base-role. Base-role should be one of :readonly, :editor,
:admin, :standard or :superuser. A readonly user can only select existing data in the
specified tables or databases. An editor has the ability to insert, update,
delete or select data. An admin has all privileges on a database, but cannot
create new databases, roles, or replicate the system. A standard user has no
particular privileges other than connecting to databases.

 :schema defaults to :public but can be a list of schemas. User will not have
access to any schemas not in the list.

 :tables defaults to :all but can be a list of tables. User will not have access
to any tables not in the list.

 :databases defaults to :current but can be a list of databases. User will not
have access to any databases not in the list.

 :allow-whitespace - Whitespace in either the name or password is not allowed by
default.

 :allow-utf8 defaults to nil. If t, the name and password will be normalized. If
nil, the name and password are limited to printable ascii characters. For fun
reading on utf8 user names see
https://labs.spotify.com/2013/06/18/creative-usernames. Also interesting reading
is https://github.com/flurdy/bad_usernames and https://github.com/dsignr/disallowed-usernames/blob/master/disallowed%20usernames.csv,
and https://www.b-list.org/weblog/2018/feb/11/usernames/

 :allow-disallowed-names defaults to nil. If nil, the user name will be checked
against *disallowed-role-names*.

 As an aside, if allowing utf8 in names, you might want to think about whether
you should second copy of the username in the original casing and normalized as
NFC for display purposes as opposed to normalizing to NFKC. It might be viewed
as culturally insensitive to change the display of the name."
  (cond ((and (not allow-whitespace)
              (or (whitespace-in-string name)
                  (whitespace-in-string password)))
         (return-from create-role "Illegal whitespace in name or password"))
        ((and (not allow-disallowed-names)
              (gethash name *disallowed-role-names*))
         (return-from create-role "Disallowed user name"))
        ((and (not allow-utf8)
              (not (cl-postgres:string-printable-ascii-p name))
              (not (cl-postgres:string-printable-ascii-p password)))
         (return-from create-role "Non-ascii characters are not allowed in user
names. Sorry"))
        (t (setf name (cl-postgres:saslprep-normalize name))
           (setf password (cl-postgres:saslprep-normalize password))))
  (if (role-exists-p name)
      (return-from create-role "Role name already in use.")
      (case base-role
        (:readonly
         (create-role-helper :readonly name password :schema schema
                                                     :tables tables
                                                     :databases databases))
        (:editor
         (create-role-helper :editor name password :schema schema
                                                   :tables tables
                                                   :databases databases))
        (:admin
         (create-role-helper :admin name password :schema schema
                                                  :tables tables
                                                  :databases databases))
        (:standard
         (create-role-helper :standard name password))
        (t (create-role-helper :readonly name password :schema schema
                                                       :tables tables
                                                       :databases databases))))
  (when comment (query (format nil "comment on role ~a is '~a'"
                               name comment))))

(defun alter-role-search-path (role search-path)
  "Changes the priority of where a role looks for tables (which schema first,
second, etc. Role should be a string or symbol. Search-path could be a list of schema
names either as strings or symbols."
  (when (listp search-path)
    (setf search-path
          (format nil "~{~a~^, ~}" (mapcar 'to-sql-name search-path))))
  (when (symbolp role) (setf role (to-sql-name role)))
  (query (format nil "alter role ~a set search_path = ~a" role search-path)))

(defun drop-role (role-name &optional (new-owner "postgres") (database :all))
  "The role-name and optional new-owner name should be strings. If they are
symbols, they will be converted to string and hyphens will be converted to
underscores.

Before dropping the role, you must drop all the objects it owns (or reassign
their ownership) and revoke any privileges the role has been granted on other
objects. If database is :all, drop-role will loop through all databases in
the cluster ensuring that the role has no privileges or owned objects in
every database. Otherwise drop-role will drop objects owned by a role in the
current database.

We will reassign ownership of the objects to the postgres role
unless otherwise specified in the optional second parameter. Returns t if
successful. Will not drop the postgres role.

As a minor matter of note, a role can own objects in databases it is not
granted connection rights."
  (when (symbolp role-name) (setf role-name (to-sql-name role-name)))
  (when (symbolp new-owner) (setf new-owner (to-sql-name new-owner)))
  (if (eq database :all)
      (loop for x in (list-databases :names-only t) do
        (with-connection (list x (cl-postgres::connection-user *database*)
                               (cl-postgres::connection-password *database*)
                               (cl-postgres::connection-host *database*)
                               :port (cl-postgres::connection-port *database*)
                               :use-ssl (cl-postgres::connection-use-ssl *database*))
          (when (and (not (string= role-name "postgres"))
                     (member role-name (list-database-users) :test #'equal))
            (query (format nil "reassign owned by ~a to ~a" role-name new-owner))
            (query (format nil "drop owned by ~a" role-name)))))
      (with-connection (list database (cl-postgres::connection-user *database*)
                             (cl-postgres::connection-password *database*)
                             (cl-postgres::connection-host *database*)
                             :port (cl-postgres::connection-port *database*)
                             :use-ssl (cl-postgres::connection-use-ssl *database*))
        (when (and (not (string= role-name "postgres"))
                   (member role-name (list-database-users) :test #'equal))
          (query (format nil "reassign owned by ~a to ~a" role-name new-owner))
          (query (format nil "drop owned by ~a" role-name)))))
      (query (format nil "drop role if exists ~a" role-name))
      (not (role-exists-p role-name)))

(defun list-role-permissions (&optional role)
  "This returns a list of sublists of the permissions granted  within the
currently connected database. If an optional role is provided, the result is
limited to that role. The sublist returned will be in the form of role-name,
schema-name, table-name and then a string containing all the rights of that role
on that table in that schema."
  (if role
    (query "SELECT grantee,
            table_schema,
            table_name,
            string_agg(privilege_type, ', ' ORDER BY privilege_type) AS privileges
          FROM information_schema.role_table_grants
          WHERE grantee = $1
          GROUP BY grantee, table_catalog, table_schema, table_name;" role)
    (query "SELECT grantee,
                   table_schema,
                   table_name,
                   string_agg(privilege_type, ', ' ORDER BY privilege_type) AS privileges
            FROM information_schema.role_table_grants
            where grantee != 'postgres'
            and grantee != 'PUBLIC'
            and table_name not like 'pg_%'
            and grantor != grantee;")))

(defun change-password (role password &optional expiration-date)
  "Alters a role's password. If the optional expiration-date parameter is provided,
the password will expire at the stated date. A sample expiration date would be
'December 31, 2020'. If the expiration date is 'infinity', it will never expire.
The password will be encrypted in the system catalogs. This is
automatic with postgresql versions 10 and above."
  (when (role-exists-p role)
    (if expiration-date
        (query (format nil "alter role ~a with encrypted password '~a' valide until '~a'"
                       role password expiration-date)))
    (query (format nil "alter role ~a with encrypted password '~a'" role password))))
