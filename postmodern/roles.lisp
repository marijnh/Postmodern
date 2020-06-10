;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

;; Per the Postgresql Documentation, CREATE ROLE adds a new role to a PostgreSQL
;; database cluster. A role is an entity that can own database objects and have
;; database privileges; a role can be considered a “user”, a “group”, or both
;; depending on how it is used.
;; https://www.postgresql.org/docs/current/sql-createrole.html. The only real
;; difference between "create role" and "create user" is that create user
;; defaults to having a login attribute and create role defaults to not having
;;a login attribute.

;; Pre-Postgresql version 8.1, groups and users were distinct kinds of entities.
;; Roles have replaced that and can operate as either. Often applications will
;; have their own concept of users and the application will itself have one or
;; more types of roles to which the application user is assigned. So, for
;; example, the application may have two roles - reader and editor with which
;; it interacts with postgresql and then there are many application users
;; registered with the application and probably listed in some type of user
;; table in postgresql that the application manages. When users 1,2 or 3 log in
;; to the application, the application might connect to the postgresql cluster
;; using a role that only has read (select) permissions. When users 4 or 5 log
;; in to the application, the applicatin might connect to the postgresql cluster
;; using a role that has read, insert, update and delete permission. Postmodern
;; provides a simplified create-role system allowing easy creation of roles that
;; have readonly, editor or superuser type permissions. Further, those
;; permissions can be limited to individual databases, schemas or tables.

;; You really want to separate application users from roles. Make it easy to
;; drop application users. Dropping roles requires going through every database,
;; reassigning ownership of any objects that role might own or have privileges
;; on, then dropping ownership of objects, then dropping the role itself.

#|
CREATE ROLE myapp_readonly;
GRANT CONNECT ON DATABASE defaultdb TO myapp_readonly;
GRANT USAGE ON SCHEMA myapp TO myapp_readonly;
GRANT SELECT ON TABLE "myapp"."employees" TO myapp_readonly;
GRANT SELECT ON TABLE "myapp"."jobs" TO myapp_readonly;
GRANT SELECT (id, name) ON TABLE myapp.customers TO myapp_readonly;
CREATE USER redash WITH PASSWORD 'secret';
GRANT myapp_readonly TO redash;

Variation?
CREATE ROLE xxx LOGIN PASSWORD 'yyy';
GRANT SELECT ON ALL TABLES IN SCHEMA public TO xxx;

This only affects tables that have already been created. More powerfully, you
can automatically have default roles assigned to new objects in future:

ALTER DEFAULT PRIVILEGES IN SCHEMA public
  GRANT SELECT ON TABLES TO xxx;

Variation?
-- Create a group
CREATE ROLE readaccess;

-- Grant access to existing tables
GRANT USAGE ON SCHEMA public TO readaccess;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO readaccess;

-- Grant access to future tables
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO readaccess;

-- Create a final user with password
CREATE USER tomek WITH PASSWORD 'secret';
GRANT readaccess TO tomek;

-- Revoke ability to create tables

    REVOKE ALL ON SCHEMA public FROM public
    GRANT ALL ON SCHEMA public TO writeuser

Hi there are four databases present in this server. When i create this user,
that user can create tables in different databases. I want to restrict that too.

Variation?
CREATE USER readonly  WITH ENCRYPTED PASSWORD 'readonly';
GRANT USAGE ON SCHEMA public to readonly;
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO readonly;

-- repeat code below for each database:

GRANT CONNECT ON DATABASE foo to readonly;
\c foo
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO readonly;
--- this grants privileges on new tables generated in new database "foo"
GRANT USAGE ON SCHEMA public to readonly;
GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO readonly;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO readonly;

Notes:
In PostgreSQL, every connection is to a particular database, and nearly
everything you run has effect only within that one database. (One exception is
that creation of users/roles themselves is global to the entire "cluster",
i.e. running Postgres server.)

In the psql commands you show, you are connecting the first time without
specifying a database (psql), which connects you to a database named after the
current system user, which is postgres. This is shown at the beginning of the
psql prompt: postgres=# (the # shows that you are connected as a supersuser).


-----------------------
n PostgreSQL*, you cannot drop a database while clients are connected to it.

At least, not with the dropdb utility - which is only a simple wrapper around
DROP DATABASE server query.

Quite robust workaround follows:

Connect to your server as superuser, using psql or other client. Do not use the
database you want to drop.

psql -h localhost postgres postgres

Now using plain database client you can force drop database using three simple
steps:

    Make sure no one can connect to this database. You can use one of following
methods (the second seems safer, but does not prevent connections from
superusers).

    /* Method 1: update system catalog */
    UPDATE pg_database SET datallowconn = 'false' WHERE datname = 'mydb';

    /* Method 2: use ALTER DATABASE. Superusers still can connect!
    ALTER DATABASE mydb CONNECTION LIMIT 0; */

    Force disconnection of all clients connected to this database, using
pg_terminate_backend.

    SELECT pg_terminate_backend(pid)
    FROM pg_stat_activity
    WHERE datname = 'mydb';

    /* For old versions of PostgreSQL (up to 9.1), change pid to procpid:

    SELECT pg_terminate_backend(procpid)
    FROM pg_stat_activity
    WHERE datname = 'mydb'; */

    Drop the database.

    DROP DATABASE mydb;

Step 1 requires superuser privileges for the 1st method, and database owner
privileges for the 2nd one. Step 2 requires superuser privileges. Step 3
requires database owner privilege.
------------------------------

|#
(defparameter *alter-all-default-select-privileges*
  '("grant usage on schema ~a to ~a"
    "grant select on all tables in schema ~a to ~a"
    "grant select on all sequences in schema ~a to ~a"
    "alter default privileges in schema ~a grant select on tables to ~a"))

(defparameter *alter-all-default-editor-privileges*
  '("grant usage on schema ~a to ~a"
    "grant select, insert, update, delete on all tables in schema ~a to ~a"
    "alter default privileges in schema ~a grant select, insert, update, delete on tables to ~a"    ))

(defparameter *execute-privileges-list*
  '("grant execute on all functions in schema ~a to ~a"))

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

(defparameter *disallowed-user-names*
  (let ((words (make-hash-table :test 'equal)))
    (dolist (word '(".htaccess" ".htpasswd" ".json" ".rss" ".well-known" ".xml"
                    "about" "abuse" "access" "account" "accounts" "activate"
                    "ad" "add" "address" "adm" "admin" "admins" "administration"
                    "administrator" "administrators" "ads" "adult" "advertising"
                    "affiliate" "affiliates" "ajax" "all" "analytics" "android"
                    "anon" "anonymous" "api" "app" "apps" "archive" "atom"
                    "auth" "authentication" "authorize" "autoconfig"
                    "autodiscover" "avatar" "backend" "backoffice" "backup"
                    "bad" "banner" "banners" "best" "beta" "billing" "bin"
                    "blackberry" "blog" "blogs" "board" "bot" "bots"
                    "broadcasthost" "business" "bug" "buy" "cache" "calendar"
                    "campaign" "career" "careers" "cart" "cdn" "ceo" "cfo"
                    "cgi" "chat" "chef" "client" "clientaccesspolicy.xml"
                    "clients" "code" "codes" "commercial" "compare" "config"
                    "configuration" "connect" "contact" "contact-us" "contactus"
                    "contest" "coo" "cookie" "copyright" "corporate" "create"
                    "crossdomain" "crossdomain.xml" "css" "cto" "customer"
                    "dash" "dashboard" "data" "database" "db" "delete" "demo"
                    "design" "designer" "dev" "devel" "developer" "developers"
                    "development" "dir" "directory" "dmca" "doc" "docs"
                    "document" "documents" "documentation" "domain""domainadmin"
                    "domainadministrator" "download" "downloads" "ecommerce"
                    "edit" "editor" "email" "embed" "enquiry" "enterprise"
                    "facebook" "faq" "favicon.ico" "favorite" "favorites"
                    "favourite" "favourites" "feed" "feedback" "feeds"
                    "file" "files" "follow" "font" "fonts" "forum" "forums"
                    "free" "ftp" "gadget" "gadgets" "games" "gift" "good"
                    "google" "group" "groups" "guest" "guests" "help"
                    "helpcenter" "home" "homepage" "host" "hosting" "hostmaster"
                    "host-master" "host_master" "hostname" "html" "http" "httpd"
                    "https" "humans.txt" "image" "images" "imap" "img"
                    "index" "indice" "info" "information" "inquiry" "intranet"
                    "invite" "ipad" "iphone" "irc" "is" "isatap" "it" "java"
                    "javascript" "job" "jobs" "js" "json" "keybase.txt"
                    "knowledgebase" "ldap" "legal" "license" "list"
                    "list-request" "lists" "localdomain" "localhost" "log"
                    "login" "logout" "logs" "mail" "mailer-daemon" "majordomo"
                    "manager" "manifesto" "map" "marketing" "master" "me"
                    "media" "member" "membership" "message" "messages"
                    "messenger" "mine" "mis" "mob" "mobile" "msg" "must" "mx"
                    "my" "myaccount" "mysql" "name" "named" "net" "network"
                    "new" "newest" "news" "newsletter" "no-reply" "nobody" "noc"
                    "noreply" "notes" "oauth" "oembed" "office" "old" "oldest"
                    "online" "operator" "order" "orders" "owner " "page" "pager"
                    "pages" "panel" "password" "pay" "payment" "payments" "perl"
                    "photo" "photos" "php" "pic" "pics" "plan" "plans" "plugin"
                    "plugins" "pop" "pop3" "portfolio" "post" "postfix"
                    "postmaster" "post-master" "post_master" "posts"
                    "preferences" "press" "pricing" "privacy" "privacy-policy"
                    "private" "profile" "project" "projects" "promo" "pub"
                    "public" "python" "random" "recipe" "recipes" "register"
                    "registration" "remove" "request" "reset" "robots"
                    "robots.txt" "root" "rss" "ruby" "sale" "sales" "sample"
                    "samples" "save" "script" "scripts" "search" "secure"
                    "security" "send" "service" "services" "setting" "settings"
                    "setup" "sftp" "shop" "shopping" "signin" "signout" "signup"
                    "site" "sitemap" "sitemap.xml" "sites" "smtp" "sql" "src"
                    "ssh" "ssl" "ssladmin" "ssladministrator" "sslwebmaster"
                    "ssl_admin" "ssl-admin" "ssl_administrator"
                    "ssl-administrator" "ssl_webmaster" "ssl-webmaster"
                    "stage" "staging" "start" "stat" "static" "stats" "status"
                    "store" "stores" "subdomain" "subscribe" "support"
                    "surprise" "svn" "sys" "sysadmin" "sysadministrator" "sysop"
                    "system" "tablet" "tablets" "talk" "task" "tasks" "tech"
                    "telnet" "terms" "terms-of-use" "test" "test1" "test2"
                    "test3" "tests" "theme" "themes" "tmp" "todo" "tools" "top"
                    "tos" "trouble" "trust" "tv" "twitter" "twittr"
                    "unsubscribe" "update" "upload" "url" "usage" "usenet" "user"
                    "username" "users" "uucp" "video" "videos" "visitor" "web"
                    "weblog" "webmail""web_master" "web-master" "web_admin"
                    "web-admin" "wheel" "webmaster" "website" "websites"
                    "welcome" "wiki" "win" "work" "wpad" "ww" "wws" "www" "www1"
                    "www2" "www3" "www4" "www5" "www6" "www7" "wwws" "wwww"
                    "xml" "xpg" "xxx" "yahoo" "you" "yourdomain" "yourname"
                    "yoursite" "yourusername"))
      (setf (gethash word words) t))
    words)
  "A set of words that maybe should be disallowed from user names. Edit as you
please.")

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
         (list-databases :names-only t))
        ((listp databases)
         (intersection (mapcar #'to-sql-name databases)
                       (list-databases :names-only t)
                       :test #'equal))
        ((stringp databases)
         (if (database-exists-p databases)
             (list (to-sql-name databases))
             nil))
        (t (list (to-sql-name databases)))))

(defun admin-permissions (schema-name user-name &optional (table-name nil))
  "Note that we are giving some function execute permissions if table-name is
nil, but if the table-name is specified, those are not provided. Your mileage
may vary on how many privileges you want to provide to a editor role with access
to only a limited number of tables."
  (cond ((not table-name)
         (query (format nil "grant all on all tables in schema ~a to ~a"
                        schema-name user-name))
         (query (format nil "grant all on all sequences in schema ~a to ~a"
                        schema-name user-name))
         (query (format nil "alter default privileges in schema ~a grant all on tables to ~a"
                        schema-name user-name))
         (query (format nil "alter default privileges in schema ~a grant all on sequences to ~a"
                        schema-name user-name))
         (loop for x in *execute-privileges-list* do
           (query (format nil x schema-name user-name))))
        (t
         (query (format nil "grant all on table ~a.~a to ~a"
                        schema-name table-name user-name)))))

(defun editor-permissions (schema-name user-name &optional (table-name nil))
  "Note that we are giving some function execute permissions if table-name is
nil, but if the table-name is specified, those are not provided. Your mileage
may vary on how many privileges you want to provide to a editor role with access
to only a limited number of tables."
  (cond ((not table-name)
         (loop for x in *alter-all-default-editor-privileges* do
           (query (format nil x schema-name user-name)))
         (loop for x in *execute-privileges-list* do
           (query (format nil x schema-name user-name))))
        (t
         (format nil "grant select, insert, update, delete on table ~a.~a to ~a"
                 schema-name table-name user-name))))

(defun readonly-permissions (schema-name user-name &optional (table-name nil))
  "Note that we are giving some function execute permissions if table-name is
nil, but if the table-name is specified, those are not provided. Your mileage
may vary on how many privileges you want to provide to a read-only role with
access to only a limited number of tables."
  (cond ((not table-name)
         (loop for x in *alter-all-default-select-privileges* do
           (let ((query-string (format nil x schema-name user-name)))
             (query query-string)))
         (loop for x in *execute-privileges-list* do
           (let ((query-string (format nil x schema-name user-name)))
             (query query-string))))
        (t
         (query (format nil "grant select on table ~a.~a to ~a"
                        schema-name table-name user-name)))))

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
                    (admin-permissions schema-x name))
                   (:editor
                    (editor-permissions schema-x name))
                   (:readonly
                    (readonly-permissions schema-x name))
                   (t (query (readonly-permissions schema-x name)))))
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
                          (admin-permissions schema-x name table-y))
                         (:editor
                          (editor-permissions schema-x name table-y))
                         (:readonly
                          (readonly-permissions schema-x name table-y))
                         (t (readonly-permissions schema-x name table-y)))))))
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
particular privileges or restrictions.

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
against *disallowed-user-names*.

 As an aside, if allowing utf8 in names, you might want to think about whether
you should second copy of the username in the original casing and normalized as
NFC for display purposes as opposed to normalizing to NFKC. It might be viewed
as culturally insensitive to change the display of the name."
  (cond ((and (not allow-whitespace)
              (or (whitespace-in-string name)
                  (whitespace-in-string password)))
         (return-from create-role "Illegal whitespace in name or password"))
        ((and (not allow-disallowed-names)
              (gethash name *disallowed-user-names*))
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

(defun find-objects-by-owner (owner)
  "Returns a list of lists in the form of (schema, object) for each object in
the current database owned by the owner. Procedures owned by the owner will
have an additional result of the procedure number e.g.
 (schema, procedure-name, object number). A procedure could be sequences."
  (query "SELECT
              n.nspname AS schema_name,
              c.relname AS rel_name,
              c.relkind AS rel_kind
            FROM pg_class c
            JOIN pg_namespace n ON n.oid = c.relnamespace
            where pg_get_userbyid(c.relowner) = $1
            UNION ALL
            -- functions (or procedures)
            SELECT
              n.nspname AS schema_name,
              p.proname,
              'p'
            FROM pg_proc p
            JOIN pg_namespace n ON n.oid = p.pronamespace
            where pg_get_userbyid(p.proowner) = $2;" owner owner))

(defun drop-role (name &optional (new-owner "postgres") (database :all))
  "Before dropping the role, you must drop all the objects it owns (or reassign
their ownership) and revoke any privileges the role has been granted on other
objects. Drop-role will drop objects owned by a role in the current database.
Question 1. Is the role attached to other databases? The reassignment and
dropping objects will only apply to the currently connected database.
Question 2. We will reassign ownership of the objects to the postgres role
unless otherwise specified in the optional second parameter. Returns t if
successful. Will not drop the postgres role."
  (if (eq database :all)
      (loop for x in (list-databases :names-only t) do
        (with-connection (list x (cl-postgres::connection-user *database*)
                               (cl-postgres::connection-password *database*)
                               (cl-postgres::connection-host *database*)
                               :port (cl-postgres::connection-port *database*)
                               :use-ssl (cl-postgres::connection-use-ssl *database*))
          (when (and (not (string= name "postgres"))
                     (member name (list-database-users) :test #'equal))
            (query (format nil "reassign owned by ~a to ~a" name new-owner))
            (query (format nil "drop owned by ~a" name)))))
      (with-connection (list database (cl-postgres::connection-user *database*)
                             (cl-postgres::connection-password *database*)
                             (cl-postgres::connection-host *database*)
                             :port (cl-postgres::connection-port *database*)
                             :use-ssl (cl-postgres::connection-use-ssl *database*))
        (when (and (not (string= name "postgres"))
                   (member name (list-database-users) :test #'equal))
          (query (format nil "reassign owned by ~a to ~a" name new-owner))
          (query (format nil "drop owned by ~a" name)))))
      (query (format nil "drop role if exists ~a" name))
      (not (role-exists-p name)))

(defun list-role-permissions (&optional role)
  "This checks the permissions granted  within the currently connected database.
If an optional role is provided, the result is limited to that role."
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


#|
Notes:
Every connection is specific to a particular database. However, creating roles
or users is global to the entire cluster (the running postgresql server). You
can create policies for any individual database, schema or table, but you need
to ensure that those policies also apply to any subsequently created database,
schema or table. Note that each user is automatically a member of the public
group, so you need to change those policies for public as well.

Option for limiting user name and password to ascii, otherwise normalize both.

Do we need to implement confusables? See http://www.unicode.org/reports/tr39/

Do we want to throw an error if whitespace in user name or password?
Do we want to throw an error if role already exists (after normalizing and with
equalp?

How to handle confusables?
https://confusable-homoglyphs.readthedocs.io/en/latest/index.html


CREATE ROLE xxx LOGIN PASSWORD 'yyy';
GRANT SELECT ON ALL TABLES IN SCHEMA public TO xxx;

This only affects tables that have already been created. More powerfully, you
can automatically have default roles assigned to new objects in future:

* alter default privileges in schema public grant select on tables to xxx;


Variation?


-- repeat code below for each database:

GRANT CONNECT ON DATABASE foo to readonly;
\c foo
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO readonly;
--- this grants privileges on new tables generated in new database "foo"
GRANT USAGE ON SCHEMA public to readonly;
GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO readonly;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO readonly;

-- Find objects with owner X

|#
