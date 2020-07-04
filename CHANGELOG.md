# Changelog v. 1.32.1

## Fix S-SQL issue 239 (:drop-table ...) expanded incorrectly

(:drop-table ...) can again use variable input
Allowable permutations are:

    (let ((table-var1 "table-1")
          (table-var2 'table-1))
      (query (:drop-table :if-exists "table_1"))

      (query (:drop-table :if-exists table-var1 :cascade))

      (query (:drop-table :if-exists "table-1" :cascade))

      (query (:drop-table :if-exists 'table-1 :cascade))

      (query (:drop-table :if-exists table-var2 :cascade))

      (query (:drop-table (:if-exists "table-1") :cascade))

      (query (:drop-table :if-exists table-var1))

      (query (:drop-table :if-exists "table-1"))

      (query (:drop-table :if-exists 'table-1))

      (query (:drop-table :if-exists table-var2))

      (query (:drop-table (:if-exists "table-1")))

      (query (:drop-table table-var1 :cascade))

      (query (:drop-table "table-1" :cascade))

      (query (:drop-table 'table-1 :cascade))

      (query (:drop-table table-var2 :cascade))

      (query (:drop-table  "table-1" :cascade))

      (query (:drop-table table-var1))

      (query (:drop-table "table-1"))

      (query (:drop-table 'table-1))

      (query (:drop-table table-var2))

      (query (:drop-table 'table-1))

      (query (:drop-table "table-1")))

## Fix S-SQL issue 236 (:create-table ...) error on multiple attributes

The s-sql version of (:create-table ...) will now accept:

Basic table name permutations with :temp, :if-not-exists and :unlogged

    (query (:create-table 'distributors-in-hell)

    (query (:create-table "distributors-in_hell")

    (query (:create-table (:temp 'distributors-in_hell))

    (query (:create-table (:temp "distributors-in_hell"))

    (query (:create-table (:temp :if-not-exists "distributors-in_hell"))

    (query (:create-table (:temp :if-not-exists 'distributors-in-hell))

    (query (:create-table (:temporary 'distributors-in-hell))

    (query (:create-table (:temporary "distributors-in_hell"))

    (query (:create-table (:temporary :if-not-exists 'distributors-in_hell))

    (query (:create-table (:temporary :if-not-exists "distributors-in_hell"))

    (query (:create-table (:unlogged 'distributors-in_hell))

    (query (:create-table (:unlogged "distributors-in_hell"))

    (query (:create-table (:unlogged :if-not-exists 'distributors-in_hell))

    (query (:create-table (:unlogged :if-not-exists "distributors-in_hell"))

    (query (:create-table (:temp :unlogged 'distributors-in_hell))

    (query (:create-table (:temp :unlogged "distributors-in_hell"))

    (query (:create-table (:temp :unlogged :if-not-exists "distributors-in_hell"))

    (query (:create-table (:temp :unlogged :if-not-exists 'distributors-in-hell))


Expanding table names with composite types

    (query (:create-table (:of distributors-in-hell 'employee-type))

    (query (:create-table (:of distributors-in-hell "employee-type"))

    (query (:create-table (:temp :of distributors-in-hell 'employee-type))

    (query (:create-table (:temp :of distributors-in-hell "employee-type"))

    (query (:create-table (:temp :unlogged :of distributors-in-hell 'employee-type))

    (query (:create-table (:temp :unlogged :of distributors-in-hell "employee_type"))

    (query (:create-table (:temp :if-not-exists :of distributors-in-hell 'employee-type))

    (query (:create-table (:temp :if-not-exists :of distributors-in-hell "employee-type"))

    (query (:create-table (:temp :unlogged :if-not-exists :of distributors-in-hell 'employee-type))

    (query (:create-table (:temp :unlogged :if-not-exists :of distributors-in-hell "employee_type"))

    (query (:create-table (:unlogged :of distributors-in-hell 'employee-type))

    (query (:create-table (:unlogged :of distributors-in-hell "employee_type"))


Expanding table names with wrapping the table name in a sublist with :if-not-exists

    (query (:create-table (:temp (:if-not-exists "distributors-in_hell")))

    (query (:create-table (:temp (:if-not-exists 'distributors-in-hell)))

    (query (:create-table (:temporary (:if-not-exists 'distributors-in_hell)))

    (query (:create-table (:temporary (:if-not-exists "distributors-in_hell")))

    (query (:create-table (:unlogged (:if-not-exists 'distributors-in_hell)))

    (query (:create-table (:unlogged (:if-not-exists "distributors-in_hell")))

    (query (:create-table (:temp :unlogged (:if-not-exists "distributors-in_hell")))

    (query (:create-table (:temp :unlogged (:if-not-exists 'distributors-in-hell)))


Note: (:create-table ...) does not accept variables as the table name

### Some additional tests, and small formatting and documentation changes


# Changelog v. 1.32

## Highlights

- Daos now have the ability to specify columns as identity, unique, references,
  primary key or check.

- New create-database and create-role functions make it easier to set readonly
  or edit-only permissions on databases, schemas or tables.

- More documentation on connections (toplevel v. with-connection), dao-utilitization
  and more examples for s-sql

## New Functionality for DAOs
Daos now have the ability to specify columns as identity, unique, references,
primary key or check.

As an example:


     (defclass country ()
       ((id :col-type integer :col-identity t :accessor id)
        (name :col-type string :col-unique t :check (:<> 'name "")
              :initarg :name :reader country-name)
        (inhabitants :col-type integer :initarg :inhabitants
                     :accessor country-inhabitants)
        (sovereign :col-type (or db-null string) :initarg :sovereign
                   :accessor country-sovereign)
        (region-id :col-type integer :col-references ((regions id))
                   :initarg :region-id :accessor region-id))
       (:metaclass dao-class)
       (:table-name countries))

In this example we have an id column which is specified to be an identity column.
Postgresql will automatically generate a sequence of of integers and this will
be the primary key.

We have a name column which is specified as unique and is not null and the
check will ensure that the database refuses to accept an empty string as the name.

We have a region-id column which references the id column in the regions table.
This is a foreign key constraint and Postgresql will not accept inserting a country
into the database unless there is an existing region with an id that matches this
number. Postgresql will also not allow deleting a region if there are countries
that reference that region's id. If we wanted Postgresql to delete countries when
regions are deleted, that column would be specified as:

     (region-id :col-type integer :col-references ((regions id) :cascade)
       :initarg :region-id :accessor region-id)


Now you can see why the double parens.

## New S-SQL Functionality

Fetch is a more efficient way to do pagination instead of using limit and
offset. Fetch allows you to retrieve a limited set of rows, optionally offset
by a specified number of rows. In order to ensure this works correctly, you
should use the order-by clause. If the amount is not provided, it assumes
you only want to return 1 row. https://www.postgresql.org/docs/current/sql-select.html
Examples:

     (query (:fetch (:order-by (:select 'id :from 'historical-events) 'id) 5))

     ((1) (2) (3) (4) (5))

     (query (:fetch (:order-by (:select 'id :from 'historical-events) 'id) 5 10))

     ((11) (12) (13) (14) (15))


## New Postmodern Functions

- function get-database-comment (database-name)

  Returns the comment, if any, attached to a database.

- function change-toplevel-database (new-database user password host)

  Just changes the database assuming you are using a toplevel connection.
  Recommended only for development work. Returns the name of the newly connected
  database as a string.

### database-management

- function create-database (database-name &key (encoding "UTF8") (connection-limit -1)
  owner limit-public-access comment collation template)

  Creates and initializes a database. Besides the obvious database-name parameter,
  you can also use key parameters to set encoding (defaults to UTF8), owner,
  connection-limit (defaults to no limit)). If limit-public-access is set to t,
  then only superuser roles or roles with explicit access to this database will
  be able to access it.  If collation is set, the assumption is that template0
  needs to be used as the base of the database rather than template1 which may
  contain encoding specific or locale specific data.

- function list-templates ()

  Returns a list of existing database template names.

- function list-available-collations ()

  Get a list of the collations available from the current database cluster.
  Collations are a mess as different operating systems provide different
  collations. We might get some sanity if Postgresql can use ICU as the default.
  See https://wiki.postgresql.org/wiki/Collations.

- function list-database-access-rights (&optional database-name)

  If the database parameter is specifed, this returns an list of lists where
  each sublist is a role name and whether they have access rights (t or nil) to that
  particular database. If the database-name is not provided, the sublist is
  a database name, a role name and whether they have access rights (t or nil). This
  excludes the template databases.

### roles

- function list-role-permissions (&optional role)

  This returns a list of sublists of the permissions granted  within the
  currently connected database. If an optional role is provided, the result is
  limited to that role. The sublist returned will be in the form of role-name,
  schema-name, table-name and then a string containing all the rights of that role
  on that table in that schema.

- function create-role (name password &key (base-role :readonly) (schema :public)
                                    (tables :all) (databases :current)
                                    (allow-whitespace nil)
                                    (allow-utf8 nil)
                                    (allow-disallowed-names nil) (comment nil))

  Keyword parameters: Base-role. Base-role should be one of :readonly, :editor,
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
    is https://github.com/flurdy/bad_usernames and
    https://github.com/dsignr/disallowed-usernames/blob/master/disallowed%20usernames.csv,
    and https://www.b-list.org/weblog/2018/feb/11/usernames/

    :allow-disallowed-names defaults to nil. If nil, the user name will be checked
    against *disallowed-role-names*.

    As an aside, if allowing utf8 in names, you might want to think about whether
    you should create second copy of the username in the original casing and normalized
    as NFC for display purposes as opposed to normalizing to NFKC. It might be viewed
    as culturally insensitive to change the display of the name.

- function alter-role-search-path (role search-path)

  Changes the priority of where a role looks for tables (which schema first,
  second, etc. Role should be a string or symbol. Search-path could be a list of schema
  names either as strings or symbols.

- function change-password (role password &optional expiration-date)

  Alters a role's password. If the optional expiration-date parameter is provided,
  the password will expire at the stated date. A sample expiration date would be
  'December 31, 2020'. If the expiration date is 'infinity', it will never expire.
  The password will be encrypted in the system catalogs. This is
  automatic with postgresql versions 10 and above.

- function grant-role-permissions (role-type name &key (schema :public) (tables :all)
                                             (databases :all))

  Grant-role-permissions assumes that a role has already been created, but
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
  the connected database at the time of execution of this function.

- function grant-readonly-permissions (schema-name role-name &optional (table-name nil))

  Grants select privileges to a role for the named schema. If the optional
  table-name parameter is provided, the privileges are only granted with respect
  to that table. Note that we are giving some function execute permissions if
  table-name is nil, but if the table-name is specified, those are not provided.
  Your mileage may vary on how many privileges you want to provide to a
  read-only role with access to only a limited number of tables.

- function grant-editor-permissions (schema-name role-name &optional (table-name nil))

  Grants select, insert, update and delete privileges to a role for the named
  schema. If the optional table-name parameter is provided, the privileges are only
  granted with respect to that table. Note that we are giving some function execute
  permissions if table-name is nil, but if the table-name is specified, those are
  not provided. Your mileage may vary on how many privileges you want to provide
  to a editor role with access to only a limited number of tables.

- function grant-admin-permissions (schema-name role-name &optional (table-name nil))

  Grants all privileges to a role for the named schema. If the optional table-name
  parameter is provided, the privileges are only granted with respect to that table.

- function revoke-all-on-table (table-name role-name)

  Takes a table-name which could be a string, symbol or list of strings or symbols
  of tables names, a role name and revokes all privileges that role-name may have
  with that/those tables. This is limited to the currently connected database and
  can only revoke the privileges granted by the caller of the function.

- function list-role-accessible-databases (role-name)

  Returns a list of the databases to which the specified role can connect.

### tables

- function get-table-comment (table-name &optional schema-name)

  Retrieves the comment, if any attached to the table

### general utilities

- function add-comment (type name comment &optional (second-name ""))

  Attempts to add a comment to a particular database object. The first parameter
  is a keyword for the type of database object. The second parameter is the name of
  the object. The third parameter is the comment itself. Some objects require an
  additional identifier. The names can be strings or symbols.

  Example usage would be:

      (add-comment :database 'my-database-name "Does anyone actually use this database?".)

      (add-comment :column 'country-locations.name "Is what it looks like - the name of a country".)

      (add-comment :column "country_locations.name" "Is what it looks like - the name of a country".)

  Example usage where two identifiers are required would be constraints:

      (add-comment :constraint 'constraint1  "Some kind of constraint descriptions here".
                   'country-locations)


- function postgres-array-string-to-list (str)

  Takes a postgresql array in the form of a string like
 "{sabra=CTc/sabra,a=c/sabra,b=c/sabra}"

  and returns a lisp list like

  ("sabra=CTc/sabra" \"a=c/sabra" "b=c/sabra")

- function postgres-array-string-to-array (str)

  Same thing but returns an array
