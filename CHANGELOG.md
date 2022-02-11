# Changelog v. 1.33.4
Fix bug in a warning in execute-file that referred to the current package rather than Postmodern.

Added retry-transaction restart in the call-with-transaction function

# Changelog v. 1.33.2

Fix bug in export functions when user tries to export nil into a database

Fix bug in import functions when functions are called but the current package has changed.
Note to self. Pay attention to needs for fully qualified symbols (including package names) and how to test them for equality.

# Changelog v. 1.33.1
Dao Export and Import Functions (Postmodern v. 1.33.1 and newer)

There may be times when the types of values in a dao slot do not have comparable types in Postgresql. For purposes of the following example, assume you have slots that you want to contain lists. Postgresql does not have a "list" data type. Postgresql arrays must be homogeneous but CL lists do not have that limitation. What to do?

One method would be to use text columns or jsonb columns in Postgresql and have functions that convert as necessary going back and forth. In the following example we will use text columns in Postgresql and write CL list data to string when we "export" the data to Postgresql and then convert from string when we "import" the data from Postgresql into a dao-class instance.

Consider the following dao-class definition. We have added additional column keyword parameters :col-export and :col-import. These parameters refer to functions which will convert the values from that slot to a valid Postgresql type (in our example, a string) on export to the database and from that Postgresql type to the type we want in this slot on import from the database.

```lisp
    (defclass listy ()
      ((id :col-type integer :col-identity t :accessor id)
       (name :col-type text :col-unique t :col-check (:<> 'name "")
             :initarg :name :accessor name)
       (rlist :col-type (or text db-null) :initarg :rlist :accessor rlist
              :col-export list-to-string :col-import string-to-list)
       (alist :col-type (or text db-null) :initarg :alist :accessor alist
              :col-export list-to-string :col-import string-to-alist)
       (plist :col-type (or text db-null) :initarg :plist :accessor plist
              :col-export list-to-string :col-import string-to-plist))
      (:metaclass dao-class)
      (:table-name listy))
```

Now we need to define the import functions. When writing your import functions, pay attention to how you want to handle nil or :NULL values as well as how you might want to error check the conversion from a Postgresql datatype to a CL datatype.

```lisp
    (defun string-to-list (str)
      "Take a string representation of a list and return a lisp list.
    Note that you need to handle :NULLs."
      (cond ((eq str :NULL)
             :NULL)
            (str
             (with-input-from-string (s str) (read s)))
            (t nil)))
```

And now we need to define the export function. In our example we are just going to be using format to write the CL value to a string. You are responsible for writing an export function that does what you need. This example just tells Postgresql to insert a string "unknown" if the slot value is not a list. You would need more error checking and condition handling.

```lisp
    (defun list-to-string (val)
      "Simply uses (format ..) to write a list out as a string"
      (if (listp val)
          (format nil "~a" val)
          "unknown"))
```

# Changelog v. 1.33.0
This version of Postmodern now provides the ability to pass parameters to Postgresql in binary format IF that format is available for that datatype. Currently this means int2, int4, int8, float, double-float (except clisp) and boolean. Rational numbers continue to be passed as text.

The flag is set in the database connection object. (Thank you Cyrus Harmon for suggesting that). This means it can be set either in the initial connection to the database or using the use-binary-parameters function to set it after the initial connection has been established. If you are using multiple connections, some can be set to use binary parameters, some not.

If a query to Postgresql does not have a table column which would allow Postgresql to determine the correct datatype and you do not specify differently, Postgresql will treat the parameters passed with the query as text. The default text setting with results:

```lisp
    (query "select $1" 1 :single)
    "1"
    (query "select $1" 1.5 :single)
    "1.5"
    (query "select $1" T :single)
    "true"
    (query "select $1" nil :single)
    "false"
    (query "select $1" :NULL :single)
    :NULL
```

You can specify parameter type as so:

```lisp
    (query "select $1::integer" 1 :single)
    1
```

Setting the use-binary slot in the database connection object to t has the following results:

```lisp
    (query "select $1" 1 :single)
    1
    (query "select $1" 1.5 :single)
    1.5
    (query "select $1" T :single)
    T
    (query "select $1" nil :single)
    NIL
    (query "select $1" :NULL :single)
    :NULL
```

The default for cl-postgres/Postmodern is to continue to pass parameters to Postgresql as text (not in binary format) in order to avoid breaking existing user code. If you want to pass parameters to Postgresql in binary format and want to set that up when you are making the database connection, the following examples may help. We continue the difference in the signatures (cl-postgres using optional parameters and postmodern using keyword parameters) because of the expected downstream breakage if we shifted cl-postgres:open-database to using keyword parameters.

The signature for opening a database in cl-postgres:

```lisp
    (defun open-database (database user password host
                          &optional (port 5432) (use-ssl :no)
                          (service "postgres") (application-name "")
                          (use-binary nil))
        ...)
```

or your favorite macro.

In postmodern you have the connect function or the with-connection macro:

```lisp
    (defun connect (database-name user-name password host
                    &key (port 5432) pooled-p
                    (use-ssl *default-use-ssl*)
                    (use-binary nil)
                    (service "postgres")
                    (application-name ""))
      ...)

    (defmacro with-connection (spec &body body)
      `(let ((*database* (apply #'connect ,spec)))
         (unwind-protect (progn ,@body)
           (disconnect *database*))))
```

In any case, you can set the flag after the connection is established with the use-binary-parameters function:

```lisp
    (pomo:use-binary-parameters *database* t)

    (cl-postgres:use-binary-parameters some-database-connection t)
```

Using binary parameters does tighten type checking when using prepared queries. You will not be able to use prepared queries with varying formats. In other words, if you have a prepared query that you pass an integer as the first parameter and a string as the second parameter the first time it is used, any subsequent uses of that prepared query during that session will also have to pass an integer as the first parameter and a string as the second parameter.

Benchmarking does indicate a slight speed and consing benefit to passing parameters as binary, but your mileage will vary depending on your use case.

In addition, this version also adds the ability to have queries returned as vectors of vectors, using a vectors keyword.

```lisp
    (query "select id, some_int, some_text from tests_data :where id = 1" :vectors)
;; or
    (query (:select 'id 'some-int 'some-text :from 'test-data)
         :vectors)
    #(#(1 2147483645 "text one")
      #(2 0 "text two")
      #(3 3 "text three"))
```

Like :array-hash, if there is no result it will return an empty array, not nil.
# Changelog v. 1.32.9
Adds new utility functions

- table-description-menu which allows you to pick and choose
what table characteristics you want returned. See giant docstring for details.

- get-schema-comment which takes a schema name and returns the schema comment
as a string

- list-check-constraints which takes a fully qualified table name and returns
a list of lists of check constraints where each sublist has the form
of (check-constraint-name check).

Example: (list-check-constraints "s2.employees")
(("employees_birth_date_check" "CHECK (birth_date > '1900-01-01'::date)")
 ("employees_check" "CHECK (start_date > birth_date)")
 ("employees_salary_check" "CHECK (salary > 0::numeric)"))

Now exports
get-column-comments (the parameter string has changed if you were using the internal version)
get-all-table-comments

Bug Fixes:

Fixes a bug when trying to connect to a database using ssl. If the keyword :try was used,
the connection would not fall back to non-ssl connections.

# Changelog v. 1.32.8
S-SQL Enhancements

## :Update
without the :columns parameter, :update requires alternating column value like so:

```lisp
    (query (:update 'weather
            :set 'temp-lo (:+ 'temp-lo 1)
                 'temp-hi (:+ 'temp-lo 15)
                 'prcp :default
            :where (:and (:= 'city "San Francisco")
                         (:= 'date "2003-07-03"))
            :returning 'temp-lo 'temp-hi 'prcp))
```

:update now accepts a :columns parameter. This allows the use of either :set or :select (both of which need to be enclosed in a form) to provide the values, allowing update queries like:

```lisp
    (query (:update 'weather
            :columns 'temp-lo 'temp-hi 'prcp
                     (:set (:+ 'temp-lo 1)  (:+ 'temp-lo 15) :DEFAULT)
            :where (:and (:= 'city "San Francisco")
                         (:= 'date "2003-07-03"))))

    (query (:update 't1
            :columns 'database-name 'encoding
                     (:select 'x.datname 'x.encoding
                     :from (:as 'pg-database 'x)
                     :where (:= 'x.oid 't1.oid))))
```

## :Insert-into
Insert-into also now accepts a :columns parameter which allows more precise use of select to insert values into specific row(s). A sample query could look like:

```lisp
    (query (:insert-into 't11
            :columns 'region 'subregion 'country
            (:select (:as 'region-name 'region)
                     (:as 'sub-region-name 'subregion)
                     'country
             :from 'regions)))
```

## Joins
### Lateral Joins
Joins are now expanded to include lateral joins. So addition join types are

- :join-lateral (best practice is still to be specific on what kind of join you want)
- :left-join-lateral
- :right-join-lateral
- :inner-join-lateral
- :outer-join-lateral
- :cross-join-lateral

### Ordinality
Selects can now use :with-ordinality or :with-ordinality-as parameters. Postgresql will give the new ordinality column the name of ordinality. :with-ordinality-as allows you to set different names for the columns in the result set.

```lisp
    (query (:select '*
            :from (:generate-series 4 1 -1)
            :with-ordinality))


    (query (:select 't1.*
            :from (:json-object-keys "{\"a1\":\"1\",\"a2\":\"2\",\"a3\":\"3\"}")
            :with-ordinality-as (:t1 'keys 'n)
```

## New Utility copy-from-csv
Just a convenience function. It runs the psql copy command from inside lisp using uiop:run-program

# Changelog v. 1.32.7

Additional capabilities for s-sql functions :insert-into and :insert-rows-into

Specifically, both can now use:

- overriding-system-value
- overriding-user-value
- on-conflict-do-nothing
- on-conflict
- on-conflict-on-constraint
- on-conflict-update
- do-nothing
- update-set
- from
- where
- returning

See updated s-sql docs for examples.

# Changelog v. 1.32.4

Added the ability to return results as json-encoded results as follows:

- :Json-strs
Return a list of strings where the row returned is a json object expressed as a string

    (query (:select 'id 'int4 'text :from 'short-data-type-tests :where (:< 'id 3)) :json-strs)
    ("{\"id\":1,\"int4\":2147483645,\"text\":\"text one\"}"
     "{\"id\":2,\"int4\":0,\"text\":\"text two\"}")

- :Json-str
Return a single string where the row returned is a json object expressed as a string

    (query (:select 'id 'int4 'text :from 'short-data-type-tests :where (:= 'id 3)) :json-str)
    "{\"id\":3,\"int4\":3,\"text\":\"text three\"}"

- :Json-array-str
Return a string containing a json array, each element in the array is a selected row expressed as a json object

    (query (:select 'id 'int4 'text :from 'short-data-type-tests :where (:< 'id 3)) :json-array-str)
    "[{\"id\":1,\"int4\":2147483645,\"text\":\"text one\"}, {\"id\":2,\"int4\":0,\"text\":\"text two\"}]"

# Changelog v. 1.32.3

Added flag to avoid SSL certificate verification if required by user

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
