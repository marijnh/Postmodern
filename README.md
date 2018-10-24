# Postmodern
A Common Lisp PostgreSQL programming interface
---
Postmodern is a Common Lisp library for interacting with [PostgreSQL](http://www.postgresql.org) databases. It is under active development. Features are:

- Efficient communication with the database server without need for foreign libraries.
- Support for UTF-8 on Unicode-aware Lisp implementations
- A syntax for mixing SQL and Lisp code
- Convenient support for prepared statements and stored procedures
- A metaclass for simple database-access objects

The biggest differences between this library and CLSQL/CommonSQL or cl-dbi are that Postmodern has no intention of being portable across different SQL implementations (it embraces non-standard PostgreSQL features), and approaches extensions like lispy SQL and database access objects in a quite different way. This library was written because the CLSQL approach did not really work for me, your mileage may vary.

## Contents
---
* [Dependencies](#dependencies)
* [License](#dependencies)
* [Download and installation](#download-and-installation)
* [Quickstart](#quickstart)
* [Running tests](#running-tests)
* [Reference](#reference)
* [Caveats and to-dos](#caveats-and-to-dos)
* [Resources](#resources)

## Dependencies
---
The library depends on usocket (except on SBCL and ACL, where the built-in socket library is used), md5, closer-mop, bordeaux-threads if you want thread-safe connection pools, and CL+SSL when SSL connections are needed.

Postmodern itself is split into four different packages, some of which can be used independently. Simple-date is a very basic implementation of date and time objects, used to support storing and retrieving time-related SQL types. CL-postgres is the low-level library used for interfacing with a PostgreSQL server over a socket. S-SQL is used to compile s-expressions to strings of SQL code, escaping any Lisp values inside, and doing as much as possible of the work at compile time. Finally, Postmodern itself is the library that tries to put all these things together into a convenient programming interface.

## License
---
Postmodern is released under a zlib-style license. Which approximately means you can use the code in whatever way you like, except for passing it off as your own or releasing a modified version without indication that it is not the original.

## Download and installation
---
We suggest using [quicklisp](https://www.quicklisp.org/beta/) for installation.

A git repository with the most recent changes can be viewed or checked out at https://github.com/marijnh/Postmodern


## Quickstart
---
This quickstart is intended to give you a feel of the way coding with Postmodern works. Further details about the workings of the library can be found in the reference manual.

Assuming you have already installed it, first load and use the system:


    (ql:quickload :postmodern)
    (use-package :postmodern)


If you have a PostgreSQL server running on localhost, with a database called 'testdb' on it, which is accessible for user 'foucault' with password 'surveiller', you can connect like this:


    (connect-toplevel "testdb" "foucault" "surveiller" "localhost")


Which will establish a connection to be used by all code, except for that wrapped in a with-connection form, which takes the same arguments but only establishes the connection locally.

Now for a basic sanity test:


    (query "select 22, 'Folie et déraison', 4.5")
    ;; => ((22 "Folie et déraison" 9/2))


That should work. query is the basic way to send queries to the database. The same query can be expressed like this:


    (query (:select 22 "Folie et déraison" 4.5))
    ;; => ((22 "Folie et déraison" 9/2))


In many contexts, query strings and lists starting with keywords can be used interchangeably. The lists will be compiled to SQL. The S-SQL manual describes the syntax used by these expressions. Lisp values occurring in them are automatically escaped. In the above query, only constant values are used, but it is possible to transparently use run-time values as well:


    (defun database-powered-addition (a b)
      (query (:select (:+ a b)) :single))
    (database-powered-addition 1030 204)
    ;; => 1234


That last argument, :single, indicates that we want the result not as a list of lists (for the result rows), but as a single value, since we know that we are only selecting one value. Some other options are :rows, :row, :column, :alists, and :none. Their precise effect is documented in the reference manual.

You do not have to pull in the whole result of a query at once, you can also iterate over it with the doquery macro:


    (doquery (:select 'x 'y :from 'some-imaginary-table) (x y)
      (format t "On this row, x = ~A and y = ~A.~%" x y))

You can work directly with the database or you can use a database-access-class (aka dao).
This is what a database-access class looks like:


    (defclass country ()
      ((name :col-type string :initarg :name
             :reader country-name)
       (inhabitants :col-type integer :initarg :inhabitants
                    :accessor country-inhabitants)
       (sovereign :col-type (or db-null string) :initarg :sovereign
                  :accessor country-sovereign))
      (:metaclass dao-class)
      (:keys name))


The above defines a class that can be used to handle records in a table with three columns: name, inhabitants, and sovereign. In simple cases, the information above is enough to define the table as well:


    (dao-table-definition 'country)
    ;; => "CREATE TABLE country (
    ;;      name TEXT NOT NULL,
    ;;      inhabitants INTEGER NOT NULL,
    ;;      sovereign TEXT,
    ;;      PRIMARY KEY (name))"
    (execute (dao-table-definition 'country))


This defines our table in the database. execute works like query, but does not expect any results back.

You can create tables directly without the need to define a class, and in more complicated cases, you will need to use the create-table operator. One example would be the following:


    (query (:create-table so-items
             ((item-id :type integer)
              (so-id :type (or integer db-null) :references ((so-headers id)))
              (product-id :type (or integer db-null))
              (qty :type (or integer db-null))
              (net-price :type (or numeric db-null)))
             (:primary-key item-id so-id)))


In the above case, the new table's name will be so-items (actually in the database it will be so_items because sql does not allow hyphens. The column item-id is an integer and cannot be null. The column so-id is also an integer, but is allowed to be null and is a foreign key to the id field in the so-headers table so-headers. The primary key is actually a composite of item-id and so-id. (If we wanted the primary key to be just item-id, we could have specified that in the form defining item-id.)

Let us go back to our approach using a dao class and add a few countries:


    (insert-dao (make-instance 'country :name "The Netherlands"
                                        :inhabitants 16800000
                                        :sovereign "Willem-Alexander"))
    (insert-dao (make-instance 'country :name "Croatia"
                                        :inhabitants 4400000))


Then, to update Croatia's population, we could do this:


    (let ((croatia (get-dao 'country "Croatia")))
      (setf (country-inhabitants croatia) 4500000)
      (update-dao croatia))
    (query (:select '* :from 'country))
    ;; => (("The Netherlands" 16800000 "Willem-Alexander")
    ;;     ("Croatia" 4500000 :NULL))


Next, to demonstrate a bit more of the S-SQL syntax, here is the query the utility function list-tables uses to get a list of the tables in a database:


    (sql (:select 'relname :from 'pg-catalog.pg-class
          :inner-join 'pg-catalog.pg-namespace :on (:= 'relnamespace 'pg-namespace.oid)
          :where (:and (:= 'relkind "r")
                       (:not-in 'nspname (:set "pg_catalog" "pg_toast"))
                       (:pg-catalog.pg-table-is-visible 'pg-class.oid))))
    ;; => "(SELECT relname FROM pg_catalog.pg_class
    ;;      INNER JOIN pg_catalog.pg_namespace ON (relnamespace = pg_namespace.oid)
    ;;      WHERE ((relkind = 'r') and (nspname NOT IN ('pg_catalog', 'pg_toast'))
    ;;             and pg_catalog.pg_table_is_visible(pg_class.oid)))"


sql is a macro that will simply compile a query, it can be useful for seeing how your queries are expanded or if you want to do something unexpected with them.

As you can see, lists starting with keywords are used to express SQL commands and operators (lists starting with something else will be evaluated and then inserted into the query). Quoted symbols name columns or tables (keywords can also be used but might introduce ambiguities). The syntax supports subqueries, multiple joins, stored procedures, etc. See the S-SQL reference manual for a complete treatment.

Finally, here is an example of the use of prepared statements:


    (defprepared sovereign-of
      (:select 'sovereign :from 'country :where (:= 'name '$1))
      :single!)
    (sovereign-of "The Netherlands")
    ;; => "Willem-Alexander"


The defprepared macro creates a function that takes the same amount of arguments as there are $X placeholders in the given query. The query will only be parsed and planned once (per database connection), which can be faster, especially for complex queries.


    (disconnect-toplevel)

## Running tests
---

Postmodern uses [FiveAM](https://github.com/sionescu/fiveam) for
testing.  The different component systems of Postmodern have tests
defined in corresponding test systems, each defining a test suite.
The test systems and corresponding top-level test suites are:

- `:postmodern` in `postmodern/tests`,
- `:cl-postgres` in `cl-postgres/tests`,
- `:s-sql` in `s-sql/tests`, and
- `:simple-date` in `simple-date/tests`.

Before running the tests make sure PostgreSQL is running and a test
database is created.  By default tests use the following connection
parameters to run the tests:

- Database name: test
- User: test
- Password: <empty>
- Hostname: localhost

If connection with these parameters fails then you will be asked to
provide the connection parameters interactively.  The parameters will
be stored in `cl-postgres-tests:*test-connection*` variable and
automatically used on successive test runs.  This variable can also be
set manually before running the tests.

To test a particular component one would first load the corresponding
test system, and then run the test suite.  For example, to test the
`postmodern` system in the REPL one would do the following:

    (ql:quickload "postmodern/tests")
    (5am:run! :postmodern)
    ;; ... test output ...

It is also possible to test multiple components at once by first
loading test systems and then running all tests:

    (ql:quickload '("cl-postgres/tests" "s-sql/tests"))
    (5am:run-all-tests)
    ;; ... test output ...

To run the tests from command-line specify the same forms using your
implementation's command-line syntax.  For instance, to test all
Postmodern components on SBCL, use the following command:

    env DB_USER=$USER sbcl --noinform \
        --eval '(ql:quickload "postmodern/tests")' \
        --eval '(ql:quickload "cl-postgres/tests")' \
        --eval '(ql:quickload "s-sql/tests")' \
        --eval '(ql:quickload "simple-date/tests")' \
        --eval '(progn (setq 5am:*print-names* nil) (5am:run-all-tests))' \
        --eval '(sb-ext:exit)'

As you can see from above, database connection parameters can be
provided using environment variables:

- `DB_NAME`: database name,
- `DB_USER`: user,
- `DB_PASS`: password,
- `DB_HOST`: hostname.

## Reference
---
The reference manuals for the different components of Postmodern are kept in separate files. For using the library in the most straightforward way, you only really need to read the Postmodern reference and glance over the S-SQL reference. The simple-date reference explains the time-related data types included in Postmodern, and the CL-postgres reference might be useful if you just want a low-level library for talking to a PostgreSQL server.

- [Postmodern](/doc/postmodern.html)
- [S-SQL](/doc/s-sql.html)
- [Simple-date](/doc/simple-date.html)
- [CL-postgres](/doc/cl-postgres.html)

## Caveats and to-dos
---
### Timezones
It is important to understand how postgresql (not postmodern) handles timestamps and timestamps with time zones. Postgresql keeps everything in UTC, it does not store a timezone even in a timezone aware column. If you use a timestamp with timezone column, postgresql will calculate the UTC time and will normalize the timestamp data to UTC. When you later select the record, postgresql will look at the timezone for the postgresql session, retrieve the data and then provide the data recalculated from UTC to the timezone for that postgresql session. There is a good writeup of timezones at [http://blog.untrod.com/2016/08/actually-understanding-timezones-in-postgresql.html](http://blog.untrod.com/2016/08/actually-understanding-timezones-in-postgresql.html) and [http://phili.pe/posts/timestamps-and-time-zones-in-postgresql/](http://phili.pe/posts/timestamps-and-time-zones-in-postgresql/).

Keeping that in mind, [Simple-date](http://marijnhaverbeke.nl/postmodern/simple-date.html) has no concept of time zones. If you really need your time-keeping to be reliable and/or universal you might consider using [local-time](https://github.com/dlowe-net/local-time), which solves the same problem as simple-date, but does understand time zones. We are considering the best ways to make life easier for users of the two libraries.

### Portability
The Lisp code in Postmodern is theoretically portable across implementations, and seems to work on all major ones and even less major ones such as  Genera. Implementations that do not have meta-object protocol support will not have DAOs, but all other parts of the library should work (all widely used implementations do support this).

The library is not likely to work for PostgreSQL versions older than 8.4.
Other features only work in newer Postgresql versions as the features
were only introduced in those newer versions.


### Things that should be implemented
Postmodern is under active development so Issues and feature requests should
be flagged on [[https://github.com/marijnh/Postmodern][Postmodern's site on github]].

It would be a nice feature if Postmodern could help you with defining your
database schemas and, more importantly, updating your databases when your code
changes. It would theoretically not be hard to build a function that compares
a schema on the Lisp side with the state of the database, and helps you to
interactively update your database. PostgreSQL has a quite complete
introspection system. Unfortunately it would be a lot of work to implement
this, since databases can contain so many different types of entities
(tables, views, indices, procedures, constraints, sequences, etc.) which are
all created, changed, and dropped in different ways.

Some areas that are currently under consideration can be found in the ROADMAP.md
file.

## Resources
---
- [Mailing List](https://mailman.common-lisp.net/listinfo/postmodern-devel)
- [A collection of Postmodern examples](https://sites.google.com/site/sabraonthehill/postmodern-examples)
- [The PostgreSQL manuals](http://www.postgresql.org/docs/current/static/index.html)
- [The wire protocol Postmodern uses](http://www.postgresql.org/docs/current/static/protocol.html)
- [Common Lisp Postgis library](https://github.com/filonenko-mikhail/cl-ewkb)
- [Local-time](http://common-lisp.net/project/local-time/)
