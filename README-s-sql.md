This is the README for the S-SQL component of the postmodern library.

S-SQL provides a lispy syntax for SQL queries, and knows how to convert various
lisp types to their textual SQL representation. It takes care to do as much of
the work as possible at compile-time, so that at runtime a string concatenation
is all that is needed to produce the final SQL query.


# SQL Syntax
An S-SQL form is converted to a query through the following rules:

- Lists starting with a keyword are operators. They are expanded as described below if they are known, otherwise they are expanded in the standard way: operator(arguments, ...)

- Quoted symbols or keywords are interpreted as names of columns or tables, and converted to strings with to-sql-name.

- Anything else is evaluated and the resulting Lisp value is converted to its textual SQL representation (or an error is raised when there is no rule for converting objects of this type). Self-quoting atoms may be converted to strings at compile-time.

# SQL Types
S-SQL knows the SQL equivalents to a number of Lisp types, and defines some
extra types that can be used to denote other SQL types. The following
table shows the correspondence:

 Lisp type                      | 	SQL type
------------------------------  | --------------------
 smallint	                    | smallint
 integer	                    | integer
 bigint	                        | bigint
 (numeric X Y)                  | numeric(X, Y)
 float, real	                | real
 double-float, double-precision | double-precision
 string, text                   | text
 (string X)                     | char(X)
 (varchar X)	                | varchar(X)
 boolean	                    | boolean
 bytea	                        | bytea
 date	                        | date
 timestamp	                    | timestamp
 interval	                    | interval
 array                          | array

- type db-null

This is a type of which only the keyword :null is a member. It is used to represent
NULL values from the database.

<a id="org6afbee2"></a>

# Dynamic Queries, Composition and Parameterized Queries


<a id="org93eb5ad"></a>

## Overview

The question gets asked how to build dynamic or composable queries
in postmodern. First we need to understand the context - is the programmer building the query or are you taking data from a user and using that to build a query?


<a id="orgc3f4259"></a>

### Programmer Built Queries

If you are not using s-sql, then it becomes easy. The query macro assumes that everything that is not a list starting with a keyword will evaluate to a string. That means you can build it with a simple format string

    (query (format nil "select ~a from ~a where ~a"  "carrots" "garden" "length > 3"))

With s-sql, there are generally two approaches to building dynamic or composible queries:
either use sql-compile or use :raw.

For purposes of this example, we will use the following employee table:

    (query (:create-table employee ((id :type int)
                                    (name :type text)
                                    (salary :type numeric)
                                    (start_date :type date)
                                    (city :type text)
                                    (region :type char)
                                    (age :type int))))

    (query (:insert-rows-into 'employee
                              :columns 'id 'name 'salary 'start-date 'city 'region 'age
                              :values '((1 "Jason" 40420 "02/01/94" "New York" "W" 29)
                                        (2 "Robert" 14420 "01/02/95" "Vancouver" "N" 21)
                                        (3 "Celia" 24020 "12/03/96" "Toronto" "W" 24)
                                        (4 "Linda" 40620 "11/04/97" "New York" "N" 28)
                                        (5 "David" 80026 "10/05/98" "Vancouver" "W" 31)
                                        (6 "James" 70060 "09/06/99" "Toronto" "N" 26)
                                        (7 "Alison" 90620 "08/07/00" "New York" "W" 38)
                                        (8 "Chris" 26020 "07/08/01" "Vancouver" "N" 22)
                                        (9 "Mary" 60020 "06/08/02" "Toronto" "W" 34))))

1.  Approach #1 Use sql-compile

    Sql-compile does a run-time compilation of an s-sql expression. In the
    following example, we create a function that accepts a where-clause,
    a table-name, 3 columns to select and two parameters to go into the where
    clause.

        (defun toy-example (where-clause table-name col1 col2 col3 arg1 arg2)
          (with-test-connection
           (query (sql-compile
                   (append `(:select ,col1 ,col2 ,col3 :from ,table-name :where)
                           where-clause))
                  arg1 arg2)))

        (toy-example '((:and (:= 'city '$1) (:> 'salary '$2))) 'employee 'id 'name 'city "Toronto" 45000)

        ((6 "James" "Toronto") (9 "Mary" "Toronto"))

    If we just look at what this call to sql-compile in toy-example generates, it would look like:

        "(SELECT id, name, city FROM employee WHERE ((city = $1) and (salary > $2)))"

    This example is still a parameterized query but for security reasons you will
    need to be very careful how you generate the where clause.

    Another example with sql-compile and append, in this case updating a table and
    setting two columns to NULL.

        (sql-compile (append '(:update :table1 :set)
                             (loop for a in '("col1" "col2")
                                   collect a
                                   collect :NULL)))

        "UPDATE table1 SET E'col1' = NULL, E'col2' = NULL"

    Lets think about it differently. What if we know the universe of columns we
    want to select, but want to conditionally select some of them. Suppose we
    know our targetted table has columns:

    'id 'name 'salary 'start-date 'city 'region 'age.

    We may decide we always want name, city and age, but salary and start-date are
    conditional.

        (defun toy-example-2 (salaryp start-date-p)
          (sql-compile
           (remove nil `(:select 'name 'city 'age
                                 ,(if salaryp 'salary nil)
                                 ,(if start-date-p 'start-date nil)
                                 :from 'employee))))

        (query (toy-example-2 t t))

        (("Jason" "New York" 29 40420 #<SIMPLE-DATE:DATE 01-02-1994>)
         ("Robert" "Vancouver" 21 14420 #<SIMPLE-DATE:DATE 02-01-1995>)
         ("Celia" "Toronto" 24 24020 #<SIMPLE-DATE:DATE 03-12-1996>)
         ("Linda" "New York" 28 40620 #<SIMPLE-DATE:DATE 04-11-1997>)
         ("David" "Vancouver" 31 80026 #<SIMPLE-DATE:DATE 05-10-1998>)
         ("James" "Toronto" 26 70060 #<SIMPLE-DATE:DATE 06-09-1999>)
         ("Alison" "New York" 38 90620 #<SIMPLE-DATE:DATE 07-08-2000>)
         ("Chris" "Vancouver" 22 26020 #<SIMPLE-DATE:DATE 08-07-2001>)
         ("Mary" "Toronto" 34 60020 #<SIMPLE-DATE:DATE 08-06-2002>))

        (query (toy-example-2 t nil))

        (("Jason" "New York" 29 40420) ("Robert" "Vancouver" 21 14420)
         ("Celia" "Toronto" 24 24020) ("Linda" "New York" 28 40620)
         ("David" "Vancouver" 31 80026) ("James" "Toronto" 26 70060)
         ("Alison" "New York" 38 90620) ("Chris" "Vancouver" 22 26020)
         ("Mary" "Toronto" 34 60020))

    You could skip the (remove nil&#x2026; portion and substitute t for nil. E.g.

        (defun toy-example-2 (salaryp start-date-p)
          (sql-compile
           `(:select 'name 'city 'age
                     ,(if salaryp 'salary t)
                     ,(if start-date-p 'start-date t)
                     :from 'employee)))

    But I prefer to remove those segments completely from the query.

    Following on this same thread of thought, you can define a portion of the sql in a let clause:

        (let ((sql1 '(:= name "Jason")))
          (query (sql-compile
            `(:select 'name 'city 'age :from 'employee :where ,sql1))))

        (("Jason" "New York" 29))

    An example of this would be getting more columns depending on the postgresql server versionr:

        (defun more-table-info (table-name)
          "Returns variable amounts of information depending on the postgresql server version"
          (let* ((version>11 (postgresql-version-at-least "12.0" *database*))
                 (version>10 (postgresql-version-at-least "11.0" *database*))
                 (select-query (sql-compile
                                `(:order-by
                                  (:select (:as 'a.attnum 'ordinal-position)
                                           (:as 'a.attname 'column-name)
                                           (:as 'tn.typname 'data-type)
                                           ,(if version>10 'a.attidentity t)
                                           ,(if version>11 'a.attgenerated t)
                                           :from (:as 'pg_class 'c)
                                           (:as 'pg_attribute 'a)
                                           (:as 'pg_type 'tn)
                                           :where (:and
                                                   (:= 'c.relname '$1)
                                                   (:> 'a.attnum 0)
                                                   (:= 'a.attrelid 'c.oid)
                                                   (:= 'a.atttypid 'tn.oid)))
                                  'a.attnum))))
            (query select-query
                   (to-sql-name table-name))))

2.  Approach #2 Use :raw

    To quote Marijn, the :raw keyword takes a string and inserts it straight
    into the query. I try to stay away from :raw if possible, but sometimes&#x2026;

        (query (:select (:raw "tmp1.name") :from (:as 'baz (:raw "tmp1"))))


<a id="org73dbeb0"></a>

### Queries with User Input

In any of the above approaches to building queries you will need to ensure that either you have control over the inputs or they still result in parameterized queries. If not you have opened yourself up to an sql injection attack.
