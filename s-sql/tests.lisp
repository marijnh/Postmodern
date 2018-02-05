(defpackage :s-sql-tests
  (:use :common-lisp :fiveam :s-sql :cl-postgres :cl-postgres-error)
  (:export #:prompt-connection #:*test-connection* #:with-test-connection))

(in-package :s-sql-tests)

(defparameter *test-connection* '("test" "test" "" "localhost"))

(defun prompt-connection (&optional (list *test-connection*))
  (flet ((ask (name pos)
           (format *query-io* "~a (enter to keep '~a'): " name (nth pos list))
           (finish-output *query-io*)
           (let ((answer (read-line *query-io*)))
             (unless (string= answer "") (setf (nth pos list) answer)))))
    (format *query-io* "~%To run this test, you must configure a database connection.~%")
    (ask "Database name" 0)
    (ask "User" 1)
    (ask "Password" 2)
    (ask "Hostname" 3)))

;; Adjust the above to some db/user/pass/host/[port] combination that
;; refers to a valid postgresql database, then after loading the file,
;; run the tests with (fiveam:run! :cl-postgres)

(def-suite :s-sql-suite)
(in-suite :s-sql-suite)
#|
(defmacro with-test-connection (&body body)
  `(let ((connection (apply 'open-database *test-connection*)))
    (unwind-protect (progn ,@body)
      (close-database connection))))

(defmacro with-default-readtable (&body body)
  `(let ((*sql-readtable* (default-sql-readtable)))
    ,@body))

(defmacro with-rollbacked-transaction (&body body)
  `(progn
    (exec-query connection "start transaction")
    (unwind-protect (progn ,@body)
      (exec-query connection "rollback"))))

(test connect-sanity
  (with-test-connection
      (is (database-open-p connection))))

(test sql-error)
|#
(test strcat
  "Testing strcat"
  (is (equal (s-sql::strcat '("a" "b")) "ab"))
  (is (equal (s-sql::strcat '("a " "b")) "a b")))

(test implode
  "Testing implode"
  (is (equal (s-sql::implode "/" '("aa" "bb" " " "cc"))) "aa/bb/ /cc"))

(test split-on-keywords%
  "Testing split-on-keywords%"
  (is (equal (s-sql::split-on-keywords% '((owner ?)) '(:owner "Sabra"))
             '((OWNER "Sabra")))))

(test split-on-keywords
  "Testing split-on-keywords"
  (is (equal (s-sql::split-on-keywords ((owner ?)) '(:owner "Sabra") `("West Indies " ,@(s-sql::sql-expand "Sabra")))
             '("West Indies " "E'Sabra'")))
  (is (equal (s-sql::split-on-keywords ((owner ? *)) '(:owner "Sabra") `("West Indies " ,@(s-sql::sql-expand "Sabra")))
             '("West Indies " "E'Sabra'")))
  (signals sql-error (s-sql::split-on-keywords ((owner ?)) '(:owner "Sabra" :tourist "Geoffrey")
                       `("West Indies " ,@(s-sql::sql-expand "Sabra"))))
  (signals sql-error (s-sql::split-on-keywords ((owner ? -)) '(:owner "Sabra") `("West Indies " ,@(s-sql::sql-expand "Sabra")))))

(test to-sql-name
  "Testing to-sql-name"
  (is (equal (s-sql::to-sql-name "George-Harriet")
             "george_harriet"))
  (is (equal (s-sql::to-sql-name "George/Harriet")
             "george_harriet"))
  (is (equal (s-sql::to-sql-name "George Harriet")
             "george_harriet"))
  (is (equal (s-sql::to-sql-name "George \"Harriet\"" nil)
             "george__harriet_"))
  (is (equal (s-sql::to-sql-name "George.Harriet")
             "george.harriet"))
  (is (equal (s-sql::to-sql-name "current-schema" :auto)
             "\"current_schema\""))
  (is (equal (s-sql::to-sql-name "current-schema" nil)
             "current_schema"))
  (is (equal (s-sql::to-sql-name "create" :auto)
             "\"create\""))
  (is (equal (s-sql::to-sql-name "create" nil)
             "create")))

(test from-sql-name
  "Testing from-sql-name"
  (is (equal (s-sql::from-sql-name "create_all")
             :CREATE-ALL))
  (is (equal (s-sql::from-sql-name "region/los_angeles")
             :REGION/LOS-ANGELES)))

(test sql-type-name
      "Testing sql-type-name"
      (is (equal (sql-type-name 'string "5")
                 "CHAR(5)"))
      (is (equal (sql-type-name 'string)
                 "TEXT"))
      (is (equal (sql-type-name 'double-float)
                 "DOUBLE PRECISION"))
      (is (equal (sql-type-name 'ratio)
                 "RATIO"))
      (is (equal (sql-type-name 'float)
                 "REAL"))
      (signals error (sql-type-name 'array))
      (is (equal (sql-type-name 'array 'integer)
                 "INTEGER[]"))
      (signals error (sql-type-name 'array "boulder")))

(test s-sql::to-type-name
  "Testing to-type-name"
  (is (equal (to-type-name 'float)
             "REAL"))
  (is (equal (to-type-name '(string "5"))
             "CHAR(5)")))

(test s-sql:sql-escape-string
  "Testing sql-escape-string"
    (is (equal (sql-escape-string "Puss in 'Boots'")
               "E'Puss in ''Boots'''")))

(test sql-escape
  "Testing sql-escape"
    (is (equal (sql-escape (/ 1 13))
               "0.0769230769230769230769230769230769230")))

(test sql-expand
  "Testing sql-expand"
    (is (equal (sql-expand (/ 1 13))
               "0.0769230769230769230769230769230769230")))

(test sql-expand-list
  "Testing sql-expand-list"
    (is (equal )))

(test sql-expand-names
  "Testing sql-expand-names"
    (is (equal )))

(test reduce-strings
  "Testing reduce-strings"
    (is (equal )))

(test sql-macro
  "Testing sql-macro"
    (is (equal )))

(test sql-compile
  "Testing sql-compile"
    (is (equal )))

(test sql-template
  "Testing sql-template"
    (is (equal )))

(test s-sql-reader
  "Testing s-sql-reader"
    (is (equal )))

(test enable-s-sql-syntax
  "Testing enable-s-sql-syntax"
    (is (equal )))

(test expand-sql-op
  "Testing expand-sql-op"
    (is (equal )))

(test def-sql-op-macro
  "Testing def-sql-op-macro"
    (is (equal )))

(test make-expander
  "Testing make-expander"
    (is (equal )))

(test register-sql-operators
  "Testing register-sql-operators"
    (is (equal )))



(test select
  "Testing select modifiers"
  (is (equal (sql (:select 'item :from 'item-table))
             "(SELECT item FROM item_table)"))
  (is (equal (sql (:select 'item :from 'item-table :where (:= 'id 2)))
             "(SELECT item FROM item_table WHERE (id = 2))"))
  (is (equal (sql (:select 'item :distinct :from 'item-table :where (:= 'col1 "Albania")))
             "(SELECT DISTINCT item FROM item_table WHERE (col1 = E'Albania'))"))
  (is (equal (sql (:select 'item 'groups :from 'item-table 'item-groups :where (:= 'item-table.group-id 'item-groups.id)))
             "(SELECT item, groups FROM item_table, item_groups WHERE (item_table.group_id = item_groups.id))"))
  (is (equal (sql (:select (:over (:sum 'salary) 'w)
                (:over (:avg 'salary) 'w)
                :from 'empsalary :window
                (:as 'w (:partition-by 'depname :order-by (:desc 'salary)))))
             "(SELECT (sum(salary) OVER w), (avg(salary) OVER w) FROM empsalary WINDOW w AS (PARTITION BY depname ORDER BY salary DESC))"))
  (is (equal (let ((param-latitude nil) (param-longitude t))
         (sql (:select 'id 'name (when param-latitude '0)
                       (when param-longitude 'longitude)
                       :from 'countries
                       :where (:= 'id 20))))
             "(SELECT id, name, false, longitude FROM countries WHERE (id = 20))"))
  (is (equal (sql (:with (:as 'upd
                    (:parens
                     (:update 'employees :set 'sales-count (:= 'sales-count 1)
                              :where (:= 'id
                                         (:select 'sales-person
                                                  :from 'accounts
                                                  :where (:= 'name "Acme Corporation")))
                              :returning '*)))
               (:insert-into 'employees-log
                             (:select '* 'current-timestamp :from
                                      'upd))))
             "WITH upd AS  (UPDATE employees SET sales_count = (sales_count = 1) WHERE (id = (SELECT sales_person FROM accounts WHERE (name = E'Acme Corporation'))) RETURNING *) INSERT INTO employees_log (SELECT *, \"current_timestamp\" FROM upd)"))
  (is (equal (sql (:with-recursive
        (:as (:t1 'n)
             (:union-all (:values 1)
                         (:select (:n 1)
                                  :from 't1
                                  :where (:< 'n 100))))
        (:select (:sum 'n) :from 't1)))
             "WITH RECURSIVE t1(n) AS (values(1) union all (SELECT n(1) FROM t1 WHERE (n < 100)))(SELECT sum(n) FROM t1)"))
  (is (equal (sql (:with-recursive
        (:as (:included-parts 'sub-part 'part 'quantity)
             (:union-all
              (:select 'sub-part 'part 'quantity
                       :from 'parts
                       :where (:= 'part "our-product"))
              (:select 'p.sub-part 'p.part 'p.quantity
                       :from (:as 'included-parts 'pr)
                       (:as 'parts 'p)
                       :where (:= 'p.part 'pr.sub-part))))
        (:select 'sub-part (:as (:sum 'quantity) 'total-quantity)
                 :from 'included-parts
                 :group-by 'sub-part)))
             "WITH RECURSIVE included_parts(sub_part, part, quantity) AS ((SELECT sub_part, part, quantity FROM parts WHERE (part = E'our-product')) union all (SELECT p.sub_part, p.part, p.quantity FROM included_parts AS pr, parts AS p WHERE (p.part = pr.sub_part)))(SELECT sub_part, sum(quantity) AS total_quantity FROM included_parts GROUP BY sub_part)"))
  (is (equal (sql (:with-recursive
        (:as (:search-graph 'id 'link 'data 'depth)
             (:union-all (:select 'g.id 'g.link 'g.data 1
                                  :from (:as 'graph 'g))
                         (:select 'g.id 'g.link 'g.data (:= 'sg.depth 1)
                                  :from (:as 'graph 'g) (:as 'search-graph 'sg)
                                  :where (:= 'g.id 'sg.link))))
        (:select '* :from 'search-graph)))
             "WITH RECURSIVE search_graph(id, link, data, depth) AS ((SELECT g.id, g.link, g.data, 1 FROM graph AS g) union all (SELECT g.id, g.link, g.data, (sg.depth = 1) FROM graph AS g, search_graph AS sg WHERE (g.id = sg.link)))(SELECT * FROM search_graph)"))
  (is (equal (sql (:with-recursive
        (:as (:search-graph 'id 'link 'data'depth 'path 'cycle)
             (:union-all
              (:select 'g.id 'g.link 'g.data 1
                       (:[] 'g.f1 'g.f2) nil
                       :from (:as 'graph 'g))
              (:select 'g.id 'g.link 'g.data (:= 'sg.depth 1)
                       (:|| 'path (:row 'g.f1 'g.f2))
                       (:= (:row 'g.f1 'g.f2)
                           (:any* 'path))
                       :from (:as 'graph 'g)
                       (:as 'search-graph 'sg)
                       :where (:and (:= 'g.id 'sg.link)
                                    (:not 'cycle)))))
        (:select '* :from 'search-graph)))
             "WITH RECURSIVE search_graph(id, link, data, depth, path, cycle) AS ((SELECT g.id, g.link, g.data, 1, (g.f1)[g.f2], false FROM graph AS g) union all (SELECT g.id, g.link, g.data, (sg.depth = 1), (path || row(g.f1, g.f2)), (row(g.f1, g.f2) = ANY(path)) FROM graph AS g, search_graph AS sg WHERE ((g.id = sg.link) and (not cycle))))(SELECT * FROM search_graph)"))
  (is (equal (sql
            (:with-recursive
            (:as 'children
                 (:union
                  (:select 'depended-on :from 'dependencies :where (:= 'depends-on '$1))
                  (:select 'a.depended-on :from (:as 'dependencies 'a)
                           :inner-join (:as 'children 'b)
                           :on (:= 'a.depends-on 'b.depended-on))))
            (:select '* :from 'children)))
             "WITH RECURSIVE children AS ((SELECT depended_on FROM dependencies WHERE (depends_on = $1)) union (SELECT a.depended_on FROM dependencies AS a INNER JOIN children AS b ON (a.depends_on = b.depended_on)))(SELECT * FROM children)"))
  (is (equal (sql "(SELECT countries.id, (countries.name || '-' || regions.name)
        FROM countries, regions
        WHERE ((regions.id = countries.region_id) and (countries.name = 'US')))")
             "E'(SELECT countries.id, (countries.name || ''-'' || regions.name)
        FROM countries, regions
        WHERE ((regions.id = countries.region_id) and (countries.name = ''US'')))'"))
  (is (equal (sql (:select (:+ 'id 12) 'name :from 'regions :where (:= 'name "South America")))
             "(SELECT (id + 12), name FROM regions WHERE (name = E'South America'))"))
  )


(test insert-into
  "Testing Insert-into"
  (is (equal (sql (:insert-into 'test :set 'id 15 'number-string "12" 'numeric-item 12.45
                                'ratio-item (/ 1 13) 'created-at "2018-02-01"))))
  "INSERT INTO test (id, number_string, numeric_item, ratio_item, created_at) VALUES (15, E'12', 12.45, 0.0769230769230769230769230769230769230, E'2018-02-01')")


(test dissect-type-0
      "Testing dissect-type"
      (multiple-value-bind (type null?) (dissect-type 'char)
        (is (eq type 'char))
        (is (not null?))))

(test dissect-type-1
      "Testing dissect-type"
      (multiple-value-bind (type null?) (dissect-type '(or char db-null))
        (is (eq type 'char))
        (is null?)))

(test dissect-type-2
      "Testing dissect-type"
      (multiple-value-bind (type null?) (dissect-type '(or "char(5)" db-null))
        (is (equal type "char(5)"))
        (is null?)))


(test expand-table-column
      "Testing expand-table-column"
      (is (equal (s-sql::expand-table-column 'code '(:type varchar :primary-key 't))
                 ("code" " " "VARCHAR" " NOT NULL" " PRIMARY KEY")))
      (is (equal (s-sql::expand-table-column 'code '(:type (or char db-null) :primary-key 't))
                 ("code" " " "CHAR" " PRIMARY KEY")))
      (is (equal (s-sql::expand-table-column 'code '(:type (or (string 5) db-null) :primary-key 't))
                 ("code" " " "CHAR(5)" " PRIMARY KEY"))))

;;; CREATE TABLE TESTS
(test create-table-1
      "Testing Create Table. First example from https://www.postgresql.org/docs/10/static/sql-createtable.html
 Right now we do not have the intervals fixed."
      (is (equal (s-sql:sql (:create-table films
                             ((code :type (or (string 5) db-null) :constraint 'firstkey :primary-key t)
                              (title :type (varchar 40))
                              (did :type integer)
                              (date-prod :type (or date db-null))
                              (kind :type (or (varchar 10) db-null))
                              (len :type (or interval db-null)))))
                 "CREATE TABLE films (code CHAR(5) CONSTRAINT firstkey PRIMARY KEY, title VARCHAR(40) NOT NULL, did INTEGER NOT NULL, date_prod DATE, kind VARCHAR(10), len INTERVAL)"))) ; Create table films and table distributors:

(test create-table-2
      "Second example from https://www.postgresql.org/docs/10/static/sql-createtable.html"
      (is (equal (s-sql:sql (:create-table distributors ((did :type (or integer db-null)
                                                              :primary-key "generated by default as identity")
                                                         (name :type (varchar 40) :check (:<> 'name "")))))
                 "CREATE TABLE distributors (did INTEGER PRIMARY KEY generated by default as identity, name VARCHAR(40) NOT NULL CHECK (name <> E''))")))

(test Create-table-with-a-2-dimensional-array
      "Create a table with a 2-dimensional array"
      (is (equal (sql (:create-table array_int ((vector :type (or int[][] db-null)))))
                 "CREATE TABLE array_int (vector INT[][])")))

(test create-table-with-constraint
      "Create a table with constraint"
      (is (equal )))
Define a unique table constraint for the table films. Unique table constraints can be defined on one or more columns of the table:

CREATE TABLE films (
    code        char(5),
    title       varchar(40),
    did         integer,
    date_prod   date,
    kind        varchar(10),
    len         interval hour to minute,
    CONSTRAINT production UNIQUE(date_prod)
);
Define a check column constraint:

CREATE TABLE distributors (
    did     integer CHECK (did > 100),
    name    varchar(40)
);
Define a check table constraint:

CREATE TABLE distributors (
    did     integer,
    name    varchar(40)
    CONSTRAINT con1 CHECK (did > 100 AND name <> '')
);
Define a primary key table constraint for the table films:

CREATE TABLE films (
    code        char(5),
    title       varchar(40),
    did         integer,
    date_prod   date,
    kind        varchar(10),
    len         interval hour to minute,
    CONSTRAINT code_title PRIMARY KEY(code,title)
);
Define a primary key constraint for table distributors. The following two examples are equivalent, the first using the table constraint syntax, the second the column constraint syntax:

CREATE TABLE distributors (
    did     integer,
    name    varchar(40),
    PRIMARY KEY(did)
);

CREATE TABLE distributors (
    did     integer PRIMARY KEY,
    name    varchar(40)
);
Assign a literal constant default value for the column name, arrange for the default value of column did to be generated by selecting the next value of a sequence object, and make the default value of modtime be the time at which the row is inserted:

CREATE TABLE distributors (
    name      varchar(40) DEFAULT 'Luso Films',
    did       integer DEFAULT nextval('distributors_serial'),
    modtime   timestamp DEFAULT current_timestamp
);


(test create-table-two-column-constraints
      "Define two NOT NULL column constraints on the table distributors, one of which is explicitly given a name.
Note the need for quoting the no-null constraint in the column. Is there any way to fix this?"
      (is (equal (s-sql:sql (:create-table distributors ((did :type integer :constraint 'no-null) (name :type (varchar 40 )))))
                 "CREATE TABLE distributors (did INTEGER NOT NULL CONSTRAINT no_null, name VARCHAR(40) NOT NULL)")))

Define a unique constraint for the name column:

CREATE TABLE distributors (
    did     integer,
    name    varchar(40) UNIQUE
);
The same, specified as a table constraint:

CREATE TABLE distributors (
    did     integer,
    name    varchar(40),
    UNIQUE(name)
);
Create the same table, specifying 70% fill factor for both the table and its unique index:

CREATE TABLE distributors (
    did     integer,
    name    varchar(40),
    UNIQUE(name) WITH (fillfactor=70)
)
WITH (fillfactor=70);
Create table circles with an exclusion constraint that prevents any two circles from overlapping:

CREATE TABLE circles (
    c circle,
    EXCLUDE USING gist (c WITH &&)
);
Create table cinemas in tablespace diskvol1:

CREATE TABLE cinemas (
        id serial,
        name text,
        location text
) TABLESPACE diskvol1;
Create a composite type and a typed table:

CREATE TYPE employee_type AS (name text, salary numeric);

CREATE TABLE employees OF employee_type (
    PRIMARY KEY (name),
    salary WITH OPTIONS DEFAULT 1000
);
Create a range partitioned table:

CREATE TABLE measurement (
    logdate         date not null,
    peaktemp        int,
    unitsales       int
) PARTITION BY RANGE (logdate);
Create a range partitioned table with multiple columns in the partition key:

CREATE TABLE measurement_year_month (
    logdate         date not null,
    peaktemp        int,
    unitsales       int
) PARTITION BY RANGE (EXTRACT(YEAR FROM logdate), EXTRACT(MONTH FROM logdate));
Create a list partitioned table:

CREATE TABLE cities (
    city_id      bigserial not null,
    name         text not null,
    population   bigint
) PARTITION BY LIST (left(lower(name), 1));
Create partition of a range partitioned table:

CREATE TABLE measurement_y2016m07
    PARTITION OF measurement (
    unitsales DEFAULT 0
) FOR VALUES FROM ('2016-07-01') TO ('2016-08-01');
Create a few partitions of a range partitioned table with multiple columns in the partition key:

CREATE TABLE measurement_ym_older
    PARTITION OF measurement_year_month
    FOR VALUES FROM (MINVALUE, MINVALUE) TO (2016, 11);

CREATE TABLE measurement_ym_y2016m11
    PARTITION OF measurement_year_month
    FOR VALUES FROM (2016, 11) TO (2016, 12);

CREATE TABLE measurement_ym_y2016m12
    PARTITION OF measurement_year_month
    FOR VALUES FROM (2016, 12) TO (2017, 01);

CREATE TABLE measurement_ym_y2017m01
    PARTITION OF measurement_year_month
    FOR VALUES FROM (2017, 01) TO (2017, 02);
Create partition of a list partitioned table:

CREATE TABLE cities_ab
    PARTITION OF cities (
    CONSTRAINT city_id_nonzero CHECK (city_id != 0)
) FOR VALUES IN ('a', 'b');
Create partition of a list partitioned table that is itself further partitioned and then add a partition to it:

CREATE TABLE cities_ab
    PARTITION OF cities (
    CONSTRAINT city_id_nonzero CHECK (city_id != 0)
) FOR VALUES IN ('a', 'b') PARTITION BY RANGE (population);

CREATE TABLE cities_ab_10000_to_100000
    PARTITION OF cities_ab FOR VALUES FROM (10000) TO (100000);
