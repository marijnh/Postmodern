(defpackage :s-sql-tests
  (:use :common-lisp :fiveam :s-sql :cl-postgres :cl-postgres-error :cl-postgres-tests :postmodern))

(in-package :s-sql-tests)

;; Adjust the above to some db/user/pass/host/[port] combination that
;; refers to a valid postgresql database, then after loading the file,
;; run the tests with (fiveam:run! :cl-postgres)

(fiveam:def-suite :s-sql
   :description "Master suite for s-sql")

(fiveam:in-suite :s-sql)

(defmacro with-test-connection (&body body)
  `(postmodern:with-connection *test-connection* ,@body))

(defmacro protect (&body body)
  `(unwind-protect (progn ,@(butlast body)) ,(car (last body))))

(test connect-sanity
  (with-test-connection
      (is (not (null *database*)))))

(test sql-error)

(test strcat
  "Testing strcat. Concatenate a list of strings into a single one."
  (is (equal (s-sql::strcat '("a" "b")) "ab"))
  (is (equal (s-sql::strcat '("a " "b")) "a b")))

(test implode
  "Testing implode. Reduce a list of strings to a single string, inserting a separator
between them."
  (is (equal (s-sql::implode "/" '("aa" "bb" " " "cc")) "aa/bb/ /cc")))

(test split-on-keywords%
  "Testing split-on-keywords%. Helper function for split-on-keywords. Extracts the values
associated with the keywords from an argument list, and checks for errors."
  (is (equal (s-sql::split-on-keywords% '((owner ?)) '(:owner "Sabra"))
             '((OWNER "Sabra"))))
  (is (equal (s-sql::split-on-keywords% '((interval ? *)) '(:interval 5 hours))
             '((INTERVAL 5 HOURS))))
  (is (equal (s-sql::split-on-keywords% '((a1 * ?) (b2 ?) (c3 ? *)) '(:a1 "Alpha1 " :b2 "Beta2 " :c3 "Ceta3 "))
             '((A1 "Alpha1 ") (B2 "Beta2 ") (C3 "Ceta3 "))))
  (is (equal (s-sql::split-on-keywords% '((a1 * ?) (c3 ? *)) '(:a1 "Alpha1 " :b2 "Beta2" :c3 "Ceta3 "))
             '((A1 "Alpha1 " :B2 "Beta2") (C3 "Ceta3 "))))
  (signals sql-error (s-sql::split-on-keywords% '((a1 * ?) (b2 ?) (c3 ? *)) '(:a1 "Alpha1 "  :c3 "Ceta3 ")))
  (signals sql-error (s-sql::split-on-keywords% '((a1 * ?) (b2 -) (c3 ? *)) '(:a1 "Alpha1 " :b2 "Beta2" :c3 "Ceta3 ")))
  (signals sql-error (s-sql::split-on-keywords% '((a1 * ?) (b2 ) (c3 ? *)) '(:a1 "Alpha1 "  :c3 "Ceta3 ")))
  (signals sql-error (s-sql::split-on-keywords% '((owner ?)) '(:owner "Sabra" :tourist "Geoffrey")))
  (signals sql-error (s-sql::split-on-keywords% '((a1 * ?) (c3 ? )) '(:a1 "Alpha1 " :c3 "Ceta3 " "Ceta3.5")))
  (is (equal (s-sql::split-on-keywords% '((fraction *)) `(:fraction 0.5))
             '((FRACTION 0.5)))))

(test split-on-keywords
  "Testing split-on-keywords. Used to handle arguments to some complex SQL operations.
Arguments are divided by keywords, which are interned with the name of the
non-keyword symbols in words, and bound to these symbols. After the
naming symbols, a ? can be used to indicate this argument group is
optional, an * to indicate it can consist of more than one element,
and a - to indicate it does not take any elements."
  (is (equal (s-sql::split-on-keywords ((a1 * ?) (b2 ?) (c3 ? *)) '(:a1 "Alpha1 " :b2 "Beta2 " :c3 "Ceta3 ")
         `("Results " ,@ (when a1 a1) ,@ (when c3 c3) ,@(when b2 b2)))
             '("Results " "Alpha1 " "Ceta3 " "Beta2 ")))
  (signals sql-error (s-sql::split-on-keywords ((a1 * ?) (b2 ?) (c3 ? *)) '(:a1 "Alpha1 "  :c3 "Ceta3 ")
                       `("Results " ,@ (when a1 a1) ,@ (when c3 c3) ,@(when b2 b2)))
           '("Results " "Alpha1 " "Ceta3 "))
  (is (equal (s-sql::split-on-keywords ((a1 * ?) (c3 ? *)) '(:a1 "Alpha1 " :b2 "Beta2" :c3 "Ceta3 ")
         `("Results " ,@ (when a1 a1) ,@ (when c3 c3)))
             '("Results " "Alpha1 " :B2 "Beta2" "Ceta3 ")))
  ;; Keyword does not take any arguments
  (signals sql-error (s-sql::split-on-keywords ((a1 * ?) (b2 -) (c3 ? *)) '(:a1 "Alpha1 " :b2 "Beta2" :c3 "Ceta3 ")
                       `("Results " ,@ (when a1 a1) ,@ (when c3 c3) ,@(when b2 b2))))
  ;; Required keyword missing
  (signals sql-error (s-sql::split-on-keywords ((a1 * ?) (b2 ) (c3 ? *)) '(:a1 "Alpha1 "  :c3 "Ceta3 ")
                       `("Results " ,@ (when a1 a1) ,@ (when c3 c3) ,@(when b2 b2))))
  (is  (equal (s-sql::split-on-keywords ((a1 * ?) (c3 ? *)) '(:a1 "Alpha1 " :b2 "Beta2"  :c3 "Ceta3 ")
                `("Results " ,@ (when a1 a1) ,@ (when c3 c3)))
              '("Results " "Alpha1 " :B2 "Beta2" "Ceta3 ")))
  ;;too many elements for a keyword
  (signals sql-error (s-sql::split-on-keywords ((a1 * ?) (c3 ? )) '(:a1 "Alpha1 " :c3 "Ceta3 " "Ceta3.5")
         `("Results " ,@ (when a1 a1) ,@ (when c3 c3)))))

(test to-sql-name
  "Testing to-sql-name. Convert a symbol or string into a name that can be an sql table,
column, or operation name. Add quotes when escape-p is true, or
escape-p is :auto and the name contains reserved words."
  (is (equal (s-sql::to-sql-name 'George-Harrison)
             "george_harrison"))
  (is (equal (s-sql::to-sql-name "George-Harrison")
             "george_harrison"))
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
             "create"))
  (is (equal (s-sql::to-sql-name 'George-Harrison :literal)
             "\"GEORGE-HARRISON\""))
  (is (equal (s-sql::to-sql-name "George-Harrison" :literal)
             "\"George-Harrison\""))
  (is (equal (s-sql::to-sql-name "George/Harrison" :literal)
             "\"George/Harrison\""))
  (is (equal (s-sql::to-sql-name "George/Harrison" :auto)
             "george_harrison"))
  (is (equal (s-sql::to-sql-name "George_Harrison" :literal)
             "\"George_Harrison\""))
  (is (equal (s-sql::to-sql-name "George\\Harrison" :literal)
             "\"George\\Harrison\""))
  (is (equal (s-sql::to-sql-name "George-Harrison" :literal)
             "\"George-Harrison\"")))

(test from-sql-name
  "Testing from-sql-name. Convert a string to something that might have been its original
lisp name \(does not work if this name contained non-alphanumeric
characters other than #\-)"
  (is (equal (s-sql::from-sql-name "create_all")
             :CREATE-ALL))
  (is (equal (s-sql::from-sql-name "region/los_angeles")
             :REGION/LOS-ANGELES)))

(test sql-type-name
      "Testing sql-type-name. Transform a lisp type into a string containing
something SQL understands. Default is to just use the type symbol's
name."
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

(test to-type-name
  "Testing to-type-name. Turn a Lisp type expression into an SQL typename."
  (is (equal (s-sql::to-type-name 'float)
             "REAL"))
  (is (equal (s-sql::to-type-name '(string "5"))
             "CHAR(5)")))

(test s-sql:sql-escape-string
  "Testing sql-escape-string. Escape string data so it can be used in a query."
    (is (equal (sql-escape-string "Puss in 'Boots'")
               "E'Puss in ''Boots'''")))

(test sql-escape
  "Testing sql-escape. Get the representation of a Lisp value so that it
can be used in a query."
    (is (equal (sql-escape (/ 1 13))
               "0.0769230769230769230769230769230769230"))
    (is (equal (sql-escape #("Baden-Wurttemberg" "Bavaria" "Berlin" "Brandenburg"))
               "ARRAY[E'Baden-Wurttemberg', E'Bavaria', E'Berlin', E'Brandenburg']")))

(test sql-expand
  "Testing sql-expand. Compile-time expansion of forms into lists of stuff that evaluates
to strings \(which will form an SQL query when concatenated)."
    (is (equal (s-sql::sql-expand (/ 1 13))
               '("0.0769230769230769230769230769230769230")))
    (is (equal (s-sql::sql-expand '("george" "paul" "john" "ringo"))
               '((SQL-ESCAPE ("george" "paul" "john" "ringo")))))
    (is (equal (s-sql::sql-expand '(george "paul"))
               '((SQL-ESCAPE (GEORGE "paul")))))
    (is (equal (s-sql::sql-expand '(:george  "paul" "Beatles"))
               '("george" "(" "E'paul'" ", " "E'Beatles'" ")")))
    (is (equal (s-sql::sql-expand '(:george  "rhythm" :group "Beatles"))
               '("george" "(" "E'rhythm'" ", " "\"group\"" ", " "E'Beatles'" ")")))
    (is (equal (s-sql::sql-expand '(george  "rhythm" :group "Beatles"))
               '((SQL-ESCAPE (GEORGE "rhythm" :GROUP "Beatles")))))
    (is (equal (s-sql::sql-expand '(:= 'facs.facid 1))
               '("(" "facs.facid" " = " "1" ")"))))

(test sql-expand-list
  "Testing sql-expand-list. Expand a list of elements, adding a separator in between them."
    (is (equal (s-sql::sql-expand-list '(george paul john "ringo" "mary-ann" carol-anne))
               '((SQL-ESCAPE GEORGE) ", " (SQL-ESCAPE PAUL) ", " (SQL-ESCAPE JOHN) ", "
                 "E'ringo'" ", " "E'mary-ann'" ", " (SQL-ESCAPE CAROL-ANNE))))
    (is (equal (s-sql::sql-expand-list '((:desc 'today) 'tomorrow 'yesterday))
               '("today" " DESC" ", " "tomorrow" ", " "yesterday")))
    (is (equal (s-sql::sql-expand-list (remove nil '(george paul john "ringo" "mary-ann" nil carol-anne nil)))
               '((SQL-ESCAPE GEORGE) ", " (SQL-ESCAPE PAUL) ", " (SQL-ESCAPE JOHN) ", "
                "E'ringo'" ", " "E'mary-ann'" ", " (SQL-ESCAPE CAROL-ANNE)))))

(test sql-expand-names
  "Testing sql-expand-names"
    (is (equal (s-sql::sql-expand-names '("george" "paul" "john" "ringo" "mary-ann"))
               '("george" ", " "paul" ", " "john" ", " "ringo" ", " "mary_ann")))
    (is (equal (s-sql::sql-expand-names '((george. "paul") "john" "ringo" "mary-ann"))
               '("george." "(" "E'paul'" ")" ", " "john" ", " "ringo" ", " "mary_ann")))
    (is (equal (s-sql::sql-expand-names '((george . paul) "john" "ringo" "mary-ann"))
               '("george" "(" ")" ", " "john" ", " "ringo" ", " "mary_ann")))
    (is (equal (s-sql::sql-expand-names '((george  "paul") "john" "ringo" "mary-ann"))
               '("george" "(" "E'paul'" ")" ", " "john" ", " "ringo" ", " "mary_ann"))))

(test reduce-strings
  "Testing reduce-strings. Join adjacent strings in a list, leave other values intact."
    (is (equal (s-sql::reduce-strings '("john" 7 "paul" "ringo" "george"))
               '("john" 7 "paulringogeorge")))
    (is (equal (s-sql::reduce-strings '("john" 7 "paul" "ringo" george))
               '("john" 7 "paulringo" GEORGE))))

(test sql-macro
  "Testing sql-macro. Compile form to an sql expression as far as possible."
    (is (equal (sql (:select 'name :from 'items :where (:= 'id 1)))
               "(SELECT name FROM items WHERE (id = 1))")))

(test sql-compile
  "Testing sql-compile"
    (is (equal (sql-compile '(:select 'name :from 'items :where (:= 'id 1)))
               "(SELECT name FROM items WHERE (id = 1))")))

(test sql-template
  "Testing sql-template"
    (is (functionp (sql-template '(:select 'name :from 'items :where (:= 'id 1))))))

(test expand-sql-op
      "Testing expand-sql-op"
      (is (equal (s-sql::expand-sql-op :max '(1 2 3))
                 '("MAX(" "1" ", " "2" ", " "3" ")"))))

(test make-expander
      "Testing make-expander"
      (is (equal (funcall (s-sql::make-expander :unary "unary1") '("like"))
                 '("(" "unary1" " " "E'like'" ")")))
      (is (equal (funcall (s-sql::make-expander :unary-postfix "unary2") '("like"))
                 '("(" "E'like'" " " "unary2" ")")))
      (is (equal (funcall (s-sql::make-expander :n-ary "unary3") '("like" "a" "b"))
                 '("(" "E'like'" " unary3 " "E'a'" " unary3 " "E'b'" ")")))
      (is (equal (funcall (s-sql::make-expander :2+-ary "unary4") '("like" "a" "b"))
                 '("(" "E'like'" " unary4 " "E'a'" " unary4 " "E'b'" ")")))
      (is (equal (funcall (s-sql::make-expander :2+-ary "unary4") '("like" "a"))
                 '("(" "E'like'" " unary4 " "E'a'" ")")))
      (signals sql-error (funcall (s-sql::make-expander :2+-ary "unary4") '("like")))
      (is (equal (funcall (s-sql::make-expander :n-or-unary "unary5") '("like" "a" "b"))
                 '("(" "E'like'" " unary5 " "E'a'" " unary5 " "E'b'" ")")))
      (is (equal (funcall (s-sql::make-expander :n-or-unary "unary5") '("like"))
                 '("(" "unary5" " " "E'like'" ")")))
      (is (equal (funcall (s-sql::make-expander :n-or-unary "unary5") '("like" "a"))
                 '("(" "E'like'" " unary5 " "E'a'" ")"))))

(test select-simple
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
             "(SELECT (SUM(salary) OVER w), (AVG(salary) OVER w) FROM empsalary WINDOW w AS (PARTITION BY depname ORDER BY salary DESC))"))
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


  (is (equal (sql "(SELECT countries.id, (countries.name || '-' || regions.name)
                           FROM countries, regions
                           WHERE ((regions.id = countries.region_id) and (countries.name = 'US')))")
             "E'(SELECT countries.id, (countries.name || ''-'' || regions.name)
                           FROM countries, regions
                           WHERE ((regions.id = countries.region_id) and (countries.name = ''US'')))'"))
  (is (equal (sql (:select (:+ 'id 12) 'name :from 'regions :where (:= 'name "South America")))
             "(SELECT (id + 12), name FROM regions WHERE (name = E'South America'))"))
  (is (equal (sql (:select 'ta :from 'a :where (:not (:is-null 'ta))))
             "(SELECT ta FROM a WHERE (not (ta IS NULL)))"))
  (is (equal (sql (:select 'ta :from 'a :where (:not-null 'ta)))
             "(SELECT ta FROM a WHERE (ta IS NOT NULL))")))

(test select-distinct
      "Testing select with distinct. From https://www.pgexercises.com/questions/basic/unique.html"
      (is (equal (sql (:limit (:order-by (:select 'surname :distinct :from 'cd.members) 'surname) 10))
                 "(((SELECT DISTINCT surname FROM cd.members) ORDER BY surname) LIMIT 10)")))

(test select-distinct-on
      "Testing select with distinct on. https://www.postgresql.org/docs/current/static/sql-select.html.
SELECT DISTINCT ON ( expression [, ...] ) keeps only the first row of each set of rows where the given expressions evaluate to equal. The DISTINCT ON expressions are interpreted using the same rules as for ORDER BY (see above). Note that the “first row” of each set is unpredictable unless ORDER BY is used to ensure that the desired row appears first. "
      (is (equal (sql (:order-by (:select 'location 'time 'report
                                          :distinct-on 'location
                                          :from 'weather-reports)
                                 'location  (:desc 'time)))
                 "((SELECT DISTINCT ON (location) location, time, report FROM weather_reports) ORDER BY location, time DESC)")))

(test select-join-1
      "Testing basic join. Note full use of as. https://www.postgresql.org/docs/current/static/sql-select.html
To join the table films with the table distributors:"
      (is (equal (sql (:select 'f.title 'f.did 'd.name 'f.date-prod 'f.kind
                               :from (:as 'distributors 'd) (:as 'films 'f)
                               :where (:= 'f.did 'd.did)))
                 "(SELECT f.title, f.did, d.name, f.date_prod, f.kind FROM distributors AS d, films AS f WHERE (f.did = d.did))"))
      ;; Cross Join
      (is (equal (sql (:select '* :from 't1 :cross-join 't2))
                 "(SELECT * FROM t1 CROSS JOIN t2)"))

      (is (equal (sql (:select '* :from 't1 't2))
                 "(SELECT * FROM t1, t2)"))
;; Examples from https://www.postgresql.org/docs/current/static/queries-table-expressions.html#QUERIES-WINDOW
;; Inner Join
      (is (equal (sql (:select '* :from 't1 :inner-join 't2 :on (:= 't1.num 't2.num)))
                 "(SELECT * FROM t1 INNER JOIN t2 ON (t1.num = t2.num))"))
;; From https://www.pgexercises.com/questions/joins/simplejoin.html
      (is (equal (sql (:select 'bks.starttime
                               :from (:as 'cd.bookings 'bks)
                               :inner-join (:as 'cd.members 'mems)
                               :on (:= 'mems.memid 'bks.memid)
                               :where (:and (:= 'mems.firstname "David")
                                            (:= 'mems.surname "Farrell"))))
                 "(SELECT bks.starttime FROM cd.bookings AS bks INNER JOIN cd.members AS mems ON (mems.memid = bks.memid) WHERE ((mems.firstname = E'David') and (mems.surname = E'Farrell')))"))

;; From https://www.pgexercises.com/questions/joins/self.html
      (is (equal (sql (:order-by (:select (:as 'recs.firstname 'firstname)
                                          (:as 'recs.surname 'surname)
                                          :distinct
                                          :from (:as 'cd.members 'mems)
                                          :inner-join (:as 'cd.members 'recs)
                                          :on (:= 'recs.memid 'mems.recommendedby))
                                 'surname 'firstname))
                 "((SELECT DISTINCT recs.firstname AS firstname, recs.surname AS surname FROM cd.members AS mems INNER JOIN cd.members AS recs ON (recs.memid = mems.recommendedby)) ORDER BY surname, firstname)"))

      ;; inner join with min from
      (is (equal (sql (:order-by
                       (:select 'mems.surname 'mems.firstname 'mems.memid (:as (:min 'bks.starttime) 'starttime)
                                :from (:as 'cd.bookings 'bks)
                                :inner-join (:as 'cd.members 'mems)
                                :on (:= 'mems.memid 'bks.memid)
                                :where (:>= 'starttime "2012-09-01")
                                :group-by 'mems.surname 'mems.firstname 'mems.memid)
                       'mems.memid))
                 "((SELECT mems.surname, mems.firstname, mems.memid, MIN(bks.starttime) AS starttime FROM cd.bookings AS bks INNER JOIN cd.members AS mems ON (mems.memid = bks.memid) WHERE (starttime >= E'2012-09-01') GROUP BY mems.surname, mems.firstname, mems.memid) ORDER BY mems.memid)"))

;; Inner Join with using
      (is (equal (sql (:select '* :from 't1 :inner-join 't2 :using ('num)))

                 "(SELECT * FROM t1 INNER JOIN t2 USING (num))"))

      ;; inner join with case from https://www.pgexercises.com/questions/joins/threejoin2.html
      (is (equal (sql (:order-by (:select (:as (:|| 'mems.firstname " " 'mems.surname) 'member) (:as 'facs.name 'facility)
                                (:as (:case ((:= 'mems.memid 0 ) (:* 'bks.slots 'facs.guestcost))
                                       (:else (:* 'bks.slots 'facs.membercost)))
                                     'cost)
                                :from (:as 'cd.members 'mems)
                                :inner-join (:as 'cd.bookings 'bks)
                                :on (:= 'mems.memid 'bks.memid)
                                :inner-join (:as 'cd.facilities 'facs)
                                :on (:= 'bks.facid 'facs.facid)
                                :where
                                (:and (:>= 'bks.starttime "2012-09-14")
                                      (:<= 'bks.starttime "2012-09-15")
                                      (:or (:and (:= 'mems.memid 0)
                                                 (:> (:* 'bks.slots 'facs.guestcost) 30))
                                           (:and (:not (:= 'mems.memid 0))
                                                 (:> (:* 'bks.slots 'facs.membercost) 30)))))
                       (:desc 'cost)))
                 "((SELECT (mems.firstname || E' ' || mems.surname) AS member, facs.name AS facility, CASE WHEN (mems.memid = 0) THEN (bks.slots * facs.guestcost) ELSE (bks.slots * facs.membercost) END AS cost FROM cd.members AS mems INNER JOIN cd.bookings AS bks ON (mems.memid = bks.memid) INNER JOIN cd.facilities AS facs ON (bks.facid = facs.facid) WHERE ((bks.starttime >= E'2012-09-14') and (bks.starttime <= E'2012-09-15') and (((mems.memid = 0) and ((bks.slots * facs.guestcost) > 30)) or ((not (mems.memid = 0)) and ((bks.slots * facs.membercost) > 30))))) ORDER BY cost DESC)"))

;; Natural Inner Join
      (is (equal (sql (:select '* :from 't1 :natural :inner-join 't2))
                 "(SELECT * FROM t1 NATURAL INNER JOIN t2)"))

;; Left Join (also known as left outer join
      (is (equal (sql (:select '* :from 't1 :left-join 't2 :on (:= 't1.num 't2.num)))
                 "(SELECT * FROM t1 LEFT JOIN t2 ON (t1.num = t2.num))"))

;; from https://www.pgexercises.com/questions/joins/self2.html
      (is (equal (sql (:order-by (:select (:as 'mems.firstname 'memfname)
                                          (:as 'mems.surname 'memsname)
                                          (:as 'recs.firstname 'recfname)
                                          (:as 'recs.surname 'recsname)
                                          :from (:as 'cd.members 'mems)
                                          :left-join (:as 'cd.members 'recs)
                                          :on (:= 'recs.memid 'mems.recommendedby))
                                 'memsname 'memfname))
                 "((SELECT mems.firstname AS memfname, mems.surname AS memsname, recs.firstname AS recfname, recs.surname AS recsname FROM cd.members AS mems LEFT JOIN cd.members AS recs ON (recs.memid = mems.recommendedby)) ORDER BY memsname, memfname)"))
;; multiple inner join with column concatenate from https://www.pgexercises.com/questions/joins/threejoin.html
      (is (equal (sql (:order-by (:select  (:as (:|| 'mems.firstname " " 'mems.surname) 'member)
                                           (:as 'facs.name 'facility)
                                           :distinct
                                           :from (:as 'cd.members 'mems)
                                           :inner-join (:as 'cd.bookings 'bks) :on (:= 'mems.memid 'bks.memid)
                                           :inner-join (:as 'cd.facilities 'facs) :on (:= 'bks.facid 'facs.facid)
                                           :where (:in 'bks.facid (:set 0 1)))
                                 'member))
"((SELECT DISTINCT (mems.firstname || E' ' || mems.surname) AS member, facs.name AS facility FROM cd.members AS mems INNER JOIN cd.bookings AS bks ON (mems.memid = bks.memid) INNER JOIN cd.facilities AS facs ON (bks.facid = facs.facid) WHERE (bks.facid IN (0, 1))) ORDER BY member)"))
;; Right Join
      (is (equal (sql (:select '* :from 't1 :right-join 't2 :on (:= 't1.num 't2.num)))
                 "(SELECT * FROM t1 RIGHT JOIN t2 ON (t1.num = t2.num))"))

;; Full Outer Join
      (is (equal (sql (:select '* :from 't1 :outer-join 't2 :on (:= 't1.num 't2.num)))
                 "(SELECT * FROM t1 FULL OUTER JOIN t2 ON (t1.num = t2.num))")))

(test subselects
      "Testing subselects"
;; From https://www.pgexercises.com/questions/joins/sub.html
      (is (equal (sql (:order-by (:select (:as (:|| 'mems.firstname " " 'mems.surname) 'member)
                                          (:select (:as (:|| 'recs.firstname " " 'recs.surname) 'recommender)
                                                   :from (:as 'cd.members 'recs)
                                                   :where (:= 'recs.memid 'mems.recommendedby)) :distinct
                                                   :from (:as 'cd.members 'mems))
                                 'member))
                 "((SELECT DISTINCT (mems.firstname || E' ' || mems.surname) AS member, (SELECT (recs.firstname || E' ' || recs.surname) AS recommender FROM cd.members AS recs WHERE (recs.memid = mems.recommendedby)) FROM cd.members AS mems) ORDER BY member)"))

      (is (equal (sql (:order-by (:select 'member 'facility 'cost
                                          :from
                                          (:as (:select (:as (:|| 'mems.firstname " " 'mems.surname) 'member)
                                                        (:as 'facs.name 'facility)
                                                        (:as (:case ((:= 'mems.memid 0 ) (:* 'bks.slots 'facs.guestcost))
                                                               (:else (:* 'bks.slots 'facs.membercost)))
                                                             'cost)
                                                        :from (:as 'cd.members 'mems)
                                                        :inner-join (:as 'cd.bookings 'bks)
                                                        :on (:= 'mems.memid 'bks.memid)
                                                        :inner-join (:as 'cd.facilities 'facs)
                                                        :on (:= 'bks.facid 'facs.facid)
                                                        :where
                                                        (:and (:>= 'bks.starttime "2012-09-14")
                                                              (:<= 'bks.starttime "2012-09-15")))
                                               'bookings)
                                          :where (:> 'cost 30))
                                 (:desc 'cost)))
                 "((SELECT member, facility, cost FROM (SELECT (mems.firstname || E' ' || mems.surname) AS member, facs.name AS facility, CASE WHEN (mems.memid = 0) THEN (bks.slots * facs.guestcost) ELSE (bks.slots * facs.membercost) END AS cost FROM cd.members AS mems INNER JOIN cd.bookings AS bks ON (mems.memid = bks.memid) INNER JOIN cd.facilities AS facs ON (bks.facid = facs.facid) WHERE ((bks.starttime >= E'2012-09-14') and (bks.starttime <= E'2012-09-15'))) AS bookings WHERE (cost > 30)) ORDER BY cost DESC)")))

(test select-case
      "Testing case statements from https://www.pgexercises.com/questions/basic/classify.html"
      (is (equal (sql (:select 'name (:as (:case ((:> 'monthlymaintenance 100) "expensive")
                                            (:else "cheap")) 'cost)
                               :from 'cd.facilities))
                 "(SELECT name, CASE WHEN (monthlymaintenance > 100) THEN E'expensive' ELSE E'cheap' END AS cost FROM cd.facilities)")))

(test select-dates
      "Testing date selection from https://www.pgexercises.com/questions/basic/date.html"
      (is (equal (sql (:select 'memid 'surname 'firstname 'joindate :from 'cd.members :where (:>= 'joindate "2012-09-01")))
                 "(SELECT memid, surname, firstname, joindate FROM cd.members WHERE (joindate >= E'2012-09-01'))")))

(test select-group-by
      "https://www.postgresql.org/docs/current/static/sql-select.html
To sum the column len of all films and group the results by kind:"
      (is (equal (sql (:select 'kind (:as (:sum 'len) 'total) :from 'films :group-by 'kind))
                 "(SELECT kind, SUM(len) AS total FROM films GROUP BY kind)")))

(test select-sum-group-interval
      "CAUTION: DOES NOT WORK. VALIDATE THAT THIS ACTUALLY WORKS WITH THE ESCAPED INTERVAL. To sum the column len of all films, group the results by kind and show those group totals that are less than 5 hours:"
;;; SELECT kind, sum(len) AS total
;;;    FROM films
;;;    GROUP BY kind
;;; HAVING sum(len) < interval '5 hours';

      (is (equal (sql (:select 'kind (:as (:sum 'len) 'total)
                               :from 'films
                               :group-by 'kind
                               :having (:< (:sum 'len) 'interval "5 hours")))
                 "(SELECT kind, SUM(len) AS total FROM films GROUP BY kind HAVING (SUM(len) < interval < E'5 hours'))")))

(test select-except
      "Testing the use of except in two select statements. Except removes all matches. Except all is slightly different. If the first select statement has two rows that match a single row in the second select statement, only one is removed."
      (is (equal (sql (:except (:select 'id 'name
                         :from 'countries
                         :where (:like 'name "%New%"))
                (:select 'id 'name
                         :from 'countries
                         :where (:like 'name "%Zealand%"))))
                 "((SELECT id, name FROM countries WHERE (name like E'%New%')) except (SELECT id, name FROM countries WHERE (name like E'%Zealand%')))"))
      (is (equal (sql (:except-all (:select '* :from 'clients) (:select '* :from 'vips)))
                 "((SELECT * FROM clients) except all (SELECT * FROM vips))")))

(test select-intersect
      "Testing the intersect in two select clauses. Note that intersect removes duplicates and intersect-all does not remove duplicates."
      (is (equal (sql (:intersect-all (:select '* :from 'clients) (:select '* :from 'vips)))
                 "((SELECT * FROM clients) intersect all (SELECT * FROM vips))"))
      (is (equal (sql (:intersect (:select '* :from 'clients) (:select '* :from 'vips)))
                 "((SELECT * FROM clients) intersect (SELECT * FROM vips))")))

;;; Aggregate Tests

(test avg-test
  "Testing the avg aggregate functions"
  (is (equal (sql (:select (:as (:round (:avg 'replacement-cost) 2) 'avg-replacement-cost) :from 'film ))
             "(SELECT round(AVG(replacement_cost), 2) AS avg_replacement_cost FROM film)")))

(test count-test
  "Testing the count aggregate function"
;; From https://www.pgexercises.com/questions/aggregates/count.html
      (is (equal (sql (:select (:count '*) :from 'cd.facilities))
                 "(SELECT COUNT(*) FROM cd.facilities)"))
      (is (equal (sql (:select 'facid (:select (:count '*) :from 'cd.facilities) :from 'cd.facilities))
                 "(SELECT facid, (SELECT COUNT(*) FROM cd.facilities) FROM cd.facilities)"))
      ;; From https://www.pgexercises.com/questions/aggregates/count2.html
      (is (equal (sql (:select (:count '*) :from 'cd.facilities :where (:>= 'guestcost 10)))
                 "(SELECT COUNT(*) FROM cd.facilities WHERE (guestcost >= 10))"))
      ;; From https://www.pgexercises.com/questions/aggregates/count3.html
      (is (equal (sql (:order-by (:select 'recommendedby (:count '*)
                                          :from 'cd.members
                                          :where (:not (:is-null 'recommendedby))
                                          :group-by 'recommendedby)
                                 'recommendedby))
                 "((SELECT recommendedby, COUNT(*) FROM cd.members WHERE (not (recommendedby IS NULL)) GROUP BY recommendedby) ORDER BY recommendedby)"))
      ;; From https://www.pgexercises.com/questions/aggregates/members1.html
      (is (equal (sql (:select (:count 'memid :distinct) :from 'cd.bookings))
                 "(SELECT COUNT(DISTINCT memid) FROM cd.bookings)"))
      (is (equal (sql (:select (:as (:count '*) 'unfiltered) (:as (:count '* :filter (:= 1 'bid)) 'filtered) :from 'testtable))
                 "(SELECT COUNT(*) AS unfiltered, COUNT(*) FILTER (WHERE (1 = bid)) AS filtered FROM testtable)"))
      (is (equal (sql (:select (:as (:count '* :distinct) 'unfiltered) (:as (:count '* :filter (:= 1 'bid)) 'filtered) :from 'testtable))
                 "(SELECT COUNT(DISTINCT *) AS unfiltered, COUNT(*) FILTER (WHERE (1 = bid)) AS filtered FROM testtable)"))
      (is (equal (with-test-connection (pomo:query (:select (:as (:count '*) 'unfiltered) (:as (:count '* :filter (:< 'i 5)) 'filtered)
                                                            :from (:as (:generate-series 1 10) 's 'i))))
                 '((10 4)))))

(test sum-test
  "Testing the sum aggregate function"
  ;; From https://www.pgexercises.com/questions/aggregates/fachours.html
      (is (equal (sql (:order-by (:select 'facid (:as (:sum 'slots) 'total-slots)
                                          :from 'cd.bookings
                                          :group-by 'facid)
                                 'facid))
                 "((SELECT facid, SUM(slots) AS total_slots FROM cd.bookings GROUP BY facid) ORDER BY facid)"))
;; From https://www.pgexercises.com/questions/aggregates/fachoursbymonth.html
      (is (equal (sql (:order-by (:select 'facid (:as (:sum 'slots) 'total-slots)
                                          :from 'cd.bookings
                                          :where (:and (:>= 'starttime "2012-09-01")
                                                       (:< 'starttime "2012-10-01"))
                                          :group-by 'facid)
                                 (:sum 'slots)))
                 "((SELECT facid, SUM(slots) AS total_slots FROM cd.bookings WHERE ((starttime >= E'2012-09-01') and (starttime < E'2012-10-01')) GROUP BY facid) ORDER BY SUM(slots))"))
      ;; From https://www.pgexercises.com/questions/aggregates/fachoursbymonth2.html
      (is (equal (sql (:order-by (:select 'facid (:as (:extract 'month 'starttime) 'month)
                                          (:as (:sum 'slots) 'total-slots)
                                          :from 'cd.bookings
                                          :where (:and (:>= 'starttime "2012-01-01")
                                                       (:< 'starttime "2013-01-01"))
                                          :group-by 'facid 'month)
                                 'facid 'month))
                 "((SELECT facid, EXTRACT(month FROM starttime) AS month, SUM(slots) AS total_slots FROM cd.bookings WHERE ((starttime >= E'2012-01-01') and (starttime < E'2013-01-01')) GROUP BY facid, month) ORDER BY facid, month)"))



;; From https://www.pgexercises.com/questions/aggregates/fachours1a.html
      (is (equal (sql (:order-by (:select 'facid (:as (:sum 'slots) 'total-slots)
                                          :from 'cd.bookings
                                          :group-by 'facid
                                          :having (:> (:sum 'slots)
                                                      1000))
                                 'facid))
                 "((SELECT facid, SUM(slots) AS total_slots FROM cd.bookings GROUP BY facid HAVING (SUM(slots) > 1000)) ORDER BY facid)"))
      ;; From https://www.pgexercises.com/questions/aggregates/facrev.html
      (is (equal (sql (:order-by (:select 'facs.name (:as (:sum (:* 'slots
                                                                    (:case ((:= 'memid 0) 'facs.guestcost)
                                                                      (:else 'facs.membercost))))
                                                          'revenue)
                                          :from (:as 'cd.bookings 'bks)
                                          :inner-join (:as 'cd.facilities 'facs)
                                          :on (:= 'bks.facid 'facs.facid)
                                          :group-by 'facs.name)
                                 'revenue))
                 "((SELECT facs.name, SUM((slots * CASE WHEN (memid = 0) THEN facs.guestcost ELSE facs.membercost END)) AS revenue FROM cd.bookings AS bks INNER JOIN cd.facilities AS facs ON (bks.facid = facs.facid) GROUP BY facs.name) ORDER BY revenue)"))

      ;;       From https://www.pgexercises.com/questions/aggregates/facrev2.html
      ;; V1
      (is (equal (sql (:order-by
                       (:select 'name 'revenue
                                :from (:as (:select 'facs.name
                                                    (:as (:sum (:case ((:= 'memid 0) (:* 'slots 'facs.guestcost))
                                                                 (:else (:* 'slots 'membercost))))
                                                         'revenue)
                                                    :from (:as 'cd.bookings 'bks)
                                                    :inner-join (:as 'cd.facilities 'facs)
                                                    :on (:= 'bks.facid 'facs.facid)
                                                    :group-by 'facs.name)
                                           'agg)
                                :where (:< 'revenue 1000))
                       'revenue))
                 "((SELECT name, revenue FROM (SELECT facs.name, SUM(CASE WHEN (memid = 0) THEN (slots * facs.guestcost) ELSE (slots * membercost) END) AS revenue FROM cd.bookings AS bks INNER JOIN cd.facilities AS facs ON (bks.facid = facs.facid) GROUP BY facs.name) AS agg WHERE (revenue < 1000)) ORDER BY revenue)"))
      ;; V2
      (is (equal (sql (:order-by
                       (:select 'facs.name (:as (:sum (:case ((:= 'memid 0) (:* 'slots 'facs.guestcost))
                                                        (:else (:* 'slots 'membercost))))
                                                'revenue)
                                :from (:as 'cd.bookings 'bks)
                                :inner-join (:as 'cd.facilities 'facs)
                                :on (:= 'bks.facid 'facs.facid)
                                :group-by 'facs.name
                                :having (:< (:sum (:case ((:= 'memid 0) (:* 'slots 'facs.guestcost))
                                                    (:else (:* 'slots 'membercost))))
                                            1000))
                       'revenue))
                 "((SELECT facs.name, SUM(CASE WHEN (memid = 0) THEN (slots * facs.guestcost) ELSE (slots * membercost) END) AS revenue FROM cd.bookings AS bks INNER JOIN cd.facilities AS facs ON (bks.facid = facs.facid) GROUP BY facs.name HAVING (SUM(CASE WHEN (memid = 0) THEN (slots * facs.guestcost) ELSE (slots * membercost) END) < 1000)) ORDER BY revenue)"))

;; From https://www.pgexercises.com/questions/aggregates/fachours2.html
      ;; V1
      (is (equal (sql (:limit (:order-by (:select 'facid (:as (:sum 'slots) 'total-slots)
                                                  :from 'cd.bookings
                                                  :group-by 'facid)
                                         (:desc (:sum 'slots)))
                              1))
                 "(((SELECT facid, SUM(slots) AS total_slots FROM cd.bookings GROUP BY facid) ORDER BY SUM(slots) DESC) LIMIT 1)"))

      ;; V2
      (is (equal (sql (:select 'facid (:as (:sum 'slots) 'totalslots)
                               :from 'cd.bookings
                               :group-by 'facid
                               :having (:= (:sum 'slots) (:select (:max 'sum2.totalslots)
                                                                  :from (:as (:select (:as (:sum 'slots ) 'totalslots)
                                                                                      :from 'cd.bookings
                                                                                      :group-by 'facid)
                                                                             'sum2)))))
                 "(SELECT facid, SUM(slots) AS totalslots FROM cd.bookings GROUP BY facid HAVING (SUM(slots) = (SELECT MAX(sum2.totalslots) FROM (SELECT SUM(slots) AS totalslots FROM cd.bookings GROUP BY facid) AS sum2)))"))

      ;; From https://www.pgexercises.com/questions/aggregates/fachoursbymonth3.html
      ;; V1
      (is (equal (sql (:order-by (:select 'facid (:as (:extract 'month 'starttime) 'month) (:as (:sum 'slots) 'slots)
                                          :from 'cd.bookings
                                          :where (:and (:>= 'starttime "2012-01-01")
                                                       (:< 'starttime "2013-01-01"))
                                          :group-by (:rollup 'facid 'month))
                                 'facid 'month))
                 "((SELECT facid, EXTRACT(month FROM starttime) AS month, SUM(slots) AS slots FROM cd.bookings WHERE ((starttime >= E'2012-01-01') and (starttime < E'2013-01-01')) GROUP BY rollup(facid, month)) ORDER BY facid, month)"))

      ;; V2
      (is (equal (sql (:order-by (:union-all (:select 'facid (:as (:extract 'month 'starttime) 'month) (:as (:sum 'slots) 'slots)
                                                      :from 'cd.bookings
                                                      :where (:and (:>= 'starttime "2012-01-01")
                                                                   (:< 'starttime "2013-01-01"))
                                                      :group-by 'facid 'month)
                                             (:select 'facid :null (:as (:sum 'slots) 'slots)
                                                      :from 'cd.bookings
                                                      :where (:and (:>= 'starttime "2012-01-01")
                                                                   (:< 'starttime "2013-01-01"))
                                                      :group-by 'facid)
                                             (:select :null :null (:as (:sum 'slots) 'slots)
                                                      :from 'cd.bookings
                                                      :where (:and (:>= 'starttime "2012-01-01")
                                                                   (:< 'starttime "2013-01-01")))) 'facid 'month))
                 "(((SELECT facid, EXTRACT(month FROM starttime) AS month, SUM(slots) AS slots FROM cd.bookings WHERE ((starttime >= E'2012-01-01') and (starttime < E'2013-01-01')) GROUP BY facid, month) union all (SELECT facid, NULL, SUM(slots) AS slots FROM cd.bookings WHERE ((starttime >= E'2012-01-01') and (starttime < E'2013-01-01')) GROUP BY facid) union all (SELECT NULL, NULL, SUM(slots) AS slots FROM cd.bookings WHERE ((starttime >= E'2012-01-01') and (starttime < E'2013-01-01')))) ORDER BY facid, month)")))

(test max-aggregation
      "Testing aggregation functions."
      (is (equal (sql (:select (:as (:max 'joindate) 'latest) :from 'cd.members))
                 "(SELECT MAX(joindate) AS latest FROM cd.members)"))
      (is (equal (sql (:select 'firstname 'surname 'joindate
                               :from 'cd.members
                               :where (:= 'joindate (:select (:max 'joindate)
                                                             :from 'cd.members))))
                 "(SELECT firstname, surname, joindate FROM cd.members WHERE (joindate = (SELECT MAX(joindate) FROM cd.members)))")))

(test mode-aggregation-test
  "Testing the aggregate sql-op mode"
  (is (equal (sql (:select (:mode 'items) :from 'item-table))
             "(SELECT mode() within group (order by items) FROM item_table)")))

(test every-aggregation-test
  "Testing the aggregation sql-op every"
  (is (equal (with-test-connection (query (:select '* (:every (:like 'studname "%h"))
                                                   :from 'tbl-students
                                                   :group-by 'studname 'studid 'studgrades)))
             '((4 "Ali" "B" NIL) (7 "Roy" "C" NIL) (6 "Sofia" "A" NIL) (5 "Mukesh" "D" T)
               (2 "Kimly" "B" NIL) (3 "Jenny" "C" NIL) (1 "Anvesh" "A" T)
               (8 "Martin" "C" NIL)))))

(test string-agg
  "Testing string-agg sql-op"
  (is (equal (sql (:select (:as (:string-agg 'bp.step-type "," ) 'step-summary) :from 'business-process))
             "(SELECT STRING_AGG(bp.step_type, E',') AS step_summary FROM business_process)"))
  (is (equal (sql (:select 'mid (:as (:string-agg  'y "," :distinct) 'words) :from 'moves))
             "(SELECT mid, STRING_AGG(DISTINCT y, E',') AS words FROM moves)"))
  (is (equal (sql (:select 'mid (:as (:string-agg  'y "," :distinct :order-by (:desc 'y) ) 'words) :from 'moves))
             "(SELECT mid, STRING_AGG(DISTINCT y, E',' ORDER BY y DESC) AS words FROM moves)")))

(test array-agg
  "Testing array-agg. Note the first example filters out null values as well as separating the y and n users."
  (is (equal (sql (:select 'g.id
                           (:as (:array-agg 'g.users :filter (:= 'g.canonical "Y")) 'canonical-users)
                           (:as (:array-agg 'g.users :filter (:= 'g.canonical "N")) 'non-canonical-users)
                   :from (:as 'groups 'g)
                   :group-by 'g.id))
             "(SELECT g.id, ARRAY_AGG(g.users) FILTER (WHERE (g.canonical = E'Y')) AS canonical_users, ARRAY_AGG(g.users) FILTER (WHERE (g.canonical = E'N')) AS non_canonical_users FROM groups AS g GROUP BY g.id)")))

(test percentile-cont
  "Testing percentile-cont."
  (is (equal (sql (:select (:percentile-cont :fraction 0.5 :order-by 'number-of-staff)
                           :from 'schools))
             "(SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY number_of_staff) FROM schools)"))
  (is (equal (sql (:select (:percentile-cont :fraction array[0.25 0.5 0.75 1] :order-by 'number-of-staff)
                    :from  'schools))
             "(SELECT PERCENTILE_CONT(ARRAY[0.25 0.5 0.75 1]) WITHIN GROUP (ORDER BY number_of_staff) FROM schools)"))
  (is (equal (sql (:order-by (:select 'day
                                      (:over (:percentile-cont :fraction 0.25 :order-by (:asc 'duration)) (:partition-by 'day))
                                      (:over (:percentile-cont :fraction 0.5 :order-by (:asc 'duration)) (:partition-by 'day))
                                      (:over (:percentile-cont :fraction 0.75 :order-by (:asc 'duration)) (:partition-by 'day))
                                      (:over (:percentile-cont :fraction 0.85 :order-by (:asc 'duration)) (:partition-by 'day))
                              :from 'query-durations :group-by 1 ) 1))
             "((SELECT day, (PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY duration ASC) OVER (PARTITION BY day)), (PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY duration ASC) OVER (PARTITION BY day)), (PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY duration ASC) OVER (PARTITION BY day)), (PERCENTILE_CONT(0.85) WITHIN GROUP (ORDER BY duration ASC) OVER (PARTITION BY day)) FROM query_durations GROUP BY 1) ORDER BY 1)")))

(test percentile-dist
  "Testing percentile-dist sql-op"
  (is (equal (sql (:select (:percentile-dist :fraction 0.5 :order-by 'number-of-staff)
                           :from 'schools))
             "(SELECT PERCENTILE_DIST(0.5) WITHIN GROUP (ORDER BY number_of_staff) FROM schools)"))
  (is (equal (sql (:select (:percentile-dist :fraction array[0.25 0.5 0.75 1] :order-by 'number-of-staff)
                           :from  'schools))
             "(SELECT PERCENTILE_DIST(ARRAY[0.25 0.5 0.75 1]) WITHIN GROUP (ORDER BY number_of_staff) FROM schools)")))

(test corr
  "Testing correlation coefficient. To quote from wikipedia (https://en.wikipedia.org/wiki/Covariance)
 In probability theory and statistics, covariance is a measure of the joint variability of two random variables. If the greater values of one variable mainly correspond with the greater values of the other variable, and the same holds for the lesser values, (i.e., the variables tend to show similar behavior), the covariance is positive. In the opposite case, when the greater values of one variable mainly correspond to the lesser values of the other, (i.e., the variables tend to show opposite behavior), the covariance is negative. The sign of the covariance therefore shows the tendency in the linear relationship between the variables. The magnitude of the covariance is not easy to interpret because it is not normalized and hence depends on the magnitudes of the variables. The normalized version of the covariance, the correlation coefficient, however, shows by its magnitude the strength of the linear relation."
  (is (equal (sql (:select (:corr 'height 'weight) :from 'people))
             "(SELECT CORR(height , weight) FROM people)")))

(test covar-pop
  "Testing population covariance."
    (is (equal (sql (:select (:covar-pop 'height 'weight) :from 'people))
               "(SELECT COVAR_POP(height , weight) FROM people)")))

(test covar-samp
  "Testing sample covariance."
    (is (equal (sql (:select (:covar-samp 'height 'weight) :from 'people))
             "(SELECT COVAR_SAMP(height , weight) FROM people)")))

(test employee-table
  "Build employee table"
  (with-test-connection
    (when (table-exists-p 'employee)
      (query (:drop-table 'employee)))
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
                                        (6 "James" 70060 "09/06/99" "Toronot" "N" 26)
                                        (7 "Alison" 90620 "08/07/00" "New York" "W" 38)
                                        (8 "Chris" 26020 "07/08/01" "Vancouver" "N" 22)
                                        (9 "Mary" 60020 "06/08/02" "Toronto" "W" 34))))
    (is-true (table-exists-p 'employee))))

(test stddev1
  "Testing statistical functions 1"
  (with-test-connection
        (is (equal (format nil "~,6f" (query (:select (:stddev 'salary) :from 'employee) :single))
                   "26805.934000"))
        (is (equal (query (:select (:variance 'salary) :from 'employee) :single)
                   718558064))
        (is (equal (query (:select (:var-pop 'salary) :from 'employee) :single)
                   63871827911111111/100000000))
        (is (equal (query (:select (:var-samp 'salary) :from 'employee) :single)
                   718558064))
        (is (equal (format nil "~,4f" (query (:select (:stddev-samp 'salary) :from 'employee) :single))
                   "26805.9340"))
        (is (equal (format nil "~,4f" (query (:select (:stddev-pop 'salary) :from 'employee) :single))
                   "25272.8770"))
        (is (equal (format nil "~,4f" (query (:select (:avg 'salary) :from 'employee) :single))
                   "49580.6680"))
        (is (equal (format nil "~,4f" (query (:select (:max 'salary) :from 'employee) :single))
                   "90620.0000"))
        (is (equal (format nil "~,4f" (query (:select (:min 'salary) :from 'employee) :single))
                   "14420.0000"))))

(test regr-functions
  "Testing standard deviation functions"
  (with-test-connection

    (is (equal (format nil "~,4f" (query (:select (:regr-avgx 'salary 'age) :from 'employee) :single))
               "28.1111"))
    (is (equal (format nil "~,4f" (query (:select (:regr-avgx 'age 'salary) :from 'employee) :single))
               "49580.6667"))
    (is (equal (query (:select (:regr-avgy 'salary 'age) :from 'employee) :single)
               49580.666666666664d0))
    (is (equal (query (:select (:regr-avgy 'age 'salary) :from 'employee) :single)
               28.11111111111111d0))
    (is (equal (query (:select (:regr-count 'salary 'age) :from 'employee) :single)
               9))
    (is (equal (query (:select (:regr-count 'age 'salary) :from 'employee) :single)
               9))
    (is (equal (query (:select (:regr-intercept 'salary 'age) :from 'employee) :single)
               -62911.0363153233d0))
    (is (equal (query (:select (:regr-intercept 'age 'salary) :from 'employee) :single)
               19.451778623108986d0))
    (is (equal (query (:select (:regr-r2 'salary 'age) :from 'employee) :single)
               0.6988991834467292d0))
    (is (equal (query (:select (:regr-r2 'age 'salary) :from 'employee) :single)
               0.6988991834467292d0))
    (is (equal (query (:select (:regr-slope 'salary 'age) :from 'employee) :single)
               4001.6811337466784d0))
    (is (equal (query (:select (:regr-slope 'age 'salary) :from 'employee) :single)
               1.7465139277410806d-4))
    (is (equal (query (:select (:regr-sxx 'salary 'age) :from 'employee) :single)
               250.88888888888889d0))
    (is (equal (query (:select (:regr-sxx 'age 'salary) :from 'employee) :single)
               5.748464512d9))
    (is (equal (query (:select (:regr-sxy 'salary 'age) :from 'employee) :single)
               1003977.3333333334d0))
    (is (equal (query (:select (:regr-sxy 'age 'salary) :from 'employee) :single)
               1003977.3333333334d0))
    (is (equal (query (:select (:regr-syy 'salary 'age) :from 'employee) :single)
               5.748464512d9))
    (is (equal (query (:select (:regr-syy 'age 'salary) :from 'employee) :single)
               250.88888888888889d0))))

(test select-union
      "testing basic union."
;;; https://www.postgresql.org/docs/current/static/typeconv-union-case.html
      (is (equal (sql (:union (:select (:as "a" 'text) ) (:select "b")))
                 "((SELECT E'a' AS text) union (SELECT E'b'))"))
      (is (equal (sql (:union (:select (:as 1.2 "numeric")) (:select 1)))
                 "((SELECT 1.2 AS E'numeric') union (SELECT 1))"))


;;; This shows how to obtain the union of the tables distributors and actors, restricting the results to those that begin with the letter W in each table. Only distinct rows are wanted, so the key word ALL is omitted. https://www.postgresql.org/docs/current/static/sql-select.html

      (is (equal (sql  (:union (:select 'distributors.name
                                        :from 'distributors
                                        :where (:like 'distributors.name "W%"))
                               (:select 'actors.name
                                        :from 'actors
                                        :where (:like 'actors.name "W%"))))
                 "((SELECT distributors.name FROM distributors WHERE (distributors.name like E'W%')) union (SELECT actors.name FROM actors WHERE (actors.name like E'W%')))"))

;;; Union-all with a simple with clause https://www.postgresql.org/docs/current/static/sql-select.html
      (is (equal (sql (:union-all
                       (:with (:as 't1 (:select (:as (:random) 'x)
                                                :from (:generate-series 1 3)))
                              (:select '* :from 't1)) (:select '* :from 't1)))
                 "(WITH t1 AS (SELECT random() AS x FROM generate_series(1, 3))(SELECT * FROM t1) union all (SELECT * FROM t1))")))
(test select-with
      "Testing select having a CTE with function From https://www.pgexercises.com/questions/aggregates/fachours2.html"
      (is (equal (sql (:with (:as 'sum (:select 'facid (:as (:sum 'slots) 'totalslots)
                                                :from 'cd.bookings
                                                :group-by 'facid))
                             (:select 'facid 'totalslots
                                      :from 'sum
                                      :where (:= 'totalslots (:select (:max 'totalslots) :from 'sum)))))
                 "WITH sum AS (SELECT facid, SUM(slots) AS totalslots FROM cd.bookings GROUP BY facid)(SELECT facid, totalslots FROM sum WHERE (totalslots = (SELECT MAX(totalslots) FROM sum)))"))

      ;; From https://www.pgexercises.com/questions/aggregates/fachoursbymonth3.html
      (is (equal (sql (:order-by (:with (:as 'bookings (:select 'facid (:as (:extract 'month 'starttime) 'month) 'slots
                                                                :from 'cd.bookings
                                                                :where (:and (:>= 'starttime "2012-01-01")
                                                                             (:< 'starttime "2013-01-01"))))
                                        (:union-all (:select 'facid 'month (:sum 'slots) :from 'bookings :group-by 'facid 'month)
                                                    (:select 'facid :null (:sum 'slots) :from 'bookings :group-by 'facid)
                                                    (:select :null :null (:sum 'slots) :from 'bookings))) 'facid 'month))
                 "(WITH bookings AS (SELECT facid, EXTRACT(month FROM starttime) AS month, slots FROM cd.bookings WHERE ((starttime >= E'2012-01-01') and (starttime < E'2013-01-01')))((SELECT facid, month, SUM(slots) FROM bookings GROUP BY facid, month) union all (SELECT facid, NULL, SUM(slots) FROM bookings GROUP BY facid) union all (SELECT NULL, NULL, SUM(slots) FROM bookings)) ORDER BY facid, month)"))

      (is (equal (sql (:order-by (:select 'facs.facid 'facs.name
                                          (:as (:trim (:to-char (:/ (:sum 'bks.slots) 2.0) "9999999999999999D99")) 'total-hours)
                                          :from (:as 'cd.bookings 'bks)
                                          :inner-join (:as 'cd.facilities 'facs)
                                          :on (:= 'facs.facid 'bks.facid)
                                          :group-by 'facs.facid 'facs.name)
                                 'facs.facid))
"((SELECT facs.facid, facs.name, trim(to_char((SUM(bks.slots) / 2.0), E'9999999999999999D99')) AS total_hours FROM cd.bookings AS bks INNER JOIN cd.facilities AS facs ON (facs.facid = bks.facid) GROUP BY facs.facid, facs.name) ORDER BY facs.facid)"))
      )


(test select-order-by
  "Testing with order-by."
  (is (equal (sql (:order-by (:select 'id 'name
                                      :from 'users)
                             'name))
             "((SELECT id, name FROM users) ORDER BY name)"))
  (is (equal (sql (:order-by (:select 'id 'name
                                      :from 'users)
                             (:desc 'name)))
             "((SELECT id, name FROM users) ORDER BY name DESC)"))
  (is (equal (sql (:order-by (:select 'id 'name
                                      :from 'users)
                             (:asc 'name)))
             "((SELECT id, name FROM users) ORDER BY name ASC)"))
  (is (equal (sql (:order-by
                   (:select 'firstname 'surname
                             :from 'cd.members)
                   'surname))
             "((SELECT firstname, surname FROM cd.members) ORDER BY surname)"))
  (is (equal (sql (:select (:over (:first-value 'x) (:order-by 'x))
                           (:over (:first-value 'x) (:order-by (:desc 'x)))
                           :from (:as (:generate-series 1 5) 'x)))
             "(SELECT (first_value(x) OVER ( ORDER BY x)), (first_value(x) OVER ( ORDER BY x DESC)) FROM generate_series(1, 5) AS x)"))
  (is (equal (sql (:order-by (:select 'studgrades (:as (:string-agg 'studname ",") 'studpergrade)
                                      :from 'tbl-students
                                      :group-by 'studgrades)
                             1))
             "((SELECT studgrades, STRING_AGG(studname, E',') AS studpergrade FROM tbl_students GROUP BY studgrades) ORDER BY 1)"))
  (is (equal (sql (:select (:string-agg 'a "," :order-by 'a) :from 'tiny))
             "(SELECT STRING_AGG(a, E',' ORDER BY a) FROM tiny)"))
  (is (equal (sql (:select (:string-agg 'a "," :order-by (:desc 'a)) :from 'tiny))
             "(SELECT STRING_AGG(a, E',' ORDER BY a DESC) FROM tiny)")))


(test select-over
  "Testing with over and partition by. From https://www.pgexercises.com/questions/aggregates/countmembers.html"
  (is (equal (sql (:order-by
                   (:select  (:over (:count '*)) 'firstname 'surname
                             :from 'cd.members)
                   'joindate))
             "((SELECT (COUNT(*) OVER ()) , firstname, surname FROM cd.members) ORDER BY joindate)"))

  (is (equal (sql (:order-by (:select (:over (:count '*)
                                             (:partition-by (:date-trunc "month" 'joindate)))
                                      'firstname 'surname
                                      :from 'cd.members )
                             'joindate))
             "((SELECT (COUNT(*) OVER (PARTITION BY date_trunc(E'month', joindate))), firstname, surname FROM cd.members) ORDER BY joindate)"))
;; From https://www.pgexercises.com/questions/aggregates/nummembers.html
  (is (equal (sql (:order-by
                   (:select (:over (:row-number) (:order-by 'joindate)) 'firstname 'surname
                            :from 'cd.members)
                   'joindate))
             "((SELECT (row_number() OVER ( ORDER BY joindate)), firstname, surname FROM cd.members) ORDER BY joindate)"))

  ;;From https://www.pgexercises.com/questions/aggregates/fachours4.html
  (is (equal (sql (:select 'facid 'total
                           :from (:as (:select 'facid (:as (:sum 'slots) 'total)
                                               (:as (:over (:rank) (:order-by  (:desc (:sum 'slots))))
                                                    'rank)
                                               :from 'cd.bookings
                                               :group-by 'facid)
                                      'ranked)
                           :where (:= 'rank 1)))
             "(SELECT facid, total FROM (SELECT facid, SUM(slots) AS total, (rank() OVER ( ORDER BY SUM(slots) DESC)) AS rank FROM cd.bookings GROUP BY facid) AS ranked WHERE (rank = 1))"))

  (is (equal (sql (:select 'facid 'total
                           :from (:as (:select 'facid 'total  (:as (:over (:rank) (:order-by (:desc 'total)))
                                                                   'rank)
                                               :from (:as (:select 'facid (:as (:sum 'slots) 'total)
                                                                   :from 'cd.bookings
                                                                   :group-by 'facid) 'sumslots))
                                      'ranked)
                           :where (:= 'rank 1)))
             "(SELECT facid, total FROM (SELECT facid, total, (rank() OVER ( ORDER BY total DESC)) AS rank FROM (SELECT facid, SUM(slots) AS total FROM cd.bookings GROUP BY facid) AS sumslots) AS ranked WHERE (rank = 1))"))

  ;; from https://www.pgexercises.com/questions/aggregates/classify.html
  (is (equal (sql (:order-by (:select 'name (:as (:case ((:= 'class 1) "high")
                                  ((:= 'class 2) "average")
                                  (:else "low"))
                                'revenue)
                     :from (:as
                            (:select (:as 'facs.name 'name)
                                     (:as (:over (:ntile 3)
                                                 (:order-by
                                                  (:desc
                                                   (:sum (:case
                                                             ((:= 'memid 0) (:* 'slots 'facs.guestcost))
                                                           (:else (:* 'slots 'membercost)))))))
                                          'class)
                                     :from (:as 'cd.bookings 'bks)
                                     :inner-join (:as 'cd.facilities 'facs)
                                     :on (:= 'bks.facid 'facs.facid)
                                     :group-by 'facs.name)
                            'subq))
                      'class 'name))
             "((SELECT name, CASE WHEN (class = 1) THEN E'high' WHEN (class = 2) THEN E'average' ELSE E'low' END AS revenue FROM (SELECT facs.name AS name, (ntile(3) OVER ( ORDER BY SUM(CASE WHEN (memid = 0) THEN (slots * facs.guestcost) ELSE (slots * membercost) END) DESC)) AS class FROM cd.bookings AS bks INNER JOIN cd.facilities AS facs ON (bks.facid = facs.facid) GROUP BY facs.name) AS subq) ORDER BY class, name)")))




(test select-with-recursive
      "Testing with recursive. When working with recursive queries it is important to be sure that the recursive part of the query will eventually return no tuples, or else the query will loop indefinitely. Sometimes, using UNION instead of UNION ALL can accomplish this by discarding rows that duplicate previous output rows. However, often a cycle does not involve output rows that are completely duplicate: it may be necessary to check just one or a few fields to see if the same point has been reached before. The standard method for handling such situations is to compute an array of the already-visited values."

      (is (equal (sql
                  (:with-recursive
                      (:as (:t1 'n)
                           (:union-all (:values 1)
                                       (:select (:n 1)
                                                :from 't1
                                                :where (:< 'n 100))))
                    (:select (:sum 'n) :from 't1)))
             "WITH RECURSIVE t1(n) AS (values(1) union all (SELECT n(1) FROM t1 WHERE (n < 100)))(SELECT SUM(n) FROM t1)"))

;; the following query that searches a table graph using a link field:
  (is (equal (sql
              (:with-recursive
                  (:as (:search-graph 'id 'link 'data 'depth)
                       (:union-all (:select 'g.id 'g.link 'g.data 1
                                            :from (:as 'graph 'g))
                                   (:select 'g.id 'g.link 'g.data (:= 'sg.depth 1)
                                            :from (:as 'graph 'g) (:as 'search-graph 'sg)
                                            :where (:= 'g.id 'sg.link))))
                (:select '* :from 'search-graph)))
             "WITH RECURSIVE search_graph(id, link, data, depth) AS ((SELECT g.id, g.link, g.data, 1 FROM graph AS g) union all (SELECT g.id, g.link, g.data, (sg.depth = 1) FROM graph AS g, search_graph AS sg WHERE (g.id = sg.link)))(SELECT * FROM search_graph)"))

      ;;      Recursive queries are typically used to deal with hierarchical or tree-structured data. A useful example is this query to find all the direct and indirect sub-parts of a product, given only a table that shows immediate inclusions:

      (is (equal (sql
                  (:with-recursive
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
                 "WITH RECURSIVE included_parts(sub_part, part, quantity) AS ((SELECT sub_part, part, quantity FROM parts WHERE (part = E'our-product')) union all (SELECT p.sub_part, p.part, p.quantity FROM included_parts AS pr, parts AS p WHERE (p.part = pr.sub_part)))(SELECT sub_part, SUM(quantity) AS total_quantity FROM included_parts GROUP BY sub_part)"))

      ;; This query will loop if the link relationships contain cycles. Because we require a “depth” output, just changing UNION ALL to UNION would not eliminate the looping. Instead we need to recognize whether we have reached the same row again while following a particular path of links. We add two columns path and cycle to the loop-prone query:

;; In the general case where more than one field needs to be checked to recognize a cycle, use an array of rows. For example, if we needed to compare fields f1 and f2:

      (is (equal
           (sql
            (:with-recursive
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

      ;; Aside from preventing cycles, the array value is often useful in its own right as representing the “path” taken to reach any particular row.

      (is (equal (sql
                  (:with (:as 'regional-sales (:select 'region (:as (:sum 'amount) 'total-sales)
                                                       :from 'orders :group-by 'region))
                         (:as 'top-regions (:select 'region
                                                    :from 'regional-sales
                                                    :where (:> 'total-sales (:select (:/ (:sum 'total-sales) 10)
                                                                                     :from 'regional-sales))))
                         (:select 'region 'product (:as (:sum 'quantity) 'product-units)
                                  (:as (:sum 'amount) 'product-sales)
                                  :from 'orders
                                  :where (:in 'region (:select 'region :from 'top-regions))
                                  :group-by 'region 'product)))
                 "WITH regional_sales AS (SELECT region, SUM(amount) AS total_sales FROM orders GROUP BY region), top_regions AS (SELECT region FROM regional_sales WHERE (total_sales > (SELECT (SUM(total_sales) / 10) FROM regional_sales)))(SELECT region, product, SUM(quantity) AS product_units, SUM(amount) AS product_sales FROM orders WHERE (region IN (SELECT region FROM top_regions)) GROUP BY region, product)"))

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


;; https://www.postgresql.org/docs/current/static/sql-select.html
;; This example uses WITH RECURSIVE to find all subordinates (direct or indirect) of the employee Mary, and their level of indirectness, from a table that shows only direct subordinates:
      (is (equal (sql (:with-recursive
                          (:as (:employee-recursive 'distance 'employee-name 'manager-name)
                               (:union-all
                                (:select 1 'employee-name 'manager-name :from 'employee :where (:= 'manager-name "Mary"))
                                (:select (:+ 'er.distance 1) 'e.employee-name 'e.manager-name
                                         :from (:as 'employee-recursive 'er) (:as 'employee 'e)
                                         :where (:= 'er.employee-name 'e.manager-name))))
                        (:select 'distance 'employee-name :from 'employee-recursive)))
"WITH RECURSIVE employee_recursive(distance, employee_name, manager_name) AS ((SELECT 1, employee_name, manager_name FROM employee WHERE (manager_name = E'Mary')) union all (SELECT (er.distance + 1), e.employee_name, e.manager_name FROM employee_recursive AS er, employee AS e WHERE (er.employee_name = e.manager_name)))(SELECT distance, employee_name FROM employee_recursive)")))

(test insert-into
      "Testing insert into"
      (is (equal (sql (:insert-into 'cd.facilities :set 'facid 9 'name "Spa" 'membercost 20 'guestcost 30
                                    'initialoutlay 100000 'monthlymaintenance 800))
                 "INSERT INTO cd.facilities (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance) VALUES (9, E'Spa', 20, 30, 100000, 800)"))

      (is (equal (sql (:insert-into 'test :set 'id 15 'number-string "12" 'numeric-item 12.45
                                'ratio-item (/ 1 13) 'created-at "2018-02-01"))
  "INSERT INTO test (id, number_string, numeric_item, ratio_item, created_at) VALUES (15, E'12', 12.45, 0.0769230769230769230769230769230769230, E'2018-02-01')"))


;; From https://www.pgexercises.com/questions/updates/insert2.html
      (is (equal (sql (:insert-rows-into 'cd.facilities
            :columns 'facid 'name 'membercost 'guestcost 'initialoutlay 'monthlymaintenance
            :values '((9 "Spa" 20 30 100000 800) (10 "Squash Court 2" 3.5 17.5 5000 80))))
                 "INSERT INTO cd.facilities (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance) VALUES (9, E'Spa', 20, 30, 100000, 800), (10, E'Squash Court 2', 3.5, 17.5, 5000, 80)"))

;; From https://www.pgexercises.com/questions/updates/insert3.html
      (is (equal (sql (:insert-into 'cd.facilities
                                    :set 'facid (:select (:+ (:select (:max 'facid) :from 'cd.facilities) 1))
                                         'name "Spa" 'membercost 20 'guestcost 30 'initialoutlay 100000 'monthlymaintenance 800))
                 "INSERT INTO cd.facilities (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance) VALUES ((SELECT ((SELECT MAX(facid) FROM cd.facilities) + 1)), E'Spa', 20, 30, 100000, 800)"))
;; Now using rows https://www.pgexercises.com/questions/updates/insert3.html
      (is (equal (sql (:insert-rows-into 'cd.facilities
                         :columns 'facid  'name  'membercost  'guestcost 'initialoutlay 'monthlymaintenance
                         :values '(((:select (:+ (:select (:max 'facid) :from 'cd.facilities) 1)) "Spa" 20 30 100000 800 ))))
                 "INSERT INTO cd.facilities (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance) VALUES ((SELECT ((SELECT MAX(facid) FROM cd.facilities) + 1)), E'Spa', 20, 30, 100000, 800)")))

(test update
      "Testing updates"
;; From https://www.pgexercises.com/questions/updates/update.html
      (is (equal (sql (:update 'cd.facilities :set 'initialoutlay 10000 :where (:= 'facid 1)))
                 "UPDATE cd.facilities SET initialoutlay = 10000 WHERE (facid = 1)"))
;; From https://www.pgexercises.com/questions/updates/updatemultiple.html
      (is (equal (sql (:update 'cd.facilities :set 'membercost 6 'guestcost 30 :where (:in 'facid (:set 0 1))))
                 "UPDATE cd.facilities SET membercost = 6, guestcost = 30 WHERE (facid IN (0, 1))"))
      ;; From https://www.pgexercises.com/questions/updates/updatecalculated.html
      (is (equal (sql (:update (:as 'cd.facilities 'facs)
                               :set 'membercost (:select (:* 'membercost 1.1)
                                                         :from 'cd.facilities
                                                         :where (:= 'facid 0))
                               'guestcost (:select (:* 'guestcost 1.1)
                                                   :from 'cd.facilities
                                                   :where (:= 'facid 0))
                               :where (:= 'facs.facid 1)))
                 "UPDATE cd.facilities AS facs SET membercost = (SELECT (membercost * 1.1) FROM cd.facilities WHERE (facid = 0)), guestcost = (SELECT (guestcost * 1.1) FROM cd.facilities WHERE (facid = 0)) WHERE (facs.facid = 1)"))

      ;; Version 2
      (is (equal (sql (:update (:as 'cd.facilities 'facs)
                               :set 'membercost (:* 'facs2.membercost 1.1)
                               'guestcost (:* 'facs2.guestcost 1.1)
                               :from (:as (:select '*
                                                   :from 'cd.facilities
                                                   :where (:= 'facid 0))
                                          'facs2)
                               :where (:= 'facs.facid 1)))

                 "UPDATE cd.facilities AS facs SET membercost = (facs2.membercost * 1.1), guestcost = (facs2.guestcost * 1.1) FROM (SELECT * FROM cd.facilities WHERE (facid = 0)) AS facs2 WHERE (facs.facid = 1)")))

(test delete
      "Testing deletes"
      (is (equal (sql (:delete-from 'cd.bookings :where (:= 'id 5)))
                 "DELETE FROM cd.bookings WHERE (id = 5)"))
      ;; From https://www.pgexercises.com/questions/updates/deletewh2.html
      (is (equal (sql (:delete-from 'cd.members
                                    :where (:not (:in 'memid (:select 'memid :from 'cd.bookings)))))
                 "DELETE FROM cd.members WHERE (not (memid IN (SELECT memid FROM cd.bookings)))"))

      (is (equal (sql (:delete-from (:as 'cd.members 'mems)
                                    :where (:not (:exists (:select 1
                                                                   :from 'cd.bookings
                                                                   :where (:= 'memid 'mems.memid))))))
                 "DELETE FROM cd.members AS mems WHERE (not (EXISTS (SELECT 1 FROM cd.bookings WHERE (memid = mems.memid))))")))

(test arrays
      "Testing arrays"
      (is (equal (sql (:create-table array-provinces ((name :type text) (provinces :type text[]))))
                 "CREATE TABLE array_provinces (name TEXT NOT NULL, provinces TEXT[] NOT NULL)"))
      (is (equal (sql (:insert-rows-into 'array-provinces
                                         :columns 'name 'provinces
                                         :values '(("Germany" #("Baden-Wurttemberg" "Bavaria" "Berlin" "Brandenburg"))
                                                   ("Switzerland" #("Aargau" "Appenzell Ausserrhoden" "Basel-Landschaft" "Fribourg")))))
                 "INSERT INTO array_provinces (name, provinces) VALUES (E'Germany', ARRAY[E'Baden-Wurttemberg', E'Bavaria', E'Berlin', E'Brandenburg']), (E'Switzerland', ARRAY[E'Aargau', E'Appenzell Ausserrhoden', E'Basel-Landschaft', E'Fribourg'])"))
      (is (equal (sql (:insert-into 'array-provinces :set 'name "Canada" 'provinces #("Alberta" "British Columbia" "Manitoba" "Ontario")))
                 "INSERT INTO array_provinces (name, provinces) VALUES (E'Canada', ARRAY[E'Alberta', E'British Columbia', E'Manitoba', E'Ontario'])"))
      (is (equal (sql (:select (:[] 'provinces 2) :from 'array-provinces))
                 "(SELECT (provinces)[2] FROM array_provinces)"))
      (is (equal (sql (:select (:[] 'provinces 1) :from 'array-provinces :where (:= 'name "Germany")))
                 "(SELECT (provinces)[1] FROM array_provinces WHERE (name = E'Germany'))"))
      (is (equal (sql (:select (:[] 'provinces 2) :from 'array-provinces :where (:= (:[] 'provinces 2) "Bavaria")))
                 "(SELECT (provinces)[2] FROM array_provinces WHERE ((provinces)[2] = E'Bavaria'))"))
      (is (equal (sql (:select '* :from 'array-provinces :where (:= (:[] 'provinces 2) "Bavaria")))
                 "(SELECT * FROM array_provinces WHERE ((provinces)[2] = E'Bavaria'))"))


      )

(test multi-dimension-arrays
      "Testing multi-dimensional arrays"
      (is (equal (sql (:create-table sal-emp ((name :type text) (pay-by-quarter :type integer[]) (schedule :type  text[][]))))
                 "CREATE TABLE sal_emp (name TEXT NOT NULL, pay_by_quarter INTEGER[] NOT NULL, schedule TEXT[][] NOT NULL)"))
      (is (equal (sql (:insert-into 'sal-emp :set 'name "Bill" 'pay-by-quarter #(10000 10000 10000 10000) 'schedule #(#( "meeting" "lunch") #("training" "presentation"))))
                 "INSERT INTO sal_emp (name, pay_by_quarter, schedule) VALUES (E'Bill', ARRAY[10000, 10000, 10000, 10000], ARRAY[ARRAY[E'meeting', E'lunch'], ARRAY[E'training', E'presentation']])"))
      (is (equal (sql (:select 'name :from 'sal-emp :where (:<> (:[] 'pay-by-quarter 1) (:[] 'pay-by-quarter 2))))
                 "(SELECT name FROM sal_emp WHERE ((pay_by_quarter)[1] <> (pay_by_quarter)[2]))"))
      (is (equal (sql (:update 'sal-emp :set 'schedule #(#( "breakfast" "consulting") #("meeting" "lunch"))
                               :where (:= 'name "Carol")))
                 "UPDATE sal_emp SET schedule = ARRAY[ARRAY[E'breakfast', E'consulting'], ARRAY[E'meeting', E'lunch']] WHERE (name = E'Carol')")))


#|


https://www.postgresql.org/docs/current/static/sql-select.html
This example shows how to use a function in the FROM clause, both with and without a column definition list:

CREATE FUNCTION distributors(int) RETURNS SETOF distributors AS $$
    SELECT * FROM distributors WHERE did = $1;
$$ LANGUAGE SQL;

SELECT * FROM distributors(111);

CREATE FUNCTION distributors_2(int) RETURNS SETOF record AS $$
    SELECT * FROM distributors WHERE did = $1;
$$ LANGUAGE SQL;

SELECT * FROM distributors_2(111) AS (f1 int, f2 text);

Here is an example of a function with an ordinality column added:

SELECT * FROM unnest(ARRAY['a','b','c','d','e','f']) WITH ORDINALITY;


https://www.postgresql.org/docs/current/static/sql-select.html
This example uses LATERAL to apply a set-returning function get_product_names() for each row of the manufacturers table:

SELECT m.name AS mname, pname
FROM manufacturers m, LATERAL get_product_names(m.id) pname;

Manufacturers not currently having any products would not appear in the result, since it is an inner join. If we wished to include the names of such manufacturers in the result, we could do:

SELECT m.name AS mname, pname
FROM manufacturers m LEFT JOIN LATERAL get_product_names(m.id) pname ON true;

|#




(test dissect-type-0
      "Testing dissect-type"
      (multiple-value-bind (type null?)
          (s-sql::dissect-type 'char)
        (is (eq type 'char))
        (is (not null?))))

(test dissect-type-1
      "Testing dissect-type"
      (multiple-value-bind (type null?)
          (s-sql::dissect-type '(or char db-null))
        (is (eq type 'char))
        (is (equal null? t))))

(test dissect-type-2
      "Testing dissect-type"
      (multiple-value-bind (type null?)
          (s-sql::dissect-type '(or "char(5)" db-null))
        (is (equal type "char(5)"))
        (is (eq null? t))))


(test expand-table-column
      "Testing expand-table-column"
      (is (equal (s-sql::expand-table-column 'code '(:type varchar :primary-key 't))
                 '("code" " " "VARCHAR" " NOT NULL" " PRIMARY KEY ")))
      (is (equal (s-sql::expand-table-column 'code '(:type (or char db-null) :primary-key 't))
                 '("code" " " "CHAR" " PRIMARY KEY ")))
      (is (equal (s-sql::expand-table-column 'code '(:type (or (string 5) db-null) :primary-key 't))
                 '("code" " " "CHAR(5)" " PRIMARY KEY "))))

;;; CREATE TABLE TESTS
(test create-table-1
      "Testing Create Table. First example from https://www.postgresql.org/docs/10/static/sql-createtable.html
 Right now we do not have the intervals fixed."
      (is (equal (s-sql:sql (:create-table films
                             ((code :type (or (string 5) db-null) :constraint 'firstkey :primary-key 't)
                              (title :type (varchar 40))
                              (did :type integer)
                              (date-prod :type (or date db-null))
                              (kind :type (or (varchar 10) db-null))
                              (len :type (or interval db-null)))))
                 "CREATE TABLE films (code CHAR(5) CONSTRAINT firstkey PRIMARY KEY , title VARCHAR(40) NOT NULL, did INTEGER NOT NULL, date_prod DATE, kind VARCHAR(10), len INTERVAL)"))) ; Create table films and table distributors:

(test create-table-2
      "Second example from https://www.postgresql.org/docs/10/static/sql-createtable.html"
      (is (equal (s-sql:sql (:create-table distributors ((did :type (or integer db-null)
                                                              :primary-key "generated by default as identity")
                                                         (name :type (varchar 40) :check (:<> 'name "")))))
                 "CREATE TABLE distributors (did INTEGER PRIMARY KEY generated by default as identity, name VARCHAR(40) NOT NULL CHECK (name <> E''))")))

(test create-table-with-a-2-dimensional-array
      "Create a table with a 2-dimensional array"
      (is (equal (sql (:create-table array_int ((vector :type (or int[][] db-null)))))
                 "CREATE TABLE array_int (vector INT[][])")))

(test create-table-full-1
      "Test :create-table with extended table constraints."
      (is (equal (s-sql:sql (:create-table-full faa.d_airports
			    ((AirportID :type integer)
			     (Name      :type text)
			     (City      :type text)
			     (Country   :type text)
			     (airport_code :type text)
			     (ICOA_code :type text)
			     (Latitude  :type float8)
			     (Longitude :type float8)
			     (Altitude  :type float8)
			     (TimeZoneOffset :type float)
			     (DST_Flag  :type text)
			     (TZ        :type text))
			    ()
			    ((:distributed-by (airport_code)))))
	  "CREATE TABLE faa.d_airports (airportid INTEGER NOT NULL, name TEXT NOT NULL, city TEXT NOT NULL, country TEXT NOT NULL, airport_code TEXT NOT NULL, icoa_code TEXT NOT NULL, latitude FLOAT8 NOT NULL, longitude FLOAT8 NOT NULL, altitude FLOAT8 NOT NULL, timezoneoffset REAL NOT NULL, dst_flag TEXT NOT NULL, tz TEXT NOT NULL) DISTRIBUTED BY (airport_code) ")))

(test sequence-tests
      "sequence testing"
      (with-test-connection
        (when (pomo:sequence-exists-p :knobo-seq)
          (pomo:query (:drop-sequence :knobo-seq)))

        ;; Setup new sequence
        (is (eq
             (pomo:query (:create-sequence :knobo-seq) :single)
             nil))

        ;; Test that we can set increment
        (is (equal (sql (:alter-sequence :knobo-seq :increment 1))
                   "ALTER SEQUENCE knobo_seq INCREMENT BY 1"))
        (is (eq (pomo:query (:alter-sequence :knobo-seq :increment 1))
                nil))

        ;; Test that currval is not yet set
        (signals error (pomo:query (:select (:currval :knobo-seq)) :single))

        ;; Test next value
        (is (equal (pomo:query (:select (:nextval :knobo-seq)) :single)
                   1))

        ;; Test currval
        (is (eq (pomo:query (:select (:currval :knobo-seq)) :single) 1))

        ;; Test that we can set restart at 2
        ;; TODO Test that when we restart, we get 2.
        (is (equal (sql (:alter-sequence :knobo-seq :start 2))
                   "ALTER SEQUENCE knobo_seq START 2"))

        (is (eq (pomo:query (:alter-sequence :knobo-seq :start 2))
                nil))

         ;; Testing that we can set max value
        (is (equal (sql (:alter-sequence :knobo-seq :max-value 5))
                   "ALTER SEQUENCE knobo_seq MAXVALUE 5"))
        (is (eq (pomo:query (:alter-sequence :knobo-seq :max-value 5))
                nil))

  ;; TODO: chech here that we don't can do next past max-value
        (is (equal (pomo:query (:select (:nextval :knobo-seq)) :single) 2))

        (is (equal (sql (:alter-sequence :knobo-seq :start 3))
                   "ALTER SEQUENCE knobo_seq START 3"))

        (is (eq (pomo:query (:alter-sequence :knobo-seq :start 3))
                nil))


        ;; Test set min value
        (is (equal (sql (:alter-sequence :knobo-seq :min-value 2))
                   "ALTER SEQUENCE knobo_seq MINVALUE 2"))
        (signals error (pomo:query (:alter-sequence :knobo-seq :min-value 3)))
        (is (equal (pomo:query (:alter-sequence :knobo-seq :min-value 2)) nil))

        ;; test remove max value
        (is (equal (sql (:alter-sequence :knobo-seq :no-max))
                   "ALTER SEQUENCE knobo_seq NO MAXVALUE"))
        (is (eq (pomo:query (:alter-sequence :knobo-seq :no-max))
                nil))

        ;; test remove min value
        (is (equal (sql (:alter-sequence :knobo-seq :no-min))
                   "ALTER SEQUENCE knobo_seq NO MINVALUE"))
        (is (eq (pomo:query (:alter-sequence :knobo-seq :no-min))
                nil))

        (is (eq (pomo:query (:alter-sequence :knobo-seq :cycle))
                nil))

        (is (eq (pomo:query (:alter-sequence :knobo-seq :no-cycle))
                nil))

        (is (eq (pomo:query (:alter-sequence :knobo-seq :cache 1))
                nil))

        (unless (pomo:table-exists-p "seq-test")
          (pomo:query (:create-table "seq-test" ((:id :type :int)))))

        (is (eq (pomo:query (:alter-sequence :knobo-seq :owned-by :seq-test.id))
                nil))

        ;; cleanup
        (pomo:query (:drop-sequence :knobo-seq))))

#|

Define a unique table constraint for the table films. Unique table constraints can be defined on one or more columns of the table:
How do we get the interval to be hour to minute?

(pomo:query (:create-table films
 ((code :type (or (string 5) db-null) :constraint 'firstkey :primary-key 't)
                              (title :type (varchar 40))
                              (did :type integer)
                              (date-prod :type (or date db-null))
                              (kind :type (or (varchar 10) db-null))
                              (len :type (or interval db-null)))))
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

|#
