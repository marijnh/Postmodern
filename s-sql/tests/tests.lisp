;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: S-SQL-TESTS; -*-
(in-package :s-sql-tests)

;; Adjust the above to some db/user/pass/host/[port] combination that
;; refers to a valid postgresql database, then after loading the file,
;; run the tests with (run! :cl-postgres)

(def-suite :s-sql
  :description "Master suite for s-sql")

(in-suite :s-sql)

(def-suite :s-sql-base
  :description "Base suite for s-sql"
  :in :s-sql)

(in-suite :s-sql-base)

(defun prompt-connection-to-s-sql-db-spec (param-lst)
  "Takes the 6 item parameter list from prompt-connection and restates it for pomo:with-connection. Note that cl-postgres does not provide the pooled connection - that is only in postmodern - so that parameter is not passed."
  (when (and (listp param-lst)
             (= 6 (length param-lst)))
    (destructuring-bind (db user password host port use-ssl) param-lst
      (list db user password host :port port :use-ssl use-ssl))))

(defvar *s-sql-db-spec* '()
  "A list of connection parameters to use when running the tests.  The order is: database name, user, password, hostname, port and use-ssl.")

(defmacro with-test-connection (&body body)
  `(pomo:with-connection (prompt-connection-to-s-sql-db-spec
                          (cl-postgres-tests:prompt-connection))
     ,@body))

(defmacro protect (&body body)
  `(unwind-protect (progn ,@(butlast body)) ,(car (last body))))

(def-suite :s-sql-base
  :description "Base test suite for s-sql"
  :in :s-sql)

(in-suite :s-sql-base)

(test connect-sanity
  (with-test-connection
    (is (not (null *database*)))))

(defun build-null-test-table ()
  "Building a simple table just to test the implementation can return :null."
  (with-test-connection
    (query (:drop-table :if-exists 'null-test :cascade))
    (query (:create-table 'null-test ((id :type serial :primary-key t :unique)
                                      (nullable :type (or integer db-null)))))
    (query (:insert-into 'null-test :set 'id 1 'nullable 10))
    (query (:insert-into 'null-test :set 'id 2))
    (query (:insert-into 'null-test :set 'id 3))))

(test abcl-null
  (build-null-test-table)
  (with-test-connection
    (is (equal (query (:select 'nullable :from 'null-test :where (:= 'id 1)) :single)
               10))
    (is (equal (query (:select 'nullable :from 'null-test :where (:= 'id 2)) :single)
               :null))
    (is (equal (query (:select '* :from 'null-test :where (:= 'id 2)))
               '((2 :null))))
    (query (:drop-table :if-exists 'null-test :cascade))))

(defun build-recipe-tables ()
  "Build recipe tables uses in array tests"
  (with-test-connection
    (query (:drop-table :if-exists 'recipes :cascade))
    (query (:drop-table :if-exists 'recipe-tags-array :cascade))
    (query (:create-table recipes
                          ((recipe-id :type serial :constraint 'recipekey-id
                                      :primary-key 't :unique)
                           (name :type text)
                           (text :type text))))

    (query (:create-table recipe-tags-array
                          ((recipe-id :type integer :references ((recipes recipe-id)))
                           (tags :type text[] :default "{}"))))

    (query (:create-unique-index 'recipe-tags-id-recipe-id
            :on "recipe-tags-array"  :fields 'recipe-id))
    (query (:create-index 'recipe-tags-id-tags
            :on "recipe-tags-array" :using gin :fields 'tags))

    (loop for x in '(("Fattoush" #("greens" "pita bread" "olive oil" "garlic"
                                   "lemon" "salt" "spices"))
                     ("Shawarma" #("meat" "tahini sauce" "pita bread"))
                     ("Baba Ghanoush" #("pita bread" "olive oil" "eggplant" "tahini sauce"))
                     ("Shish Taouk" #("chicken" "lemon juice" "garlic" "paprika"
                                      "yogurt" "tomato paste" "pita bread"))
                     ("Kibbe nayeh" #("raw meat" "bulgur" "onion" "spices" "pita bread"))
                     ("Manakeesh" #("meat" "cheese" "zaatar" "kishik" "tomatoes"
                                    "cucumbers" "mint leaves" "olives"))
                     ("Fakafek" #("chickpeas" "pita bread" "tahini sauce"))
                     ("Tabbouleh" #("bulgur" "tomatoes" "onions" "parsley"))
                     ("Kofta" #("minced meat" "parsley" "spices" "onions"))
                     ("Kunafeh" #("cheese" "sugar syrup" "pistachios"))
                     ("Baklava" #("filo dough" "honey" "nuts"))) do
                       (query (:insert-into 'recipes :set 'name (first x) 'text
                                            (format nil "~a" (rest x))))
                       (query
                        (:insert-into 'recipe-tags-array
                         :set 'recipe-id
                         (:select 'recipe-id
                          :from 'recipes
                          :where (:= 'recipes.name (first x)))
                         'tags (second x))))))

(defun build-employee-table ()
  "Build employee table for test purposes"
  (setf cl-postgres:*sql-readtable*
        (cl-postgres:copy-sql-readtable
         cl-postgres::*default-sql-readtable*))
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
                      (6 "James" 70060 "09/06/99" "Toronto" "N" 26)
                      (7 "Alison" 90620 "08/07/00" "New York" "W" 38)
                      (8 "Chris" 26020 "07/08/01" "Vancouver" "N" 22)
                      (9 "Mary" 60020 "06/08/02" "Toronto" "W" 34))))))

(test employee-table
  (with-test-connection
    (build-employee-table)
    (is-true (table-exists-p 'employee))))

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
  (is (equal (s-sql::split-on-keywords% '((a1 * ?) (b2 ?) (c3 ? *))
                                        '(:a1 "Alpha1 " :b2 "Beta2 " :c3 "Ceta3 "))
             '((A1 "Alpha1 ") (B2 "Beta2 ") (C3 "Ceta3 "))))
  (is (equal (s-sql::split-on-keywords% '((a1 * ?) (c3 ? *))
                                        '(:a1 "Alpha1 " :b2 "Beta2" :c3 "Ceta3 "))
             '((A1 "Alpha1 " :B2 "Beta2") (C3 "Ceta3 "))))
  (signals sql-error (s-sql::split-on-keywords% '((a1 * ?) (b2 ?) (c3 ? *))
                                                '(:a1 "Alpha1 "  :c3 "Ceta3 ")))
  (signals sql-error (s-sql::split-on-keywords% '((a1 * ?) (b2 -) (c3 ? *))
                                                '(:a1 "Alpha1 " :b2 "Beta2" :c3 "Ceta3 ")))
  (signals sql-error (s-sql::split-on-keywords% '((a1 * ?) (b2 ) (c3 ? *))
                                                '(:a1 "Alpha1 "  :c3 "Ceta3 ")))
  (signals sql-error (s-sql::split-on-keywords% '((owner ?))
                                                '(:owner "Sabra" :tourist "Geoffrey")))
  (signals sql-error (s-sql::split-on-keywords% '((a1 * ?) (c3 ? ))
                                                '(:a1 "Alpha1 " :c3 "Ceta3 " "Ceta3.5")))
  (is (equal (s-sql::split-on-keywords% '((fraction *)) `(:fraction 0.5))
             '((FRACTION 0.5)))))

(test split-on-keywords
  "Testing split-on-keywords. Used to handle arguments to some complex SQL operations.
Arguments are divided by keywords, which are interned with the name of the
non-keyword symbols in words, and bound to these symbols. After the
naming symbols, a ? can be used to indicate this argument group is
optional, an * to indicate it can consist of more than one element,
and a - to indicate it does not take any elements."
  (is (equal (s-sql::split-on-keywords ((a1 * ?) (b2 ?) (c3 ? *))
                 '(:a1 "Alpha1 " :b2 "Beta2 " :c3 "Ceta3 ")
               `("Results " ,@(when a1 a1) ,@(when c3 c3) ,@(when b2 b2)))
             '("Results " "Alpha1 " "Ceta3 " "Beta2 ")))
  (signals sql-error (s-sql::split-on-keywords ((a1 * ?) (b2 ?) (c3 ? *))
                         '(:a1 "Alpha1 "  :c3 "Ceta3 ")
                       `("Results " ,@(when a1 a1) ,@(when c3 c3) ,@(when b2 b2))))
  (is (equal (s-sql::split-on-keywords ((a1 * ?) (c3 ? *))
                 '(:a1 "Alpha1 " :b2 "Beta2" :c3 "Ceta3 ")
               `("Results " ,@(when a1 a1) ,@(when c3 c3)))
             '("Results " "Alpha1 " :B2 "Beta2" "Ceta3 ")))
  ;; Keyword does not take any arguments
  (signals sql-error (s-sql::split-on-keywords ((a1 * ?) (b2 -) (c3 ? *))
                         '(:a1 "Alpha1 " :b2 "Beta2" :c3 "Ceta3 ")
                       `("Results " ,@(when a1 a1) ,@(when c3 c3) ,@(when b2 b2))))
  ;; Required keyword missing
  (signals sql-error (s-sql::split-on-keywords ((a1 * ?) (b2 ) (c3 ? *))
                         '(:a1 "Alpha1 "  :c3 "Ceta3 ")
                       `("Results " ,@(when a1 a1) ,@(when c3 c3) ,@(when b2 b2))))
  (is  (equal (s-sql::split-on-keywords ((a1 * ?) (c3 ? *)) '(:a1 "Alpha1 " :b2 "Beta2"  :c3 "Ceta3 ")
                `("Results " ,@(when a1 a1) ,@ (when c3 c3)))
              '("Results " "Alpha1 " :B2 "Beta2" "Ceta3 ")))
  ;;too many elements for a keyword
  (signals sql-error (s-sql::split-on-keywords ((a1 * ?) (c3 ? ))
                         '(:a1 "Alpha1 " :c3 "Ceta3 " "Ceta3.5")
                       `("Results " ,@(when a1 a1) ,@(when c3 c3)))))

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
             "\"George-Harrison\""))
  (is (equal (s-sql:to-sql-name "george-gracie!@~#$%^&*()_+=-0987654321`QWERTGFDSAZXCVBNM<>?:LKJHYIUOP{}|/.,mnhjkl;']\[poiuy")
             "george_gracie________*______0987654321_qwertgfdsazxcvbnm____lkjhyiuop____._mnhjkl____poiuy"))
  (is (equal (s-sql:to-sql-name "Seyðisfjörður")  ;checking unicode names
             "seyðisfjörður")))

(test from-sql-name
  "Testing from-sql-name. Convert a string to something that might have been its original
lisp name \(does not work if this name contained non-alphanumeric
characters other than #\-)"
  (is (equal (s-sql::from-sql-name "create_all")
             :CREATE-ALL))
  (is (equal (s-sql::from-sql-name "region/los_angeles")
             :REGION/LOS-ANGELES))
  (is (equal (from-sql-name "Seyðisfjörður") ;checking unicode names
             :SEYÐISFJÖRÐUR))
  (is (equal (let ((name "create_all")) (s-sql::from-sql-name name))
             :CREATE-ALL))
  (is (equal (let ((name 'create_all)) (s-sql::from-sql-name name))
             :CREATE-ALL)))

(test sql-type-name
  "Testing sql-type-name. Transform a lisp type into a string containing
something SQL understands. Default is to just use the type symbol's
name."
  (is (equal (sql-type-name 'string "5")
             "CHAR(5)"))
  (is (equal (let ((name 'string)) (sql-type-name name "5"))
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
             "CHAR(5)"))
  (is (equal (let ((name 'float)) (s-sql::to-type-name name))
             "REAL"))
  (signals error (let ((name "float")) (s-sql::to-type-name name))))

(test s-sql:sql-escape-string
  "Testing sql-escape-string. Escape string data so it can be used in a query."
  (is (equal (sql-escape-string "Puss in 'Boots'")
             "E'Puss in ''Boots'''"))
  (is (equal (let ((name "float")) (sql-escape-string name))
             "E'float'"))
  (signals error (let ((name 'float)) (sql-escape-string name))))

(test sql-escape
  "Testing sql-escape. Get the representation of a Lisp value so that it
can be used in a query."
  (is (equal (sql-escape "tr'-x")
             "E'tr''-x'"))
  (is (equal (sql-escape (/ 1 13))
             "0.0769230769230769230769230769230769230"))
  (is (equal (sql-escape #("Baden-Wurttemberg" "Bavaria" "Berlin" "Brandenburg"))
             "ARRAY[E'Baden-Wurttemberg', E'Bavaria', E'Berlin', E'Brandenburg']"))
  (is (equal (let ((name 'float)) (sql-escape name))
             "float")))

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
  (is (equal (s-sql::sql-expand-list (remove nil '(george paul john "ringo" "mary-ann"
                                                   nil carol-anne nil)))
             '((SQL-ESCAPE GEORGE) ", " (SQL-ESCAPE PAUL) ", " (SQL-ESCAPE JOHN) ", "
               "E'ringo'" ", " "E'mary-ann'" ", " (SQL-ESCAPE CAROL-ANNE)))))

(test sql-expand-names
  "Testing sql-expand-names"
  (is (equal (let ((name 'float)) (s-sql::sql-expand-names name))
             NIL))
  (is (equal (let ((name "float")) (s-sql::sql-expand-names name))
             NIL))
  (is (equal (let ((name '("float"))) (s-sql::sql-expand-names name))
             '("float")))
  (is (equal (let ((name '('float))) (s-sql::sql-expand-names name))
             '("float")))
  (is (equal (s-sql::sql-expand-names '("george" "paul" "john" "ringo" "mary-ann"))
             '("george" ", " "paul" ", " "john" ", " "ringo" ", " "mary_ann")))
  (is (equal (s-sql::sql-expand-names '((george. "paul") "john" "ringo" "mary-ann"))
             '("george." "(" "E'paul'" ")" ", " "john" ", " "ringo" ", " "mary_ann")))
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
             "(SELECT name FROM items WHERE (id = 1))"))
  (is (equal (sql (:select "name" :from 'items :where (:= 'id 1))) ;note that Postgresql will error on the escaped columns
             "(SELECT E'name' FROM items WHERE (id = 1))")))

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
  (is (equal (sql (:select 'item 'groups :from 'item-table 'item-groups
                   :where (:= 'item-table.group-id 'item-groups.id)))
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

(test cast
  "Testing cast using cast or type"
  (is (equal (sql (:select (:type 1.0 int)))
             "(SELECT 1.0::INT)"))
  (is (equal (sql (:select (:type "true" boolean)))
             "(SELECT E'true'::BOOLEAN)"))
  (is (equal (sql (:select (:type "1" int)))
             "(SELECT E'1'::INT)"))
  (is (equal (sql (:select (:type "2018-01-01" date)))
             "(SELECT E'2018-01-01'::DATE)"))
  (is (equal (sql (:select (:type "1 minute" interval)))
             "(SELECT E'1 minute'::INTERVAL)"))
  (is (equal (sql (:select (:as (:cast (:as (:* 50 (:random)) 'int)) 'x)
                   :from (:generate-series 1 3)))
             "(SELECT CAST((50 * random()) AS int) AS x FROM generate_series(1, 3))"))
  (is (equal (sql (:select (:as (:- (:type (:now) date) 'x) 'some-date)
                   :from (:as (:generate-series 1 10) 'x)))
             "(SELECT (now()::DATE - x) AS some_date FROM generate_series(1, 10) AS x)"))
  (is (equal (with-test-connection
               (let ((type 'text))(query (:select (:cast (:as "20" type)))
                                         :single)))
             "20"))
  (is (equal (with-test-connection
               (let ((type 'integer))(query (:select (:cast (:as "20" type)))
                                            :single)))
             20)))

(test values
  "Testing values. Escaped string results have been validated."
  (is (equal (sql (:select 'a 'b 'c (:cast (:as (:* 50 (:random)) 'int))
                           :from (:as (:values (:set "a") (:set "b")) (:d1 'a))
                           (:as (:values (:set "c") (:set "d")) (:d2 'b))
                           (:as (:values (:set "e") (:set "f")) (:d3 'c))))
             "(SELECT a, b, c, CAST((50 * random()) AS int) FROM (VALUES (E'a'), (E'b')) AS d1(a), (VALUES (E'c'), (E'd')) AS d2(b), (VALUES (E'e'), (E'f')) AS d3(c))"))


  (is (equal (sql (:select '* :from (:as (:values (:set 1 "one") (:set 2 "two")
                                                  (:set 3 "three"))
                                         (:t1 'num 'letter))))
             "(SELECT * FROM (VALUES (1, E'one'), (2, E'two'), (3, E'three')) AS t1(num, letter))")))

(test any
  (is (equal (sql (:select 'sub-region-name :from 'regions :where (:= 'id (:any* '$1))))
             "(SELECT sub_region_name FROM regions WHERE (id = ANY($1)))"))
  (is (equal (sql (:select 'sub-region-name :from 'regions :where (:= 'id (:any '$1))))
             "(SELECT sub_region_name FROM regions WHERE (id = ANY $1))")))

(test select-limit-offset
  (is (equal (sql (:limit (:select 'country :from 'un-m49) 5 10))
             "((SELECT country FROM un_m49) LIMIT 5 OFFSET 10)")))

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

(test fetch
  "Testing the fetch sql-op"
  (is (equal
       (sql (:fetch (:order-by (:select 'id :from 'historical-events) 'id) 5))
       "(((SELECT id FROM historical_events) ORDER BY id) FETCH FIRST 5 ROWS ONLY)"))
  (is (equal (sql (:fetch
                   (:order-by (:select 'id
                               :from 'historical-events)
                              'id)))
             "(((SELECT id FROM historical_events) ORDER BY id) FETCH FIRST  ROWS ONLY)"))
  (is (equal (sql (:fetch
                   (:order-by (:select 'id
                               :from 'historical-events)
                              'id)
                   5))
             "(((SELECT id FROM historical_events) ORDER BY id) FETCH FIRST 5 ROWS ONLY)"))
  (is (equal (sql (:fetch
                   (:order-by
                    (:select 'id
                     :from 'historical-events)
                    'id)
                   5 10))
             "(((SELECT id FROM historical_events) ORDER BY id) OFFSET 10 FETCH FIRST 5 ROWS ONLY)")))

(test select-join-1
  "Testing basic join. Note full use of as. https://www.postgresql.org/docs/current/static/sql-select.html
To join the table films with the table distributors:"
  (is (equal (sql (:select 'f.title 'f.did 'd.name 'f.date-prod 'f.kind
                   :from (:as 'distributors 'd) (:as 'films 'f)
                   :where (:= 'f.did 'd.did)))
             "(SELECT f.title, f.did, d.name, f.date_prod, f.kind FROM distributors AS d, films AS f WHERE (f.did = d.did))"))
  ;; Overview
  (is (equal
       (sql (:select 'i.* 'p.*
                     :from (:as 'individual 'i)
                     :inner-join (:as 'publisher 'p)
                     :using ('individualid)
                     :inner-join (:as 'anothertable 'a)
                     :on (:= 'a.identifier 'i.individualid)
                     :where (:= 'a.something "something")))
       "(SELECT i.*, p.* FROM individual AS i INNER JOIN publisher AS p USING (individualid) INNER JOIN anothertable AS a ON (a.identifier = i.individualid) WHERE (a.something = E'something'))"))

  (is (equal (sql (:select 'c.customer 'c.state 'e.entry
                   :from (:as 'customer 'c)
                   :left-join (:as 'entry 'e)
                   :on (:and (:= 'c.customer 'e.customer)
                             (:= 'e.category "D"))))
             "(SELECT c.customer, c.state, e.entry FROM customer AS c LEFT JOIN entry AS e ON ((c.customer = e.customer) and (e.category = E'D')))"))

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
                   (:select 'mems.surname 'mems.firstname 'mems.memid (:as (:min 'bks.starttime)
                                                                           'starttime)
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
  (is (equal (sql (:order-by (:select (:as (:|| 'mems.firstname " " 'mems.surname) 'member)
                                      (:as 'facs.name 'facility)
                                      (:as (:case ((:= 'mems.memid 0 )
                                                   (:* 'bks.slots 'facs.guestcost))
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
  (is (equal
       (sql
        (:order-by
         (:select (:as (:|| 'mems.firstname " " 'mems.surname) 'member)
                  (:select (:as (:|| 'recs.firstname " " 'recs.surname) 'recommender)
                   :from (:as 'cd.members 'recs)
                   :where (:= 'recs.memid 'mems.recommendedby)) :distinct
                  :from (:as 'cd.members 'mems))
         'member))
       "((SELECT DISTINCT (mems.firstname || E' ' || mems.surname) AS member, (SELECT (recs.firstname || E' ' || recs.surname) AS recommender FROM cd.members AS recs WHERE (recs.memid = mems.recommendedby)) FROM cd.members AS mems) ORDER BY member)"))

  (is (equal
       (sql
        (:order-by
         (:select 'member 'facility 'cost
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
                                        (:else "cheap"))
                                      'cost)
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
  (is (equal (sql (:select (:as (:round (:avg 'replacement-cost) 2) 'avg-replacement-cost)
                   :from 'film ))
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
  (is (equal (sql (:select (:as (:count '*) 'unfiltered)
                           (:as (:count '* :filter (:= 1 'bid))
                                'filtered)
                           :from 'testtable))
             "(SELECT COUNT(*) AS unfiltered, COUNT(*) FILTER (WHERE (1 = bid)) AS filtered FROM testtable)"))
  (is (equal (sql (:select (:as (:count '* :distinct) 'unfiltered)
                           (:as (:count '* :filter (:= 1 'bid)) 'filtered)
                           :from 'testtable))
             "(SELECT COUNT(DISTINCT *) AS unfiltered, COUNT(*) FILTER (WHERE (1 = bid)) AS filtered FROM testtable)"))

  (is (equal (with-test-connection
               (pomo:query (:select (:as (:count '*) 'unfiltered)
                                    (:as (:count '* :filter (:< 'i 5))
                                         'filtered)
                                    :from (:as (:generate-series 1 10)
                                               's 'i))))
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
  (is (equal (sql (:order-by
                   (:select 'facs.name (:as (:sum (:* 'slots
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
  (is (equal
       (sql (:order-by
             (:select 'name 'revenue
                      :from (:as (:select 'facs.name
                                          (:as (:sum (:case ((:= 'memid 0)
                                                             (:* 'slots 'facs.guestcost))
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
                                                      :from (:as (:select (:as (:sum 'slots )
                                                                               'totalslots)
                                                                  :from 'cd.bookings
                                                                  :group-by 'facid)
                                                                 'sum2)))))
             "(SELECT facid, SUM(slots) AS totalslots FROM cd.bookings GROUP BY facid HAVING (SUM(slots) = (SELECT MAX(sum2.totalslots) FROM (SELECT SUM(slots) AS totalslots FROM cd.bookings GROUP BY facid) AS sum2)))"))

  ;; From https://www.pgexercises.com/questions/aggregates/fachoursbymonth3.html
  ;; V1
  (is (equal (sql (:order-by
                   (:select 'facid (:as (:extract 'month 'starttime) 'month)
                            (:as (:sum 'slots) 'slots)
                    :from 'cd.bookings
                    :where (:and (:>= 'starttime "2012-01-01")
                                 (:< 'starttime "2013-01-01"))
                    :group-by (:rollup 'facid 'month))
                   'facid 'month))
             "((SELECT facid, EXTRACT(month FROM starttime) AS month, SUM(slots) AS slots FROM cd.bookings WHERE ((starttime >= E'2012-01-01') and (starttime < E'2013-01-01')) GROUP BY rollup(facid, month)) ORDER BY facid, month)"))

  ;; V2
  (is (equal (sql
              (:order-by
               (:union-all
                (:select 'facid (:as (:extract 'month 'starttime) 'month) (:as (:sum 'slots) 'slots)
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
                              (:< 'starttime "2013-01-01"))))
               'facid 'month))
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
  "Testing the aggregation sql-op every. True if all input values are true, otherwise false."
  (with-test-connection
    (unless (table-exists-p 'employee) (build-employee-table))
    (is (equal (query (:order-by (:select 'id 'name 'city 'salary (:every (:like 'name "J%"))
                                  :from 'employee
                                  :group-by 'name 'id 'salary 'city)
                                 'name))
               '((7 "Alison" "New York" 90620 NIL) (3 "Celia" "Toronto" 24020 NIL)
                 (8 "Chris" "Vancouver" 26020 NIL) (5 "David" "Vancouver" 80026 NIL)
                 (6 "James" "Toronto" 70060 T) (1 "Jason" "New York" 40420 T)
                 (4 "Linda" "New York" 40620 NIL) (9 "Mary" "Toronto" 60020 NIL)
                 (2 "Robert" "Vancouver" 14420 NIL))))
    (is (equal (query (:select 'id 'name 'city 'salary (:over (:every (:like 'name "J%"))
                                                              (:partition-by 'id))
                       :from 'employee ))
               '((1 "Jason" "New York" 40420 T) (2 "Robert" "Vancouver" 14420 NIL)
                 (3 "Celia" "Toronto" 24020 NIL) (4 "Linda" "New York" 40620 NIL)
                 (5 "David" "Vancouver" 80026 NIL) (6 "James" "Toronto" 70060 T)
                 (7 "Alison" "New York" 90620 NIL) (8 "Chris" "Vancouver" 26020 NIL)
                 (9 "Mary" "Toronto" 60020 NIL))))
    (is (equal (query (:select 'id 'name 'city 'salary (:over (:every (:ilike 'name "j%"))
                                                              (:partition-by 'id))
                       :from 'employee ))
               '((1 "Jason" "New York" 40420 T) (2 "Robert" "Vancouver" 14420 NIL)
                 (3 "Celia" "Toronto" 24020 NIL) (4 "Linda" "New York" 40620 NIL)
                 (5 "David" "Vancouver" 80026 NIL) (6 "James" "Toronto" 70060 T)
                 (7 "Alison" "New York" 90620 NIL) (8 "Chris" "Vancouver" 26020 NIL)
                 (9 "Mary" "Toronto" 60020 NIL))))))

(test grouping-sets-selects
  "Testing grouping sets in a select clause. Reminder requires postgresql 9.5 or later."
  (is (equal (sql (:select 'c1 'c2 'c3 (:sum 'c4)
                           :from 'table-name
                           :group-by (:roll-up 'c1 'c2 'c3)))
             "(SELECT c1, c2, c3, SUM(c4) FROM table_name GROUP BY roll_up(c1, c2, c3))"))
  (is (equal (sql (:select 'c1 'c2 'c3 (:sum 'c4)
                           :from 'table-name
                           :group-by (:cube 'c1 'c2 'c3)))
             "(SELECT c1, c2, c3, SUM(c4) FROM table_name GROUP BY cube(c1, c2, c3))"))
  (is (equal (sql (:select 'c1 'c2 'c3 (:string-agg 'c3)
                           :from 'table-name
                           :group-by (:grouping-sets (:set 'c1 'c2) (:set 'c1) (:set 'c2))))
             "(SELECT c1, c2, c3, STRING_AGG(c3) FROM table_name GROUP BY GROUPING SETS (c1, c2), (c1), (c2))"))
  (is (equal (sql (:select 'd1 'd2 'd3 (:sum 'v)
                           :from 'test-cube
                           :group-by (:grouping-sets
                                      (:set (:set 'd1)
                                            (:set 'd2 'd3)))))
             "(SELECT d1, d2, d3, SUM(v) FROM test_cube GROUP BY GROUPING SETS ((d1), (d2, d3)))"))
  (is (equal (with-test-connection
               (query (:select 'city (:as (:extract 'year 'start-date)  'joining-year)
                               (:as (:count 1) 'employee_count)
                               :from 'employee
                               :group-by (:grouping-sets (:set 'city (:extract 'year 'start-date))))))
             '(("Vancouver" :NULL 3) ("New York" :NULL 3) ("Toronto" :NULL 3)
               (:NULL 2001.0d0 1) (:NULL 1997.0d0 1) (:NULL 1994.0d0 1) (:NULL 2000.0d0 1)
               (:NULL 2002.0d0 1) (:NULL 1996.0d0 1) (:NULL 1995.0d0 1) (:NULL 1998.0d0 1)
               (:NULL 1999.0d0 1))))
  (is (equal (sql (:select 'appnumber 'day (:sum 'inserts) (:sum 'updates)
                           (:sum 'deletes) (:sum 'transactions)
                           :from 'db-details
                           :group-by (:grouping-sets (:set 'appnumber 'day) )))
             "(SELECT appnumber, day, SUM(inserts), SUM(updates), SUM(deletes), SUM(transactions) FROM db_details GROUP BY GROUPING SETS (appnumber, day))"))

  (is (equal (sql (:select 'appnumber 'day (:sum 'inserts) (:sum 'updates)
                           (:sum 'deletes) (:sum 'transactions)
                           :from 'db-details
                           :group-by (:grouping-sets (:set 'appnumber 'day (:empty-set)) )))
             "(SELECT appnumber, day, SUM(inserts), SUM(updates), SUM(deletes), SUM(transactions) FROM db_details GROUP BY GROUPING SETS (appnumber, day, ()))")))

(test string-agg
  "Testing string-agg sql-op"
  (with-test-connection
    (is (equal (sql (:select (:as (:string-agg 'bp.step-type "," ) 'step-summary)
                     :from 'business-process))
               "(SELECT STRING_AGG(bp.step_type, E',') AS step_summary FROM business_process)"))
    (is (equal (sql (:select 'mid (:as (:string-agg  'y "," :distinct) 'words) :from 'moves))
               "(SELECT mid, STRING_AGG(DISTINCT y, E',') AS words FROM moves)"))
    (is (equal (sql (:select 'mid (:as (:string-agg  'y "," :distinct :order-by (:desc 'y)) 'words)
                             :from 'moves))
               "(SELECT mid, STRING_AGG(DISTINCT y, E',' ORDER BY y DESC) AS words FROM moves)"))
    (unless (table-exists-p 'employee) (build-employee-table))
    (is (equal
         (query (:select (:string-agg  'name "," :order-by (:desc 'name) :filter (:< 'id 4))
                 :from 'employee))
         '(("Robert,Jason,Celia"))))))

(test percentile-cont
  "Testing percentile-cont."
  (is (equal (sql (:select (:percentile-cont :fraction 0.5 :order-by 'number-of-staff)
                   :from 'schools))
             "(SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY number_of_staff) FROM schools)"))
  (is (equal (sql (:select (:percentile-cont :fraction array[0.25 0.5 0.75 1]
                            :order-by 'number-of-staff)
                   :from  'schools))
             "(SELECT PERCENTILE_CONT(ARRAY[0.25 0.5 0.75 1]) WITHIN GROUP (ORDER BY number_of_staff) FROM schools)"))
  (is (equal (sql (:order-by
                   (:select 'day
                            (:over (:percentile-cont :fraction 0.25 :order-by (:asc 'duration))
                                   (:partition-by 'day))
                            (:over (:percentile-cont :fraction 0.5 :order-by (:asc 'duration))
                                   (:partition-by 'day))
                            (:over (:percentile-cont :fraction 0.75 :order-by (:asc 'duration))
                                   (:partition-by 'day))
                            (:over (:percentile-cont :fraction 0.85 :order-by (:asc 'duration))
                                   (:partition-by 'day))
                            :from 'query-durations
                            :group-by 1 )
                   1))
             "((SELECT day, (PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY duration ASC) OVER (PARTITION BY day)), (PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY duration ASC) OVER (PARTITION BY day)), (PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY duration ASC) OVER (PARTITION BY day)), (PERCENTILE_CONT(0.85) WITHIN GROUP (ORDER BY duration ASC) OVER (PARTITION BY day)) FROM query_durations GROUP BY 1) ORDER BY 1)")))

(test percentile-dist
  "Testing percentile-dist sql-op"
  (is (equal (sql (:select (:percentile-dist :fraction 0.5 :order-by 'number-of-staff)
                   :from 'schools))
             "(SELECT PERCENTILE_DIST(0.5) WITHIN GROUP (ORDER BY number_of_staff) FROM schools)"))
  (is (equal (sql (:select (:percentile-dist :fraction array[0.25 0.5 0.75 1]
                            :order-by 'number-of-staff)
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

(test stddev1
  "Testing statistical functions 1"
  (with-test-connection
    (unless (table-exists-p 'employee) (build-employee-table))
    (is (equal (format nil "~,6f"
                       (query (:select (:stddev 'salary) :from 'employee) :single))
               "26805.934000"))
    (is (equal (query (:select (:variance 'salary) :from 'employee) :single)
               718558064))
    (is (equal (query (:select (:var-pop 'salary) :from 'employee) :single)
               63871827911111111/100000000))
    (is (equal (query (:select (:var-samp 'salary) :from 'employee) :single)
               718558064))
    (is (equal (format nil "~,4f"
                       (query (:select (:stddev-samp 'salary) :from 'employee) :single))
               "26805.9340"))
    (is (equal (format nil "~,4f"
                       (query (:select (:stddev-pop 'salary) :from 'employee) :single))
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
    (unless (table-exists-p 'employee) (build-employee-table))
    (is (equal (query (:select (:regr-avgx 'salary 'age) :from 'employee) :single)
               28.11111111111111d0))
    (is (equal (query (:select (:regr-avgx 'age 'salary) :from 'employee) :single)
               49580.666666666664d0))
    (is (equal (query (:select (:regr-avgy 'salary 'age) :from 'employee) :single)
               49580.666666666664d0))
    (is (equal (query (:select (:regr-avgy 'age 'salary) :from 'employee) :single)
               28.11111111111111d0))
    (is (equal (query (:select (:regr-count 'salary 'age) :from 'employee) :single)
               9))
    (is (equal (query (:select (:regr-count 'age 'salary) :from 'employee) :single)
               9))
    (is (equal (round (query (:select (:regr-intercept 'salary 'age) :from 'employee)
                             :single))
               (round -62911.0363153233d0)))  ;; using round because postgresql 12 generates a slightly different number than postgresql 11
    (is (equal (query (:select (:regr-intercept 'age 'salary) :from 'employee) :single)
               19.451778623108986d0))
    (is (equal (query (:select (:regr-r2 'salary 'age) :from 'employee) :single)
               0.6988991834467292d0))
    (is (equal (query (:select (:regr-r2 'age 'salary) :from 'employee) :single)
               0.6988991834467292d0))
    (is (equal (round (query (:select (:regr-slope 'salary 'age) :from 'employee) :single))
               (round 4001.6811337466784d0)))  ;;  using round because postgresql 12 generates a slightly different number than postgresql 11
    (is (equal (round (query (:select (:regr-slope 'age 'salary) :from 'employee) :single))
               (round 1.7465139277410806d-4))) ;;  using round because postgresql 12 generates a slightly different number than postgresql 11
    (is (equal (round (query (:select (:regr-sxx 'salary 'age) :from 'employee) :single))
               (round 250.88888888888889d0)))  ;;  using round because postgresql 12 generates a slightly different number than postgresql 11
    (is (equal (query (:select (:regr-sxx 'age 'salary) :from 'employee) :single)
               5.748464512d9))
    (is (equal (round (query (:select (:regr-sxy 'salary 'age) :from 'employee) :single))
               (round 1003977.3333333334d0))) ;;  using round because postgresql 12 generates a slightly different number than postgresql 11
    (is (equal (round (query (:select (:regr-sxy 'age 'salary) :from 'employee) :single))
               (round 1003977.3333333334d0)))  ;;  using round because postgresql 12 generates a slightly different number than postgresql 11
    (is (equal (query (:select (:regr-syy 'salary 'age) :from 'employee) :single)
               5.748464512d9))
    (is (equal (round (query (:select (:regr-syy 'age 'salary) :from 'employee) :single))
               (round 250.88888888888889d0))))) ;;  using round because postgresql 12 generates a slightly different number than postgresql 11

(test union
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

(test with
  "Testing select having a CTE with function From https://www.pgexercises.com/questions/aggregates/fachours2.html"
  (is
   (equal
    (sql
     (:with (:as 'regional-sales (:select 'region (:as (:sum 'amount) 'total-sales)
                                          :from 'orders :group-by 'region))
            (:as 'top-regions (:select 'region
                               :from 'regional-sales
                               :where (:> 'total-sales
                                          (:select (:/ (:sum 'total-sales) 10)
                                           :from 'regional-sales))))
            (:select 'region 'product (:as (:sum 'quantity) 'product-units)
                     (:as (:sum 'amount) 'product-sales)
                     :from 'orders
                     :where (:in 'region (:select 'region :from 'top-regions))
                     :group-by 'region 'product)))
    "WITH regional_sales AS (SELECT region, SUM(amount) AS total_sales FROM orders GROUP BY region), top_regions AS (SELECT region FROM regional_sales WHERE (total_sales > (SELECT (SUM(total_sales) / 10) FROM regional_sales)))(SELECT region, product, SUM(quantity) AS product_units, SUM(amount) AS product_sales FROM orders WHERE (region IN (SELECT region FROM top_regions)) GROUP BY region, product)"))
  (is
   (equal
    (sql
     (:with (:as (:movies-by-tags 'tag-id 'name 'created-at 'rank)
                 (:select 'tag-id 'name 'created-at
                          (:over (:row-number)
                                 (:partition-by 'tag-id :order-by 'tag-id
                                                (:desc 'created-at)))
                          :from 'movies))
            (:select '*
             :from (:as 'movies-by-tags 'mbt)
             :where (:< 'mbt.rank 3))))
    "WITH movies_by_tags(tag_id, name, created_at, rank) AS (SELECT tag_id, name, created_at, (row_number() OVER (PARTITION BY tag_id ORDER BY tag_id, created_at DESC)) FROM movies)(SELECT * FROM movies_by_tags AS mbt WHERE (mbt.rank < 3))"))
  (is
   (equal
    (sql
     (:with (:as 'sum (:select 'facid (:as (:sum 'slots) 'totalslots)
                               :from 'cd.bookings
                               :group-by 'facid))
            (:select 'facid 'totalslots
                     :from 'sum
                     :where (:= 'totalslots (:select (:max 'totalslots)
                                             :from 'sum)))))
    "WITH sum AS (SELECT facid, SUM(slots) AS totalslots FROM cd.bookings GROUP BY facid)(SELECT facid, totalslots FROM sum WHERE (totalslots = (SELECT MAX(totalslots) FROM sum)))"))

  ;; From https://www.pgexercises.com/questions/aggregates/fachoursbymonth3.html
  (is
   (equal
    (sql
     (:order-by (:with (:as 'bookings
                            (:select 'facid
                                     (:as (:extract 'month 'starttime) 'month) 'slots
                             :from 'cd.bookings
                             :where (:and (:>= 'starttime "2012-01-01")
                                          (:< 'starttime "2013-01-01"))))
                       (:union-all (:select 'facid 'month (:sum 'slots)
                                    :from 'bookings :group-by 'facid 'month)
                                   (:select 'facid :null (:sum 'slots)
                                    :from 'bookings :group-by 'facid)
                                   (:select :null :null (:sum 'slots)
                                    :from 'bookings)))
                'facid 'month))
    "(WITH bookings AS (SELECT facid, EXTRACT(month FROM starttime) AS month, slots FROM cd.bookings WHERE ((starttime >= E'2012-01-01') and (starttime < E'2013-01-01')))((SELECT facid, month, SUM(slots) FROM bookings GROUP BY facid, month) union all (SELECT facid, NULL, SUM(slots) FROM bookings GROUP BY facid) union all (SELECT NULL, NULL, SUM(slots) FROM bookings)) ORDER BY facid, month)"))

  (is
   (equal
    (sql
     (:order-by (:select 'facs.facid 'facs.name
                         (:as (:trim (:to-char (:/ (:sum 'bks.slots) 2.0)
                                               "9999999999999999D99"))
                              'total-hours)
                         :from (:as 'cd.bookings 'bks)
                         :inner-join (:as 'cd.facilities 'facs)
                         :on (:= 'facs.facid 'bks.facid)
                         :group-by 'facs.facid 'facs.name)
                'facs.facid))
    "((SELECT facs.facid, facs.name, trim(to_char((SUM(bks.slots) / 2.0), E'9999999999999999D99')) AS total_hours FROM cd.bookings AS bks INNER JOIN cd.facilities AS facs ON (facs.facid = bks.facid) GROUP BY facs.facid, facs.name) ORDER BY facs.facid)")))


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


(test over
  "Testing with over and partition by. From https://www.pgexercises.com/questions/aggregates/countmembers.html"
    (is (equal (sql (:over (:sum 'salary)))
             "(SUM(salary) OVER ()) "))
  (is (equal (sql (:over (:sum 'salary) 'w))
             "(SUM(salary) OVER w)"))
  (is (equal (sql (:over (:count '*)
                         (:partition-by (:date-trunc "month" 'joindate))))
             "(COUNT(*) OVER (PARTITION BY date_trunc(E'month', joindate)))"))
  (is (equal (sql (:over (:rank) (:order-by (:desc 'total))))
             "(rank() OVER ( ORDER BY total DESC))"))
  (is (equal (sql (:over (:percentile-cont :fraction 0.25 :order-by (:asc 'duration))
                         (:partition-by 'day)))
             "(PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY duration ASC) OVER (PARTITION BY day))"))
  (is (equal (sql (:select 'tag-id 'name 'created-at
                           (:over (:row-number)
                                  (:partition-by 'tag-id
                                   :order-by 'tag-id
                                                 (:desc 'created-at)))
                           :from 'movies))
             "(SELECT tag_id, name, created_at, (row_number() OVER (PARTITION BY tag_id ORDER BY tag_id, created_at DESC)) FROM movies)"))
  (is (equal
       (sql (:order-by
             (:select  (:over (:count '*)) 'firstname 'surname
              :from 'cd.members)
             'joindate))
       "((SELECT (COUNT(*) OVER ()) , firstname, surname FROM cd.members) ORDER BY joindate)"))

  (is (equal
       (sql (:order-by (:select (:over (:count '*)
                                       (:partition-by (:date-trunc "month" 'joindate)))
                                'firstname 'surname
                                :from 'cd.members )
                       'joindate))
       "((SELECT (COUNT(*) OVER (PARTITION BY date_trunc(E'month', joindate))), firstname, surname FROM cd.members) ORDER BY joindate)"))
  ;; From https://www.pgexercises.com/questions/aggregates/nummembers.html
  (is (equal
       (sql (:order-by
             (:select (:over (:row-number) (:order-by 'joindate)) 'firstname 'surname
              :from 'cd.members)
             'joindate))
       "((SELECT (row_number() OVER ( ORDER BY joindate)), firstname, surname FROM cd.members) ORDER BY joindate)"))

  ;;From https://www.pgexercises.com/questions/aggregates/fachours4.html
  (is (equal
       (sql (:select 'facid 'total
                     :from (:as (:select 'facid (:as (:sum 'slots) 'total)
                                         (:as (:over (:rank)
                                                     (:order-by (:desc (:sum 'slots))))
                                              'rank)
                                 :from 'cd.bookings
                                 :group-by 'facid)
                                'ranked)
                     :where (:= 'rank 1)))
       "(SELECT facid, total FROM (SELECT facid, SUM(slots) AS total, (rank() OVER ( ORDER BY SUM(slots) DESC)) AS rank FROM cd.bookings GROUP BY facid) AS ranked WHERE (rank = 1))"))

  (is (equal
       (sql (:select 'facid 'total
                     :from (:as (:select 'facid 'total
                                         (:as (:over (:rank) (:order-by (:desc 'total)))
                                              'rank)
                                 :from (:as (:select 'facid (:as (:sum 'slots) 'total)
                                                     :from 'cd.bookings
                                                     :group-by 'facid)
                                            'sumslots))
                                'ranked)
                     :where (:= 'rank 1)))
       "(SELECT facid, total FROM (SELECT facid, total, (rank() OVER ( ORDER BY total DESC)) AS rank FROM (SELECT facid, SUM(slots) AS total FROM cd.bookings GROUP BY facid) AS sumslots) AS ranked WHERE (rank = 1))"))

  ;; from https://www.pgexercises.com/questions/aggregates/classify.html
  (is (equal
       (sql (:order-by (:select 'name (:as (:case ((:= 'class 1) "high")
                                             ((:= 'class 2) "average")
                                             (:else "low"))
                                           'revenue)
                                :from (:as
                                       (:select (:as 'facs.name 'name)
                                                (:as (:over (:ntile 3)
                                                            (:order-by
                                                             (:desc
                                                              (:sum (:case
                                                                        ((:= 'memid 0)
                                                                         (:* 'slots 'facs.guestcost))
                                                                      (:else (:* 'slots 'membercost)))))))
                                                     'class)
                                                :from (:as 'cd.bookings 'bks)
                                                :inner-join (:as 'cd.facilities 'facs)
                                                :on (:= 'bks.facid 'facs.facid)
                                                :group-by 'facs.name)
                                       'subq))
                       'class 'name))
       "((SELECT name, CASE WHEN (class = 1) THEN E'high' WHEN (class = 2) THEN E'average' ELSE E'low' END AS revenue FROM (SELECT facs.name AS name, (ntile(3) OVER ( ORDER BY SUM(CASE WHEN (memid = 0) THEN (slots * facs.guestcost) ELSE (slots * membercost) END) DESC)) AS class FROM cd.bookings AS bks INNER JOIN cd.facilities AS facs ON (bks.facid = facs.facid) GROUP BY facs.name) AS subq) ORDER BY class, name)")))

(test between
  (is (equal (sql (:between 'latitude -10 10))
             "(latitude BETWEEN -10 AND 10)"))
  (is (equal (sql (:between (:- 'population.year 'ma-population.year)  0 2))
             "((population.year - ma_population.year) BETWEEN 0 AND 2)")))

(test over-range-between
  (is (equal
       (sql (:limit
             (:select (:as 'country 'country-name)
                      (:as 'population 'country-population)
                      (:as (:over (:sum 'population)
                                  (:range-between :order-by 'country :unbounded-preceding
                                   :unbounded-following))
                           'global-population)
                      :from 'population
                      :where (:and (:not-null 'iso2)
                                   (:= 'year 1976)))
             5))
       "((SELECT country AS country_name, population AS country_population, (SUM(population) OVER (ORDER BY country RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING )) AS global_population FROM population WHERE ((iso2 IS NOT NULL) and (year = 1976))) LIMIT 5)"))
  (is (equal
       (sql (:limit
             (:select (:as 'country 'country-name)
                      (:as 'population 'country-population)
                      (:as (:over (:sum 'population)
                                  (:range-between :order-by 'country :current-row
                                   :unbounded-following))
                           'global-population)
                      :from 'population
                      :where (:and (:not-null 'iso2)
                                   (:= 'year 1976)))
             5))
       "((SELECT country AS country_name, population AS country_population, (SUM(population) OVER (ORDER BY country RANGE BETWEEN  CURRENT ROW AND UNBOUNDED FOLLOWING )) AS global_population FROM population WHERE ((iso2 IS NOT NULL) and (year = 1976))) LIMIT 5)")))

(test over-row-between
  (is (equal
       (sql (:limit
             (:select (:as 'country 'country-name)
                      (:as 'population 'country-population)
                      (:as (:over (:sum 'population)
                                  (:rows-between :order-by 'country :preceding 2
                                                 :following 2))
                           'global-population)
                      :from 'population
                      :where (:and (:not-null 'iso2)
                                   (:= 'year 1976)))
             5))
       "((SELECT country AS country_name, population AS country_population, (SUM(population) OVER (ORDER BY country ROWS BETWEEN 2 PRECEDING AND 2 FOLLOWING )) AS global_population FROM population WHERE ((iso2 IS NOT NULL) and (year = 1976))) LIMIT 5)"))
  (is (equal
       (sql (:limit
             (:select (:as 'country 'country-name)
                      (:as 'population 'country-population)
                      (:as (:over (:sum 'population)
                                  (:rows-between :order-by 'country :current-row
                                   :following 2))
                           'global-population)
                      :from 'population
                      :where (:and (:not-null 'iso2)
                                   (:= 'year 1976)))
             5))
       "((SELECT country AS country_name, population AS country_population, (SUM(population) OVER (ORDER BY country ROWS BETWEEN  CURRENT ROW AND 2 FOLLOWING )) AS global_population FROM population WHERE ((iso2 IS NOT NULL) and (year = 1976))) LIMIT 5)"))
  (is (equal
       (sql (:limit
             (:select (:as 'country 'country-name)
                      (:as 'population 'country-population)
                      (:as (:over (:sum 'population)
                                  (:rows-between :order-by 'country :preceding 2
                                                 :current-row))
                           'global-population)
                      :from 'population
                      :where (:and (:not-null 'iso2)
                                   (:= 'year 1976)))
             5))
       "((SELECT country AS country_name, population AS country_population, (SUM(population) OVER (ORDER BY country ROWS BETWEEN 2 PRECEDING AND  CURRENT ROW )) AS global_population FROM population WHERE ((iso2 IS NOT NULL) and (year = 1976))) LIMIT 5)")))

(test over-with-partition-with-range-or-row-between
  (is (equal
       (sql (:limit
             (:select (:as 'population.country 'country-name)
                      (:as 'population 'country-population)
                      'region-name
                      (:as (:over (:sum 'population)
                                  (:partition-by 'region-name :order-by 'population.country
                                   :range-between :unbounded-preceding :current-row))
                           'regional-population)
                      :from 'population
                      :inner-join 'regions
                      :on (:= 'population.iso3 'regions.iso3)
                      :where (:and (:not-null 'population.iso2)
                                   (:= 'year 1976)))
             5))
       "((SELECT population.country AS country_name, population AS country_population, region_name, (SUM(population) OVER (PARTITION BY region_name ORDER BY population.country RANGE BETWEEN UNBOUNDED PRECEDING AND  CURRENT ROW )) AS regional_population FROM population INNER JOIN regions ON (population.iso3 = regions.iso3) WHERE ((population.iso2 IS NOT NULL) and (year = 1976))) LIMIT 5)"))
  (is (equal
       (sql (:limit
             (:select (:as 'population.country 'country-name)
                      (:as 'population 'country-population)
                      'region-name
                      (:as (:over (:sum 'population)
                                  (:partition-by 'region-name :order-by 'region-name
                                   :range-between :unbounded-preceding :current-row))
                           'regional-population)
                      :from 'population
                      :inner-join 'regions
                      :on (:= 'population.iso3 'regions.iso3)
                      :where (:and (:not-null 'population.iso2)
                                   (:= 'year 1976)))
             5))
       "((SELECT population.country AS country_name, population AS country_population, region_name, (SUM(population) OVER (PARTITION BY region_name ORDER BY region_name RANGE BETWEEN UNBOUNDED PRECEDING AND  CURRENT ROW )) AS regional_population FROM population INNER JOIN regions ON (population.iso3 = regions.iso3) WHERE ((population.iso2 IS NOT NULL) and (year = 1976))) LIMIT 5)"))
  (is (equal
       (sql
        (:limit
         (:select (:as 'population.country 'country-name)
                  (:as 'population 'country-population)
                  'region-name
                  (:as (:over (:sum 'population)
                              (:partition-by 'region-name :order-by 'region-name
                               :rows-between :unbounded-preceding :current-row))
                       'regional-population)
                  :from 'population
                  :inner-join 'regions
                  :on (:= 'population.iso3 'regions.iso3)
                  :where (:and (:not-null 'population.iso2)
                               (:= 'year 1976)))
         5))
       "((SELECT population.country AS country_name, population AS country_population, region_name, (SUM(population) OVER (PARTITION BY region_name ORDER BY region_name ROWS BETWEEN UNBOUNDED PRECEDING AND  CURRENT ROW )) AS regional_population FROM population INNER JOIN regions ON (population.iso3 = regions.iso3) WHERE ((population.iso2 IS NOT NULL) and (year = 1976))) LIMIT 5)")))

(test with-recursive
  "Testing with recursive. When working with recursive queries it is important to be sure that the recursive part of the query will eventually return no tuples, or else the query will loop indefinitely. Sometimes, using UNION instead of UNION ALL can accomplish this by discarding rows that duplicate previous output rows. However, often a cycle does not involve output rows that are completely duplicate: it may be necessary to check just one or a few fields to see if the same point has been reached before. The standard method for handling such situations is to compute an array of the already-visited values."

  (is (equal (with-test-connection
               (query
                (:with-recursive
                    (:as (:t1 'n)
                         (:union-all (:values (:set 1))
                                     (:select (:+ 'n 1)
                                      :from 't1
                                      :where (:< 'n 100))))
                  (:select (:sum 'n) :from 't1))
                :single))
             5050))

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
                            (:select 1 'employee-name 'manager-name
                             :from 'employee
                             :where (:= 'manager-name "Mary"))
                            (:select (:+ 'er.distance 1) 'e.employee-name 'e.manager-name
                             :from (:as 'employee-recursive 'er) (:as 'employee 'e)
                                     :where (:= 'er.employee-name 'e.manager-name))))
                    (:select 'distance 'employee-name :from 'employee-recursive)))
             "WITH RECURSIVE employee_recursive(distance, employee_name, manager_name) AS ((SELECT 1, employee_name, manager_name FROM employee WHERE (manager_name = E'Mary')) union all (SELECT (er.distance + 1), e.employee_name, e.manager_name FROM employee_recursive AS er, employee AS e WHERE (er.employee_name = e.manager_name)))(SELECT distance, employee_name FROM employee_recursive)")))

(test ordinality
  (with-test-connection
    (when (pomo:table-exists-p 'pets)
      (execute (:drop-table 'pets :cascade)))
    (query "CREATE TABLE pets(pet varchar(100) PRIMARY KEY, tags text[])")
    (query "INSERT INTO pets(pet, tags)
    VALUES ('dog', '{big, furry, friendly, eats steak}'::text[]),
        ('cat', '{small, snob, eats greenbeans, plays with mouse}'::text[]),
        ('mouse', '{very small, fits in pocket, eat peanuts, watches cat}'::text[]),
        ('fish', NULL);")

    (is (equal (sql (:select '*
                     :from (:generate-series 4 1 -1)
                     :with-ordinality-as (:t1 'x 'y)))
               "(SELECT * FROM generate_series(4, 1, -1) WITH ORDINALITY AS t1(x, y))"))
    (is (equal (sql (:select '*
                     :from (:generate-series 4 1 -1)
                     :with-ordinality))
               "(SELECT * FROM generate_series(4, 1, -1) WITH ORDINALITY )"))
    (is (equal (sql (:select '*
                     :from (:json-object-keys "{\"a1\":\"1\",\"a2\":\"2\",\"a3\":\"3\"}")
                     :with-ordinality-as (:t1 'keys 'n)))
               "(SELECT * FROM json_object_keys(E'{\"a1\":\"1\",\"a2\":\"2\",\"a3\":\"3\"}') WITH ORDINALITY AS t1(keys, n))"))

    (is (equal
         (sql (:limit (:select '* :from 'pets (:unnest 'tags) :with-ordinality) 3))
         "((SELECT * FROM pets, unnest(tags) WITH ORDINALITY ) LIMIT 3)"))

    (is (equal (sql (:select 'pet 'sort-order 'tag
                     :from 'pets
                     :left-join-lateral (:unnest 'tags)
                     :with-ordinality-as (:f 'tag 'sort-order)
                     :on (:= 1 1)))
               "(SELECT pet, sort_order, tag FROM pets LEFT JOIN LATERAL unnest(tags) WITH ORDINALITY AS f(tag, sort_order) ON (1 = 1))"))

    (is (equal (sql (:select 't1.id 'a.elem 'a.nr
                     :from (:as 't12 't1)
                     :left-join (:unnest (:string-to-array 't1.elements ","))
                     :with-ordinality
                     :on (:= 't1.id 'a.id)
                     :left-join 't14
                     :on (:= 't2.id 'a.id)))
               "(SELECT t1.id, a.elem, a.nr FROM t12 AS t1 LEFT JOIN unnest(string_to_array(t1.elements, E',')) WITH ORDINALITY  ON (t1.id = a.id) LEFT JOIN t14 ON (t2.id = a.id))"))

    (is (equal (sql (:select 't1.id 'a.elem 'a.nr
                     :from (:as 't12 't1)
                     :left-join (:unnest (:string-to-array 't1.elements ","))
                     :with-ordinality-as (:a 'elem 'nr)
                     :on (:= 1 1)))
               "(SELECT t1.id, a.elem, a.nr FROM t12 AS t1 LEFT JOIN unnest(string_to_array(t1.elements, E',')) WITH ORDINALITY AS a(elem, nr) ON (1 = 1))"))
    (is (equal (sql (:select 't1.id 'a.elem 'a.nr
                     :from (:as 't12 't1)
                     :left-join (:unnest (:string-to-array 't1.elements ","))
                     :with-ordinality-as (:a 'elem 'nr)
                     :on 't))
               "(SELECT t1.id, a.elem, a.nr FROM t12 AS t1 LEFT JOIN unnest(string_to_array(t1.elements, E',')) WITH ORDINALITY AS a(elem, nr) ON true)"))

    (is (equal (sql (:select 'pet 'ordinality 'tag
                     :from 'pets
                     :left-join-lateral (:unnest 'tags)
                     :with-ordinality
                     :on (:= 1 1)))
               "(SELECT pet, ordinality, tag FROM pets LEFT JOIN LATERAL unnest(tags) WITH ORDINALITY  ON (1 = 1))"))

    (is (equal (query (:select 'pet 'sort-order 'tag
                       :from 'pets
                       :left-join-lateral (:unnest 'tags)
                       :with-ordinality-as (:f 'tag 'sort-order)
                       :on (:= 1 1)))
               '(("dog" 1 "big") ("dog" 2 "furry") ("dog" 3 "friendly") ("dog" 4 "eats steak")
                 ("cat" 1 "small") ("cat" 2 "snob") ("cat" 3 "eats greenbeans")
                 ("cat" 4 "plays with mouse") ("mouse" 1 "very small")
                 ("mouse" 2 "fits in pocket") ("mouse" 3 "eat peanuts")
                 ("mouse" 4 "watches cat") ("fish" :NULL :NULL))))

    (is (equal (query (:select 'pet 'sort-order 'tag
                       :from 'pets (:unnest 'tags)
                       :with-ordinality-as (:f 'tag 'sort-order)))
               '(("dog" 1 "big") ("dog" 2 "furry") ("dog" 3 "friendly") ("dog" 4 "eats steak")
                 ("cat" 1 "small") ("cat" 2 "snob") ("cat" 3 "eats greenbeans")
                 ("cat" 4 "plays with mouse") ("mouse" 1 "very small")
                 ("mouse" 2 "fits in pocket") ("mouse" 3 "eat peanuts")
                 ("mouse" 4 "watches cat"))))))

(test lateral
  (is
   (equal
    (sql
     (:select 'x 'y
              :from (:as (:values (:set 1) (:set 2) (:set 3))
                         (:t1 'x))
              :lateral (:generate-series 0 't.x) (:u 'y)))
    "(SELECT x, y FROM (VALUES (1), (2), (3)) AS t1(x) , LATERAL generate_series(0, t.x), u(y))"))
  (is
   (equal
    (sql
     (:select '*
      :from (:as 'tags 't1)
      :join-lateral (:as
                     (:fetch
                      (:order-by
                       (:select 'm.*
                        :from (:as 'movies 'm)
                        :where (:= 'm.tag-id 't1.id))
                       (:desc 'm.created-at))
                      2)
                     'e1)
      :on (:= 1 1)))
    "(SELECT * FROM tags AS t1 JOIN LATERAL (((SELECT m.* FROM movies AS m WHERE (m.tag_id = t1.id)) ORDER BY m.created_at DESC) FETCH FIRST 2 ROWS ONLY) AS e1 ON (1 = 1))"))
  (is (equal
       (sql
        (:select '*
         :from (:as 'tags 't1)
         :inner-join-lateral (:as
                              (:fetch
                               (:order-by
                                (:select 'm.*
                                 :from (:as 'movies 'm)
                                 :where (:= 'm.tag-id 't1.id))
                                (:desc 'm.created-at))
                               2)
                              'e1)
         :on 't))
       "(SELECT * FROM tags AS t1 INNER JOIN LATERAL (((SELECT m.* FROM movies AS m WHERE (m.tag_id = t1.id)) ORDER BY m.created_at DESC) FETCH FIRST 2 ROWS ONLY) AS e1 ON true)"))
  (is (equal
       (sql
        (:select '*
         :from (:as 'tags 't1)
         :lateral (:as
                   (:fetch
                    (:order-by
                     (:select 'm.*
                      :from (:as 'movies 'm)
                      :where (:= 'm.tag-id 't1.id))
                     (:desc 'm.created-at))
                    2)
                   'e1)))
       "(SELECT * FROM tags AS t1 , LATERAL (((SELECT m.* FROM movies AS m WHERE (m.tag_id = t1.id)) ORDER BY m.created_at DESC) FETCH FIRST 2 ROWS ONLY) AS e1)"))
  (is
   (equal
    (sql
     (:select '*
      :from (:as 'tags 't1)
      :cross-join-lateral (:as
                           (:fetch
                            (:order-by
                             (:select 'm.*
                              :from (:as 'movies 'm)
                              :where (:= 'm.tag-id 't1.id))
                             (:desc 'm.created-at))
                            2)
                           'e1)))
    "(SELECT * FROM tags AS t1 CROSS JOIN LATERAL (((SELECT m.* FROM movies AS m WHERE (m.tag_id = t1.id)) ORDER BY m.created_at DESC) FETCH FIRST 2 ROWS ONLY) AS e1)"))
  (is
   (equal
    (sql
     (:select 'p.* (:as 'dads.id 'dad-id) (:as 'moms.id 'mom-id)
      :from (:as 'people 'p)
      :left-join-lateral (:as (:select '*
                               :from 'people
                               :where (:and (:= 'gender "m")
                                            (:= 'surname-1 'p.surname-1)
                                            (:<> 'pack-id 'p.pack-id)))
                              'dads)
      :on 't
      :left-join-lateral (:as (:select '*
                               :from 'people
                               :where (:and (:= 'gender "f")
                                            (:= 'surname-1 'p.surname-2)
                                            (:<> 'pack-id 'p.pack-id)
                                            (:<> 'pack-id 'dads.pack-id)))
                              'moms)
      :on 't))
    "(SELECT p.*, dads.id AS dad_id, moms.id AS mom_id FROM people AS p LEFT JOIN LATERAL (SELECT * FROM people WHERE ((gender = E'm') and (surname_1 = p.surname_1) and (pack_id <> p.pack_id))) AS dads ON true LEFT JOIN LATERAL (SELECT * FROM people WHERE ((gender = E'f') and (surname_1 = p.surname_2) and (pack_id <> p.pack_id) and (pack_id <> dads.pack_id))) AS moms ON true)"))
  (is
   (equal
    (sql
     (:select 'geo.zipcode 'geo.state 'movie.name
      :from 'geo
      :cross-join-lateral
      (:as
       (:limit
        (:order-by
         (:select 'movie-name
          :from 'streams
          :where (:= 'geo.zipcode 'streams.zipcode))
         (:desc 'streams.country))
        5)
       (:movie 'name))))
    "(SELECT geo.zipcode, geo.state, movie.name FROM geo CROSS JOIN LATERAL (((SELECT movie_name FROM streams WHERE (geo.zipcode = streams.zipcode)) ORDER BY streams.country DESC) LIMIT 5) AS movie(name))"))
  ;; the following borrowed from
  ;; https://popsql.com/learn-sql/postgresql/how-to-use-lateral-joins-in-postgresql#data-set
  (is
   (equal
    (sql
     (:select 'pledged-usd 'avg-pledge-usd 'amt-from-goal 'duration
              (:as (:/ 'usd-from-goal 'duration) 'usd-needed-daily)
              :from 'kickstarter-data
              :lateral (:as (:select (:as (:/ 'pledged 'fx-rate)
                                          'pledged-usd))
                            'pu)
              :lateral (:as (:select (:as (:/ 'pledged-usd 'backers-count)
                                          'avg-pledge-usd))
                            'apu)
              :lateral (:as (:select (:as (:/ 'goal 'fx-rate)
                                          'goal-usd))
                            'gu)
              :lateral (:as (:select (:as (:- 'goal-usd 'pledged-usd)
                                          'usd-from-goal))
                            'ufg)
              :lateral (:as (:select (:as (:/ (:- 'deadline 'launched-at) 86400.00)
                                          'duration))
                            'dr)))
    "(SELECT pledged_usd, avg_pledge_usd, amt_from_goal, duration, (usd_from_goal / duration) AS usd_needed_daily FROM kickstarter_data , LATERAL (SELECT (pledged / fx_rate) AS pledged_usd) AS pu , LATERAL (SELECT (pledged_usd / backers_count) AS avg_pledge_usd) AS apu , LATERAL (SELECT (goal / fx_rate) AS goal_usd) AS gu , LATERAL (SELECT (goal_usd - pledged_usd) AS usd_from_goal) AS ufg , LATERAL (SELECT ((deadline - launched_at) / 86400.0) AS duration) AS dr)")))

(test insert-into
  "Testing insert into"
  (is (equal (sql (:insert-into 'cd.facilities
                   :set 'facid 9 'name "Spa" 'membercost 20 'guestcost 30
                   'initialoutlay 100000 'monthlymaintenance 800))
             "INSERT INTO cd.facilities (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance)  VALUES (9, E'Spa', 20, 30, 100000, 800)"))

  ;; Testing with a calculation in the value
  (is (equal (sql (:insert-into 'test
                   :set 'id 15 'number-string "12" 'numeric-item 12.45
                   'ratio-item (/ 1 13) 'created-at "2018-02-01"))
             "INSERT INTO test (id, number_string, numeric_item, ratio_item, created_at)  VALUES (15, E'12', 12.45, 0.0769230769230769230769230769230769230, E'2018-02-01')"))

  ;; Testing select
  (is (equal
       (sql (:insert-into 'users
                          (:select (:uuid-generate-v4) "Lucie"
                                   "Hawkins""Lucie-Jones@gmail.com")))
             "INSERT INTO users (SELECT uuid_generate_v4(), E'Lucie', E'Hawkins', E'Lucie-Jones@gmail.com')"))
  (is (equal (sql (:insert-into 't11
                   :columns 'region 'subregion 'country
                   (:select (:as 'region-name 'region)
                            (:as 'sub-region-name 'subregion)
                            'country
                            :from 'regions)))
             "INSERT INTO t11  (region, subregion, country) (SELECT region_name AS region, sub_region_name AS subregion, country FROM regions)"))
  (is (equal (sql (:insert-into 't6 (:select 'id :from 't5)))
             "INSERT INTO t6 (SELECT id FROM t5)"))
  ;; Testing select in insert statement
  ;; From https://www.pgexercises.com/questions/updates/insert3.html
  (is (equal (sql (:insert-into 'cd.facilities
                   :set 'facid
                   (:select (:+ (:select (:max 'facid)
                                 :from 'cd.facilities)
                                1))
                   'name "Spa" 'membercost 20 'guestcost 30
                   'initialoutlay 100000 'monthlymaintenance 800))
             "INSERT INTO cd.facilities (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance)  VALUES ((SELECT ((SELECT MAX(facid) FROM cd.facilities) + 1)), E'Spa', 20, 30, 100000, 800)"))
  ;; Testing overriding-user-value
  (is (equal (sql (:insert-into 'employee
                   :set 'id 1 'name "Paul"
                   :overriding-user-value
                   :on-conflict-do-nothing))
             "INSERT INTO employee (id, name)  OVERRIDING USER VALUE  VALUES (1, E'Paul') ON CONFLICT DO NOTHING"))
  ;; Testing overriding system-value
  (is (equal (sql (:insert-into 'employee
                   :set 'id 1 'name "Paul"
                   :overriding-system-value
                   :on-conflict-do-nothing))
             "INSERT INTO employee (id, name)  OVERRIDING SYSTEM VALUE  VALUES (1, E'Paul') ON CONFLICT DO NOTHING")))

(test insert-into-on-conflict-do-nothing
  (is (equal (sql (:insert-into 'distributors
                   :set 'did 7 'dname "Readline GmbH"
                   :on-conflict-do-nothing))
             "INSERT INTO distributors (did, dname)  VALUES (7, E'Readline GmbH') ON CONFLICT DO NOTHING"))
  ;; basic :on-conflict with separate :do-nothing keyword
  (is (equal (sql (:insert-into 'test
                   :set 'some-col "a" 'some-val 5
                   :on-conflict 'some-col
                   :do-nothing))
             "INSERT INTO test (some_col, some_val)  VALUES (E'a', 5) ON CONFLICT (some_col) DO NOTHING "))
  ;; with where condition
  (is (equal (sql (:insert-into 'distributors
                   :set 'did 10 'dname "Conrad International"
                   :on-conflict 'did
                   :do-nothing
                   :where 'is-active))
             "INSERT INTO distributors (did, dname)  VALUES (10, E'Conrad International') ON CONFLICT (did) WHERE is_active DO NOTHING "))
  ;; With returning
  (is (equal (sql (:insert-into 'distributors
                   :set 'did 8 'dname "Readline GmbH"
                   :on-conflict 'did 'dname
                   :do-nothing
                   :returning 'id))
             "INSERT INTO distributors (did, dname)  VALUES (8, E'Readline GmbH') ON CONFLICT (did, dname) DO NOTHING  RETURNING id"))
  ;; with where and returning
  (is (equal (sql (:insert-into 'test-table
                   :set 'column-A '$1 'column-B '$2
                   :on-conflict 'column-A
                   :do-nothing
                   :where (:= 'test-table.column-A '$1)
                   :returning '*))
             "INSERT INTO test_table (column_a, column_b)  VALUES ($1, $2) ON CONFLICT (column_a) WHERE (test_table.column_a = $1) DO NOTHING  RETURNING *"))
  ;; With on-conflict-on-constraint and do-nothing as a separate operator
  (is (equal (sql (:insert-into 'distributors
                   :set 'did 10 'dname "Readline GmbH"
                   :on-conflict-on-constraint 'distributors-pkey
                   :do-nothing
                   :returning 'id))
             "INSERT INTO distributors (did, dname)  VALUES (10, E'Readline GmbH') ON CONFLICT ON CONSTRAINT distributors_pkey DO NOTHING  RETURNING id"))
  ;; basic :on-conflict with separate :do-nothing keyword and returning
  (is (equal (sql (:insert-into 'test
                   :set 'some-key "a" 'some-val 4
                   :on-conflict 'some-key
                   :do-nothing
                   :returning '*))
             "INSERT INTO test (some_key, some_val)  VALUES (E'a', 4) ON CONFLICT (some_key) DO NOTHING  RETURNING *")))

(test insert-into-on-conflict-update
  ;; Testing On Conflict update
  (is (equal (sql (:insert-into 'test-table
                   :set 'column-A '$1 'column-B '$2
                   :on-conflict-update 'column-A
                   :update-set 'column-B '$2
                   :where (:= 'test-table.column-A '$1)))
             "INSERT INTO test_table (column_a, column_b)  VALUES ($1, $2) ON CONFLICT (column_a) DO UPDATE SET column_b = $2 WHERE (test_table.column_a = $1)"))
  ;; basic version single row
  (is (equal
       (sql (:insert-into 'users
                          (:select (:uuid-generate-v4) "Lucie" "Hawkins" "Lucie-Jones@gmail.com")
                          :on-conflict-update 'email
                          :update-set 'first-name 'excluded.first-name 'last-name 'excluded.last-name))
             "INSERT INTO users (SELECT uuid_generate_v4(), E'Lucie', E'Hawkins', E'Lucie-Jones@gmail.com') ON CONFLICT (email) DO UPDATE SET first_name = excluded.first_name, last_name = excluded.last_name"))
  ;; Basic version multiple row and specified columns
  (is (equal (sql (:insert-into 'distributors
                   :set 'did 5 'dname "Gizmo Transglobal"
                   :on-conflict-update 'did
                   :update-set 'dname 'excluded.dname))
             "INSERT INTO distributors (did, dname)  VALUES (5, E'Gizmo Transglobal') ON CONFLICT (did) DO UPDATE SET dname = excluded.dname"))
  ;; with where clause
  (is (equal
       (sql (:insert-into 'users
                          (:select (:uuid-generate-v4) "Lucie" "Hawkins" "Lucie-Jones@gmail.com")
                          :on-conflict-update 'email
                          :update-set 'first-name 'excluded.first-name 'last-name 'excluded.last-name
             :where (:<> 'u.first-name "Lucie")))
             "INSERT INTO users (SELECT uuid_generate_v4(), E'Lucie', E'Hawkins', E'Lucie-Jones@gmail.com') ON CONFLICT (email) DO UPDATE SET first_name = excluded.first_name, last_name = excluded.last_name WHERE (u.first_name <> E'Lucie')"))
  ;; with concatenation function in the update-set clause
  (is (equal
       (sql (:insert-into 'distributors
             :set 'did 8 'dname "Anvil Distribution"
             :on-conflict-update 'did
             :update-set 'dname (:|| 'excluded.dname  " (formerly " 'd.dname ")")
             :where (:<> 'd.zipcode "21201")))
             "INSERT INTO distributors (did, dname)  VALUES (8, E'Anvil Distribution') ON CONFLICT (did) DO UPDATE SET dname = (excluded.dname || E' (formerly ' || d.dname || E')') WHERE (d.zipcode <> E'21201')"))
  ;; with on-conflict-on-constraint
  (is (equal (sql (:insert-into 'test
                   :set 'some-key "a" 'some-val 5
                   :on-conflict-on-constraint 'somekey
                   :update-set 'some-val 'excluded.some-val))
             "INSERT INTO test (some_key, some_val)  VALUES (E'a', 5) ON CONFLICT ON CONSTRAINT somekey DO UPDATE SET some_val = excluded.some_val"))
  ;; with on-conflict-on-constraint and returning clause
  (is (equal (sql (:insert-into 'test
                   :set 'some-key "a" 'some-val 2
                   :on-conflict-on-constraint 'somekey
                   :update-set 'some-val 'excluded.some-val
                   :returning '*))
             "INSERT INTO test (some_key, some_val)  VALUES (E'a', 2) ON CONFLICT ON CONSTRAINT somekey DO UPDATE SET some_val = excluded.some_val RETURNING *"))
  ;; on-conflict-on-constraint with addition function in the update-set clause
  (is (equal (sql (:insert-into 'test
                   :set 'some-key "a"
                   :on-conflict-on-constraint 'somekey
                   :update-set 'some-val (:+ 'test.some-val 1)))
             "INSERT INTO test (some_key)  VALUES (E'a') ON CONFLICT ON CONSTRAINT somekey DO UPDATE SET some_val = (test.some_val + 1)"))
  ;; with select clause which returns a single row
  (is (equal (sql (:insert-into 'attendence
                   :set 'event-id (:select 'id
                                   :from 'event
                                   :where (:= (:lower 'event-dt) "2020-01-11 17:00:00"))
                   'client-id 3 'attend-status "No Show"
                   :on-conflict-on-constraint 'attendance-pkey
                   :update-set 'attend-status 'excluded.attend_status))
             "INSERT INTO attendence (event_id, client_id, attend_status)  VALUES ((SELECT id FROM event WHERE (lower(event_dt) = E'2020-01-11 17:00:00')), 3, E'No Show') ON CONFLICT ON CONSTRAINT attendance_pkey DO UPDATE SET attend_status = excluded.attend_status")))

(test insert-rows-into
  ;; Testing basic inserting-rows-into
  (is (equal (sql (:insert-rows-into 'my-table :values '((42 "foobar") (23 "foobaz"))))
             "INSERT INTO my_table VALUES (42, E'foobar'), (23, E'foobaz')"))

  ;; Testing columns
  ;; From https://www.pgexercises.com/questions/updates/insert2.html
  (is (equal (sql (:insert-rows-into 'cd.facilities
                   :columns 'facid 'name 'membercost 'guestcost 'initialoutlay 'monthlymaintenance
                   :values '((9 "Spa" 20 30 100000 800) (10 "Squash Court 2" 3.5 17.5 5000 80))))
             "INSERT INTO cd.facilities (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance) VALUES (9, E'Spa', 20, 30, 100000, 800), (10, E'Squash Court 2', 3.5, 17.5, 5000, 80)"))
  ;; Testing select in values in insert-rows-into
  (is (equal (sql (:insert-rows-into 't6
                   :columns 'tags
                   :values '(((:select 'id
                               :from 't5)))))
             "INSERT INTO t6 (tags) VALUES ((SELECT id FROM t5))"))
  ;; Now using rows https://www.pgexercises.com/questions/updates/insert3.html
  (is (equal (sql (:insert-rows-into 'cd.facilities
                   :columns 'facid 'name 'membercost 'guestcost 'initialoutlay 'monthlymaintenance
                   :values '(((:select (:+ (:select (:max 'facid)
                                            :from 'cd.facilities)
                                        1))
                              "Spa" 20 30 100000 800 ))))
             "INSERT INTO cd.facilities (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance) VALUES ((SELECT ((SELECT MAX(facid) FROM cd.facilities) + 1)), E'Spa', 20, 30, 100000, 800)"))
  (is (equal (sql (:insert-rows-into 'table1
                   :columns 'c1 'c2
                   :overriding-system-value
                   :values '((1 "a") (2 "b"))))
             "INSERT INTO table1 (c1, c2)  OVERRIDING SYSTEM VALUE VALUES (1, E'a'), (2, E'b')")))

(test insert-rows-into-on-conflict-do-nothing
  ;; Testing inserting rows with on conflict do nothing
  (is (equal (sql (:insert-rows-into 'distributors
                   :columns 'did 'dname
                   :values '((7 "Readline GmbH"))
                   :on-conflict-do-nothing))
             "INSERT INTO distributors (did, dname) VALUES (7, E'Readline GmbH') ON CONFLICT  DO NOTHING"))
  ;; basic :on-conflict with separate :do-nothing keyword
  (is (equal (sql (:insert-rows-into 'test :columns 'some-key 'some-val
                                     :values '(("a" 5) ("b" 6) ("c" 7))
                                     :on-conflict 'some-key
                                     :do-nothing))
             "INSERT INTO test (some_key, some_val) VALUES (E'a', 5), (E'b', 6), (E'c', 7) ON CONFLICT (some_key) DO NOTHING "))
  ;; With where condition
  (is (equal (sql (:insert-rows-into 'distributors
                   :columns 'did 'dname
                   :values '((10 "Conrad International"))
                   :on-conflict 'did
                   :do-nothing
                   :where 'is-active))
             "INSERT INTO distributors (did, dname) VALUES (10, E'Conrad International') ON CONFLICT (did) WHERE is_active DO NOTHING "))
  ;; With returning
  (is (equal (sql (:insert-rows-into 'distributors :columns 'did 'dname
                                     :values '((8 "Readline GmbH"))
                                     :on-conflict 'did 'dname
                                     :do-nothing
                                     :returning 'id))
             "INSERT INTO distributors (did, dname) VALUES (8, E'Readline GmbH') ON CONFLICT (did, dname) DO NOTHING  RETURNING id"))

  ;; With on-conflict-on-constraint and do-nothing as a separate operator
  (is (equal (sql (:insert-rows-into 'distributors :columns 'did 'dname
                                     :values '((10 "Readline GmbH"))
                                     :on-conflict-on-constraint 'distributors-pkey
                                     :do-nothing
                                     :returning 'id))
             "INSERT INTO distributors (did, dname) VALUES (10, E'Readline GmbH') ON CONFLICT ON CONSTRAINT distributors_pkey DO NOTHING  RETURNING id"))
  ;; basic :on-conflict with separate :do-nothing keyword and returning
  (is (equal (sql (:insert-rows-into 'test :columns 'some-key 'some-val
                                     :values '(("a" 4) ("b" 6) ("c" 7))
                                     :on-conflict 'some-key
                                     :do-nothing
                                     :returning '*))
             "INSERT INTO test (some_key, some_val) VALUES (E'a', 4), (E'b', 6), (E'c', 7) ON CONFLICT (some_key) DO NOTHING  RETURNING *"))
  ;; multiple values basic :on-conflict-on-constraint with separate :do-nothing keyword and returning
  (is (equal (sql (:insert-rows-into 'test :columns 'some-key 'some-val
                                     :values '(("a" 3) ("b" 6) ("c" 7))
                                     :on-conflict-on-constraint 'somekey
                                     :do-nothing
                                     :returning '*))
             "INSERT INTO test (some_key, some_val) VALUES (E'a', 3), (E'b', 6), (E'c', 7) ON CONFLICT ON CONSTRAINT somekey DO NOTHING  RETURNING *")))

(test insert-rows-into-on-conflict-update
  ;; Testing inserting rows with on conflict update
  ;; basic version single row
  (is (equal (sql (:insert-rows-into 'users
                   :values '(((:uuid-generate-v4) "Lucie" "Hawkins" "Lucie-Jones@gmail.com"))
                   :on-conflict-update 'email
                   :update-set 'first-name 'excluded.first-name 'last-name 'excluded.last-name))
             "INSERT INTO users VALUES (uuid_generate_v4(), E'Lucie', E'Hawkins', E'Lucie-Jones@gmail.com') ON CONFLICT (email) DO UPDATE SET first_name = excluded.first_name, last_name = excluded.last_name"))
  ;; Basic version multiple row and specified columns
  (is (equal (sql (:insert-rows-into 'distributors
                   :columns 'did 'dname
                   :values '((5 "Gizmo Transglobal") (6 "Associated Computing Inc."))
                   :on-conflict-update 'did
                   :update-set 'dname 'excluded.dname))
             "INSERT INTO distributors (did, dname) VALUES (5, E'Gizmo Transglobal'), (6, E'Associated Computing Inc.') ON CONFLICT (did) DO UPDATE SET dname = excluded.dname"))
  ;; with where clause
  (is (equal (sql (:insert-rows-into 'users
                   :values '(((:uuid-generate-v4) "Lucie" "Hawkins" "Lucie-Jones@gmail.com"))
                   :on-conflict-update 'email
                   :update-set 'first-name 'excluded.first-name 'last-name 'excluded.last-name
                   :where (:<> 'u.first-name "Lucie")))
             "INSERT INTO users VALUES (uuid_generate_v4(), E'Lucie', E'Hawkins', E'Lucie-Jones@gmail.com') ON CONFLICT (email) DO UPDATE SET first_name = excluded.first_name, last_name = excluded.last_name WHERE (u.first_name <> E'Lucie')"))
  ;; with an as clause at the table level
  (is (equal (sql (:insert-rows-into (:as 'users 'u)
                   :values '(((:uuid-generate-v4) "Lucie" "Jones" "Lucie-Jones@gmail.com"))
                   :on-conflict-update 'email
                   :update-set 'first-name 'excluded.first-name 'last-name 'excluded.last-name
                   :where (:<> 'u.first-name "Lucie")))
             "INSERT INTO users AS u VALUES (uuid_generate_v4(), E'Lucie', E'Jones', E'Lucie-Jones@gmail.com') ON CONFLICT (email) DO UPDATE SET first_name = excluded.first_name, last_name = excluded.last_name WHERE (u.first_name <> E'Lucie')"))
  ;; with concatenation function in the update-set clause
  (is (equal (sql (:insert-rows-into (:as 'distributors 'd)
                   :columns 'did 'dname
                   :values '((8 "Anvil Distribution"))
                   :on-conflict-update 'did
                   :update-set 'dname (:|| 'excluded.dname  " (formerly " 'd.dname ")")
                   :where (:<> 'd.zipcode "21201")))
             "INSERT INTO distributors AS d (did, dname) VALUES (8, E'Anvil Distribution') ON CONFLICT (did) DO UPDATE SET dname = (excluded.dname || E' (formerly ' || d.dname || E')') WHERE (d.zipcode <> E'21201')"))
  ;; with on-conflict-on-constraint
  (is (equal (sql (:insert-rows-into 'test
                   :columns 'some-key 'some-val
                   :values '(("a" 5))
                   :on-conflict-on-constraint 'somekey
                   :update-set 'some-val 'excluded.some-val))
             "INSERT INTO test (some_key, some_val) VALUES (E'a', 5) ON CONFLICT ON CONSTRAINT somekey DO UPDATE SET some_val = excluded.some_val"))
  ;; with on-conflict-on-constraint and returning clause
  (is (equal (sql (:insert-rows-into 'test
                   :columns 'some-key 'some-val
                   :values '(("a" 2) ("b" 6) ("c" 7))
                   :on-conflict-on-constraint 'somekey
                   :update-set 'some-val 'excluded.some-val
                   :returning '*))
             "INSERT INTO test (some_key, some_val) VALUES (E'a', 2), (E'b', 6), (E'c', 7) ON CONFLICT ON CONSTRAINT somekey DO UPDATE SET some_val = excluded.some_val RETURNING *"))
  ;; on-conflict-on-constraint with addition function in the update-set clause
  (is (equal (sql (:insert-rows-into 'test
                   :columns 'some-key
                   :values '(("a"))
                   :on-conflict-on-constraint 'somekey
                   :update-set 'some-val (:+ 'test.some-val 1)))
             "INSERT INTO test (some_key) VALUES (E'a') ON CONFLICT ON CONSTRAINT somekey DO UPDATE SET some_val = (test.some_val + 1)"))
  ;; with select clause which returns a single row
  (is (equal (sql (:insert-rows-into 'attendence :columns 'event-id 'client-id 'attend-status
                   :values '(((:select 'id
                               :from 'event
                               :where (:= (:lower 'event-dt) "2020-01-11 17:00:00"))
                              3
                              "No Show"))
                   :on-conflict-on-constraint 'attendance-pkey
                   :update-set 'attend-status 'excluded.attend_status))
             "INSERT INTO attendence (event_id, client_id, attend_status) VALUES ((SELECT id FROM event WHERE (lower(event_dt) = E'2020-01-11 17:00:00')), 3, E'No Show') ON CONFLICT ON CONSTRAINT attendance_pkey DO UPDATE SET attend_status = excluded.attend_status")))

(test update
  "Testing updates"
  ;; From Postgresql documentation https://www.postgresql.org/docs/current/sql-update.html
  (is (equal (sql (:update 'films :set 'kind "Dramatic" :where (:= 'kind "Drama")))
             "UPDATE films SET kind = E'Dramatic' WHERE (kind = E'Drama')"))
  (is (equal (sql (:update 'weather
                   :set 'temp-lo (:+ 'temp-lo 1) 'temp-hi (:+ 'temp-lo 15) 'prcp :default
                   :where (:and (:= 'city "San Francisco")
                                (:= 'date "2003-07-03"))))
             "UPDATE weather SET temp_lo = (temp_lo + 1), temp_hi = (temp_lo + 15), prcp =  DEFAULT  WHERE ((city = E'San Francisco') and (date = E'2003-07-03'))"))
  (is (equal (sql (:update 'weather
                   :set 'temp-lo (:+ 'temp-lo 1) 'temp-hi (:+ 'temp-lo 15) 'prcp :default
                   :where (:and (:= 'city "San Francisco") (:= 'date "2003-07-03"))
                   :returning 'temp-lo 'temp-hi 'prcp))
             "UPDATE weather SET temp_lo = (temp_lo + 1), temp_hi = (temp_lo + 15), prcp =  DEFAULT  WHERE ((city = E'San Francisco') and (date = E'2003-07-03')) RETURNING temp_lo, temp_hi, prcp"))
  (is (equal (sql (:update 'weather
                   :columns 'temp-lo 'temp-hi 'prcp
                   (:set (:+ 'temp-lo 1)  (:+ 'temp-lo 15) :DEFAULT)
                   :where (:and (:= 'city "San Francisco")
                                (:= 'date "2003-07-03"))))
             "UPDATE weather SET  (temp_lo, temp_hi, prcp) = ((temp_lo + 1), (temp_lo + 15),  DEFAULT ) WHERE ((city = E'San Francisco') and (date = E'2003-07-03'))"))
  (is (equal (sql (:update'employees :set 'sales-count (:+ 'sales-count 1)
                   :from 'accounts
                   :where (:and (:= 'accounts.name "Acme Corporation")
                                (:= 'employees.id 'accounts.sales-person))))
             "UPDATE employees SET sales_count = (sales_count + 1) FROM accounts WHERE ((accounts.name = E'Acme Corporation') and (employees.id = accounts.sales_person))"))
  (is (equal (sql (:update 'employees :set 'sales-count (:+ 'sales-count 1)
                           :where (:= 'id (:select 'sales-person
                                           :from 'accounts
                                           :where (:= 'name "Acme Corporation")))))
             "UPDATE employees SET sales_count = (sales_count + 1) WHERE (id = (SELECT sales_person FROM accounts WHERE (name = E'Acme Corporation')))"))
  (is (equal (sql (:update 't1 :columns 'database-name 'encoding
                           (:select 'x.datname 'x.encoding
                                    :from (:as 'pg-database 'x)
                                    :where (:= 'x.oid 't1.oid))))
             "UPDATE t1 SET  (database_name, encoding) = (SELECT x.datname, x.encoding FROM pg_database AS x WHERE (x.oid = t1.oid))"))
  (is (equal (sql (:update 'accounts
                   :columns 'contact-first-name 'contact-last-name
                   (:select 'first-name 'last-name
                            :from 'salesmen
                            :where (:= 'salesman.id 'accounts.sales-id))))
             "UPDATE accounts SET  (contact_first_name, contact_last_name) = (SELECT first_name, last_name FROM salesmen WHERE (salesman.id = accounts.sales_id))"))
  (is (equal (sql (:update 'accounts
                   :set 'contact-first-name 'first-name 'contact-last-name 'last-name
                   :from 'salesmen
                   :where (:= 'salesmen.id 'accounts.sales-id)))
             "UPDATE accounts SET contact_first_name = first_name, contact_last_name = last_name FROM salesmen WHERE (salesmen.id = accounts.sales_id)"))
  (is (equal (sql (:update (:as 'summary 's)
                   :columns 'sum-x 'sum-y 'avg-x 'avg-y
                   (:select (:sum 'x) (:sum 'y) (:avg 'x) (:avg 'y)
                            :from (:as 'data 'd)
                            :where (:= 'd.group-id 's.group-id))))
             "UPDATE summary AS s SET  (sum_x, sum_y, avg_x, avg_y) = (SELECT SUM(x), SUM(y), AVG(x), AVG(y) FROM data AS d WHERE (d.group_id = s.group_id))"))

  ;; From https://www.pgexercises.com/questions/updates/update.html
  (is (equal (sql (:update 'cd.facilities
                   :set 'initialoutlay 10000
                   :where (:= 'facid 1)))
             "UPDATE cd.facilities SET initialoutlay = 10000 WHERE (facid = 1)"))
  ;; From https://www.pgexercises.com/questions/updates/updatemultiple.html
  (is (equal (sql (:update 'cd.facilities
                   :set 'membercost 6 'guestcost 30
                   :where (:in 'facid (:set 0 1))))
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
  (is (equal
       (sql (:update (:as 'cd.facilities 'facs)
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
  (is (equal
       (sql (:delete-from 'cd.bookings :where (:= 'id 5)))
       "DELETE FROM cd.bookings WHERE (id = 5)"))
  ;; From https://www.pgexercises.com/questions/updates/deletewh2.html
  (is (equal
       (sql (:delete-from 'cd.members
             :where (:not (:in 'memid (:select 'memid :from 'cd.bookings)))))
       "DELETE FROM cd.members WHERE (not (memid IN (SELECT memid FROM cd.bookings)))"))

  (is (equal
       (sql (:delete-from (:as 'cd.members 'mems)
             :where (:not (:exists (:select 1
                                    :from 'cd.bookings
                                    :where (:= 'memid 'mems.memid))))))
       "DELETE FROM cd.members AS mems WHERE (not (EXISTS (SELECT 1 FROM cd.bookings WHERE (memid = mems.memid))))")))

(test truncate
  "Testing Truncate"
  (is (equal (sql (:truncate 'bigtable 'fattable))
             "TRUNCATE bigtable, fattable"))
  (is (equal (sql (:truncate 'bigtable 'fattable :only))
             "TRUNCATE  ONLY bigtable, fattable"))
  (is (equal (sql (:truncate 'bigtable 'fattable :only :continue-identity))
             "TRUNCATE  ONLY bigtable, fattable CONTINUE IDENTITY "))
  (is (equal (sql (:truncate 'bigtable 'fattable :only :restart-identity))
             "TRUNCATE  ONLY bigtable, fattable RESTART IDENTITY "))
  (is (equal (sql (:truncate 'bigtable 'fattable :only :restart-identity :cascade ))
             "TRUNCATE  ONLY bigtable, fattable RESTART IDENTITY  CASCADE "))
  (is (equal (sql (:truncate 'bigtable 'fattable :only :continue-identity :cascade ))
             "TRUNCATE  ONLY bigtable, fattable CONTINUE IDENTITY  CASCADE "))
  (is (equal (sql (:truncate 'bigtable 'fattable :continue-identity :cascade ))
             "TRUNCATE bigtable, fattable CONTINUE IDENTITY  CASCADE ")))

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

(test create-index
  "Testing create-index. Available parameters - in order after name -
are :concurrently, :on, :using, :fields and :where.The advantage to using the
keyword :concurrently is that writes to the table
from other sessions are not locked out while the index is is built. The disadvantage is
that the table will need to be scanned twice. Everything is a trade-off."
  (is (equal (sql (:create-index 'films_idx :on 'films :fields 'title))
             "CREATE INDEX films_idx ON films (title)"))
  (is (equal (sql (:create-index 'films-idx :on "films" :fields 'title))
             "CREATE INDEX films_idx ON films (title)"))
  (is (equal (sql (:create-index 'films-idx :on "films" :fields 'title 'id))
             "CREATE INDEX films_idx ON films (title, id)"))
  (is (equal (sql (:create-index 'films_idx :on "films" :using gin :fields 'title))
             "CREATE INDEX films_idx ON films USING gin (title)"))
  (is (equal (sql (:create-index 'doc-tags-id-tags
                   :on "doc-tags-array" :using gin :fields 'tags))
             "CREATE INDEX doc_tags_id_tags ON doc_tags_array USING gin (tags)"))
  (is (equal (sql (:create-unique-index 'doc-tags-id-doc-id
                   :on "doc-tags-array"  :fields 'doc-id))
             "CREATE UNIQUE INDEX doc_tags_id_doc_id ON doc_tags_array (doc_id)"))
  (is (equal (sql (:create-index 'films-idx :concurrently
                                 :on "films" :using 'btree :fields 'created-at))
             "CREATE INDEX CONCURRENTLY films_idx ON films USING btree (created_at)"))
  (is (equal (sql (:create-index 'films-idx :unique :concurrently :on "films"
                   :using 'btree :fields 'created-at))
             "CREATE UNIQUE INDEX CONCURRENTLY films_idx ON films USING btree (created_at)"))
  (is (equal (sql (:create-index (:if-not-exists 'test-uniq-1-idx)
                   :on test-uniq :fields 'name))
             "CREATE INDEX IF NOT EXISTS test_uniq_1_idx ON test_uniq (name)"))
  (with-test-connection
    (query (:drop-table :if-exists 'george :cascade))
    (is (eq (table-exists-p 'george) nil))
    (query (:create-table 'george ((id :type integer))))
    (is (eq (table-exists-p 'george) t))
    (query (:create-index 'george-idx :on 'george :fields 'id))
    (is (pomo:index-exists-p 'george-idx))
    (is (pomo:index-exists-p "george-idx"))))


(test create-view
  "Testing create-view syntax"
  (is (equal (sql (:create-view 'quagmire (:select 'id 'name :from 'employee)))
             "CREATE VIEW quagmire AS (SELECT id, name FROM employee)"))
  (is (equal (sql (:create-view :quagmire (:select 'id 'name :from 'employee)))
             "CREATE VIEW quagmire AS (SELECT id, name FROM employee)"))
  (is (equal (sql (:create-view "quagmire" (:select 'id 'name :from 'employee)))
             "CREATE VIEW quagmire AS (SELECT id, name FROM employee)"))
  (is (equal (sql (:create-view 'quagmire-hollow (:select 'id 'name :from 'employee)))
             "CREATE VIEW quagmire_hollow AS (SELECT id, name FROM employee)"))
  (is (equal (sql (:create-view "quagmire-hollow" (:select 'id 'name :from 'employee)))
             "CREATE VIEW quagmire_hollow AS (SELECT id, name FROM employee)"))
  (with-test-connection
    (unless (table-exists-p 'employee) (build-employee-table))
    (when (view-exists-p 'quagmire) ; this avoids a warning compared to (query (:drop-view :if-exists 'quagmire))
      (query (:drop-view 'quagmire)))
    (query (:create-view 'quagmire (:select 'id 'name :from 'employee)))
    (is (member :QUAGMIRE (list-views) :test 'eq))
    (is (member "quagmire" (list-views t) :test 'equal))
    (is (view-exists-p 'quagmire))
    (is (view-exists-p :quagmire))
    (is (view-exists-p "quagmire"))

    ;; cleanup
    (query (:drop-view 'quagmire))
    (is (equal (sql (:drop-view :quagmire))
               "DROP VIEW quagmire"))))

;; Test create-table
(test reserved-column-names-s-sql
  (with-test-connection
    (when (pomo:table-exists-p 'from-test-data1)
      (execute (:drop-table 'from-test-data1 :cascade)))
    (when (pomo:table-exists-p 'iceland-cities)
      (execute (:drop-table 'iceland-cities :cascade)))
    (query (:create-table 'iceland-cities
                          ((id :type serial)
                           (name :type (or (varchar 100) db-null)
                                 :unique t))))
    (query
     (:create-table 'from-test-data1
                    ((id :type serial)
                     (flight :type (or integer db-null))
                     (from :type (or (varchar 100) db-null)
                           :references ((iceland-cities name)))
                     (to-destination :type (or (varchar 100) db-null)))
                    (:primary-key id)
                    (:constraint iceland-city-name-fkey
                     :foreign-key (to-destination) (iceland-cities name))))
    (query (:insert-into 'iceland-cities :set 'name "Reykjavík"))
    (query (:insert-rows-into 'iceland-cities
            :columns 'name
            :values '(("Seyðisfjörður") ("Stykkishólmur") ("Bolungarvík")
                      ("Kópavogur"))))
    ;; test insert-into
    (query (:insert-into 'from-test-data1
            :set 'flight 1 'from "Reykjavík" 'to-destination "Seyðisfjörður"))
    ;; test query select
    (is (equal (query (:select 'from 'to-destination
                               :from 'from-test-data1
                               :where (:= 'flight 1))
                      :row)
               '("Reykjavík" "Seyðisfjörður")))
    (is (equal (query (:select 'flight
                       :from 'from-test-data1
                       :where (:and (:= 'from "Reykjavík")
                                    (:= 'to-destination "Seyðisfjörður")))
                      :single)
               1))
    ;; test insert-rows-into
    (query (:insert-rows-into 'from-test-data1
            :columns 'flight 'from 'to-destination
            :values '((2 "Stykkishólmur" "Reykjavík"))))
    (is (equal (query (:select 'from 'to-destination
                               :from 'from-test-data1
                               :where (:= 'flight 2))
                      :row)
               '("Stykkishólmur" "Reykjavík")))

    (is (equal (query (:select 'flight
                       :from 'from-test-data1
                       :where (:and (:= 'from "Stykkishólmur")
                                    (:= 'to-destination "Reykjavík")))
                      :single)
               2))
    (query (:alter-table 'from-test-data1 :rename-column 'from 'origin))
    (is (equal (query (:select 'flight
                       :from 'from-test-data1
                       :where (:= 'origin "Stykkishólmur"))
                      :single)
               2))
    ;; test alter-table
    (query (:alter-table 'from-test-data1 :rename-column 'origin 'from ))
    (is (equal (query (:select 'flight
                       :from 'from-test-data1
                       :where (:and (:= 'from "Stykkishólmur")
                                    (:= 'to-destination "Reykjavík")))
                      :single)
               2))
    ;; test constraint
    (signals error (query (:insert-into 'from-test-data1
                           :set 'flight 1 'from "Reykjavík" 'to-destination "Akureyri")))
    (signals error (query (:insert-into 'from-test-data1
                           :set 'flight 1 'from "Akureyri" 'to-destination "Reykjavík")))
    ;; test update
    (query (:update 'from-test-data1
            :set 'from "Kópavogur"
            :where (:= 'to-destination "Seyðisfjörður")))
    (is (equal (query (:select 'flight
                       :from 'from-test-data1
                       :where (:and (:= 'from "Kópavogur")
                                    (:= 'to-destination "Seyðisfjörður")))
                      :single)
               1))
    (query (:update 'from-test-data1
            :set 'to-destination "Kópavogur"
            :where (:= 'from "Stykkishólmur")))
    (is (equal (query (:select 'flight
                       :from 'from-test-data1
                       :where (:and (:= 'to-destination "Kópavogur")
                                    (:= 'from "Stykkishólmur")))
                      :single)
               2))
    (execute (:drop-table 'from-test-data1 :cascade))
    (execute (:drop-table 'iceland-cities :cascade))))

(test posix-regex
  (with-test-connection
    (is (equalp (query (:select (:regexp_match "foobarbequebaz" "bar.*que")) :single)
                #("barbeque")))
    (is (equal (query (:select (:regexp_match "foobarbequebaz" "bar.~que")) :single)
               :NULL))
    (is (equal (query (:select (:~ "foobarbequebaz" "bar.*que") ) :single)
               t))
    (is (equal (query (:select (:!~ "foobarbequebaz" "bar.*que") ) :single)
               nil))
    (is (equal (query (:select (:~ "foobarbequebaz" "barque") ) :single)
               nil))
    (is (equal (query (:select (:~ "foobarbequebaz" "barbeque") ) :single)
               t))
    (is (equal (query (:select (:~ "foobarBequebaz" "barbeque") ) :single)
               nil))
    (is (equal (query (:select (:~* "foobarBequebaz" "barbeque") ) :single)
               t))))

(test text-search
  (with-test-connection
    (when (pomo:table-exists-p 'text-search)
      (execute (:drop-table 'text-search :cascade)))
    (query (:create-table 'text-search ((id :type serial)
                                        (text :type text))))
    (query (:insert-rows-into
            'text-search
            :columns 'text
            :values '(("Each person who knows you has a different perception of who you are.")
                      ("Nothing is as cautiously cuddly as a pet porcupine.")
                      ("Courage and stupidity were all that he had.")
                      ("Hit me with your pet shark!")
                      ("He swore he just saw his sushi move."))))

    (is (equalp (query (:select 'id (:regexp-matches 'text "(s[A-z]+)") :from 'text-search))
                '((1 #("son")) (2 #("sly")) (3 #("stupidity")) (4 #("shark")) (5 #("swore")))))
    (is (equalp (query (:select 'id (:regexp-matches 'text "(s[A-z]+)" "g")
                                :from 'text-search))
                '((1 #("son")) (2 #("sly")) (3 #("stupidity")) (4 #("shark")) (5 #("swore"))
                  (5 #("st")) (5 #("saw")) (5 #("sushi")))))
    (is (equalp (query (:select 'id (:regexp-replace 'text "(s[A-z]+)" "g")
                                :from 'text-search))
                '((1 "Each perg who knows you has a different perception of who you are.")
                  (2 "Nothing is as cautioug cuddly as a pet porcupine.")
                  (3 "Courage and g were all that he had.") (4 "Hit me with your pet g!")
                  (5 "He g he just saw his sushi move."))))
    (is (equalp (query (:select 'id 'text :from 'text-search :where (:~ 'text "sushi")))
                '((5 "He swore he just saw his sushi move."))))))

(test variable-parameters
  (is (equal (let ((column 'latitude))
               (sql (:select column :from 'countries)))
             "(SELECT latitude FROM countries)"))
  (is (equal (let ((column 'latitude) (table 'countries))
               (sql (:select column :from table)))
             "(SELECT latitude FROM countries)"))
  (is (equal (let ((select 'countries.name))
               (sql (:select select
                     :from 'countries 'regions
                     :where (:and
                             (:or (:= 'regions.name '$1)
                                  (:= 'regions.name '$2))
                             (:= 'regions.id 'countries.region-id)))))
             "(SELECT countries.name FROM countries, regions WHERE (((regions.name = $1) or (regions.name = $2)) and (regions.id = countries.region_id)))"))
  (is (equal (let ((select "countries.name"))
               (sql (:select select
                     :from 'countries 'regions
                     :where (:and
                             (:or (:= 'regions.name '$1)
                                  (:= 'regions.name '$2))
                             (:= 'regions.id 'countries.region-id)))))
             "(SELECT E'countries.name' FROM countries, regions WHERE (((regions.name = $1) or (regions.name = $2)) and (regions.id = countries.region_id)))"))
  (is (equal (let ((select "countries.name") (table1 'countries) (table2 'regions))
               (sql (:select select
                     :from table1 table2
                     :where (:and
                             (:or (:= 'regions.name '$1)
                                  (:= 'regions.name '$2))
                             (:= 'regions.id 'countries.region-id)))))
             "(SELECT E'countries.name' FROM countries, regions WHERE (((regions.name = $1) or (regions.name = $2)) and (regions.id = countries.region_id)))"))
  (is (equal (let ((select "countries.name") (table1 "countries") (table2 "regions"))
               (sql (:select select
                     :from table1 table2
                     :where (:and
                             (:or (:= 'regions.name '$1)
                                  (:= 'regions.name '$2))
                             (:= 'regions.id 'countries.region-id)))))
             "(SELECT E'countries.name' FROM E'countries', E'regions' WHERE (((regions.name = $1) or (regions.name = $2)) and (regions.id = countries.region_id)))")))

(test drop-table-variations
  (let ((table-var1 "table-1")
        (table-var2 'table-1))
    (labels ((create-and-check ()
               (is-false (table-exists-p "table_1"))
               (query (:create-table (:if-not-exists "table_1") ((id :type integer))))
               (is-true (table-exists-p "table_1"))))
      (with-test-connection
        (query (:drop-table :if-exists "table_1"))
        (create-and-check)
        (is (equal (sql (:drop-table :if-exists table-var1 :cascade))
                   "DROP TABLE IF EXISTS table_1 CASCADE"))
        (is (equal (sql (:drop-table :if-exists "table-1" :cascade))
                   "DROP TABLE IF EXISTS table_1 CASCADE"))
        (is (equal (sql (:drop-table :if-exists 'table-1 :cascade))
                   "DROP TABLE IF EXISTS table_1 CASCADE"))
        (is (equal (sql (:drop-table :if-exists table-var2 :cascade))
                   "DROP TABLE IF EXISTS table_1 CASCADE"))
        (is (equal (sql (:drop-table (:if-exists "table-1") :cascade))
                   "DROP TABLE IF EXISTS table_1 CASCADE"))
        (is (equal (sql (:drop-table :if-exists table-var1))
                   "DROP TABLE IF EXISTS table_1"))
        (is (equal (sql (:drop-table :if-exists "table-1"))
                   "DROP TABLE IF EXISTS table_1"))
        (is (equal (sql (:drop-table :if-exists 'table-1))
                   "DROP TABLE IF EXISTS table_1"))
        (is (equal (sql (:drop-table :if-exists table-var2))
                   "DROP TABLE IF EXISTS table_1"))
        (is (equal (sql (:drop-table (:if-exists "table-1")))
                   "DROP TABLE IF EXISTS table_1"))
        (is (equal (sql (:drop-table table-var1 :cascade))
                   "DROP TABLE table_1 CASCADE"))
        (is (equal (sql (:drop-table "table-1" :cascade))
                   "DROP TABLE table_1 CASCADE"))
        (is (equal (sql (:drop-table 'table-1 :cascade))
                   "DROP TABLE table_1 CASCADE"))
        (is (equal (sql (:drop-table table-var2 :cascade))
                   "DROP TABLE table_1 CASCADE"))
        (is (equal (sql (:drop-table  "table-1" :cascade))
                   "DROP TABLE table_1 CASCADE"))
        (is (equal (sql (:drop-table table-var1))
                   "DROP TABLE table_1"))
        (is (equal (sql (:drop-table "table-1"))
                   "DROP TABLE table_1"))
        (is (equal (sql (:drop-table 'table-1))
                   "DROP TABLE table_1"))
        (is (equal (sql (:drop-table table-var2))
                   "DROP TABLE table_1"))
        (is (equal (sql (:drop-table 'table-1))
                   "DROP TABLE table_1"))
        (is (equal (sql (:drop-table "table-1"))
                   "DROP TABLE table_1"))))))

(test drop-table-variations-live
  (let ((table-var1 "table-1")
        (table-var2 'table-1))
    (labels ((create-and-check ()
               (is-false (table-exists-p "table_1"))
               (query (:create-table (:if-not-exists "table_1") ((id :type integer))))
               (is-true (table-exists-p "table_1"))))
      (with-test-connection
        (query (:drop-table :if-exists "table_1"))
        (create-and-check)
        (query (:drop-table :if-exists table-var1 :cascade))
        (create-and-check)
        (query (:drop-table :if-exists "table-1" :cascade))
        (create-and-check)
        (query (:drop-table :if-exists 'table-1 :cascade))
        (create-and-check)
        (query (:drop-table :if-exists table-var2 :cascade))
        (create-and-check)
        (query (:drop-table (:if-exists "table-1") :cascade))
        (create-and-check)
        (query (:drop-table :if-exists table-var1))
        (create-and-check)
        (query (:drop-table :if-exists "table-1"))
        (create-and-check)
        (query (:drop-table :if-exists 'table-1))
        (create-and-check)
        (query (:drop-table :if-exists table-var2))
        (create-and-check)
        (query (:drop-table (:if-exists "table-1")))
        (create-and-check)
        (query (:drop-table table-var1 :cascade))
        (create-and-check)
        (query (:drop-table "table-1" :cascade))
        (create-and-check)
        (query (:drop-table 'table-1 :cascade))
        (create-and-check)
        (query (:drop-table table-var2 :cascade))
        (create-and-check)
        (query (:drop-table  "table-1" :cascade))
        (create-and-check)
        (query (:drop-table table-var1))
        (create-and-check)
        (query (:drop-table "table-1"))
        (create-and-check)
        (query (:drop-table 'table-1))
        (create-and-check)
        (query (:drop-table table-var2))
        (create-and-check)
        (query (:drop-table 'table-1))
        (create-and-check)
        (query (:drop-table "table-1"))
        (is-false (table-exists-p "table_1"))))))

(test generate-series
  (is (equal (sql (:select 'x (:generate-series 0 'x)
                           :from (:as (:values (:set 1) (:set 2) (:set 3))
                                      (:t1 'x))))
             "(SELECT x, generate_series(0, x) FROM (VALUES (1), (2), (3)) AS t1(x))"))

  (with-test-connection
    (is (equal (query (:select 'x (:generate-series 0 'x)
                               :from (:as (:values (:set 0) (:set 1) (:set 2))
                                          (:t 'x))))
               '((0 0) (1 0) (1 1) (2 0) (2 1) (2 2))))))

(defun build-boolean-table ()
  (pomo:drop-table 'boolean-test :if-exists t)
  (query "create table boolean_test (id integer, a boolean, b text)")
  (query "insert into boolean_test values (1, true, 'I am true')")
  (query "insert into boolean_test values (2, false, 'I am false')")
  (query "insert into boolean_test values (3, null, 'I am NULL')"))

(test is-true
  (is (equal (sql (:select '* :from 'table1 :where (:is-true 'col)))
             "(SELECT * FROM table1 WHERE (col IS TRUE))"))
  (with-test-connection
    (build-boolean-table)
    (is (equal (query (:select '* :from 'boolean-test :where (:is-true 'a)))
               '((1 T "I am true"))))))

(test is-false
  (is (equal (sql (:select '* :from 'table1 :where (:is-false 'col)))
             "(SELECT * FROM table1 WHERE (col IS FALSE))"))
  (with-test-connection
    (build-boolean-table)
    (is (equal (query (:select '* :from 'boolean-test :where (:is-false 'a)))
               '((2 NIL "I am false"))))))

(test is-null
  (is (equal (sql (:select '* :from 'table1 :where (:is-false 'col)))
             "(SELECT * FROM table1 WHERE (col IS FALSE))"))
  (with-test-connection
    (build-boolean-table)
    (is (equal (query (:select '* :from 'boolean-test :where (:is-null 'a)))
               '((3 :NULL "I am NULL"))))))
