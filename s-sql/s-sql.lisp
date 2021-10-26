;;; -*- Mode: Lisp; Base: 10; Syntax: ANSI-Common-Lisp; Package: S-SQL; -*-
(in-package :s-sql)

;; Utils

(define-condition sql-error (simple-error) ())

(defun sql-error (control &rest args)
  (error 'sql-error :format-control control :format-arguments args))

(defun strcat (args)
  "Concatenate a list of strings into a single one."
  (let ((result (make-string (reduce #'+ args :initial-value 0 :key 'length))))
    (loop :for pos = 0 :then (+ pos (length arg))
          :for arg :in args
          :do (replace result arg :start1 pos))
    result))

(defun implode (sep list)
  "Reduce a list of strings to a single string, inserting a separator between
them."
  (strcat (loop :for element :on list
                :collect (car element)
                :if (cdr element)
                  :collect sep)))

(defun split-on-keywords% (shape list)
  "Helper function for split-on-keywords. Extracts the values
associated with the keywords from an argument list, and checks for
errors."
  (let ((result ()))
    (labels ((next-word (words values)
               (if words
                   (let* ((me (intern (symbol-name (caar words)) :keyword))
                          (optional (member '? (car words)))
                          (multi (member '* (car words)))
                          (no-args (member '- (car words)))
                          (found (position me values)))
                     (cond (found
                            (let ((after-me (nthcdr (1+ found) values)))
                              (unless (or after-me no-args)
                                (sql-error "Keyword ~A encountered at end of arguments."
                                           me))
                              (let ((next (next-word (cdr words) after-me)))
                                (cond
                                  (no-args
                                   (unless (zerop next)
                                     (sql-error "Keyword ~A does not take any arguments."
                                                me)))
                                  (multi
                                   (unless (>= next 1)
                                     (sql-error "Not enough arguments to keyword ~A."
                                                me)))
                                  (t (unless (= next 1)
                                       (sql-error "Keyword ~A takes exactly one argument."
                                                  me))))
                                (push (cons (caar words)
                                            (if no-args t
                                                (subseq after-me 0 next)))
                                      result)
                                found)))
                           (optional
                            (next-word (cdr words) values))
                           (t (sql-error "Required keyword ~A not found." me))))
                   (length values))))
      (unless (= (next-word shape list) 0)
        (sql-error "Arguments do not start with a valid keyword."))
      result)))

(defmacro split-on-keywords (words form &body body)
  "Handles arguments to some complex SQL operations. Arguments
are divided by keywords, which are interned with the name of the
non-keyword symbols in words, and bound to these symbols. After the
naming symbols, a ? can be used to indicate this argument group is
optional, an * to indicate it can consist of more than one element,
and a - to indicate it does not take any elements. When used, keywords
must appear in the order defined."
  (let ((alist (gensym)))
    `(let* ((,alist (split-on-keywords% ',words ,form))
            ,@(mapcar (lambda (word)
                        `(,(first word) (cdr (assoc ',(first word) ,alist))))
                      words))
       ,@body)))

(defun to-sql-name (name &optional (escape-p *escape-sql-names-p*)
                           (ignore-reserved-words nil))
  "Convert a symbol or string into a name that can be a sql table, column, or
operation name. Add quotes when escape-p is true, or escape-p is :auto and the
name contains reserved words. Quoted or delimited identifiers can be used by
passing :literal as the value of escape-p. If escape-p is :literal, and the
name is a string then the string is still escaped but the symbol or string is
not downcased, regardless of the setting for *downcase-symbols* and the
hyphen and forward slash characters are not replaced with underscores.

Ignore-reserved-words is only used internally for column names which are allowed
to be reserved words, but it is not recommended."
  (declare (optimize (speed 3) (debug 0)))
  (let ((*print-pretty* nil)
        (name (if (and (consp name) (eq (car name) 'quote) (equal (length name)
                                                                  2))
                  (string (cadr name))
                  (string name))))
    (with-output-to-string (*standard-output*)
      (flet ((subseq-downcase (str from to)
               (let ((result (make-string (- to from))))
                 (loop :for i :from from :below to
                       :for p :from 0
                       :do (setf (char result p)
                                 (if (and *downcase-symbols*
                                          (not (eq escape-p :literal)))
                                     (char-downcase (char str i))
                                     (char str i))))
                 result))
             (write-element (str)
               (declare (type string str))
               (let ((escape-p (cond ((and (eq escape-p :auto)
                                           (not ignore-reserved-words))
                                      (gethash str *postgres-reserved-words*))
                                     (ignore-reserved-words nil)
                                     (t escape-p))))
                 (when escape-p
                   (write-char #\"))
                 (if (and (> (length str) 1) ;; Placeholders like $2
                          (char= (char str 0) #\$)
                          (every #'digit-char-p (the string (subseq str 1))))
                     (princ str)
                     (loop :for ch :of-type character :across str
                           :do (if (or (eq ch #\*)
                                       (alphanumericp ch)
                                       (eq escape-p :literal))
                                   (write-char ch)
                                   (write-char #\_))))
                 (when escape-p
                   (write-char #\")))))

        (loop :for start := 0 :then (1+ dot)
              :for dot := (position #\. name) :then (position #\. name
                                                              :start start)
              :do (write-element (subseq-downcase name start
                                                  (or dot (length name))))
              :if dot :do (princ #\.)
                :else :do (return))))))

(defun from-sql-name (str)
  "Convert a string to a symbol, upcasing and replacing underscores with
hyphens."
  (intern (map 'string (lambda (x) (if (eq x #\_) #\- x))
               (if (eq (readtable-case *readtable*) :upcase)
                   (string-upcase str)
                   str))
          (find-package :keyword)))

;; Writing out SQL type identifiers.

;; Aliases for some types that can be expressed in SQL.
(deftype smallint ()
  "Also known as int2"
  '(signed-byte 16))
(deftype bigint ()
  "Also know as int8"
  `(signed-byte 64))
(deftype numeric (&optional precision/scale scale)
  (declare (ignore precision/scale scale))
  'number)
(deftype double-precision ()
  'double-float)
(deftype bytea ()
  '(array (unsigned-byte 8)))
(deftype text ()
  'string)
(deftype varchar (length)
  (declare (ignore length))
  `string)
(deftype serial () 'integer)
(deftype serial8 () 'integer)

(deftype db-null ()
  "Type for representing NULL values. Use like (or integer db-null)
for declaring a type to be an integer that may be null."
  '(eql :null))

;; For types integer and real, the Lisp type isn't quite the same as
;; the SQL type. Close enough though.

(defgeneric sql-type-name (lisp-type &rest args)
  (:documentation "Transform a lisp type into a string containing something
SQL understands. Default is to just use the type symbol's name.")
  (:method ((lisp-type symbol) &rest args)
    (cond ((and args (equal (symbol-name lisp-type) "GEOMETRY")) ; geometry type from postgis
           (format nil "geometry (~{~a~^, ~})" args))
          (t (substitute #\Space #\- (symbol-name lisp-type) :test #'char=))))
  (:method ((lisp-type (eql 'string)) &rest args)
    (cond (args (format nil "CHAR(~A)" (car args)))
          (t "TEXT")))
  (:method ((lisp-type (eql 'varchar)) &rest args)
    (cond (args (format nil "VARCHAR(~A)" (car args)))
          (t "VARCHAR")))
  (:method ((lisp-type (eql 'numeric)) &rest args)
    (cond ((cdr args)
           (destructuring-bind (precision scale) args
             (format nil "NUMERIC(~d, ~d)" precision scale)))
          (args (format nil "NUMERIC(~d)" (car args)))
          (t "NUMERIC")))
  (:method ((lisp-type (eql 'float)) &rest args)
    (declare (ignore args))
    "REAL")
  (:method ((lisp-type (eql 'double-float)) &rest args)
    (declare (ignore args))
    "DOUBLE PRECISION")
  (:method ((lisp-type (eql 'double-precision)) &rest args)
    (declare (ignore args))
    "DOUBLE PRECISION")
  (:method ((lisp-type (eql 'serial)) &rest args)
    (declare (ignore args))
    "SERIAL")
  (:method ((lisp-type (eql 'serial8)) &rest args)
    (declare (ignore args))
    "SERIAL8")
  (:method ((lisp-type (eql 'array)) &rest args)
    (format nil "~a[]" (to-type-name (car args))))
  (:method ((lisp-type (eql 'db-null)) &rest args)
    (declare (ignore args))
    (sql-error "Bad use of ~s." 'db-null)))

(defun to-type-name (type)
  "Turn a Lisp type expression into a SQL typename."
  (if (listp type)
      (apply 'sql-type-name type)
      (sql-type-name type)))

;; Turning lisp values into SQL strings.

(defun sql-escape-string (string &optional prefix)
  "Escape string data so it can be used in a query. Example:

    (sql-escape-string \"Puss in 'Boots'\")

     \"E'Puss in ''Boots'''\""
  (let ((*print-pretty* nil))
    (with-output-to-string (*standard-output*)
      (when prefix
        (princ prefix)
        (princ #\space))
      (unless *standard-sql-strings*
        (princ #\E))
      (princ #\')
      (if *standard-sql-strings*
          (loop :for char :across string
                :do (princ (if (char= char #\') "''" char)))
          (loop :for char :across string
                :do (princ (case char
                             (#\' "''")
                             (#\\ "\\\\")
                             (otherwise char)))))
      (princ #\'))))

(defgeneric sql-escape (arg)
  (:documentation "A generalisation of sql-escape-string looks at the type of
the value passed, and properly writes it out it for inclusion in an SQL query.
Symbols will be converted to SQL names. Examples:

  (sql-escape \"tr'-x\")

  \"E'tr''-x'\"

  (sql-escape (/ 1 13))

  \"0.0769230769230769230769230769230769230\"

  (sql-escape #(\"Baden-Wurttemberg\" \"Bavaria\" \"Berlin\" \"Brandenburg\"))

  \"ARRAY[E'Baden-Wurttemberg', E'Bavaria', E'Berlin', E'Brandenburg']\"")
  (:method ((arg symbol))
    (if (or (typep arg 'boolean) (eq arg :null))
        (call-next-method)
        (to-sql-name arg)))
  (:method ((arg vector))
    (if (or (typep arg '(vector (unsigned-byte 8)))
            (stringp arg))
        (call-next-method)
        (format nil "~:['{}'~;ARRAY[~:*~{~A~^, ~}]~]"
                (map 'list 'sql-escape arg))))
  (:method ((arg t))
    (multiple-value-bind (string escape) (cl-postgres:to-sql-string arg)
      (if escape
          (sql-escape-string string (and (not (eq escape t)) escape))
          string))))

(defparameter *expand-runtime* nil)

(defun sql-expand (arg)
  "Compile-time expansion of forms into lists of stuff that evaluate
to strings (which will form a SQL query when concatenated). NEW :default will
return ' DEFAULT' "

  (cond ((eq arg :default) (list " DEFAULT ")) ((and (consp arg) (keywordp (first arg)))
         (expand-sql-op (car arg) (cdr arg)))
        ((and (consp arg) (eq (first arg) 'quote))
         (list (sql-escape (second arg))))
        ((and (consp arg) *expand-runtime*)
         (expand-sql-op (intern (symbol-name (car arg)) :keyword) (cdr arg)))
        ((and (eq arg '$$) *expand-runtime*)
         '($$))
        (*expand-runtime*
         (list (sql-escape arg)))
        ((consp arg)
         (list `(sql-escape ,arg)))
        ((or (consp arg)
             (and (symbolp arg)
                  (not (or (keywordp arg) (eq arg t) (eq arg nil)))))
         (list `(sql-escape ,arg)))
        (t (list (sql-escape arg)))))

(defun sql-expand-list (elts &optional (sep ", "))
  "Expand a list of elements, adding a separator between them."
  (loop :for (elt . rest) :on elts
        :append (sql-expand elt)
        :if rest :collect sep))

(defun sql-expand-names (names &optional (sep ", "))
  "Takes a list of elements (symbols or strings) and returns a separated list
of strings. If the element is a cons, then "
  (loop :for (name . rest) :on names
        :if (consp name) :append (let ((*expand-runtime* t))
                                   (sql-expand name))
          :else :collect (to-sql-name name)
        :if rest :collect sep))

(defun reduce-strings (list)
  "Join adjacent strings in a list; leave other values intact."
  (let ((accum ())
        (span ""))
    (dolist (part list)
      (cond ((stringp part) (setf span (concatenate 'string span part)))
            (t (when (not (string= "" span))
                 (push span accum)
                 (setf span ""))
               (push part accum))))
    (if (not (string= "" span))
        (push span accum))
    (nreverse accum)))

(defmacro sql (form)
  "Convert the given form (a list starting with a keyword) to an SQL query
string at compile time, according to the rules described here. For example:

    (sql (:select '* :from 'country :where (:= 'a 1)))
     \"(SELECT * FROM country WHERE (a = 1))\"

but

    (sql '(:select '* :from 'country :where (:= 'a 1)))

would throw an error. For the later case you need to use sql-compile."
  (let ((list (reduce-strings (sql-expand form))))
    (if (= 1 (length list))
        (car list)
        `(strcat (list ,@list)))))

(defun sql-compile (form)
  "This is the run-time variant of the sql macro. It converts the given list to
an SQL query, with the same rules except that symbols in this list do not
have to be quoted to be interpreted as identifiers. For example:

    (sql-compile '(:select '* :from 'country :where (:= 'a 1)))
     \"(SELECT * FROM country WHERE (a = 1))\"

but

    (sql (:select '* :from 'country :where (:= 'a 1)))

would throw an error. For the later case you need to use sql."
  (let ((*expand-runtime* t))
    (strcat (sql-expand form))))

(defun sql-template (form)
  "In cases where you do need to build the query at run time, yet you do not
want to re-compile it all the time, this function can be used to compile it
once and store the result. It takes an S-SQL form, which may contain
$$ placeholder symbols, and returns a function that takes one argument for
every $$. When called, this returned function produces an SQL string in
which the placeholders have been replaced by the values of the arguments."
  (let* ((*expand-runtime* t)
         (compiled (reduce-strings (sql-expand form)))
         (*print-pretty* nil))
    (lambda (&rest args)
      (with-output-to-string (*standard-output*)
        (dolist (element compiled)
          (princ (if (eq element '$$) (sql-escape (pop args)) element)))))))

;; The reader syntax.

(defun s-sql-reader (stream char min-args)
  (declare (ignore char min-args))
  (list 'sql (read stream)))

(defun enable-s-sql-syntax (&optional (char #\Q))
  "Enable a syntactic shortcut #Q(...) for (sql \(...)). Optionally
takes a character to use instead of #\\Q."
  (set-dispatch-macro-character #\# char 's-sql-reader))

;; Definitions of sql operators

(defgeneric expand-sql-op (op args)
  (:documentation "Override expansion of operators. Default is to just
place operator name in front, arguments between parentheses and
nothing behind it.")
  (:method ((op t) args)
    `(,(to-sql-name op) "(" ,@(sql-expand-list args) ")")))

(defmacro def-sql-op (name arglist &body body)
  "Macro to make defining syntax a bit more straightforward. Name
should be the keyword identifying the operator, arglist a lambda list
to apply to the arguments, and body something that produces a list of
strings and forms that evaluate to strings."
  (alexandria:with-unique-names (args-name op)
    (multiple-value-bind (body decls docstring)
        (alexandria:parse-body body :documentation t)
      `(defmethod expand-sql-op ((,op (eql ,name)) ,args-name)
         ,@(when docstring
             (list docstring))
         ,@decls
         (destructuring-bind ,arglist ,args-name
           ,@body)))))

(defun make-expander (arity name)
  "Generates an appropriate expander function for a given operator
with a given arity."
  (let ((with-spaces (strcat (list " " name " "))))
    (flet ((check-unary (args)
             (when (or (not args) (cdr args))
               (sql-error "SQL operator ~A is unary." name)))
           (expand-n-ary (args)
             `("(" ,@(sql-expand-list args with-spaces) ")")))
      (ecase arity
        (:unary (lambda (args)
                  (check-unary args)
                  `("(" ,name " " ,@(sql-expand (car args)) ")")))
        (:unary-postfix (lambda (args)
                          (check-unary args)
                          `("(" ,@(sql-expand (car args)) " " ,name ")")))
        (:n-ary (lambda (args)
                  (if (cdr args)
                      (expand-n-ary args)
                      (sql-expand (car args)))))
        (:2+-ary (lambda (args)
                   (unless (cdr args)
                     (sql-error "SQL operator ~A takes at least two arguments."
                                name))
                   (expand-n-ary args)))
        (:n-or-unary (lambda (args)
                       (if (cdr args)
                           (expand-n-ary args)
                           `("(" ,name " " ,@(sql-expand (car args)) ")"))))))))

(defmacro register-sql-operators (arity &rest names)
  "Define simple operators. Arity is one of :unary (like
'not'), :unary-postfix (the operator comes after the operand),
:n-ary (like '+': the operator falls away when there is only one
operand), :2+-ary (like '=', which is meaningless for one operand),
or :n-or-unary (like '-', where the operator is kept in the unary
case). After the arity follow any number of operators, either just a
keyword, in which case the downcased symbol name is used as the
operator, or a two-element list containing a keyword and a name
string."
  (declare (type (member :unary :unary-postfix :n-ary :n-or-unary :2+-ary)
                 arity))
  (flet ((define-op (name)
           (let ((name (if (listp name)
                           (second name)
                           (string-downcase (symbol-name name))))
                 (symbol (if (listp name) (first name) name)))
             `(let ((expander (make-expander ,arity ,name)))
                (defmethod expand-sql-op ((op (eql ,symbol)) args)
                  (funcall expander args))))))
    `(progn ,@(mapcar #'define-op names))))

(register-sql-operators :unary :not)
(register-sql-operators :n-ary :+ :* :% :& :|\|| :|\|\|| :and :or :union
                        (:union-all "union all"))
(register-sql-operators :n-or-unary :- :~)
(register-sql-operators :2+-ary  := :/ :!= :<> :< :> :<= :>= :^ :~* :!~ :!~*
                                 :like :ilike :->> :|#>| :|#>>|
                        :intersect (:intersect-all "intersect all")
                        :except (:except-all "except all"))

;; Examples of the use of these operators is:
;; (query (:select 'id 'text :from 'text-search :where (:~ 'text "sushi")))

;; PostGIS operators
(register-sql-operators :2+-ary :&& :&< :|&<\|| :&> :<< :|<<\|| :>> :|@| :|\|&>|
                        :|\|>>| :~= :|@>| :|@<|)

;; hstore operators
(register-sql-operators :2+-ary :-> :=> :? :?& :?\| :|<@| :|#=| :unary :%% :%#)

;;; sql-op :+, :*, :%, :&, :|, :| |, :and, :or, :=, :/, :!=, :<, :>, :<=, :>=,
;;;  :^, :union, :union-all,
;;;  :intersect, :intersect-all, :except, :except-all (&rest args)
;;;
;;; These are expanded as infix operators. When meaningful, they allow more than
;;; two arguments. :- can also be used as a unary operator to negate a value.
;;; Note that the arguments to :union, :union-all, :intersect, and :except
;;; should be queries (:select forms).

;;; Note that you'll have to escape pipe characters to enter them as keywords.
;;; S-SQL handles the empty keyword symbol (written :||) specially, and treats
;;; it like :\|\|, so that it can be written without escapes. With :\|, this
;;; doesn't work.

(def-sql-op :|| (&rest args)
  "The concatenation operator combines two or more columns into a single column
return. E.g:

(query (:select 'countries.id (:|| 'countries.name \"-\" 'regions.name)
                :from 'countries 'regions
                :where (:and (:= 'regions.id 'countries.region-id)
                             (:= 'countries.name \"US\"))))

((21 \"US-North America\"))"
  `("(" ,@(sql-expand-list args " || ") ")"))

(def-sql-op :asc (arg)
  `(,@(sql-expand arg) " ASC"))

(def-sql-op :desc (arg)
  `(,@(sql-expand arg) " DESC"))

(def-sql-op :nulls-first (arg)
  `(,@(sql-expand arg) " NULLS FIRST"))

(def-sql-op :nulls-last (arg)
  `(,@(sql-expand arg) " NULLS LAST"))

(def-sql-op :as (form name &rest fields)
  `(,@(sql-expand form) " AS " ,@(sql-expand name)
    ,@(when fields
        `("(" ,@(loop :for field :in fields
                      :for (name type)
                        := (if (and (consp field)
                                    (not (eq (first field) 'quote)))
                               field
                               (list field nil))
                      :for first := t :then nil
                      :unless first :collect ", "
                        :append (sql-expand name)
                      :when type :append (list " " (to-type-name type)))
              ")"))))

(def-sql-op :|@@| (op1 op2)
  `("(" ,@(sql-expand op1) " @@ " ,@(sql-expand op2) ")"))

(def-sql-op :distinct (&rest forms)
  `("DISTINCT(" ,@(sql-expand-list forms) ")"))

(def-sql-op :any* (query)
  "Any needs to be considered as a special case. Postgres has both a function-call-style any and an infix any, and S-SQL's syntax doesn't allow them to be distinguished. As a result, postmodern has a regular :any sql-op and a :any* sql-op, which expand slightly differently."
  `("ANY(" ,@(sql-expand query) ")"))

(def-sql-op :any (query)
    "Any needs to be considered as a special case. Postgres has both a function-call-style any and an infix any, and S-SQL's syntax doesn't allow them to be distinguished. As a result, postmodern has a regular :any sql-op and a :any* sql-op, which expand slightly differently."
  `("ANY " ,@(sql-expand query)))

(def-sql-op :all (query)
  `("ALL " ,@(sql-expand query)))

(def-sql-op :array (query)
  "This is used when calling a select query into an array. See sample usage."
  `("ARRAY(" ,@(sql-expand query) ")"))

(def-sql-op :array[] (&rest args)
  "This handles statements that include functions in the query such as (:+ 1 2),
(:pi) in the array whereas just passing an array as #(1.0 2.4) does not and you
are not selecting into an array, so do not use :array."
  `("ARRAY[" ,@(sql-expand-list args) "]"))

(def-sql-op :[] (form start &optional end)
  "This slices arrays. Sample usage would be:
   (query (:select (:[] 'provinces 1) :from 'array-provinces
   :where (:= 'name \"Germany\"))"
  (if end
      `("(" ,@(sql-expand form) ")[" ,@(sql-expand start) ":" ,@(sql-expand end)
            "]")
      `("(" ,@(sql-expand form) ")[" ,@(sql-expand start) "]")))

(def-sql-op :interval (arg  &optional precision)
  "Interval takes a string.
See https://www.postgresql.org/docs/current/static/datatype-datetime.html
It optionally take a precision parameter, which causes the result to be rounded
to that many fractional digits in the seconds field. Without a precision
+parameter, the result is given to the full available precision. Precision
only applies to seconds."
  (if precision
      `("INTERVAL  " ,@(sql-expand arg) "(" ,@(sql-expand precision) ")")
      `("INTERVAL  " ,@(sql-expand arg))))

(def-sql-op :current-date ()
  "Provides the current time. The default is universal time. If you want
a more human readable approach, you can use :to-char. As an example:
  (query (:select (:current-date) (:to-char (:current-date) \"YYYY-MM-DD\")))
  ((3751488000 \"2018-11-18\"))"

  `("current_date"))

(def-sql-op :current-timestamp (&optional precision)
  "Current-time and Current-timestamp deliver values with time zones. They
optionally take a precision parameter, which causes the result to be rounded
to that many fractional digits in the seconds field. Without a precision
parameter, the result is given to the full available precision. Precision
only applies to seconds."
  (if precision
      `("current_timestamp (" ,@(sql-expand precision) ")")
      '("current_timestamp")))

(def-sql-op :current-time (&optional precision)
  "Current-time and Current-timestamp deliver values with time zones. They
optionally take a precision parameter, which causes the result to be rounded
to that many fractional digits in the seconds field. Without a precision
parameter, the result is given to the full available precision. Precision
only applies to seconds."
  (if precision
      `("current_time (" ,@(sql-expand precision) ")")
      '("current_time")))

(def-sql-op :local-timestamp (&optional precision)
  "LOCALTIME and LOCALTIMESTAMP deliver values without time zone. They optionally
take a precision parameter, which causes the result to be rounded to that many
fractional digits in the seconds field. Without a precision parameter, the
result is given to the full available precision. Precision
only applies to seconds."
  (if precision
      `("localtimestamp (" ,@(sql-expand precision) ")")
      '("localtimestamp")))

(def-sql-op :local-time (&optional precision)
  "LOCALTIME and LOCALTIMESTAMP deliver values without time zone. They optionally
take a precision parameter, which causes the result to be rounded to that many
fractional digits in the seconds field. Without a precision parameter, the
result is given to the full available precision."
  (if precision
      `("localtime (" ,@(sql-expand precision) ")")
      '("localtime")))

(def-sql-op :timestamp (arg)
  `("timestamp " ,@(sql-expand arg)))

(def-sql-op :make-interval (&rest args)
  "Takes lists of (time-unit value) and returns an interval type.
e.g. (make-interval (\"days\" 10)(\"hours\" 4))."
  `("make_interval("
    ,@(loop for ((x . y) . rest) on args
            :append `(,x " := " ,(cond ((numberp y) (write-to-string y))
                                       ((listp y)
                                        (cond ((numberp (car y))
                                               (write-to-string (car y)))
                                              ((stringp (car y))
                                               (strcat `("'" ,(car y) "'")))
                                              (t (car y))))))
            :if rest :collect ", ")
    ")"))

(def-sql-op :make-timestamp (&rest args)
  "Takes lists of (time-unit value) and returns a timestamp type.
e.g. (make-interval (\"days\" 10)(\"hours\" 4))."
  `("make_timestamp("
    ,@(loop for ((x . y) . rest) on args
            :append `(,x " := " ,(cond ((numberp y) (write-to-string y))
                                       ((listp y)
                                        (cond ((numberp (car y))
                                               (write-to-string (car y)))
                                              ((stringp (car y))
                                               (strcat `("'" ,(car y) "'")))
                                              (t (car y))))))
            :if rest :collect ", ")
    ")"))

(def-sql-op :make-timestamptz (&rest args)
  "Takes lists of (time-unit value) and returns a timestamptz type.
e.g. (make-interval (\"days\" 10)(\"hours\" 4))."
  `("make_timestamptz("
    ,@(loop for ((x . y) . rest) on args
            :append `(,x " := " ,(cond ((numberp y) (write-to-string y))
                                       ((listp y)
                                        (cond ((numberp (car y))
                                               (write-to-string (car y)))
                                              ((stringp (car y))
                                               (strcat `("'" ,(car y) "'")))
                                              (t (car y))))))
            :if rest :collect ", ")
    ")"))


(def-sql-op :age (&rest args)
  `("AGE (" ,@(sql-expand-list args) ")"))

(def-sql-op :date (arg)
  `("date " ,@(sql-expand arg)))

(def-sql-op :integer (arg)
  `("integer " ,@(sql-expand arg)))

(def-sql-op :cast (query)
  "Cast is one of two functions that help convert one type of data to another.
 The other function is type. An example use of cast is:

 (query (:select (:as (:cast (:as (:* 50 (:random)) 'int)) 'x)
                 :from (:generate-series 1 3)))

 One syntactic difference between cast and type is that the cast function
 requires that the datatype be quoted or a variable be passed as the type.

 (let ((type 'text))
   (query (:select (:cast (:as \"20\" type)))
     :single))
"
  `("CAST(" ,@(sql-expand query) ")" ))

(def-sql-op :exists (query)
  `("(EXISTS " ,@(sql-expand query) ")"))

(def-sql-op :is-true (arg)
  `("(" ,@(sql-expand arg) " IS TRUE)"))

(def-sql-op :is-false (arg)
  `("(" ,@(sql-expand arg) " IS FALSE)"))

(def-sql-op :is-null (arg)
  `("(" ,@(sql-expand arg) " IS NULL)"))

(def-sql-op :not-null (arg)
  `("(" ,@(sql-expand arg) " IS NOT NULL)"))

(def-sql-op :in (form set)
  `("(" ,@(sql-expand form) " IN " ,@(sql-expand set) ")"))

(def-sql-op :not-in (form set)
  `("(" ,@(sql-expand form) " NOT IN " ,@(sql-expand set) ")"))

(def-sql-op :extract (unit form)
  `("EXTRACT(" ,@(sql-expand unit) " FROM " ,@(sql-expand form) ")"))

(def-sql-op :values (&rest args) "values statement"
  (split-on-keywords ((vars *) (order-by * ?)) (cons :vars args)
    `("(VALUES "
      ,@(sql-expand-list vars)
      ,@(when order-by `(" ORDER BY " ,@(sql-expand-list order-by) ")"))
      ")")))

(define-condition malformed-composite-type-error (error)
  ((text :initarg :text :reader text)))

(defun cons-to-sql-name-strings (item)
  "Takes a list of two items and returns a single string separated by a space.
The items will be converted to sql compatible namestrings."
  (if (= 2 (length item))
      (implode " " (mapcar #'to-sql-name item))
      (error 'malformed-composite-type-error :text item)))

(def-sql-op :count (&rest args)
  "Count returns the number of rows. It can be the number of rows collected
by the select statement as in:

    (query (:select (:count '*) :from 'table1 :where (:= 'price 100)))

or it can be a smaller number of rows based on the allowed keyword parameters
:distinct and :filter as in:

    (query (:select (:count 'memid :distinct) :from 'cd.bookings))

  or

    (query (:select (:as (:count '* :distinct) 'unfiltered)
              (:as (:count '* :filter (:= 1 'bid)) 'filtered)
              :from 'testtable))

Note that if used, the filter must be last in the count args. If distinct is
used, it must come before filter. Unlike standard sql, the word 'where' is not
used inside the filter clause. E.g.

    (sql (:select (:count '*) (:count '* :filter (:= 1 'bid)) 'id
          :from 'pbbench-history))

See tests.lisp for examples."
  (split-on-keywords ((vars *) (distinct - ?)  (filter * ?)) (cons :vars args)
    `("COUNT("
      ,@(when distinct '("DISTINCT "))
      ,@(sql-expand-list vars)
      ,@(when filter `(") FILTER (WHERE " ,@(sql-expand (car filter))))
      ")")))

(def-sql-op :avg (&rest args)
  "Avg calculates the average value of a list of values. Allowed keyword
parameters are distinct and filter. Note that if the filter keyword is used,
the filter must be last in the avg args. If distinct is used, it must come
before filter. Unlike standard sql, the word 'where' is not used inside the
filter clause (s-sql will properly expand it). E.g.

    (query (:select (:avg '*) (:avg '* :filter (:= 1 'bid)) 'id
            :from 'pbbench-history))

See tests.lisp for examples."
  (split-on-keywords ((vars *) (distinct - ?)  (filter * ?)) (cons :vars args)
    `("AVG("
      ,@(when distinct '("DISTINCT "))
      ,@(sql-expand-list vars)
      ,@(when filter `(") FILTER (WHERE " ,@(sql-expand (car filter))))")")))

(def-sql-op :sum (&rest args)
  "Sum calculates the total of a list of values.  Allowed keyword parameters
are distinct and filter. Note that if the keyword filter is used, the filter
must be last in the sum args. If distinct is used, it must come before filter.
Unlike standard sql, the word 'where' is not used inside the filter
clause (s-sql will properly expand it). E.g.

    (query (:select (:sum '*) (:sum '* :filter (:= 1 'bid)) 'id
            :from 'pbbench-history))

See tests.lisp for examples."
  (split-on-keywords ((vars *) (distinct - ?)  (filter * ?)) (cons :vars args)
    `("SUM("
      ,@(when distinct '("DISTINCT "))
      ,@(sql-expand-list vars)
      ,@(when filter `(") FILTER (WHERE " ,@(sql-expand (car filter))))")")))

(def-sql-op :max (&rest args)
  "Max returns the maximum value of a set of values.  Allowed keyword parameters
are distinct and filter. Note that if the filter keyword is used, the filter must
be last in the max args. If distinct is used, it must come before filter. Unlike
standard sql, the word 'where' is not used inside the filter clause (s-sql will
properly expand it). E.g.

    (query (:select (:max '*) (:max '* :filter (:= 1 'bid)) 'id
            :from 'pbbench-history))

See tests.lisp for more examples."
  (split-on-keywords ((vars *) (distinct - ?)  (filter * ?)) (cons :vars args)
    `("MAX("
      ,@(when distinct '("DISTINCT "))
      ,@(sql-expand-list vars)
      ,@(when filter `(") FILTER (WHERE " ,@(sql-expand (car filter))))")")))

(def-sql-op :min (&rest args)
  "Returns the minimum value of a set of values.  Allowed keyword parameters
are distinct and filter. Note that if the filter keyword is used, the filter
must be last in the min args. If distinct is used, it must come before filter.
Unlike standard sql, the word 'where' is not used inside the filter
clause (s-sql will properly expand it). E.g.

    (query (:select (:min '*) (:min '* :filter (:= 1 'bid)) 'id
            :from 'pbbench-history))

See tests.lisp for more examples."
  (split-on-keywords ((vars *) (distinct - ?)  (filter * ?)) (cons :vars args)
    `("MIN("
      ,@(when distinct '("DISTINCT "))
      ,@(sql-expand-list vars)
      ,@(when filter `(") FILTER (WHERE " ,@(sql-expand (car filter))))")")))

(def-sql-op :every (&rest args)
  "Returns true if all input values are true, otherwise false.  Allowed
keyword parameters are distinct and filter. Note that if the filter keyword is
used, the filter must be last in the every args. If distinct is used, it must
come before filter. Unlike standard sql, the word 'where' is not used inside
the filter clause (s-sql will properly expand it). E.g.

    (query (:select '* (:every (:like 'studname \"%h\"))
            :from 'tbl-students
            :group-by 'studname 'studid 'studgrades))

See tests.lisp for examples."
  (split-on-keywords ((vars *) (distinct - ?)  (filter * ?)) (cons :vars args)
    `("EVERY("
      ,@(when distinct '("DISTINCT "))
      ,@(sql-expand-list vars)
      ,@(when filter `(") FILTER (WHERE " ,@(sql-expand (car filter))))")")))

(def-sql-op :percentile-cont (&rest args)
  "Requires Postgresql 9.4 or higher. Percentile-cont returns a value
corresponding to the specified fraction in the ordering, interpolating between
adjacent input items if needed. There are two required keyword parameters
:fraction and :order-by. If the fraction value is an array, then it returns an
array of results matching the shape of the fractions parameter, with each
non-null element replaced by the value corresponding to that percentile.

Examples:

    (query (:select (:percentile-cont :fraction 0.5 :order-by 'number-of-staff)
                    :from 'schools))
    (query (:select (:percentile-cont :fraction array[0.25 0.5 0.75 1]
                    :order-by 'number-of-staff)
                    :from  'schools))"
  (split-on-keywords ((fraction *) (order-by * )) args
    `("PERCENTILE_CONT"
      ,@(when fraction `(,(format nil "~a" fraction)))
      ,@(when order-by `(" WITHIN GROUP (ORDER BY " ,@(sql-expand-list order-by)
                                                    ")")))))

(def-sql-op :percentile-dist (&rest args)
  "Requires Postgresql 9.4 or higher. There are two required keyword parameters
:fraction and :order-by. Percentile-dist returns the first input value whose
position in the ordering equals or exceeds the specified fraction. If the
fraction parameter is an array eturns an array of results matching the shape
of the fractions parameter, with each non-null element replaced by the input
value corresponding to that percentile.

Examples:

   (query (:select (:percentile-dist :fraction 0.5 :order-by 'number-of-staff)
                    :from 'schools))

    (query (:select (:percentile-dist :fraction array[0.25 0.5 0.75 1]
                                      :order-by 'number-of-staff)
            :from  'schools))"

  (split-on-keywords ((fraction *)  (order-by * )) args
    `("PERCENTILE_DIST"
      ,@(when fraction `(,(format nil "~a" fraction)))
      ,@(when order-by `(" WITHIN GROUP (ORDER BY " ,@(sql-expand-list order-by)
                                                    ")")))))

(def-sql-op :corr (y x)
  "The corr function returns the correlation coefficient between a set of
dependent and independent variables.

Example:

    (query (:select (:corr 'height 'weight) :from 'people))"

  `("CORR(" ,@ (sql-expand y) " , " ,@ (sql-expand x) ")"))

(def-sql-op :covar-pop (y x)
  "The covar-pop function returns the population covariance between a set of
dependent and independent variables.

Example:

    (query (:select (:covar-pop 'height 'weight) :from 'people))"

  `("COVAR_POP(" ,@(sql-expand y) " , " ,@(sql-expand x) ")"))

(def-sql-op :covar-samp (y x)
  "The covar-samp function returns the sample covariance between a set of
dependent and independent variables.

Example:

    (query (:select (:covar-samp 'height 'weight) :from 'people))"

  `("COVAR_SAMP(" ,@(sql-expand y) " , " ,@(sql-expand x) ")"))

(def-sql-op :between (n start end)
  `("(" ,@(sql-expand n) " BETWEEN " ,@(sql-expand start) " AND "
        ,@(sql-expand end) ")"))

(def-sql-op :between-symmetric (n start end)
  `("(" ,@(sql-expand n) " BETWEEN SYMMETRIC " ,@(sql-expand start) " AND "
        ,@(sql-expand end) ")"))

(def-sql-op :case (&rest clauses)
  `("CASE"
    ,@(loop :for (test expr) :in clauses
            :if (eql test :else)
              :append `(" ELSE " ,@(sql-expand expr))
            :else
              :append `(" WHEN " ,@(sql-expand test) " THEN "
                                 ,@(sql-expand expr))
            :end)
    " END"))

;; This one has two interfaces. When the elements are known at
;; compile-time, they can be given as multiple arguments to the
;; operator. When they are not, a single argument that evaulates to a
;; list should be used.
(def-sql-op :set (&rest elements)
  (if (not elements)
      '("(NULL)")
      (let ((expanded (sql-expand-list elements)))
        ;; Ugly way to check if everything was expanded
        (if (stringp (car expanded))
            `("(" ,@expanded ")")
            `("(" (let ((elements ,(car elements)))
                    (if (null elements) "NULL"
                        (implode ", " (mapcar 'sql-escape elements)))) ")")))))

(def-sql-op :empty-set ()
  "Returns a list containing a string of two parentheses as an empty set."
  `("()"))

(def-sql-op :dot (&rest args)
  (sql-expand-list args "."))

(def-sql-op :type (value type)
  "Type will specify the datatype for a value. It uses the normal sql :: syntax.
The type can be quoted but does not need to be quoted. It does not accept
variables. As an example:

  (query (:select (:type \"2018-01-01\" 'date)) :single)
  (query (:select (:type \"2018-01-01\" date)) :single)"

  `(,@(sql-expand value) "::" ,(to-type-name (dequote type))))

(def-sql-op :raw (sql)
  (list sql))

;; Selecting and manipulating

(defun expand-joins (args)
  "Helper for the select operator. Turns the part following :from into
the proper SQL syntax for joining tables."
  (labels ((expand-join (natural-p)
             (let ((type (first args)) (table (second args)) kind param ordinality-as)
               (unless (or table
                           (eq type :with-ordinality))
                 (sql-error "Incomplete join clause in select."))
               (setf args (cddr args))
               (unless (or natural-p (eq type :cross-join) (eq type :lateral))
                 (setf kind (pop args))
                 (unless (or (not (is-join kind))
                             (and (or (eq kind :with-ordinality)
                                      (eq kind :with-ordinality-as)
                                      (eq kind :on)
                                      (eq kind :lateral)
                                      (eq kind :using))
                                  args))
                  (sql-error "Incorrect join form in select."))
                 (setf param (pop args)))
               `(" " ,@(when natural-p '("NATURAL "))
                     ,(ecase type
                        (:lateral ", LATERAL ")
                        (:join "JOIN ")
                        (:left-join "LEFT JOIN ")
                        (:right-join "RIGHT JOIN ")
                        (:inner-join "INNER JOIN ")
                        (:outer-join "FULL OUTER JOIN ")
                        (:cross-join "CROSS JOIN ")
                        (:join-lateral "JOIN LATERAL ")
                        (:left-join-lateral "LEFT JOIN LATERAL ")
                        (:right-join-lateral "RIGHT JOIN LATERAL ")
                        (:inner-join-lateral "INNER JOIN LATERAL ")
                        (:outer-join-lateral "FULL OUTER JOIN LATERAL ")
                        (:cross-join-lateral "CROSS JOIN LATERAL ")
                        (:with-ordinality "WITH ORDINALITY ")
                        (:with-ordinality-as "WITH ORDINALITY AS "))
                    ,@(when table;(not (eq type :with-ordinality))
                        (sql-expand table))
                    ,@(unless (or natural-p (eq type :cross-join))
                         `("" ,@(if (eq kind :with-ordinality)
                             (progn (setf kind param)
                             (setf param (pop args))
                             `(" WITH ORDINALITY " )))
                         ,@(when (eq kind :with-ordinality-as)
                             (setf ordinality-as param)
                             (setf kind (pop args))
                             (setf param (pop args))
                             `(" WITH ORDINALITY AS " . ,(sql-expand ordinality-as)))
                         ,@(when (eq kind :on)
                              `(" ON " . ,(sql-expand param)))
                         ,@(when (eq kind :using)
                            `(" USING (" ,@(sql-expand-list param) ")")))))))
           (is-join (x)
             (member x '(:join :left-join :right-join :inner-join :outer-join
                         :cross-join :join-lateral :left-join-lateral :right-join-lateral
                         :inner-join-lateral :outer-join-lateral
                         :cross-join-lateral :with-ordinality :with-ordinality-as
                         :lateral))))
    (when (null args)
      (sql-error "Empty :from clause in select"))
    (loop :for first = t :then nil :while args
          :append (cond ((is-join (car args))
                         (when first
                           (sql-error ":from clause starts with a join."))
                         (expand-join nil))
                        ((eq (car args) :natural)
                         (when first
                           (sql-error ":from clause starts with a join."))
                         (pop args)
                         (expand-join t))
                        (t
                         `(,@(if first () '(", ")) ,@(sql-expand (pop args))))))))


(def-sql-op :select (&rest args)
  "Creates a select query. The arguments are split on the keywords found among
them. The group of arguments immediately after :select is interpreted as
the expressions that should be selected. After this, an optional :distinct
may follow, which will cause the query to only select distinct rows, or
alternatively :distinct-on followed by a group of row names. Next comes the
optional keyword :from, followed by at least one table name and then any
number of join statements.

Join statements start with one of :left-join,
:right-join, :inner-join, :outer-join, :cross-join (or those with -lateral,
e.g :left-join-lateral, :right-join-lateral, :inner-join-lateral, :outer-join-lateral).
S-sql will not accept :join, use :inner-join instead.

Then comes a table name or subquery,

then there is an optional :with-ordinality or :with-ordinality-as alisa

Then the keyword :on or :using, if applicable, and then a form.
A join can be preceded by :natural (leaving off the :on clause) to use a
natural join.

After the joins an optional :where followed by a single form may occur.

And finally :group-by and :having can optionally be specified.
The first takes any number of arguments, and the second only one.

A few examples:

    (query (:select (:+ 'field-1 100) 'field-5
            :from (:as 'my-table 'x)
            :left-join 'your-table
            :on (:= 'x.field-2 'your-table.field-1)
            :where (:not-null 'a.field-3)))

    (query (:select 'i.* 'p.*
            :from (:as 'individual 'i)
            :inner-join (:as 'publisher 'p)
            :using ('individualid)
            :left-join-lateral (:as 'anothertable 'a)
            :on (:= 'a.identifier 'i.individualid)
            :where (:= 'a.something \"something\")))

    (query (:select 't1.id 'a.elem 'a.nr
            :from (:as 't12 't1)
            :left-join (:unnest (:string-to-array 't1.elements \",\"))
            :with-ordinality-as (:a 'elem 'nr)
            :on 't))"
  (split-on-keywords ((vars *) (distinct - ?) (distinct-on * ?) (from * ?)
                      (where ?) (group-by * ?) (having ?) (window ?))
      (cons :vars args)
    `("(SELECT "
      ,@(if distinct '("DISTINCT "))
      ,@(if distinct-on `("DISTINCT ON (" ,@(sql-expand-list distinct-on) ") "))
      ,@(sql-expand-list vars)
      ,@(if from (cons " FROM " (expand-joins from)))
      ,@(if where (cons " WHERE " (sql-expand (car where))))
      ,@(if group-by (cons " GROUP BY " (sql-expand-list group-by)))
      ,@(if having (cons " HAVING " (sql-expand (car having))))
      ,@(if window (cons " WINDOW " (sql-expand-list window)))
      ")")))

(def-sql-op :grouping-sets (&rest args)
  "Grouping-sets allows multiple group-by in a single query
https://www.postgresql.org/docs/current/static/queries-table-expressions.html
More complex grouping operations are possible using the concept of grouping
sets. The data selected by the FROM and WHERE clauses is grouped separately
by each specified grouping set, aggregates computed for each group just as
for simple GROUP BY clauses, and then the results returned.
This operator requires postgresql 9.5 or later. Examples:

    (query (:select 'city (:as (:extract 'year 'start-date)  'joining-year)
                    (:as (:count 1) 'employee_count)
            :from 'employee
            :group-by (:grouping-sets (:set 'city (:extract 'year 'start-date)))))

    (query (:select 'c1 'c2 'c3 (:sum 'c3)
            :from 'table-name
            :group-by (:grouping-sets (:set 'c1 'c2) (:set 'c1) (:set 'c2)
                                      (:set))))"

  `("GROUPING SETS ",@(sql-expand-list args) ))

(def-sql-op :string-agg (&rest args)
  "String-agg allows you to concatenate strings using different types of
delimiter symbols. Allowable optional keyword parameters are :distinct,
:order-by and :filter

Examples:

    (query (:select (:as (:string-agg 'bp.step-type \",\" ) 'step-summary)
            :from 'business-process))

    (query (:select 'mid (:as (:string-agg  'y \",\" :distinct
                               :order-by (:desc 'y))
                          'words)
            :from 'moves))

    (query (:select (:string-agg  'name \",\"
                                  :order-by (:desc 'name)
                                  :filter (:< 'id 4))
            :from 'employee))

Note that order-by in string-agg requires postgresql 9.0 or later. Filter
requires postgresql 9.4 or later.See tests.lisp for examples."
  (split-on-keywords ((vars *) (distinct - ?)  (order-by * ?)(filter * ?))
                     (cons :vars args)
    `("STRING_AGG("
      ,@(when distinct '("DISTINCT "))
      ,@(sql-expand-list vars)
      ,@(when order-by `(" ORDER BY " ,@(sql-expand-list order-by)))
      ,@(when filter `(") FILTER (WHERE " ,@(sql-expand (car filter))))
      ")")))

(def-sql-op :array-agg (&rest args)
  "Array-agg returns a list of values concatenated into an array.
Allowable optional keyword parameters are :distinct, :order-by and :filter.

Example:
  (query (:select 'g.id
                  (:as (:array-agg 'g.users :filter (:= 'g.canonical \"Y\"))
                       'canonical-users)
                  (:as (:array-agg 'g.users :filter (:= 'g.canonical \"N\"))
                       'non-canonical-users)
                  :from (:as 'groups 'g)
                  :group-by 'g.id)

Note that order-by in array-agg requires postgresql 9.0 or later.
Filter requires postgresql 9.4 or later. See tests.lisp for examples."
  (split-on-keywords ((vars *) (distinct - ?)(order-by * ?) (filter * ?))
                     (cons :vars args)
    `("ARRAY_AGG("
      ,@(when distinct '("DISTINCT "))
      ,@(sql-expand-list vars)
      ,@(when order-by `(" ORDER BY " ,@(sql-expand-list order-by)))
      ,@(when filter `(") FILTER (WHERE " ,@(sql-expand (car filter))))
      ")")))

(def-sql-op :mode (&rest args)
  "Mode is used to find the most frequent input value in a group. See e.g.
https://www.postgresql.org/docs/10/static/functions-aggregate.html
and article at
https://tapoueh.org/blog/2017/11/the-mode-ordered-set-aggregate-function/."
  (split-on-keywords ((vars *)) (cons :vars args)
    `("mode() within group (order by " ,@(sql-expand-list vars) ")")))


(def-sql-op :regr-avgx (y x)
  "The regr-avgx function returns the average of the independent
variable (sum(X)/N)

Example:

    (query (:select (:regr-avgx 'height 'weight) :from 'people))"

  `("REGR_AVGX(",@(sql-expand y) " , " ,@(sql-expand x) ")"))

(def-sql-op :regr-avgy (y x)
  "The regr-avgy function returns the average of the dependent
variable (sum(Y)/N).

Example:

    (query (:select (:regr-avgy 'height 'weight) :from 'people))"
  `("REGR_AVGY(" ,@(sql-expand y) " , " ,@(sql-expand x) ")"))

(def-sql-op :regr-count (y x)
  "The regr-count function returns the 	number of input rows in which both
expressions are nonnull.

Example:

    (query (:select (:regr-count 'height 'weight) :from 'people))"

  `("REGR_COUNT(",@(sql-expand y) " , " ,@(sql-expand x) ")"))

(def-sql-op :regr-intercept (y x)
  "The regr-intercept function returns the y-intercept of the least-squares-fit
linear equation determined by the (X, Y) pairs.

Example:

    (query (:select (:regr-intercept 'height 'weight) :from 'people))"

  `("REGR_INTERCEPT(",@(sql-expand y) " , " ,@(sql-expand x) ")"))

(def-sql-op :regr-r2 (y x)
  "The regr-r2 function returns the square of the correlation coefficient.

Example:

    (query (:select (:regr-r2 'height 'weight) :from 'people))"

  `("REGR_R2(",@(sql-expand y) " , " ,@(sql-expand x) ")"))

(def-sql-op :regr-slope (y x)
  "The regr-slope function returns the slope of the least-squares-fit linear
equation determined by the (X, Y) pairs.

Example:

    (query (:select (:regr-slope 'height 'weight) :from 'people))"

  `("REGR_SLOPE(",@(sql-expand y) " , " ,@(sql-expand x) ")"))

(def-sql-op :regr-sxx (y x)
  "The regr-sxx function returns the sum(X^2) - sum(X)^2/N (“sum of squares”
of the independent variable).

Example:

    (query (:select (:regr-sxx 'height 'weight) :from 'people))"

  `("REGR_SXX(",@(sql-expand y) " , " ,@(sql-expand x) ")"))

(def-sql-op :regr-sxy (y x)
  "The regr-sxy function returns the sum(X*Y) - sum(X) * sum(Y)/N
   (\"sum of products\" of independent times dependent variable).

Example:

    (query (:select (:regr-sxy 'height 'weight) :from 'people))"

  `("REGR_SXY(",@(sql-expand y) " , " ,@(sql-expand x) ")"))

(def-sql-op :regr-syy (y x)
  "The regr-syy function returns the sum(Y^2) - sum(Y)^2/N
   (\"sum of squares\" of the dependent variable).

Example:

    (query (:select (:regr-syy 'height 'weight) :from 'people))"

  `("REGR_SYY(",@(sql-expand y) " , " ,@(sql-expand x) ")"))

(def-sql-op :stddev (&rest args)
  "The stddev function returns the the sample standard deviation of the input
values. It is a historical alias for stddev-samp.

Example:

    (query (:select (:stddev 'salary) :from 'people))"

  (split-on-keywords ((vars *)) (cons :vars args)
    `("STDDEV(",@(sql-expand-list vars) ")")))

(def-sql-op :stddev-pop (&rest args)
  "The stddev-pop function returns the population standard deviation of the
input values.

Example:

    (query (:select (:stddev-pop 'salary) :from 'people))"

  (split-on-keywords ((vars *)) (cons :vars args)
    `("STDDEV_POP(",@(sql-expand-list vars) ")")))

(def-sql-op :stddev-samp (&rest args)
  "The stddev-samp function returns the sample standard deviation of the input
values.

Example:

    (query (:select (:stddev-samp 'salary) :from 'people))"

  (split-on-keywords ((vars *)) (cons :vars args)
    `("STDDEV_SAMP(",@(sql-expand-list vars) ")")))

(def-sql-op :variance (&rest args)
  "Variance is a historical alias for var_samp. The variance function returns
the sample variance of the input values (square of the sample standard deviation).

Example:

    (query (:select (:variance 'salary) :from 'people))"

  (split-on-keywords ((vars *)) (cons :vars args)
    `("VARIANCE(",@(sql-expand-list vars) ")")))

(def-sql-op :var-pop (&rest args)
  "The var-pop function returns the population variance of the input values
   (square of the population standard deviation).

Example:

    (query (:select (:var-pop 'salary) :from 'people))"

  (split-on-keywords ((vars *)) (cons :vars args)
    `("VAR_POP(",@(sql-expand-list vars) ")")))

(def-sql-op :var-samp (&rest args)
  "The var-samp function returns the sample variance of the input values
   (square of the sample standard deviation).

Example:

    (query (:select (:var-samp 'salary) :from 'people))"

  (split-on-keywords ((vars *)) (cons :vars args)
    `("VAR_SAMP(",@(sql-expand-list vars) ")")))

(def-sql-op :fetch (form &optional amount offset)
  "Fetch can be a more efficient way to do pagination instead of using limit and
offset. Fetch allows you to retrieve a limited set of rows, optionally offset
by a specified number of rows. In order to ensure this works correctly, you
should use the order-by clause. If the amount is not provided, it assumes
you only want to return 1 row.
https://www.postgresql.org/docs/current/sql-select.html

Examples:

    (query (:fetch (:order-by (:select 'id :from 'historical-events) 'id) 5))

    ((1) (2) (3) (4) (5))

    (query (:fetch (:order-by (:select 'id :from 'historical-events) 'id) 5 10))

    ((11) (12) (13) (14) (15))"
  `("(" ,@(sql-expand form)
        ,@(if offset (cons " OFFSET " (sql-expand offset)) ())
        " FETCH FIRST " ,@(if amount (sql-expand amount)
                                            (list ""))
        " ROWS ONLY)"))

(def-sql-op :limit (form amount &optional offset)
  `("(" ,@(sql-expand form) " LIMIT " ,@(if amount (sql-expand amount)
                                            (list "ALL"))
        ,@(if offset (cons " OFFSET " (sql-expand offset)) ())
        ")"))

(def-sql-op :order-by (form &rest fields)
  (if fields
      `("(" ,@(sql-expand form) " ORDER BY " ,@(sql-expand-list fields) ")")
      `("( ORDER BY " ,@(sql-expand form) ")")))

(def-sql-op :set-constraints (state &rest constraints)
  `("SET CONSTRAINTS " ,@(if constraints
                             (sql-expand-list constraints)
                             '("ALL"))
                       ,(ecase state
                          (:deferred " DEFERRED")
                          (:immediate " IMMEDIATE"))))

(defun for-update/share (share-or-update form &rest args)
  (let* ((of-position (position :of args))
         (no-wait-position (position :nowait args))
         (of-tables (when of-position (subseq args (1+ of-position)
                                              no-wait-position))))
    `("(" ,@(sql-expand form) ,(format nil " FOR ~:@(~A~)" share-or-update)
          ,@(when of-tables (list (format nil " OF ~{~A~^, ~}"
                                          (mapcar #'sql-compile of-tables))))
          ,@(when no-wait-position (list " NOWAIT"))
          ")")))

(def-sql-op :for-update (form &rest args)
  (apply #'for-update/share "UPDATE" form args))

(def-sql-op :for-share (form &rest args)
  (apply #'for-update/share "SHARE" form args))

(defun escape-sql-expression (expr)
  "Try to escape an expression at compile-time, if not possible, delay
to runtime. Used to create stored procedures."
  (let ((expanded (append (sql-expand expr) '(";"))))
    (if (every 'stringp expanded)
        (sql-escape-string (apply 'concatenate 'string expanded))
        `(sql-escape-string (concatenate 'string ,@(reduce-strings expanded))))))

(def-sql-op :function (name (&rest args) return-type stability body)
  (assert (member stability '(:immutable :stable :volatile)))
  `("CREATE OR REPLACE FUNCTION " ,@(sql-expand name) " ("
                                  ,(implode ", "
                                            (mapcar 'to-type-name args))
                                  ") RETURNS " ,(to-type-name return-type)
                                  " LANGUAGE SQL "
                                  ,(symbol-name stability) " AS "
                                  ,(escape-sql-expression body)))

(def-sql-op :insert-into (table &rest rest)
  (split-on-keywords ((method *)
                      (overriding-system-value ? -)
                      (overriding-user-value ? -)
                      (on-conflict-do-nothing ? -)
                      (on-conflict ? *)
                      (on-conflict-on-constraint ? *)
                      (on-conflict-update ? *)
                      (do-nothing ? -)
                      (update-set ? *)
                      (from * ?)
                      (where ?) (returning ? *))
      (cons :method rest)

    `("INSERT INTO " ,@(sql-expand table) " "
                     ,@(cond
                         ((eq (car method) :set)
                          (cond ((oddp (length (cdr method)))
                                 (sql-error "Invalid amount of :set arguments passed to insert-into sql operator"))
                                ((null (cdr method)) '("DEFAULT VALUES"))
                                (t `("(" ,@(sql-expand-list
                                            (loop :for (field nil)
                                                    :on (cdr method)
                                                  :by #'cddr
                                                  :collect field))
                                         ") "
                                         ,@(cond (overriding-system-value
                                                  '(" OVERRIDING SYSTEM VALUE "))
                                                 (overriding-user-value
                                                  '(" OVERRIDING USER VALUE ")))
                                         " VALUES (" ,@(sql-expand-list
                                                        (loop :for (nil value)
                                                                :on (cdr method)
                                                              :by #'cddr
                                                              :collect value))
                                         ")"))))
                         ((eq (car method) :columns)
                          `(" (" ,@(sql-expand-list (butlast (cdr method))) ") "
                                 ,@(sql-expand (car (last method)))))
                         ((and (not (cdr method)) (consp (car method))
                               (keywordp (caar method)))
                          (sql-expand (car method)))
                         (t (sql-error "No :set arguments or select operator passed to insert-into sql operator")))

                     ,@(when on-conflict-do-nothing
                         `(" ON CONFLICT"
                           ,@(if where (cons " WHERE " (sql-expand (car where))))
                           " DO NOTHING"))
                     ,@(when on-conflict
                         `(" ON CONFLICT ("
                           ,@(sql-expand-list on-conflict)
                           ")"
                           ,@(if where (cons " WHERE " (sql-expand (car where))))))
                     ,@(when on-conflict-on-constraint
                         `(" ON CONFLICT ON CONSTRAINT "
                           ,@(sql-expand-list on-conflict-on-constraint)))
                     ,@(when do-nothing
                         '(" DO NOTHING "))
                     ,@(when on-conflict-update
                         `(" ON CONFLICT ("
                           ,@(sql-expand-list on-conflict-update)
                           ") DO UPDATE SET "
                           ,@(loop :for (field value) :on update-set :by #'cddr
                                   :for first = t :then nil
                                   :append `(,@(if first () '(", "))
                                             ,@(sql-expand field) " = "
                                             ,@(sql-expand value)))
                           ,@(if from (cons " FROM " (expand-joins from)))
                           ,@(if where (cons " WHERE " (sql-expand (car where)))
                                 ())))
                     ,@(when (and update-set (not on-conflict-update))
                         `(" DO UPDATE SET "
                           ,@(loop :for (field value) :on update-set :by #'cddr
                                   :for first = t :then nil
                                   :append `(,@(if first () '(", "))
                                             ,@(sql-expand field) " = "
                                             ,@(sql-expand value)))
                           ,@(if from (cons " FROM " (expand-joins from)))
                           ,@(if where (cons " WHERE " (sql-expand (car where)))
                                 ())))
                     ,@(when returning
                         `(" RETURNING " ,@(sql-expand-list returning))))))

(def-sql-op :listen (channel)
  `("LISTEN " ,@(sql-expand channel)))

(def-sql-op :unlisten (channel)
  `("UNLISTEN " ,@(sql-expand channel)))

(def-sql-op :notify (channel &optional payload)
  `("NOTIFY " ,@(sql-expand channel)
              ,@(when payload
                  (list ", " (sql-escape-string payload)))))

(defun expand-rows (rows length)
  (unless rows (sql-error "Running :insert-rows-into without data."))
  (unless length (setf length (length (car rows))))
  (let ((*expand-runtime* t))
    (strcat
     (loop :for row :in rows :for first := t :then nil
           :when (/= (length row) length)
             :do (sql-error "Found rows of unequal length in :insert-rows-into.")
           :append `(,@(unless first '(", "))
                     "("
                     ,@(sql-expand-list row)
                     ")")))))

(def-sql-op :insert-rows-into (table &rest rest)
  (split-on-keywords ((columns ? *)
                      (overriding-system-value ? -)
                      (overriding-user-value ? -)
                      (values)
                      (on-conflict-do-nothing ? -)
                      (on-conflict ? *)
                      (on-conflict-on-constraint ? *)
                      (on-conflict-update ? *)
                      (do-nothing ? -)
                      (update-set ? *)
                      (from * ?) (where ?) (returning ? *))
                     rest
    `("INSERT INTO "
      ,@(sql-expand table) " "
      ,@(when columns `("(" ,@(sql-expand-list columns) ") "))
      ,@(cond (overriding-system-value
               '(" OVERRIDING SYSTEM VALUE "))
              (overriding-user-value
               '(" OVERRIDING USER VALUE ")))
      "VALUES "
      ,(if *expand-runtime*
           (expand-rows (car values) (and columns (length columns)))
           `(expand-rows ,(car values) ,(and columns (length columns))))
      ,@(when on-conflict-do-nothing
          `(" ON CONFLICT "
            ,@(if where (cons " WHERE " (sql-expand (car where))))
            " DO NOTHING"))
      ,@(when on-conflict
          `(" ON CONFLICT ("
            ,@(sql-expand-list on-conflict)
            ")"
            ,@(if where (cons " WHERE " (sql-expand (car where))))))
      ,@(when on-conflict-on-constraint
          `(" ON CONFLICT ON CONSTRAINT "
            ,@(sql-expand-list on-conflict-on-constraint)))
      ,@(when do-nothing
          '(" DO NOTHING "))
      ,@(when on-conflict-update
          `(" ON CONFLICT ("
            ,@(sql-expand-list on-conflict-update)
            ") DO UPDATE SET "
            ,@(loop :for (field value) :on update-set :by #'cddr
                    :for first = t :then nil
                    :append `(,@(if first () '(", "))
                              ,@(sql-expand field) " = "
                              ,@(sql-expand value)))
            ,@(if from (cons " FROM " (expand-joins from)))
            ,@(if where (cons " WHERE " (sql-expand (car where)))
                  ())))
      ,@(when (and update-set (not on-conflict-update))
          `(" DO UPDATE SET "
            ,@(loop :for (field value) :on update-set :by #'cddr
                    :for first = t :then nil
                    :append `(,@(if first () '(", "))
                              ,@(sql-expand field) " = "
                              ,@(sql-expand value)))
            ,@(if from (cons " FROM " (expand-joins from)))
            ,@(if where (cons " WHERE " (sql-expand (car where)))
                ())))
      ,@(when returning `(" RETURNING " ,@(sql-expand-list returning))))))

(def-sql-op :update (table &rest args)
  (split-on-keywords ((set * ?) (columns ? *) (from * ?) (where ?) (returning ? *)) args
    (when (oddp (length set))
      (sql-error "Invalid amount of :set arguments passed to update sql operator"))
    `("UPDATE " ,@(sql-expand table)
                ,@(when columns `(" SET  (" ,@(sql-expand-list (butlast columns)) ") = "
                    ,@(sql-expand (car (last columns)))))
                ,@(when (and set (not columns)) (list " SET "))
                ,@(when (and set (not columns)) (loop :for (field value) :on set :by #'cddr
                        :for first = t :then nil
                        :append `(,@(if first () '(", ")) ,@(sql-expand field)
                                  " = "
                                  ,@(sql-expand value))))
                ,@(if from (cons " FROM " (expand-joins from)))
                ,@(if where (cons " WHERE " (sql-expand (car where))) ())
                ,@(when returning
                    (cons " RETURNING " (sql-expand-list returning))))))


(def-sql-op :delete-from (table &rest args)
  (split-on-keywords ((where ?) (returning ? *)) args
    `("DELETE FROM " ,@(sql-expand table)
                     ,@(when where (cons " WHERE " (sql-expand (car where))))
                     ,@(when returning (cons " RETURNING "
                                             (sql-expand-list returning))))))

(def-sql-op :range-between (&rest args)
  "Range-between allows window functions to apply to different segments of a result set.
It accepts the following keywords:
:order-by, :rows-between, :range-between, :unbounded-preceding,
:current-row and :unbounded-following. Use of :preceding or :following will generate errors.
See https://www.postgresql.org/docs/current/sql-expressions.html#SYNTAX-WINDOW-FUNCTIONS for Postgresql documentation on usage.

An example which calculates a running total could look like this :

    (query
     (:select (:as 'country 'country-name)
              (:as 'population 'country-population)
              (:as (:over (:sum 'population)
                          (:range-between :order-by 'country :unbounded-preceding :current-row))
                   'global-population)
      :from 'population
      :where (:and (:not-null 'iso2)
                   (:= 'year 1976))))"
  (split-on-keywords ((order-by *) (preceding ? *)(unbounded-preceding ? -)
                      (current-row ? -) (unbounded-following ? -)(following ? *))
      args
    `("(ORDER BY ",@(sql-expand-list order-by)
                  " RANGE BETWEEN "
                  ,@(when unbounded-preceding (list "UNBOUNDED PRECEDING AND "))
                  ,(when preceding
                    (sql-error (format nil ":range-between cannot use :preceding ~a. Use :rows-between instead." preceding)))
                  ,@(when current-row (list " CURRENT ROW "))
                  ,@(when unbounded-following
                      (if current-row
                          (list "AND UNBOUNDED FOLLOWING ")
                          (list "UNBOUNDED FOLLOWING ")))
                  ,(when following
                    (sql-error (format nil ":range-between cannot use :following ~a. Use :rows-between instead." following)))
                  ")")))

(def-sql-op :rows-between (&rest args)
  "Rows-between allows window functions to apply to different segments of a result set.
It accepts the following keywords:
:order-by, :rows-between, :range-between, :preceding, :unbounded-preceding,
:current-row, :unbounded-following and :following. See https://www.postgresql.org/docs/current/sql-expressions.html#SYNTAX-WINDOW-FUNCTIONS for Postgresql documentation on usage.

An example could look like this :

    (query
     (:select (:as 'country 'country-name)
              (:as 'population 'country-population)
              (:as (:over (:sum 'population)
                          (:rows-between :order-by 'country :preceding 2 :following 2))
                   'global-population)
      :from 'population
      :where (:and (:not-null 'iso2)
                   (:= 'year 1976))))"
  (split-on-keywords ((order-by ? *) (preceding ? *) (unbounded-preceding ? -)
                      (current-row ? -) (unbounded-following ? -) (following ? *))
      args
    `(,@(when order-by (cons "(ORDER BY " (sql-expand-list order-by)))
         " ROWS BETWEEN "
         ,@(when unbounded-preceding (list "UNBOUNDED PRECEDING AND "))
         ,@(when preceding   (list (format nil "~a" (car preceding)) " PRECEDING AND "))
         ,@(when current-row (list " CURRENT ROW "))
         ,@(when unbounded-following
             (if unbounded-preceding
                 (list "UNBOUNDED FOLLOWING ")
                 (list "AND UNBOUNDED FOLLOWING ")))
         ,@(when following
             (if current-row
                 (list (format nil "AND ~a" (car following)) " FOLLOWING ")
                 (list (format nil "~a" (car following)) " FOLLOWING ")))
         ")")))

(def-sql-op :over (form &rest args)
  "Over allows functions to apply to a result set, creating an additional column.
A simple example of usage would be:

  (query (:select 'salary (:over (:sum 'salary))
                :from 'empsalary))

A more complicated version using the :range-between operator could look like this:
  (query (:limit
             (:select (:as 'country 'country-name)
                      (:as 'population 'country-population)
                      (:as (:over (:sum 'population)
                                  (:range-between :order-by 'country :unbounded-preceding
                                   :unbounded-following))
                           'global-population)
                      :from 'population
                      :where (:and (:not-null 'iso2)
                                   (:= 'year 1976)))
             5))"
  (if args `("(" ,@(sql-expand form) " OVER " ,@(sql-expand-list args) ")")
      `("(" ,@(sql-expand form) " OVER ()) ")))

(def-sql-op :partition-by (&rest args)
  "Partition-by allows aggregate or window functions to apply separately to
segments of a result. Partition-by accepts the following keywords:
:order-by, :rows-between, :range-between, :preceding, :unbounded-preceding,
:current-row, :unbounded-following and :following. See https://www.postgresql.org/docs/current/sql-expressions.html#SYNTAX-WINDOW-FUNCTIONS for Postgresql documentation on usage.

Example:
       (query
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
                       (:= 'year 1976))))"
  (split-on-keywords ((partition-by *)(order-by ? *) (rows-between ? -) (range-between ? -)
                      (preceding ? *) (unbounded-preceding ? -)
                      (current-row ? -) (unbounded-following ? -) (following ? *))
      (cons :partition-by args)
    `("(PARTITION BY " ,@(sql-expand-list partition-by)
                       ,@(when order-by
                           (cons " ORDER BY "
                                 (sql-expand-list order-by)))
                       ,@(when rows-between (list " ROWS BETWEEN "))
                       ,@(when range-between (list " RANGE BETWEEN "))
                       ,@(when unbounded-preceding (list "UNBOUNDED PRECEDING AND "))
                       ,(when (and preceding range-between)
                          (sql-error
                           (format nil
                                   ":range-between cannot use :preceding ~a. Use :rows-between."
                                   preceding)))
                       ,@(when (and preceding (not range-between))
                           (list (format nil "~a" (car preceding))
                                 " PRECEDING AND "))
                       ,@(when current-row (list " CURRENT ROW "))
                       ,@(when unbounded-following
                           (if unbounded-preceding
                               (list "UNBOUNDED FOLLOWING ")
                               (list "AND UNBOUNDED FOLLOWING ")))
                       ,(when (and following range-between)
                          (sql-error
                           (format nil
                                   ":range-between cannot use :following ~a. Use :rows-between."
                                   following)))
                       ,@(when (and following (not range-between))
                           (if current-row
                               (list (format nil "AND ~a" (car following)) " FOLLOWING ")
                               (list (format nil "~a" (car following)) " FOLLOWING ")))
                       ")")))

(def-sql-op :parens (op) `(" (" ,@(sql-expand op) ") "))

(def-sql-op :with (&rest args)
  (let ((x (butlast args)) (y (last args)))
    `("WITH " ,@(sql-expand-list x) ,@(sql-expand (car y)))))

(def-sql-op :with-recursive (form1 form2)
  `("WITH RECURSIVE " ,@(sql-expand form1) ,@(sql-expand form2)))

(def-sql-op :window (form)
  `("WINDOW " ,@(sql-expand form)))


;; Data definition

(defun dissect-type (type)
  "Return the type and whether it may be NULL. TYPE may be a list
starting with 'or' containing two, and only two, potential types to
test. "
  (if (and (consp type) (eq (car type) 'or))
      (if (and (member 'db-null type) (= (length type) 3))
          (if (eq (second type) 'db-null)
              (values (third type) t)
              (values (second type) t))
          (sql-error "Invalid type: ~a. 'or' types must have two alternatives,
one of which is ~s."
                     type 'db-null))
      (values type nil)))

(defun expand-interval (option)
  "Provide interval limit options"
  (case option
    (:year '("YEAR"))
    (:month '("MONTH"))
    (:day '("DAY"))
    (:hour '("HOUR"))
    (:minute '("MINUTE"))
    (:second '("SECOND"))
    (:year-to-month '("YEAR TO MONTH"))
    (:day-to-hour '("DAY TO HOUR"))
    (:day-to-minute '("DAY TO MINUTE"))
    (:day-to-second '("DAY TO SECOND"))
    (:hour-to-minute '("HOUR TO MINUTE"))
    (:hour-to-second '("HOUR TO SECOND"))
    (:minute-to-second '("MINUTE TO SECOND"))))

(defun expand-foreign-on* (action)
  (case action
    (:restrict "RESTRICT")
    (:set-null "SET NULL")
    (:set-default "SET DEFAULT")
    (:cascade "CASCADE")
    (:no-action "NO ACTION")
    (t (sql-error "Unsupported action for foreign key: ~A" action))))

(defun %build-foreign-reference (target on-delete on-update match)
  `(" REFERENCES "
    ,@(if (consp target)
          `(,(to-sql-name (car target)) "("
            ,@(sql-expand-names (cdr target)) ")")
          `(,(to-sql-name target)))
    ,(when match (case match
                   (:match-simple " MATCH SIMPLE")
                   (:match-full " MATCH FULL")
                   (:match-partial " MATCH PARTIAL")))
    " ON DELETE " ,(expand-foreign-on* on-delete)
    " ON UPDATE " ,(expand-foreign-on* on-update)))

(defun expand-table-constraint (option args)
  "Process table constraints that precede the closing parentheses in the table
definition for the base level create table.  The difference between this and
the expand-table-constraint-sok function is the parameter list signature. This
expects to receive no sublists. The expand-table-constraint-sok function expects
to list of sublists. This is done to maintain backwards compatibility and most
general users do not need the extended version.

Foreign keys have defaults  on-delete restrict, on-update restrict, and match
simple. If you want to change those defaults, you need to specify them in that
order.

Per the postgresql documentation at
https://www.postgresql.org/docs/10/static/sql-createtable.html

A value inserted into the referencing column(s) is matched against the values
of the referenced table and referenced columns using the given match type.
There are three match types: MATCH FULL, MATCH PARTIAL, and MATCH SIMPLE (which
is the default). MATCH FULL will not allow one column of a multicolumn foreign
key to be null unless all foreign key columns are null; if they are all null,
the row is not required to have a match in the referenced table. MATCH SIMPLE
allows any of the foreign key columns to be null; if any of them are null, the
row is not required to have a match in the referenced table. MATCH PARTIAL is not
yet implemented. (Of course, NOT NULL constraints can be applied to the
referencing column(s) to prevent these cases from arising.)"
  (case option
    (:constraint `("CONSTRAINT " ,(to-sql-name (car args)) " "
                                 ,@(expand-table-constraint (cadr args)
                                                            (cddr args))))
    (:check `("CHECK " ,@(sql-expand (car args))))
    (:primary-key `("PRIMARY KEY (" ,@(sql-expand-names args) ")"))
    (:unique `("UNIQUE (" ,@(sql-expand-names args) ")"))
    (:with `(" WITH " ,@(sql-expand (car args))))
    (:deferrable `("DEFERRABLE "))
    (:not-deferrable `("NOT DEFERRABLE "))
    (:initially-deferred `("INITIALLY DEFERRED "))
    (:initially-immediate `("INITIALLY IMMEDIATE "))
    (:foreign-key
     (destructuring-bind (columns target &optional (on-delete :restrict)
                                           (on-update :restrict)
                                           (match :match-simple))
         args
       `("FOREIGN KEY (" ,@(sql-expand-names columns) ")"
                         ,@(%build-foreign-reference target on-delete on-update
                                                     match))))))

(defun expand-table-constraint-sok (args)
  "Expand-table-constraint for the create-extended-table sql-op. The difference
between the two is the parameter list signature. This expects a list of sublists.
The regular expand-table-constraint expects to receive no sublists.
DOES NOT IMPLEMENT POSTGRESQL FUNCTION EXCLUDE."
  (split-on-keywords ((constraint ? *) (check ? *) (unique ? *) (with ? *)
                      (deferrable ? -) (primary-key ? *) (not-deferrable ? -)
                      (initially-deferred ? -)(initially-immediate ? -)
                      (foreign-key ? *))
      args
    `(,@(when args '(", "))
      ,@(when constraint `("CONSTRAINT " ,(to-sql-name (car constraint)) " "))
      ,@(when check `("CHECK " ,@(sql-expand (car check))))
      ,@(when unique `("UNIQUE (" ,@(sql-expand-names unique) ")"))
      ,@(when with `(" WITH " ,@(sql-expand (car with))))
      ,@(when deferrable `("DEFERRABLE "))
      ,@(when primary-key `("PRIMARY KEY (" ,@(sql-expand-names primary-key)
                                            ") "))
      ,@(when not-deferrable `("NOT DEFERRABLE "))
      ,@(when initially-deferred `("INITIALLY DEFERRED "))
      ,@(when initially-immediate `("INITIALLY IMMEDIATE "))
      ,@(when foreign-key
          (destructuring-bind (columns target &optional (on-delete :restrict)
                                                (on-update :restrict)
                                                (match :match-simple))
              foreign-key
            `("FOREIGN KEY (" ,@(sql-expand-names columns) ")"
                              ,@(%build-foreign-reference target on-delete
                                                          on-update match)))))))

(defun expand-extended-table-constraint (option args)
  "Process table constraints that follow the closing parentheses in the table
definition."
  (case option
    (:distributed-by `(" DISTRIBUTED BY (" ,@(sql-expand-names (car args))") "))
    (:distributed-randomly `(" DISTRIBUTED RANDOMLY "))
    (:with `(" WITH " ,@(sql-expand (car args))))
    (:tablespace `(" TABLESPACE " ,(to-sql-name (car args))))
    (:exclude `(" EXCLUDE USING" ,@(sql-expand (car args)) ,@(sql-expand
                                                              (cdr args))))
    (:partition-by-range `(" PARTITION BY RANGE (" ,@(sql-expand (car args))
                                                   ,(when (cadr args) ", ")
                                                   ,@(when (cadr args)
                                                       (sql-expand (cadr args)))
                                                   ")"))
    (:partition-of `(" PARTITION OF " ,(to-sql-name (car args))
                                      " DEFAULT "))  ;postgresql version 11 required
    (:partition-by-list `(" PARTITION BY RANGE (" ,@(sql-expand (car args))
                                                  ")"))))

(defun expand-identity (keywd)
  (cond ((eq keywd :identity-by-default)
         '(" GENERATED BY DEFAULT AS IDENTITY "))
        ((eq keywd :generated-as-identity-by-default)
         '(" GENERATED BY DEFAULT AS IDENTITY "))
        ((eq keywd :identity-always)
         '(" GENERATED ALWAYS AS IDENTITY "))
        ((eq keywd :generated-as-identity-always)
         '(" GENERATED ALWAYS AS IDENTITY "))
        ((eq keywd :add-identity-by-default)
         '(" ADD GENERATED BY DEFAULT AS IDENTITY "))
        ((eq keywd :add-identity-always)
         '(" ADD GENERATED ALWAYS AS IDENTITY "))
        ((eq keywd :set-identity-by-default)
         '(" SET GENERATED BY DEFAULT "))
        ((eq keywd :set-identity-always)
         '(" SET GENERATED ALWAYS "))
        (t "")))

(defun expand-table-column (column-name args)
  `(,(to-sql-name column-name) " "
    ,@(let ((type (or (getf args :type)
                      (sql-error "No type specified for column ~A."
                                 column-name))))
        (multiple-value-bind (type null) (dissect-type type)
          `(,(to-type-name type) ,@(when (not null) '(" NOT NULL")))))
    ,@(loop :for (option value) :on args :by #'cddr
            :append (case option
                      (:default `(" DEFAULT " ,@(sql-expand value)))
                      (:interval `(" " ,@(expand-interval value)))
                      (:identity-by-default
                       '(" GENERATED BY DEFAULT AS IDENTITY "))
                      (:identity-always
                       '(" GENERATED ALWAYS AS IDENTITY "))
                      (:generated-as-identity-by-default
                       '(" GENERATED BY DEFAULT AS IDENTITY "))
                      (:generated-as-identity-always
                       '(" GENERATED ALWAYS AS IDENTITY "))
                      (:generated-as-identity-always1
                       '(" GENERATED ALWAYS AS IDENTITY "))
                      (:generated-always
                       (when value
                         `(" GENERATED ALWAYS AS (" ,@(sql-expand-names value) ") STORED")))
                      (:primary-key (cond ((and value (stringp value))
                                           `(" PRIMARY KEY " ,value))
                                          ((and value (keywordp value))
                                           `(" PRIMARY KEY "
                                             ,@(expand-identity value)))
                                          (t '(" PRIMARY KEY "))))
                      (:constraint (when value `(" CONSTRAINT "
                                                 ,@(sql-expand value))))
                      (:collate (when value `(" COLLATE \"" ,value "\"")))
                      (:unique (cond ((and value (stringp value))
                                      `(" UNIQUE " ,@(sql-expand value)))
                                     (value '(" UNIQUE "))
                                     (t nil)))
                      (:check `(" CHECK " ,@(sql-expand value)))
                      (:references
                       (destructuring-bind (target
                                            &optional (on-delete :restrict)
                                              (on-update :restrict)
                                              (match :match-simple))
                           value
                         (%build-foreign-reference target on-delete
                                                   on-update match)))
                      (:type ())
                      (:deferrable (when (eq value t) '(" DEFERRABLE ")))
                      (:not-deferrable
                       (when (eq value t) '(" NOT DEFERRABLE ")))
                      (:initially-deferred (when (eq value t)
                                             '(" INITIALLY DEFERRED ")))
                      (:initially-immediate (when (eq value t)
                                              '(" INITIALLY IMMEDIATE ")))
                      (t (sql-error "Unknown column option: ~A." option))))))

(defun expand-composite-table-name (frm)
  "Helper function for building a composite table name"
  (strcat (list (to-sql-name (second frm)) " OF " (to-sql-name (third frm)))))

(defun expand-table-name (name &optional (tableset nil))
               "Note: temporary tables are unlogged tables. Having both :temp and :unlogged
would be redundant."
  (cond ((and name (stringp name))
         (concatenate 'string (unless tableset  "TABLE ") (to-sql-name name)))
        ((and name (symbolp name))
         (concatenate 'string (unless tableset  "TABLE ") (to-sql-name name)))
        ((and name (listp name))
          (case (car name)
            (quote (concatenate 'string (unless tableset  "TABLE ")
                          (if (stringp (cadr name))
                              (to-sql-name (cadr name))
                              (expand-table-name (cadr name) t))))
            (:temp (concatenate 'string "TEMP TABLE "
                                (expand-table-name (cdr name) t)))
            (:temporary (concatenate 'string "TEMP TABLE "
                                     (expand-table-name (cdr name) t)))
            (:unlogged (if tableset (expand-table-name (cdr name) t)
                           (concatenate 'string "UNLOGGED TABLE "
                                        (expand-table-name (cdr name) t))))
            (:if-not-exists (concatenate 'string (unless tableset  "TABLE ")
                                         "IF NOT EXISTS "
                                         (expand-table-name (cdr name) t)))
            (:of (concatenate 'string (unless tableset  "TABLE ")
                              (expand-composite-table-name name)))
            (t
             (cond ((stringp (car name))
                    (concatenate 'string (unless tableset  "TABLE ")
                            (to-sql-name (car name))))
                   ((symbolp (car name))
                    (concatenate 'string (unless tableset  "TABLE ")
                            (to-sql-name (car name))))
                   ((listp (car name))
                    (expand-table-name (car name) t))
                   (t (sql-error "Unknown table option: ~A" name))))))
        (t (sql-error "Unknown table option: ~A" name))))


(def-sql-op :create-composite-type (type-name &rest args)
  "Creates a composite type with a type-name and two or more
columns. Sample call would be:
   (sql (:create-composite-type 'fullname (first-name text) (last-name text)))"
  `("(CREATE TYPE " ,(cond ((and type-name (stringp type-name))
                            (to-sql-name type-name))
                           ((and type-name (symbolp type-name)
                                 (boundp type-name))
                            (to-sql-name type-name))
                           ((and type-name (symbolp type-name))
                            (to-sql-name type-name))
                           ((and type-name (consp type-name)
                                 (eq (car type-name) 'quote))
                            (to-sql-name (cadr type-name)))
                           (t "ERROR in create-composite-type type-name"))
                    " AS ("
                    ,(implode ", "
                              (loop for x in args
                                    collect (cons-to-sql-name-strings x)))
                    ")"))

(def-sql-op :create-table (name (&rest columns) &rest options)
  (let ((typed-table (and (listp name) (eq (car name) :of))))
    (when (and (null columns) (not typed-table))
      (sql-error "No columns defined for table ~A." name))
    `("CREATE " ,@(list (expand-table-name name)) " ("
                ,@(loop :for ((column-name . args) . rest) :on columns
                        :append (expand-table-column column-name args)
                        :if rest :collect ", ")
                ,@(when (and columns options) '(", "))
                ,@(loop :for ((option . args) . rest) :on options
                        :append (expand-table-constraint option args)
                        :if rest :collect ", ")
                ")")))

(def-sql-op :create-extended-table (name (&rest columns) &optional
                                         table-constraints
                                         extended-table-constraints)
  "Create a table with more complete syntax where table-constraints and
extended-table-constraints are lists. Note that with extended tables you can
have tables without columns that are inherited or partitioned."
  `("CREATE " ,@(list (expand-table-name name)) " ("
              ,@(loop :for ((column-name . args) . rest) :on columns
                      :append (expand-table-column column-name args)
                      :if rest :collect ", ")
              ,@(loop for constraint in table-constraints
                      :for i from (length table-constraints) downto 0
                      :append (expand-table-constraint-sok constraint)
                                        ;if (> i 0) collect ", "
                      )
              ")"
              ,@(loop :for ((constraint . args)) :on extended-table-constraints
                      :append (expand-extended-table-constraint constraint args))))

(defun alter-table-column (column-name args)
  "Generates the sql string for the portion of altering a column."
  `(,(to-sql-name column-name *escape-sql-names-p* t) " "
    ,@(loop :for (option value) :on args :by #'cddr
            :append (case option
                      (:default `(" DEFAULT " ,@(sql-expand value)))
                      (:add-identity-by-default
                       (cond  ((stringp value)
                               `(" ADD GENERATED BY DEFAULT AS IDENTITY ("
                                 ,value ")"))
                              (t '(" ADD GENERATED BY DEFAULT AS IDENTITY "))))
                      (:add-identity-always
                       (cond ((stringp value)
                              `(" ADD GENERATED ALWAYS AS IDENTITY ("
                                ,value ")"))
                              (t '(" ADD GENERATED ALWAYS AS IDENTITY "))))
                      (:set-identity-by-default
                       (cond  ((stringp value)  `(" SET GENERATED BY DEFAULT ("
                                                  ,value ")"))
                              (t '(" SET GENERATED BY DEFAULT "))))
                      (:set-identity-always
                       (cond  ((stringp value)
                               `(" SET GENERATED ALWAYS (" ,value ")"))
                              (t '(" SET GENERATED ALWAYS "))))
                      (:set-statistics
                       (when (integerp value)
                         `("SET STATISTICS " ,(write-to-string value)
                                             " ")))
                      (:collate (when (and value (stringp value))
                                  `(" COLLATE \"" ,value "\"")))
                      (:type (multiple-value-bind (type null)
                                 (dissect-type value)
                               `(" TYPE " ,(to-type-name type)
                                          ,@(when (not null)
                                              '(" NOT NULL")))))
                      (:primary-key
                       (cond ((and value (stringp value))
                              `(" PRIMARY KEY " ,value))
                             ((and value (keywordp value))
                              `(" PRIMARY KEY "
                                ,@(expand-identity value)))
                             (t '(" PRIMARY KEY "))))
                      (:unique (cond ((and value (stringp value))
                                      `(" UNIQUE " ,@(sql-expand value)))
                                     (value '(" UNIQUE "))
                                     (t nil)))
                      (:references
                       (destructuring-bind (target
                                            &optional (on-delete :restrict)
                                              (on-update :restrict)
                                              (match :match-simple))
                           value
                         (%build-foreign-reference target on-delete
                                                   on-update match)))
                      (:drop-default `(" DROP DEFAULT "))
                      (:drop-not-null '(" DROP NOT NULL "))
                      (:set-default `(" SET DEFAULT " ,@ (sql-expand value)))
                      (:set-not-null '(" SET NOT NULL "))
                      (:drop-identity (when value `(" DROP IDENTITY "
                                                    ,@(sql-expand value))))
                      (:check `(" CHECK " ,@(sql-expand value)))
                      (t (sql-error "Unknown alter column option: ~A."
                                    option))))))

(def-sql-op :alter-table (name action &rest args)
  (labels
      ((drop-action (action)
         (case action
           (:restrict " RESTRICT")
           (:cascade " CASCADE")
           (t (sql-error "Unknown DROP action ~A." action))))
       (base-action (action args)
         (case action
           (:add (cons "ADD " (expand-table-constraint (first args)
                                                       (rest args))))
           (:add-column (cons "ADD COLUMN "
                              (expand-table-column (first args) (rest args))))
           (:alter-column (cons "ALTER COLUMN "
                                (alter-table-column (first args) (rest args))))
           (:drop-column (list "DROP COLUMN "
                               (to-sql-name (first args) *escape-sql-names-p* t)
                               (if (rest args)
                                   (drop-action (second args))
                                   "")))
           (:add-constraint (append (list "ADD CONSTRAINT ")
                                    (list (to-sql-name (first args)
                                                       *escape-sql-names-p* t)
                                          " ")
                                    (expand-table-constraint (second args)
                                                             (cddr args))))
           (:drop-constraint (list "DROP CONSTRAINT "
                                   (to-sql-name (first args))
                                   (if (rest args)
                                       (drop-action (second args))
                                       "")))
           (:rename (list "RENAME TO " (to-sql-name (first args))))
           (:rename-column (list "RENAME COLUMN "
                                 (to-sql-name (first args)) " TO "
                                 (to-sql-name (second args))))
           (:rename-constraint (list "RENAME CONSTRAINT "
                                     (to-sql-name (first args)) " TO "
                                     (to-sql-name (second args))))
           (t (sql-error "Unknown ALTER TABLE action ~A" action)))))
    `("ALTER TABLE "
      ,(to-sql-name name) " "
      ,@(if (listp action)
            (loop :for (item . rest) on action
                  :append (base-action (car item) (cdr item))
                  :if rest :collect ", ")
            (base-action action args)))))


(def-sql-op :alter-sequence (name action &optional argument)
  `("ALTER SEQUENCE "
    ,(to-sql-name name)
    ,@(case action
        (:increment `(" INCREMENT BY " ,(write-to-string argument)))
        (:min-value `(" MINVALUE " ,(write-to-string argument)))
        (:max-value `(" MAXVALUE " ,(write-to-string argument)))
        (:no-min `(" NO MINVALUE"))
        (:no-max `(" NO MAXVALUE"))
        (:start `(" START " ,(write-to-string argument)))
        (:restart `(" RESTART " ,(write-to-string argument)))
        (:cache `(" CACHE " ,(write-to-string argument)))
        (:cycle `(" CYCLE"))
        (:no-cycle `(" NO CYCLE"))
        (:owned-by `(" OWNED BY " ,(to-sql-name  argument)))
        (t (sql-error "Unknown ALTER SEQUENCE action ~A" action)))))

(defun expand-create-index (name args)
  "Available parameters - in order after name - are :concurrently, :on, :using,
 :fields and :where.The advantage to using the keyword :concurrently is that
writes to the table from other sessions are not locked out while the index is is
built. The disadvantage is that the table will need to be scanned twice.
Everything is a trade-off."
  (split-on-keywords ((unique ? -) (concurrently ? -)  (on) (using ?) (fields *)
                      (where ?))
      args
    `(,@(when unique '("UNIQUE "))
      "INDEX "
      ,@(when concurrently '("CONCURRENTLY "))
      ,@(if (and (listp name) (eq (car name) :if-not-exists))
            (list "IF NOT EXISTS " (car (sql-expand (cadr name))))
            (sql-expand name))
      " ON "
      ,(to-sql-name (cond ((stringp (car on))
                           (car on))
                          ((consp (car on))
                           (cadar on))
                          (t (car on)))
                    *escape-sql-names-p* t)
      ,@(when using `(" USING " ,(cond ((stringp (car using))
                                        (to-sql-name (car using)))
                                       ((consp (car using))
                                        (to-sql-name (cadar using)))
                                       (t (to-sql-name (car using))))))
      " (" ,@(sql-expand-names fields) ")"
      ,@(when where `(" WHERE " ,@(sql-expand (first where)))))))

(def-sql-op :create-index (name &rest args)
  (cons "CREATE " (expand-create-index name args)))

(def-sql-op :create-unique-index (name &rest args)
  (cons "CREATE UNIQUE " (expand-create-index name args)))

(def-sql-op :cascade (op)
  `(,@(sql-expand op) " CASCADE"))

(defmacro def-drop-op (op-name word)
  "Def-drop-op accepts variables, strings or symbols as the identifier."
  `(def-sql-op ,op-name (&rest args)
     (let* ((concurrently (if (eq (car args) :concurrently)
                              (pop args)
                              nil))
            (if-exists (if (eq (car args) :if-exists)
                           (pop args)
                           nil))
            (name (pop args))
            (cascade (if (or (eq (car args) :cascade)
                             (eq (cadr args) :cascade))
                         t
                         nil)))
       `("DROP " ,,word " "
                 ,@(when concurrently '("CONCURRENTLY "))
                 ,@(when if-exists '("IF EXISTS "))
                 ,@(if (and (consp name) (eq :if-exists (car name)))
                       `("IF EXISTS " ,(car (cond ((stringp (cadr name))
                            (list (to-sql-name (cadr name))))
                           ((and (symbolp (cadr name))
                                (not (or (keywordp (cadr name)) (eq (cadr name) t)
                                         (eq (cadr name) nil))))
                            (list `(to-sql-name ,(cadr name))))
                           (t (sql-expand (cadr name))))))
                     (cond ((stringp name)
                            (list (to-sql-name name)))
                           ((and (symbolp name)
                                (not (or (keywordp name) (eq name t)
                                         (eq name nil))))
                            (list `(to-sql-name ,name)))
                           (t (sql-expand name))))
                 ,@(when cascade '(" CASCADE"))))))


(def-drop-op :drop-table "TABLE")
(def-drop-op :drop-index "INDEX")
(def-drop-op :drop-sequence "SEQUENCE")
(def-drop-op :drop-view "VIEW")
(def-drop-op :drop-type "TYPE")
(def-drop-op :drop-rule "RULE")

(def-sql-op :truncate (&rest args)
  "This query sql-op takes one or more table names and will truncate
  those tables (deleting all the rows. The following keyword parameters
  are optionally allowed and must be in this order.
    :only will truncate only this table and not descendent tables.
    :restart-identity will restart any sequences owned by the table.
    :continue-identity will continue sequences owned by the table.
    :cascade will cascade the truncation through tables using foreign keys."
  (split-on-keywords ((vars *) (only - ?) (restart-identity - ?)
                      (continue-identity - ?)(cascade - ? ))
      (cons :vars args)
    `("TRUNCATE "
      ,@(when only '(" ONLY "))
      ,@(sql-expand-list vars)
      ,@(cond (restart-identity '(" RESTART IDENTITY "))
              (continue-identity `(" CONTINUE IDENTITY "))
              (t '("")))
      ,@(when cascade '(" CASCADE ")))))

(defun quoted-name-p (name)
  "Helper function which may be useful for certain macros.
Takes what might be a string, a symbol or a quoted-name in the form
 '(quote name) and returns the string version of the name."
  (cond ((and (consp name) (eq (car name) 'quote) (equal (length name)
                                                         2))
         (string (cadr name)))
        ((symbolp name)
         (string name))
        ((stringp name)
         name)
        (t nil)))

(defun dequote (val)
  "Helper function for macros which look for 'something but that
has been converted to (quote something)."
  (if (and (consp val) (eq (car val) 'quote)) (cadr val) val))

(def-sql-op :nextval (name)
  `("nextval(" ,(if *expand-runtime*
                    (sql-escape-string (to-sql-name (dequote name)))
                    `(sql-escape-string (to-sql-name ,name))) ")"))

(def-sql-op :currval (name)
  `("currval(" ,(if *expand-runtime*
                    (sql-escape-string (to-sql-name (dequote name)))
                    `(sql-escape-string (to-sql-name ,name))) ")"))

(def-sql-op :create-sequence (name &key increment min-value max-value start
                                   cache cycle)
  `("CREATE SEQUENCE " ,@(sql-expand name)
                       ,@(when increment  `(" INCREMENT "
                                            ,@(sql-expand increment)))
                       ,@(when min-value `(" MINVALUE "
                                           ,@(sql-expand min-value)))
                       ,@(when max-value `(" MAXVALUE "
                                           ,@(sql-expand max-value)))
                       ,@(when start `(" START " ,@(sql-expand start)))
                       ,@(when cache `(" CACHE " ,@(sql-expand cache)))
                       ,@(when cycle `(" CYCLE"))))

(def-sql-op :create-view (name query)
  ;; does not allow to specify the columns of the view yet
  `("CREATE VIEW " ,(to-sql-name name) " AS " ,@(sql-expand query)))

(def-sql-op :create-enum (name members)
  (let ((strings (loop :for m :in members
                       :collect (etypecase m (symbol
                                              (string-downcase m))
                                           (string m)))))
    `("CREATE TYPE " ,@(sql-expand name) " AS ENUM ("
                     ,@(sql-expand-list strings)
                     ")")))

;;; https://www.postgresql.org/docs/current/static/sql-createdomain.html
(def-sql-op :create-domain (name &rest args)
  (split-on-keywords ((type) (default ?) (constraint-name ?) (check ?)) args
    (multiple-value-bind (type may-be-null) (dissect-type (car type))
      `("CREATE DOMAIN " ,@(sql-expand name) " AS " ,(to-type-name type)
                         ,@(when default `(" DEFAULT "
                                           ,@(sql-expand (car default))))
                         ,@(when constraint-name
                             `(" CONSTRAINT "
                               ,@(sql-expand (car constraint-name))))
                         ,@(unless may-be-null '(" NOT NULL"))
                         ,@(when check `(" CHECK"
                                         ,@(sql-expand (car check))))))))

(def-sql-op :drop-domain (name)
  `("DROP DOMAIN " ,@(sql-expand name)))

;;; https://www.postgresql.org/docs/current/static/sql-createrule.html
(def-sql-op :create-rule (name &rest rest)
  (split-on-keywords ((on) (to) (where ?) (instead ? -) (do ? *)) rest
    (check-type (car on) (member :select :insert :update :delete))
    `("CREATE RULE " ,@(sql-expand name)
                     " AS ON " ,(symbol-name (car on)) " TO "
                     ,@(sql-expand (car to))
                     ,@(when where `(" WHERE " ,@(sql-expand (car where))))
                     " DO" ,@(when instead '(" INSTEAD"))
                     ,@(if (or (null do) (eq do :nothing))
                           '(" NOTHING")
                           `("(" ,@(sql-expand-list do "; ") ")")))))

;;; https://www.postgresql.org/docs/current/static/sql-createdatabase.html
(def-sql-op :create-database (name &rest args)
  "Create a database.
   If the database exists an error is raised."
  (split-on-keywords ((owner ?) (template ?) (encoding ?) (lc-collate ?)
                      (lc-ctype ?) (tablespace ?)
                                (allow-connections ?) (connection-limit ?)
                                (is-template ?))
      args
    `("CREATE DATABASE "
      ,@(sql-expand name)
      ,@(when args `(" WITH"))
      ,@(when owner    `(" OWNER "    ,@(sql-expand (car owner))))
      ,@(when template `(" TEMPLATE " ,@(sql-expand (car template))))
      ,@(when encoding `(" ENCODING " ,@(sql-expand (car encoding))))
      ,@(when lc-collate `(" LC_COLLATE " ,@(sql-expand (car lc-collate))))
      ,@(when lc-ctype `(" LC_CTYPE " ,@(sql-expand (car lc-ctype))))
      ,@(when tablespace `(" TABLESPACE " ,@(sql-expand (car tablespace))))
      ,@(when allow-connections `(" ALLOW_CONNECTIONS "
                                  ,@(if (car allow-connections)
                                        `("TRUE")
                                        `("FALSE"))))
      ,@(when connection-limit `(" CONNECTION LIMIT "
                                 ,@(sql-expand (car connection-limit))))
      ,@(when is-template `(" IS_TEMPLATE " ,@(if (car is-template)
                                                  `("TRUE")
                                                  `("FALSE")))))))

(def-drop-op :drop-database "DATABASE")

;;; https://www.postgresql.org/docs/current/static/sql-createrole.html
(def-sql-op :create-role (name &rest args)
  "Add a new role. A role is an entity that can own database objects
and have database privileges; a role can be considered a “user”, a
“group”, or both depending on how it is used.

:options to create role do not require values, e.g. (:create-role
'foo :options 'SUPERUSER 'NOINHERIT).

connection-limit, valid-until, role, in-role, admin are keyword options that
accept values."
  (split-on-keywords ((options ? *) (password ?) (connection-limit ?)
                      (valid-until ?) (role * ?) (in-role * ?) (admin * ?))
      args
    `("CREATE ROLE "
      ,@(sql-expand name)
      ,@(when args `(" WITH "))
      ,@(when options `(,@(sql-expand-list options " ")))
      ,@(when password    `(" PASSWORD "    ,@(sql-expand (car password))))
      ,@(when connection-limit
          `(" CONNECTION LIMIT " ,@(sql-expand
                                    (car connection-limit))))
      ,@(when valid-until `(" VALID UNTIL " ,@(sql-expand (car valid-until))))
      ,@(when role        `(" ROLE "        ,@(sql-expand-list role) " "))
      ,@(when in-role     `(" IN ROLE "     ,@(sql-expand-list in-role) " "))
      ,@(when admin       `(" ADMIN "       ,@(sql-expand-list admin) " ")))))

(def-drop-op :drop-role "ROLE")


;;; https://www.postgresql.org/docs/current/static/sql-copy.html
(def-sql-op :copy (table &rest args)
  "Move data between Postgres tables and filesystem files."
  (split-on-keywords ((columns ? *) (from ?) (to ?) (on-segment ?) (binary ?)
                      (oids ?) (header ?) (delimiter ?) (null ?)
                                    (escape ?) (newline ?) (csv ?) (quote ?)
                                    (force-not-null ? *)
                                    (fill-missing-fields ?)
                                    (log-errors ?) (segment-reject-limit ? *))
                     args
    `("COPY "
      ,@(sql-expand table) " "
      ,@(when columns `("(" ,@(sql-expand-list columns) ") "))
      ,@(when from    `("FROM " ,@(sql-expand (car from)) " "))
      ,@(when to      `("TO " ,@(sql-expand (car to)) " "))
      ,@(when on-segment `("ON SEGMENT "))
      ,@(when binary     `("BINARY "))
      ,@(when oids       `("OIDS "))
      ,@(when header     `("HEADER "))
      ,@(when delimiter  `("DELIMITER " ,@(sql-expand (car delimiter)) " "))
      ,@(when null       `("NULL "      ,@(sql-expand (car null)) " "))
      ,@(when escape     `("ESCAPE "    ,@(sql-expand (car escape)) " "))
      ,@(when newline    `("NEWLINE "   ,@(sql-expand (car newline)) " "))
      ,@(when csv        `("CSV "))
      ,@(when quote      `("QUOTE "     ,@(sql-expand (car quote))))
      ,@(when force-not-null       `("FORCE NOT NULL "
                                     ,@(sql-expand-list force-not-null)
                                     " "))
      ,@(when fill-missing-fields  `("FILL MISSING FIELDS "))
      ,@(when log-errors           `("LOG ERRORS "))
      ,@(when segment-reject-limit
          `("SEGMENT REJECT LIMIT "
            ,@(sql-expand (car segment-reject-limit))
            " "
            ,@(if (second segment-reject-limit)
                  `(,@(sql-expand (second segment-reject-limit)))))))))
