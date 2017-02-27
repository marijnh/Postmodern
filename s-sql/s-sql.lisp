(defpackage :s-sql
  (:use :common-lisp)
  (:export #:smallint
           #:bigint
           #:numeric
           #:real
           #:double-precision
           #:bytea
           #:text
           #:varchar
           #:db-null
           #:sql-type-name
           #:*standard-sql-strings*
           #:*downcase-symbols*
           #:sql-escape-string
           #:sql-escape
           #:from-sql-name
           #:to-sql-name
           #:*escape-sql-names-p*
           #:sql
           #:sql-compile
           #:sql-template
           #:$$
           #:register-sql-operators
           #:enable-s-sql-syntax
           #:sql-error))

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
  "Reduce a list of strings to a single string, inserting a separator
between them."
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
                                (sql-error "Keyword ~A encountered at end of arguments." me))
                              (let ((next (next-word (cdr words) after-me)))
                                (cond
                                  (no-args (unless (zerop next) (sql-error "Keyword ~A does not take any arguments." me)))
                                  (multi (unless (>= next 1) (sql-error "Not enough arguments to keyword ~A." me)))
                                  (t (unless (= next 1) (sql-error "Keyword ~A takes exactly one argument." me))))
                                (push (cons (caar words) (if no-args t (subseq after-me 0 next))) result)
                                found)))
                           (optional
                            (next-word (cdr words) values))
                           (t (sql-error "Required keyword ~A not found." me))))
                   (length values))))
      (unless (= (next-word shape list) 0)
        (sql-error "Arguments do not start with a valid keyword."))
      result)))

(defmacro split-on-keywords (words form &body body)
  "Used to handle arguments to some complex SQL operations. Arguments
are divided by keywords, which are interned with the name of the
non-keyword symbols in words, and bound to these symbols. After the
naming symbols, a ? can be used to indicate this argument group is
optional, an * to indicate it can consist of more than one element,
and a - to indicate it does not take any elements."
  (let ((alist (gensym)))
    `(let* ((,alist (split-on-keywords% ',words ,form))
            ,@(mapcar (lambda (word)
                        `(,(first word) (cdr (assoc ',(first word) ,alist))))
                      words))
        ,@body)))

;; Converting between symbols and SQL strings.

(defparameter *postgres-reserved-words*
  (let ((words (make-hash-table :test 'equal)))
    (dolist (word '("all" "analyse" "analyze" "and" "any" "array" "as" "asc" "asymmetric" "authorization"
                    "between" "binary" "both" "case" "cast" "check" "collate" "column" "constraint" "create"
                    "cross" "default" "deferrable" "desc" "distinct" "do" "else" "end" "except" "false"
                    "for" "foreign" "freeze" "from" "full" "grant" "group" "having" "ilike" "in" "initially"
                    "inner" "intersect" "into" "is" "isnull" "join" "leading" "left" "like" "limit"
                    "localtime" "localtimestamp" "natural" "new" "not" "notnull" "null" "off" "offset" "old"
                    "on" "only" "or" "order" "outer" "overlaps" "placing" "primary" "references" "returning"
                    "right" "select" "similar" "some" "symmetric" "table" "then" "to" "trailing" "true"
                    "union" "unique" "user" "using" "verbose" "when" "where" "with" "for" "nowait" "share"))
      (setf (gethash word words) t))
    words)
  "A set of all PostgreSQL's reserved words, for automatic escaping.")

(defparameter *escape-sql-names-p* :auto
  "Setting this to T will make S-SQL add double quotes around
identifiers in queries. Setting it :auto will turn on this behaviour
only for reserved words.")

(defvar *downcase-symbols* t)

(defun to-sql-name (name &optional (escape-p *escape-sql-names-p*))
  "Convert a symbol or string into a name that can be an sql table,
column, or operation name. Add quotes when escape-p is true, or
escape-p is :auto and the name contains reserved words."
  (declare (optimize (speed 3) (debug 0)))
  (let ((*print-pretty* nil)
        (name (string name)))
    (with-output-to-string (*standard-output*)
      (flet ((subseq-downcase (str from to)
               (let ((result (make-string (- to from))))
                 (loop :for i :from from :below to
                       :for p :from 0
                       :do (setf (char result p) (if *downcase-symbols*
                                                     (char-downcase (char str i))
                                                     (char str i))))
                 result))
             (write-element (str)
               (declare (type string str))
               (let ((escape-p (if (eq escape-p :auto)
                                   (gethash str *postgres-reserved-words*)
                                   escape-p)))
                 (when escape-p
                   (write-char #\"))
                 (if (and (> (length str) 1) ;; Placeholders like $2
                          (char= (char str 0) #\$)
                          (every #'digit-char-p (the string (subseq str 1))))
                     (princ str)
                     (loop :for ch :of-type character :across str
                           :do (if (or (eq ch #\*) (alphanumericp ch))
                                   (write-char ch)
                                   (write-char #\_))))
                 (when escape-p
                   (write-char #\")))))

        (loop :for start := 0 :then (1+ dot)
              :for dot := (position #\. name) :then (position #\. name :start start)
              :do (write-element (subseq-downcase name start (or dot (length name))))
              :if dot :do (princ #\.)
              :else :do (return))))))

(defun from-sql-name (str)
  "Convert a string to something that might have been its original
lisp name \(does not work if this name contained non-alphanumeric
characters other than #\-)"
  (intern (map 'string (lambda (x) (if (eq x #\_) #\- x))
               (if (eq (readtable-case *readtable*) :upcase) (string-upcase str) str))
          (find-package :keyword)))

;; Writing out SQL type identifiers.

;; Aliases for some types that can be expressed in SQL.
(deftype smallint ()
  '(signed-byte 16))
(deftype bigint ()
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
  "Type for representing NULL values. Use like \(or integer db-null)
for declaring a type to be an integer that may be null."
  '(eql :null))

;; For types integer and real, the Lisp type isn't quite the same as
;; the SQL type. Close enough though.

(defgeneric sql-type-name (lisp-type &rest args)
  (:documentation "Transform a lisp type into a string containing
something SQL understands. Default is to just use the type symbol's
name.")
  (:method ((lisp-type symbol) &rest args)
    (declare (ignore args))
    (map 'string (lambda (ch) (if (eq ch #\-) #\space ch)) (symbol-name lisp-type)))
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
  "Turn a Lisp type expression into an SQL typename."
  (if (listp type)
      (apply 'sql-type-name type)
      (sql-type-name type)))

;; Turning lisp values into SQL strings.

(defparameter *standard-sql-strings* nil
  "Indicate whether S-SQL will use standard SQL strings (just use ''
  for #\'), or backslash-style escaping. Setting this to NIL is always
  safe, but when the server is configured to allow standard
  strings (parameter 'standard_conforming_strings' is 'on'), the noise
  in queries can be reduced by setting this to T.")

(defun sql-escape-string (string &optional prefix)
  "Escape string data so it can be used in a query."
  (let ((*print-pretty* nil))
    (with-output-to-string (*standard-output*)
      (when prefix
        (princ prefix)
        (princ #\space))
      (unless *standard-sql-strings*
        (princ #\E))
      (princ #\')
      (if *standard-sql-strings*
          (loop :for char :across string :do (princ (if (char= char #\') "''" char)))
          (loop :for char :across string
                :do (princ (case char
                             (#\' "''")
                             (#\\ "\\\\")
                             (otherwise char)))))
      (princ #\'))))

(defgeneric sql-escape (arg)
  (:documentation "Get the representation of a Lisp value so that it
can be used in a query.")
  (:method ((arg symbol))
    (if (or (typep arg 'boolean) (eq arg :null))
        (call-next-method)
        (to-sql-name arg)))
  (:method ((arg vector))
    (if (or (typep arg '(vector (unsigned-byte 8)))
            (stringp arg))
        (call-next-method)
        (format nil "~:['{}'~;ARRAY[~:*~{~A~^, ~}]~]" (map 'list 'sql-escape arg))))
  (:method ((arg t))
    (multiple-value-bind (string escape) (cl-postgres:to-sql-string arg)
      (if escape
          (sql-escape-string string (and (not (eq escape t)) escape))
          string))))

(defparameter *expand-runtime* nil)

(defun sql-expand (arg)
  "Compile-time expansion of forms into lists of stuff that evaluates
to strings \(which will form an SQL query when concatenated)."
  (cond ((and (consp arg) (keywordp (first arg)))
         (expand-sql-op (car arg) (cdr arg)))
        ((and (consp arg) (eq (first arg) 'quote))
         (list (sql-escape (second arg))))
        ((and (consp arg) *expand-runtime*)
         (expand-sql-op (intern (symbol-name (car arg)) :keyword) (cdr arg)))
        ((and (eq arg '$$) *expand-runtime*) '($$))
        (*expand-runtime*
         (list (sql-escape arg)))
        ((or (consp arg)
             (and (symbolp arg)
                  (not (or (keywordp arg) (eq arg t) (eq arg nil)))))
         (list `(sql-escape ,arg)))
        (t (list (sql-escape arg)))))

(defun sql-expand-list (elts &optional (sep ", "))
  "Expand a list of elements, adding a separator in between them."
  (loop :for (elt . rest) :on elts
        :append (sql-expand elt)
        :if rest :collect sep))

(defun sql-expand-names (names &optional (sep ", "))
  (loop :for (name . rest) :on names
        :if (consp name) :append (let ((*expand-runtime* t))
                                   (sql-expand name))
        :else :collect (to-sql-name name)
        :if rest :collect sep))

(defun reduce-strings (list)
  "Join adjacent strings in a list, leave other values intact."
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
  "Compile form to an sql expression as far as possible."
  (let ((list (reduce-strings (sql-expand form))))
    (if (= 1 (length list))
        (car list)
        `(strcat (list ,@list)))))

(defun sql-compile (form)
  (let ((*expand-runtime* t))
    (strcat (sql-expand form))))

(defun sql-template (form)
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
  "Enable a syntactic shortcut #Q\(...) for \(sql \(...)). Optionally
takes a character to use instead of #\\Q."
  (set-dispatch-macro-character #\# char 's-sql-reader))

;; Definitions of sql operators

(defgeneric expand-sql-op (op args)
  (:documentation "For overriding expansion of operators. Default is
to just place operator name in front, arguments between parentheses
behind it.")
  (:method ((op t) args)
    `(,(to-sql-name op) "(" ,@(sql-expand-list args) ")")))

(defmacro def-sql-op (name arglist &body body)
  "Macro to make defining syntax a bit more straightforward. Name
should be the keyword identifying the operator, arglist a lambda list
to apply to the arguments, and body something that produces a list of
strings and forms that evaluate to strings."
  (let ((args-name (gensym)))
    `(defmethod expand-sql-op ((op (eql ,name)) ,args-name)
       (destructuring-bind ,arglist ,args-name
         ,@body))))

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
                     (sql-error "SQL operator ~A takes at least two arguments." name))
                   (expand-n-ary args)))
        (:n-or-unary (lambda (args)
                       (if (cdr args)
                           (expand-n-ary args)
                           `("(" ,name " " ,@(sql-expand (car args)) ")"))))))))

(defmacro register-sql-operators (arity &rest names)
  "Define simple operators. Arity is one of :unary \(like
  'not'), :unary-postfix \(the operator comes after the operand),
  :n-ary \(like '+': the operator falls away when there is only one
  operand), :2+-ary (like '=', which is meaningless for one operand),
  or :n-or-unary (like '-', where the operator is kept in the unary
  case). After the arity follow any number of operators, either just a
  keyword, in which case the downcased symbol name is used as the
  operator, or a two-element list containing a keyword and a name
  string."
  (declare (type (member :unary :unary-postfix :n-ary :n-or-unary :2+-ary) arity))
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
(register-sql-operators :n-ary :+ :* :% :& :|\|| :|\|\|| :and :or :union (:union-all "union all"))
(register-sql-operators :n-or-unary :- :~)
(register-sql-operators :2+-ary  := :/ :!= :<> :< :> :<= :>= :^ :~* :!~ :!~* :like :ilike
                        :intersect (:intersect-all "intersect all")
                        :except (:except-all "except all"))
;; PostGIS operators
(register-sql-operators :2+-ary :&& :&< :|&<\|| :&> :<< :|<<\|| :>> :|@| :|\|&>| :|\|>>| :~= :|@>| :|@<|)

;; hstore operators
(register-sql-operators :2+-ary :-> :=> :? :?& :?\| :|<@| :#= :unary :%% :%#)

(def-sql-op :|| (&rest args)
  `("(" ,@(sql-expand-list args " || ") ")"))

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
                      :for (name type) := (if (and (consp field) (not (eq (first field) 'quote)))
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
  `("ANY(" ,@(sql-expand query) ")"))

(def-sql-op :any (query)
  `("ANY " ,@(sql-expand query)))

(def-sql-op :all (query)
  `("ALL " ,@(sql-expand query)))

(def-sql-op :exists (query)
  `("(EXISTS " ,@(sql-expand query) ")"))

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

(def-sql-op :count (what &optional distinct)
  `("COUNT(" ,@(when (eq distinct :distinct)
                 '("DISTINCT "))
             ,@(sql-expand what)  ")"))

(def-sql-op :between (n start end)
  `("(" ,@(sql-expand n) " BETWEEN " ,@(sql-expand start) " AND " ,@(sql-expand end) ")"))

(def-sql-op :between-symmetric (n start end)
  `("(" ,@(sql-expand n) " BETWEEN SYMMETRIC " ,@(sql-expand start) " AND " ,@(sql-expand end) ")"))

(def-sql-op :case (&rest clauses)
  `("CASE"
    ,@(loop :for (test expr) :in clauses
            :if (eql test :else)
              :append `(" ELSE " ,@(sql-expand expr))
            :else
              :append `(" WHEN " ,@(sql-expand test) " THEN " ,@(sql-expand expr))
            :end)
    " END"))

(def-sql-op :[] (form start &optional end)
  (if end
      `("(" ,@(sql-expand form) ")[" ,@(sql-expand start) ":" ,@(sql-expand end) "]")
      `("(" ,@(sql-expand form) ")[" ,@(sql-expand start) "]")))

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

(def-sql-op :dot (&rest args)
  (sql-expand-list args "."))

(def-sql-op :type (value type)
  `(,@(sql-expand value) "::" ,(to-type-name type)))

(def-sql-op :raw (sql)
  (list sql))

;; Selecting and manipulating

(defun expand-joins (args)
  "Helper for the select operator. Turns the part following :from into
the proper SQL syntax for joining tables."
  (labels ((expand-join (natural-p)
             (let ((type (first args)) (table (second args)) kind param)
               (unless table (sql-error "Incomplete join clause in select."))
               (setf args (cddr args))
               (unless (or natural-p (eq type :cross-join))
                 (setf kind (pop args))
                 (unless (and (or (eq kind :on) (eq kind :using)) args)
                   (sql-error "Incorrect join form in select."))
                 (setf param (pop args)))
               `(" " ,@(when natural-p '("NATURAL "))
                 ,(ecase type
                    (:left-join "LEFT") (:right-join "RIGHT")
                    (:inner-join "INNER") (:outer-join "FULL OUTER")
                    (:cross-join "CROSS")) " JOIN " ,@(sql-expand table)
                 ,@(unless (or natural-p (eq type :cross-join))
                     (ecase kind
                       (:on `(" ON " . ,(sql-expand param)))
                       (:using `(" USING (" ,@(sql-expand-list param) ")")))))))
           (is-join (x)
             (member x '(:left-join :right-join :inner-join :outer-join :cross-join))))
    (when (null args)
      (sql-error "Empty :from clause in select"))
    (loop :for first = t :then nil :while args
          :append (cond ((is-join (car args))
                         (when first (sql-error ":from clause starts with a join."))
                         (expand-join nil))
                        ((eq (car args) :natural)
                         (when first (sql-error ":from clause starts with a join."))
                         (pop args)
                         (expand-join t))
                        (t `(,@(if first () '(", ")) ,@(sql-expand (pop args))))))))

(def-sql-op :select (&rest args)
  (split-on-keywords ((vars *) (distinct - ?) (distinct-on * ?) (from * ?) (where ?) (group-by * ?)
                      (having ?) (window ?)) (cons :vars args)
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

(def-sql-op :limit (form amount &optional offset)
  `("(" ,@(sql-expand form) " LIMIT " ,@(if amount (sql-expand amount) (list "ALL")) ,@(if offset (cons " OFFSET " (sql-expand offset)) ()) ")"))

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
         (of-tables (when of-position (subseq args (1+ of-position) no-wait-position))))
    `("(" ,@(sql-expand form) ,(format nil " FOR ~:@(~A~)" share-or-update)
          ,@(when of-tables (list (format nil " OF ~{~A~^, ~}" (mapcar #'sql-compile of-tables))))
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
  `("CREATE OR REPLACE FUNCTION " ,@(sql-expand name) " (" ,(implode ", " (mapcar 'to-type-name args))
    ") RETURNS " ,(to-type-name return-type) " LANGUAGE SQL " ,(symbol-name stability) " AS " ,(escape-sql-expression body)))

(def-sql-op :insert-into (table &rest rest)
  (split-on-keywords ((method *) (returning ? *)) (cons :method rest)
  `("INSERT INTO " ,@(sql-expand table) " "
    ,@(cond ((eq (car method) :set)
             (cond ((oddp (length (cdr method)))
                    (sql-error "Invalid amount of :set arguments passed to insert-into sql operator"))
                   ((null (cdr method)) '("DEFAULT VALUES"))
                   (t `("(" ,@(sql-expand-list (loop :for (field nil) :on (cdr method) :by #'cddr
                                                     :collect field))
                        ") VALUES (" ,@(sql-expand-list (loop :for (nil value) :on (cdr method) :by #'cddr
                                                              :collect value)) ")"))))
            ((and (not (cdr method)) (consp (car method)) (keywordp (caar method)))
             (sql-expand (car method)))
            (t (sql-error "No :set arguments or select operator passed to insert-into sql operator")))
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
           :append `(,@(unless first '(", ")) "(" ,@(sql-expand-list row) ")")))))

(def-sql-op :insert-rows-into (table &rest rest)
  (split-on-keywords ((columns ? *) (values) (returning ? *)) rest
    `("INSERT INTO "
      ,@(sql-expand table) " "
      ,@(when columns `("(" ,@(sql-expand-list columns) ") "))
      "VALUES "
      ,(if *expand-runtime*
           (expand-rows (car values) (and columns (length columns)))
           `(expand-rows ,(car values) ,(and columns (length columns))))
      ,@(when returning `(" RETURNING " ,@(sql-expand-list returning))))))

(def-sql-op :update (table &rest args)
  (split-on-keywords ((set *) (from * ?) (where ?) (returning ? *)) args
    (when (oddp (length set))
      (sql-error "Invalid amount of :set arguments passed to update sql operator"))
    `("UPDATE " ,@(sql-expand table) " SET "
      ,@(loop :for (field value) :on set :by #'cddr
              :for first = t :then nil
              :append `(,@(if first () '(", ")) ,@(sql-expand field) " = " ,@(sql-expand value)))
      ,@(if from (cons " FROM " (expand-joins from)))
      ,@(if where (cons " WHERE " (sql-expand (car where))) ())
      ,@(when returning
          (cons " RETURNING " (sql-expand-list returning))))))

(def-sql-op :delete-from (table &rest args)
  (split-on-keywords ((where ?) (returning ? *)) args
    `("DELETE FROM " ,@(sql-expand table)
                     ,@(when where (cons " WHERE " (sql-expand (car where))))
                     ,@(when returning (cons " RETURNING " (sql-expand-list returning))))))

(def-sql-op :over (form &rest args)
  (if args `("(" ,@(sql-expand form) " OVER " ,@(sql-expand-list args) ")")
          `("(" ,@(sql-expand form) " OVER ()) ")))

(def-sql-op :partition-by (&rest args)
  (split-on-keywords ((partition-by *) (order-by ? *)) (cons :partition-by args)
                     `("(PARTITION BY " ,@(sql-expand-list partition-by)
                                        ,@(when order-by (cons " ORDER BY " (sql-expand-list order-by)))
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
  ;; todo: better documentation
  "Return the type and whether it may be NULL."
  (if (and (consp type) (eq (car type) 'or))
      (if (and (member 'db-null type) (= (length type) 3))
          (if (eq (second type) 'db-null)
              (values (third type) t)
              (values (second type) t))
          (sql-error "Invalid type: ~a. 'or' types must have two alternatives, one of which is ~s."
                     type 'db-null))
      (values type nil)))

(defun expand-foreign-on* (action)
  (case action
    (:restrict "RESTRICT")
    (:set-null "SET NULL")
    (:set-default "SET DEFAULT")
    (:cascade "CASCADE")
    (:no-action "NO ACTION")
    (t (sql-error "Unsupported action for foreign key: ~A" action))))

(defun %build-foreign-reference (target on-delete on-update)
  `(" REFERENCES "
    ,@(if (consp target)
          `(,(to-sql-name (car target)) "(" ,@(sql-expand-names (cdr target)) ")")
          `(,(to-sql-name target)))
    " ON DELETE " ,(expand-foreign-on* on-delete)
    " ON UPDATE " ,(expand-foreign-on* on-update)))

(defun expand-table-constraint (option args)
  (case option
    (:constraint `("CONSTRAINT " ,(to-sql-name (car args)) " " ,@(expand-table-constraint (cadr args) (cddr args))))
    (:check `("CHECK " ,@(sql-expand (car args))))
    (:primary-key `("PRIMARY KEY (" ,@(sql-expand-names args) ")"))
    (:unique `("UNIQUE (" ,@(sql-expand-names args) ")"))
    (:foreign-key
     (destructuring-bind (columns target &optional (on-delete :restrict) (on-update :restrict)) args
       `("FOREIGN KEY (" ,@(sql-expand-names columns) ")"
                         ,@(%build-foreign-reference target on-delete on-update))))))

(defun expand-table-column (column-name args)
  `(,(to-sql-name column-name) " "
     ,@(let ((type (or (getf args :type)
                       (sql-error "No type specified for column ~A." column-name))))
         (multiple-value-bind (type null) (dissect-type type)
           `(,(to-type-name type) ,@(when (not null) '(" NOT NULL")))))
     ,@(loop :for (option value) :on args :by #'cddr
             :append (case option
                       (:default `(" DEFAULT " ,@(sql-expand value)))
                       (:primary-key (when value `(" PRIMARY KEY")))
                       (:unique (when value `(" UNIQUE")))
                       (:check `(" CHECK " ,@(sql-expand value)))
                       (:references
                        (destructuring-bind (target &optional (on-delete :restrict) (on-update :restrict)) value
                          (%build-foreign-reference target on-delete on-update)))
                       (:type ())
                       (t (sql-error "Unknown column option: ~A." option))))))

(def-sql-op :create-table (name (&rest columns) &rest options)
  (when (null columns)
    (sql-error "No columns defined for table ~A." name))
  `("CREATE TABLE " ,(to-sql-name name) " ("
                    ,@(loop :for ((column-name . args) . rest) :on columns
                            :append (expand-table-column column-name args)
                            :if rest :collect ", ")
                    ,@(loop :for ((option . args)) :on options
                            :collect ", "
                            :append (expand-table-constraint option args))
                    ")"))

(def-sql-op :alter-table (name action &rest args)
  (flet
      ((drop-action (action)
         (case action
           (:restrict "RESTRICT")
           (:cascade "CASCADE")
           (t (sql-error "Unknown DROP action ~A." action)))))
    `("ALTER TABLE "
      ,(to-sql-name name) " "
      ,@ (case action
           (:add (cons "ADD " (expand-table-constraint (first args) (rest args))))
           (:add-column (cons "ADD COLUMN "
                              (expand-table-column (first args) (rest args))))
           (:alter-column (cons "ALTER COLUMN "
                              (expand-table-column (first args) (rest args))))
           (:drop-column (list "DROP COLUMN " (to-sql-name (first args))))
           (:add-constraint (append (list "ADD CONSTRAINT ")
                                    (list (to-sql-name (first args)) " ")
                                    (expand-table-constraint (second args)
                                                             (cddr args))))
           (:drop-constraint (list "DROP CONSTRAINT "
                                   (to-sql-name (first args))
                                   (if (rest args)
                                       (drop-action (second args))
                                       "")))
           (t (sql-error "Unknown ALTER TABLE action ~A" action))))))

(defun expand-create-index (name args)
  (split-on-keywords ((on) (using ?) (fields *) (where ?)) args
    `(,@(sql-expand name) " ON " ,(to-sql-name (first on))
      ,@(when using `(" USING " ,(symbol-name (first using))))
      " (" ,@(sql-expand-names fields) ")"
      ,@(when where `(" WHERE " ,@(sql-expand (first where)))))))

(def-sql-op :create-index (name &rest args)
  (cons "CREATE INDEX " (expand-create-index name args)))

(def-sql-op :create-unique-index (name &rest args)
  (cons "CREATE UNIQUE INDEX " (expand-create-index name args)))

(def-sql-op :cascade (op)
  `(,@(sql-expand op) " CASCADE"))

(defmacro def-drop-op (op-name word)
  `(def-sql-op ,op-name (&rest args)
     (let ((if-exists (if (eq (car args) :if-exists) (pop args) nil)))
       (destructuring-bind (name) args
         `("DROP " ,,word " " ,@(when if-exists '("IF EXISTS ")) ,@(sql-expand name))))))

(def-drop-op :drop-table "TABLE")
(def-drop-op :drop-index "INDEX")
(def-drop-op :drop-sequence "SEQUENCE")
(def-drop-op :drop-view "VIEW")
(def-drop-op :drop-type "TYPE")
(def-drop-op :drop-rule "RULE")

(defun dequote (val)
  (if (and (consp val) (eq (car val) 'quote)) (cadr val) val))

(def-sql-op :nextval (name)
  `("nextval(" ,(if *expand-runtime*
                    (sql-escape-string (to-sql-name (dequote name)))
                    `(sql-escape-string (to-sql-name ,name))) ")"))

(def-sql-op :create-sequence (name &key increment min-value max-value start cache cycle)
  `("CREATE SEQUENCE " ,@(sql-expand name)
    ,@(when increment `(" INCREMENT " ,@(sql-expand increment)))
    ,@(when min-value `(" MINVALUE " ,@(sql-expand min-value)))
    ,@(when max-value `(" MAXVALUE " ,@(sql-expand max-value)))
    ,@(when start `(" START " ,@(sql-expand start)))
    ,@(when cache `(" CACHE " ,@(sql-expand cache)))
    ,@(when cycle `(" CYCLE"))))

(def-sql-op :create-view (name query)
  ;; does not allow to specify the columns of the view yet
  `("CREATE VIEW " ,(to-sql-name name) " AS " ,@(sql-expand query)))

(def-sql-op :create-enum (name members)
  (let ((strings (loop :for m :in members :collect (etypecase m (symbol (string-downcase m)) (string m)))))
    `("CREATE TYPE " ,@(sql-expand name) " AS ENUM (" ,@(sql-expand-list strings) ")")))

;;; http://www.postgresql.org/docs/8.3/interactive/sql-createdomain.html
(def-sql-op :create-domain (name &rest args)
  (split-on-keywords ((type) (default ?) (constraint-name ?) (check ?)) args
    (multiple-value-bind (type may-be-null) (dissect-type (car type))
      `("CREATE DOMAIN " ,@(sql-expand name) " AS " ,(to-type-name type)
                         ,@(when default `(" DEFAULT " ,@(sql-expand (car default))))
                         ,@(when constraint-name `(" CONSTRAINT " ,@(sql-expand (car constraint-name))))
                         ,@(unless may-be-null '(" NOT NULL"))
                         ,@(when check `(" CHECK" ,@(sql-expand (car check))))))))

(def-sql-op :drop-domain (name)
  `("DROP DOMAIN " ,@(sql-expand name)))

;http://www.postgresql.org/docs/8.3/static/sql-createrule.html
(def-sql-op :create-rule (name &rest rest)
  (split-on-keywords ((on) (to) (where ?) (instead ? -) (do ? *)) rest
    (check-type (car on) (member :select :insert :update :delete))
    `("CREATE RULE " ,@(sql-expand name)
      " AS ON " ,(symbol-name (car on)) " TO " ,@(sql-expand (car to))
      ,@(when where `(" WHERE " ,@(sql-expand (car where))))
      " DO" ,@(when instead '(" INSTEAD"))
      ,@(if (or (null do) (eq do :nothing))
            '(" NOTHING")
            `("(" ,@(sql-expand-list do "; ") ")")))))
