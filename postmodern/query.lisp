;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

;; Like alist-row-reader from cl-postgres, but converts the field
;; names to keywords (with underscores converted to dashes).
(def-row-reader symbol-alist-row-reader (fields)
  (let ((symbols (map 'list (lambda (desc) (from-sql-name (field-name desc)))
                      fields)))
    (loop :while (next-row)
          :collect (loop :for field :across fields
                         :for symbol :in symbols
                         :collect (cons symbol (next-field field))))))

;; Like symbol-alist-row-reader, but return plist
(def-row-reader symbol-plist-row-reader (fields)
  (let ((symbols (map 'list (lambda (desc) (from-sql-name (field-name desc)))
                      fields)))
    (loop :while (next-row)
          :collect (loop :for field :across fields
                         :for symbol :in symbols
                         :collect symbol :collect (next-field field)))))

;; Like symbol-alist-row-reader, but return a string holding one or more json objects
(def-row-reader json-row-reader (fields)
  (let ((symbols (map 'list (lambda (desc) (from-sql-name (field-name desc)))
                      fields)))
    (mapcar #'encode-json-plist-to-string
     (loop :while (next-row)
           :collect (loop :for field :across fields
                          :for symbol :in symbols
                          :collect symbol :collect (next-field field))))))

(def-row-reader json-row-array-reader (fields)
  (let ((symbols (map 'list (lambda (desc) (from-sql-name (field-name desc)))
                      fields)))
    (format nil "[~{~a~^, ~}]"
     (mapcar #'encode-json-plist-to-string
             (loop :while (next-row)
                   :collect (loop :for field :across fields
                                  :for symbol :in symbols
                                  :collect symbol :collect (next-field field)))))))

;; Converts field names to hash table keys and returns an array of rows
(def-row-reader array-hash-row-reader (fields)
  (loop :while (next-row)
        :collect (loop :for field :across fields
                       with hash = (make-hash-table :test 'equal)
                       do  (setf (gethash  (field-name field) hash)
                                 (next-field field))
                       finally (return hash)) into result
        :finally (return (make-array (length result)
                                     :initial-contents result))))

;; A row-reader for reading only a single column, and returning a list
;; of single values.
(def-row-reader column-row-reader (fields)
  (assert (= (length fields) 1))
  (loop :while (next-row)
        :collect (next-field (elt fields 0))))

#+postmodern-thread-safe
(defvar *class-finalize-lock* (bt:make-lock))


(defun dao-spec-for-format (format)
  (if (and (consp format)
           (eq :dao (car format)))
      (cdr format)))

(defun reader-for-format (format)
  (let ((format-spec (cdr (assoc format *result-styles*))))
    (if format-spec
        `(',(car format-spec) ,@(cdr format-spec))
        (destructuring-bind (class-name &optional result)
            (dao-spec-for-format format)
          (unless class-name
            (error "~S is not a valid result style." format))
          (let ((class (gensym)))
            (list `(let ((,class (find-class ',class-name)))
                     (unless (class-finalized-p ,class)
                       #+postmodern-thread-safe
                       (bordeaux-threads:with-lock-held (*class-finalize-lock*)
                         (unless (class-finalized-p ,class)
                           (finalize-inheritance ,class)))
                       #-postmodern-thread-safe
                       (finalize-inheritance ,class))
                     (dao-row-reader ,class))
                  (if (eq result :single)
                      'single-row
                      'all-rows)))))))

(defmacro all-rows (form)
  form)

(defmacro single-row (form)
  `(multiple-value-bind (rows affected) ,form
     (if affected (values (car rows) affected) (car rows))))

(defmacro single-row! (form)
  `(multiple-value-bind (rows affected) ,form
     (unless (= (length rows) 1)
       (error 'database-error
              :message (format nil "Query for a single row returned ~a rows."
                               (length rows))))
     (if affected (values (car rows) affected) (car rows))))

(defun real-query (query)
  "Used for supporting both plain string queries and S-SQL constructs.
Looks at the argument at compile-time and wraps it in (sql ...) if it
looks like an S-SQL query."
  (if (and (consp query) (keywordp (first query)))
      `(sql ,query)
      query))

(defmacro query (query &rest args/format)
  "Execute the given query, which can be either a string or an S-SQL form (list
starting with a keyword). If the query contains placeholders ($1, $2, etc)
their values can be given as extra arguments. If one of these arguments is a
keyword occurring in the table below, it will not be used as a query
argument, but will determine the format in which the results are returned
instead. Any of the following formats can be used, with the default being :rows:

| :none	             | Ignore the result values.                                 |
| :lists, :rows      | Return a list of lists, each list containing the values   |
|                    | for a row.                                                |
| :list, :row        | Return a single row as a list.                            |
| :alists	           | Return a list of alists which map column names to values, |
|                    | with the names represented as keywords.                   |
| :alist	           | Return a single row as an alist.                          |
| :array-hash        | Return an array of hashtables which map column names to   |
|                    | hash table keys. NOTE: It will return an empty array      |
|                    | if there is no result.                                    |
| :vectors           | Returns a vector of vectors where each internal vector is |
|                    | a returned row from the query. The field names are not    |
|                    | included. NOTE: It will return an empty vector instead of |
|                    | NIL if there is no result.                                |
| :str-alists        | Like :alists, but use the original column names.          |
| :str-alist	       | Return a single row as an alist, with strings for names.  |
| :plists	           | Return a list of plists which map column names to values, |
|                    | with the names represented as keywords.                   |
| :plist	           | Return a single row as a plist.                           |
| :column	           | Return a single column as a list.                         |
| :single	           | Return a single value.                                    |
| :single!	         | Like :single, but raise an error when the number of       |
|                    | selected rows is not equal to 1.                          |
| (:dao type)	       | Return a list of DAOs of the given type. The names of the |
|                    | fields returned by the query must match slots in the DAO  |
|                    | class the same way as with query-dao.                     |
| (:dao type :single)| Return a single DAO of the given type.                    |
| :json-strs         | Return a list of strings where each row is a json object  |
|                    | expressed as a string                                     |
| :json-str          | Return a single string where the row returned is a json   |
|                    | object expressed as a string.                             |
| :json-array-str    | Return a string containing a json array, each element in  |
|                    | the array is a selected row expressed as a json object.   |
|                    | NOTE: If there is no result, it will return a string with |
|                    | an empty json array.                                      |

If the database returns information about the amount rows that were affected,
such as with updating or deleting queries, this is returned as a second value."
  (let* ((format :rows)
         (args (loop :for arg :in args/format
                     :if (or (dao-spec-for-format arg)
                             (assoc arg *result-styles*)) :do (setf format arg)
                     :else :collect arg)))
    (destructuring-bind (reader result-form) (reader-for-format format)
      (let ((base (if args
		                  (let ((vars (loop :for x :in args :collect (gensym))))
			                  `(let ,(loop :for v :in vars :for a :in args :collect `(,v ,a))
			                     (prepare-query *database* "" ,(real-query query) (list ,@vars))
			                     (exec-prepared *database* "" (list ,@vars) ,reader)))
		                  `(exec-query *database* ,(real-query query) ,reader))))
        `(,result-form ,base)))))

(defmacro execute (query &rest args)
  "Execute a query, ignore the results. So, in effect, like a query called with
format :none. Returns the amount of affected rows as its first returned
value. (Also returns this amount as the second returned value, but use of this
is deprecated.)"
  `(let ((rows (nth-value 1 (query ,query ,@args :none))))
     (if rows (values rows rows) 0)))

(defmacro doquery (query (&rest names) &body body)
  "Execute the given query (a string or a list starting with a keyword),
iterating over the rows in the result. The body will be executed with the values
in the row bound to the symbols given in names. To iterate over a
parameterised query, one can specify a list whose car is the query, and whose
cdr contains the arguments. For example:

(doquery (:select 'name 'score :from 'scores) (n s)
  (incf (gethash n *scores*) s))

(doquery ((:select 'name :from 'scores :where (:> 'score '$1)) 100) (name)
  (print name))"
  (let* ((fields (gensym))
         (query-name (gensym))
         args
         (reader-expr
           `(row-reader (,fields)
              (unless (= ,(length names) (length ,fields))
                (error "Number of field names does not match number of selected fields in query ~A."
                       ,query-name))
              (loop :while (next-row)
                    :do (let ,(loop :for i :from 0
                                    :for name :in names
                                    :collect `(,name (next-field (elt ,fields ,i))))
                          ,@body)))))
    (when (and (consp query) (not (keywordp (first query))))
      (setf args (cdr query) query (car query)))
    (if args
        `(let ((,query-name ,(real-query query)))
           (prepare-query *database* "" ,query-name (list ,@args))
           (exec-prepared *database* "" (list ,@args) ,reader-expr))
        `(let ((,query-name ,(real-query query)))
           (exec-query *database* ,query-name ,reader-expr)))))
