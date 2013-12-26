(in-package :postmodern)

;; Like alist-row-reader from cl-postgres, but converts the field
;; names to keywords (with underscores converted to dashes).
(def-row-reader symbol-alist-row-reader (fields)
  (let ((symbols (map 'list (lambda (desc) (from-sql-name (field-name desc))) fields)))
    (loop :while (next-row)
          :collect (loop :for field :across fields
                         :for symbol :in symbols
                         :collect (cons symbol (next-field field))))))

;; Like symbol-alist-row-reader, but return plist
(def-row-reader symbol-plist-row-reader (fields)
  (let ((symbols (map 'list (lambda (desc) (from-sql-name (field-name desc))) fields)))
    (loop :while (next-row)
          :collect (loop :for field :across fields
                         :for symbol :in symbols
                         :collect symbol :collect (next-field field)))))

;; A row-reader for reading only a single column, and returning a list
;; of single values.
(def-row-reader column-row-reader (fields)
  (assert (= (length fields) 1))
  (loop :while (next-row)
        :collect (next-field (elt fields 0))))

(defparameter *result-styles*
  '((:none ignore-row-reader all-rows)
    (:lists list-row-reader all-rows)
    (:list list-row-reader single-row)
    (:rows list-row-reader all-rows)
    (:row list-row-reader single-row)
    (:alists symbol-alist-row-reader all-rows)
    (:alist symbol-alist-row-reader single-row)
    (:str-alists alist-row-reader all-rows)
    (:str-alist alist-row-reader single-row)
    (:plists symbol-plist-row-reader all-rows)
    (:plist symbol-plist-row-reader single-row)
    (:column column-row-reader all-rows)
    (:single column-row-reader single-row)
    (:single! column-row-reader single-row!))
  "Mapping from keywords identifying result styles to the row-reader
that should be used and whether all values or only one value should be
returned.")

(defun dao-spec-for-format (format)
  (if (and (consp format)
	   (eq :dao (car format)))
      (cdr format)))

(defun reader-for-format (format)
  (let ((format-spec (cdr (assoc format *result-styles*))))
    (if format-spec
	`(',(car format-spec) ,@(cdr format-spec))
	(destructuring-bind (class &optional result)
	    (dao-spec-for-format format)
	  (unless class
	    (error "~S is not a valid result style." format))
	  (let ((class-name (gensym)))
	    (list `(let ((,class-name (find-class ',class)))
		     (unless (class-finalized-p ,class-name)
		       (finalize-inheritance ,class-name))
		     (dao-row-reader ,class-name))
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
      (error 'database-error :message (format nil "Query for a single row returned ~a rows." (length rows))))
    (if affected (values (car rows) affected) (car rows))))

(defun real-query (query)
  "Used for supporting both plain string queries and S-SQL constructs.
Looks at the argument at compile-time and wraps it in (sql ...) if it
looks like an S-SQL query."
  (if (and (consp query) (keywordp (first query)))
      `(sql ,query)
      query))

(defmacro query (query &rest args/format)
  "Execute a query, optionally with arguments to put in the place of
$X elements. If one of the arguments is a known result style or a class name,
it specifies the format in which the results should be returned."
  (let* ((format :rows)
         (args (loop :for arg :in args/format
		     :if (or (dao-spec-for-format arg)
			     (assoc arg *result-styles*)) :do (setf format arg)
                     :else :collect arg)))
    (destructuring-bind (reader result-form) (reader-for-format format)
      (let ((base (if args
                      `(progn
                        (prepare-query *database* "" ,(real-query query))
                        (exec-prepared *database* "" (list ,@args) ,reader))
                      `(exec-query *database* ,(real-query query) ,reader))))
        `(,result-form ,base)))))

(defmacro execute (query &rest args)
  "Execute a query, ignore the results."
  `(let ((rows (nth-value 1 (query ,query ,@args :none))))
    (if rows (values rows rows) 0)))

(defmacro doquery (query (&rest names) &body body)
 "Iterate over the rows in the result of a query, binding the given
names to the results and executing body for every row. Query can be a
string, an s-sql query, or a list starting with one of those, followed
by the arguments to parameterize the query with."
 (let* ((fields (gensym))
        (query-name (gensym))
        args
        (reader-expr
          `(row-reader (,fields)
             (unless (= ,(length names) (length ,fields))
               (error "Number of field names does not match number of selected fields in query ~A." ,query-name))
             (loop :while (next-row)
                   :do (let ,(loop :for i :from 0
                                   :for name :in names
                                   :collect `(,name (next-field (elt ,fields ,i))))
                         ,@body)))))
   (when (and (consp query) (not (keywordp (first query))))
     (setf args (cdr query) query (car query)))
   (if args
       `(let ((,query-name ,(real-query query)))
          (prepare-query *database* "" ,query-name)
          (exec-prepared *database* "" (list ,@args) ,reader-expr))
       `(let ((,query-name ,(real-query query)))
          (exec-query *database* ,query-name ,reader-expr)))))
