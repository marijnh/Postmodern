(in-package :postmodern)

(defclass dao-class (standard-class)
  ((direct-keys :initarg :keys :initform nil :reader direct-keys)
   (effective-keys :reader dao-keys)
   (table-name)
   (column-map :reader dao-column-map))
  (:documentation "Metaclass for database-access-object classes."))

(defmethod dao-keys :before ((class dao-class))
  (unless (class-finalized-p class)
    (finalize-inheritance class)))

(defmethod validate-superclass ((class dao-class) (super-class standard-class))
  t)

(defmethod dao-keys ((class-name symbol))
  (dao-keys (find-class class-name)))

(defmethod dao-keys (dao)
  (mapcar #'(lambda (slot)
              (slot-value dao slot))
          (dao-keys (class-of dao))))

(defun dao-column-slots (class)
  "Enumerate the slots in a class that refer to table rows."
  (mapcar 'slot-column
          (remove-if-not (lambda (x) (typep x 'effective-column-slot))
                         (class-slots class))))
(defun dao-column-fields (class)
  (mapcar 'slot-definition-name (dao-column-slots class)))

(defun dao-table-name (class)
  (when (symbolp class)
    (setf class (find-class class)))
  (if (slot-boundp class 'table-name)
      (slot-value class 'table-name)
      (class-name class)))

(defmethod shared-initialize :before ((class dao-class) slot-names
                                      &key table-name &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-value class 'direct-keys) nil)
  (if table-name
      (setf (slot-value class 'table-name)
            (if (symbolp (car table-name)) (car table-name) (intern (car table-name))))
      (slot-makunbound class 'table-name)))

(defun dao-superclasses (class)
  "Build a list of superclasses of a given class that are DAO
  classes."
  (let ((found ()))
    (labels ((explore (class)
               (when (typep class 'dao-class)
                 (pushnew class found))
               (mapc #'explore (class-direct-superclasses class))))
      (explore class)
      found)))

(defmethod finalize-inheritance :after ((class dao-class))
  "Building a row reader and a set of methods can only be done after
  inheritance has been finalised."
  ;; The effective set of keys of a class is the union of its keys and
  ;; the keys of all its superclasses.
  (setf (slot-value class 'effective-keys)
        (reduce 'union (mapcar 'direct-keys (dao-superclasses class))))
  (unless (every (lambda (x) (member x (dao-column-fields class))) (dao-keys class))
    (error "Class ~A has a key that is not also a slot." (class-name class)))
  (build-dao-methods class))


(defclass direct-column-slot (standard-direct-slot-definition)
  ((col-type :initarg :col-type :reader column-type)
   (col-default :initarg :col-default :reader column-default)
   (ghost :initform nil :initarg :ghost :reader ghost)
   (sql-name :reader slot-sql-name))
  (:documentation "Type of slots that refer to database columns."))

(defmethod shared-initialize :after ((slot direct-column-slot) slot-names
                                     &key col-type col-default (col-name nil col-name-p) &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-value slot 'sql-name) (to-sql-name
                                     (if col-name-p
                                         col-name
                                         (slot-definition-name slot))))
  ;; The default for nullable columns defaults to :null.
  (when (and (null col-default) (consp col-type) (eq (car col-type) 'or)
             (member 'db-null col-type) (= (length col-type) 3))
    (setf (slot-value slot 'col-default) :null)))

(defmethod direct-slot-definition-class ((class dao-class) &key column col-type &allow-other-keys)
  "Slots that have a :col-type option are column-slots."
  (if (or column col-type)
      (find-class 'direct-column-slot)
      (call-next-method)))

(defparameter *direct-column-slot* nil
  "This is used to communicate the fact that a slot is a column to
  effective-slot-definition-class.")

(defclass effective-column-slot (standard-effective-slot-definition)
  ((direct-slot :initform *direct-column-slot* :reader slot-column)))

(defmethod compute-effective-slot-definition ((class dao-class) name direct-slot-definitions)
  (declare (ignore name))
  (flet ((is-column (slot) (typep slot 'direct-column-slot)))
    (let ((*direct-column-slot* (find-if #'is-column direct-slot-definitions)))
      #+(or) ;; Things seem to work without this check. Removed for now.
      (when (and *direct-column-slot*
                 (not (every #'is-column direct-slot-definitions)))
        (error "Slot ~a in class ~a is both a column slot and a regular slot." name class))
      (call-next-method))))

(defmethod effective-slot-definition-class ((class dao-class) &rest initargs)
  (declare (ignore initargs))
  (if *direct-column-slot*
      (find-class 'effective-column-slot)
      (call-next-method)))

(defgeneric dao-exists-p (dao)
  (:documentation "Return a boolean indicating whether the given dao
  exists in the database."))
(defgeneric insert-dao (dao)
  (:documentation "Insert the given object into the database."))
(defgeneric update-dao (dao)
  (:documentation "Update the object's representation in the database
  with the values in the given instance."))
(defgeneric delete-dao (dao)
  (:documentation "Delete the given dao from the database."))
(defgeneric upsert-dao (dao)
  (:documentation "Update or insert the given dao.  If its primary key
  is already in the database and all slots are bound, an update will
  occur.  Otherwise it tries to insert it."))
(defgeneric get-dao (type &rest args)
  (:method ((class-name symbol) &rest args)
    (let ((class (find-class class-name)))
      (if (class-finalized-p class)
          (error "Class ~a has no key slots." (class-name class))
          (finalize-inheritance class))
      (apply 'get-dao class-name args)))
  (:documentation "Get the object corresponding to the given primary
  key, or return nil if it does not exist."))
(defgeneric make-dao (type &rest args &key &allow-other-keys)
  (:method ((class-name symbol) &rest args &key &allow-other-keys)
    (let ((class (find-class class-name)))
      (apply 'make-dao class args)))
  (:method ((class dao-class) &rest args &key &allow-other-keys)
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (let ((instance (apply #'make-instance class args)))
      (insert-dao instance)))
  (:documentation "Make the instance of the given class and insert it into the database"))

(defmacro define-dao-finalization (((dao-name class) &rest keyword-args) &body body)
  (let ((args-name (gensym)))
    `(defmethod make-dao :around ((class (eql ',class))
				  &rest ,args-name
				  &key ,@keyword-args &allow-other-keys)
       (declare (ignorable ,args-name))
       (let ((,dao-name (call-next-method)))
	 ,@body
	 (update-dao ,dao-name)))))

(defgeneric fetch-defaults (object)
  (:documentation "Used to fetch the default values of an object on
  creation."))

(defun %eval (code)
  (funcall (compile nil `(lambda () ,code))))

(defun build-dao-methods (class)
  "Synthesise a number of methods for a newly defined DAO class.
\(Done this way because some of them are not defined in every
situation, and each of them needs to close over some pre-computed
values.)"

  (setf (slot-value class 'column-map)
        (mapcar (lambda (s) (cons (slot-sql-name s) (slot-definition-name s))) (dao-column-slots class)))

  (%eval
    `(let* ((fields (dao-column-fields ,class))
            (key-fields (dao-keys ,class))
            (ghost-slots (remove-if-not 'ghost (dao-column-slots ,class)))
            (ghost-fields (mapcar 'slot-definition-name ghost-slots))
            (value-fields (remove-if (lambda (x) (or (member x key-fields) (member x ghost-fields))) fields))
            (table-name (dao-table-name ,class)))
       (labels ((field-sql-name (field)
                  (make-symbol (car (find field (slot-value ,class 'column-map) :key #'cdr :test #'eql))))
                (test-fields (fields)
                  `(:and ,@(loop :for field :in fields :collect (list := (field-sql-name field) '$$))))
                (set-fields (fields)
                  (loop :for field :in fields :append (list (field-sql-name field) '$$)))
                (slot-values (object &rest slots)
                  (loop :for slot :in (apply 'append slots) :collect (slot-value object slot))))

         ;; When there is no primary key, a lot of methods make no sense.
         (when key-fields
           (let ((tmpl (sql-template `(:select (:exists (:select t :from ,table-name
                                                                 :where ,(test-fields key-fields)))))))
             (defmethod dao-exists-p ((object ,class))
               (and (every (lambda (s) (slot-boundp object s)) key-fields)
                    (query (apply tmpl (slot-values object key-fields)) :single))))

           ;; When all values are primary keys, updating makes no sense.
           (when value-fields
             (let ((tmpl (sql-template `(:update ,table-name :set ,@(set-fields value-fields)
                                         :where ,(test-fields key-fields)))))
               (defmethod update-dao ((object ,class))
                 (when (zerop (execute (apply tmpl (slot-values object value-fields key-fields))))
                   (error "Updated row does not exist."))
                 object)

               (defmethod upsert-dao ((object ,class))
                 (handler-case
                     (if (zerop (execute (apply tmpl (slot-values object value-fields key-fields))))
                         (values (insert-dao object) t)
                         (values object nil))
                   (unbound-slot ()
                     (values (insert-dao object) t))))))

           (let ((tmpl (sql-template `(:delete-from ,table-name :where ,(test-fields key-fields)))))
             (defmethod delete-dao ((object ,class))
               (execute (apply tmpl (slot-values object key-fields)))))

           (let ((tmpl (sql-template `(:select * :from ,table-name :where ,(test-fields key-fields)))))
             (defmethod get-dao ((type (eql (class-name ,class))) &rest keys)
               (car (exec-query *database* (apply tmpl keys) (dao-row-reader ,class))))))

         (defmethod insert-dao ((object ,class))
           (let (bound unbound)
             (loop :for field :in fields
                :do (if (slot-boundp object field)
                        (push field bound)
                        (push field unbound)))

             (let* ((values (mapcan (lambda (x) (list (field-sql-name x) (slot-value object x)))
                                    (remove-if (lambda (x) (member x ghost-fields)) bound) ))
                    (returned (query (sql-compile `(:insert-into ,table-name
                                                                 :set ,@values
                                                                 ,@(when unbound (cons :returning unbound))))
                                     :row)))
               (when unbound
                 (loop :for value :in returned
                    :for field :in unbound
                    :do (setf (slot-value object field) value)))))
           object)


         (let* ((defaulted-slots (remove-if-not (lambda (x) (slot-boundp x 'col-default))
                                                (dao-column-slots ,class)))
                (defaulted-names (mapcar 'slot-definition-name defaulted-slots))
                (default-values (mapcar 'column-default defaulted-slots)))
           (if defaulted-slots
               (defmethod fetch-defaults ((object ,class))
                 (let (names defaults)
                   ;; Gather unbound slots and their default expressions.
                   (loop :for slot-name :in defaulted-names
                      :for default :in default-values
                      :do (unless (slot-boundp object slot-name)
                            (push slot-name names)
                            (push default defaults)))
                   ;; If there are any unbound, defaulted slots, fetch their content.
                   (when names
                     (loop :for value :in (query (sql-compile (cons :select defaults)) :list)
                        :for slot-name :in names
                        :do (setf (slot-value object slot-name) value)))))
               (defmethod fetch-defaults ((object ,class))
                 nil)))

         (defmethod shared-initialize :after ((object ,class) slot-names
                                              &key (fetch-defaults nil) &allow-other-keys)
           (declare (ignore slot-names))
           (when fetch-defaults
             (fetch-defaults object)))))))

(defparameter *custom-column-writers* nil
  "A hook for locally overriding/adding behaviour to DAO row readers.
Should be an alist mapping strings (column names) to symbols or
functions. Symbols are interpreted as slot names that values should be
written to, functions are called with the new object and the value as
arguments.")

(defmacro with-column-writers ((&rest defs) &body body)
  `(let ((*custom-column-writers* (append (list ,@(loop :for (field writer) :on defs :by #'cddr
                                                        :collect `(cons (to-sql-name ,field) ,writer)))
                                          *custom-column-writers*)))
    ,@body))

(defparameter *ignore-unknown-columns* nil)

(defun dao-from-fields (class column-map query-fields result-next-field-generator-fn)
  (let ((instance (allocate-instance class)))
    (loop :for field :across query-fields
         :for writer := (cdr (assoc (field-name field) column-map :test #'string=))
         :do (etypecase writer
               (null (if *ignore-unknown-columns*
                         (funcall result-next-field-generator-fn field)
                         (error "No slot named ~a in class ~a. DAO out of sync with table, or incorrect query used."
                                (field-name field) (class-name class))))
               (symbol (setf (slot-value instance writer) (funcall result-next-field-generator-fn field)))
               (function (funcall writer instance (funcall result-next-field-generator-fn field)))))
    (initialize-instance instance)
    instance))

(defun dao-row-reader (class)
  "Defines a row-reader for objects of a given class."
  (row-reader (query-fields)
    (let ((column-map (append *custom-column-writers* (dao-column-map class))))
      (loop :while (next-row)
            :collect (dao-from-fields class column-map query-fields #'next-field)))))

(defun save-dao (dao)
  "Try to insert the content of a DAO. If this leads to a unique key
violation, update it instead."
  (handler-case (progn (insert-dao dao) t)
    (cl-postgres-error:unique-violation ()
      (update-dao dao)
      nil)))

(defun save-dao/transaction (dao)
  (handler-case (with-savepoint save-dao/transaction (insert-dao dao) t)
    (cl-postgres-error:unique-violation ()
      (update-dao dao)
      nil)))

(defun query-dao% (type query row-reader &rest args)
  (let ((class (find-class type)))
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (if args
	(progn
	  (prepare-query *database* "" query)
	  (exec-prepared *database* "" args row-reader))
	(exec-query *database* query row-reader))))

(defmacro query-dao (type query &rest args)
  "Execute a query and return the result as daos of the given type.
The fields returned by the query must match the slots of the dao, both
by type and by name."
  `(query-dao% ,type ,(real-query query) (dao-row-reader (find-class ,type)) ,@args))

(defmacro dao-row-reader-with-body ((type type-var) &body body)
  (let ((fields (gensym))
        (column-map (gensym)))
    `(row-reader (,fields)
       (let ((,column-map (append *custom-column-writers* (dao-column-map (find-class ,type)))))
         (loop :while (next-row)
            :do (let ((,type-var (dao-from-fields (find-class ,type) ,column-map ,fields #'next-field)))
                  ,@body))))))

(defmacro do-query-dao (((type type-var) query) &body body)
  "Like query-dao, but rather than returning a list of results,
executes BODY once for each result, with TYPE-VAR bound to the DAO
representing that result."
  (let (args)
    (when (and (consp query) (not (keywordp (first query))))
      (setf args (cdr query) query (car query)))
    `(query-dao% ,type ,(real-query query)
                 (dao-row-reader-with-body (,type ,type-var)
                   ,@body)
                 ,@args)))

(defun generate-dao-query (type &optional (test t) ordering)
  (flet ((check-string (x)
           (if (stringp x) `(:raw ,x) x)))
    (let ((query `(:select '* :from (dao-table-name (find-class ,type))
                   :where ,(check-string test))))
      (when ordering
        (setf query `(:order-by ,query ,@(mapcar #'check-string ordering))))
      query)))

(defmacro select-dao (type &optional (test t) &rest ordering)
  "Select daos for the rows in its table for which the given test
holds, order them by the given criteria."
  `(query-dao% ,type (sql ,(generate-dao-query type test ordering)) (dao-row-reader (find-class ,type))))

(defmacro do-select-dao (((type type-var) &optional (test t) &rest ordering) &body body)
  "Like select-dao, but rather than returning a list of results,
executes BODY once for each result, with TYPE-VAR bound to the DAO
representing that result."
  `(query-dao% ,type (sql ,(generate-dao-query type test ordering))
               (dao-row-reader-with-body (,type ,type-var)
                 ,@body)))

(defun dao-table-definition (table)
  "Generate the appropriate CREATE TABLE query for this class."
  (unless (typep table 'dao-class)
    (setf table (find-class table)))
  (unless (class-finalized-p table)
    (finalize-inheritance table))
  (sql-compile
   `(:create-table ,(dao-table-name table)
                   ,(loop :for slot :in (dao-column-slots table)
                          :unless (ghost slot)
                          :collect `(,(slot-definition-name slot) :type ,(column-type slot)
                                     ,@(when (slot-boundp slot 'col-default)
                                             `(:default ,(column-default slot)))))
                   ,@(when (dao-keys table)
                       `((:primary-key ,@(dao-keys table)))))))
