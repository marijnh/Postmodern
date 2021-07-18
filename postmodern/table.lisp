;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

(defclass dao-class (standard-class)
  ((direct-keys :initarg :keys :initform nil :reader direct-keys)
   (effective-keys :reader dao-keys)
   (table-name)
   (column-map :reader dao-column-map))
  (:documentation "At the heart of Postmodern's DAO system is the dao-class
metaclass. It allows you to define classes for your database-access objects as
regular CLOS classes. Some of the slots in these classes will refer to columns
in the database. To specify that a slot refers to a column, give it a :col-type
option containing an S-SQL type expression (useful if you want to be able to
derive a table definition from the class definition), or simply a :column
option with value T. Such slots can also take a :col-default option, used to
provide a database-side default value as an S-SQL expression. You can use the
:col-name initarg (whose unevaluated value will be passed to to-sql-name) to
specify the slot's column's name.

DAO class definitions support two extra class options: :table-name to give the
name of the table that the class refers to (defaults to the class name), and
:keys to provide a set of primary keys for the table. If more than one key is
provided, this creates a multi-column primary key and all keys must be
specified when using operations such as update-dao and get-dao. When no primary
keys are defined, operations such as update-dao and get-dao will not work.

IMPORTANT: Class finalization for a dao class instance are wrapped with a
thread lock. However, any time you are using threads and a class that
inherits from other classes, you should ensure that classes are finalized
before you start generating threads that create new instances of that class.

The (or db-null integer) form is used to indicate a column can have NULL values
otherwise the column will be treated as NOT NULL.

Simple example:

    (defclass users ()
      ((name :col-type string :initarg :name :accessor user-name)
       (creditcard :col-type (or db-null integer) :initarg :card :col-default :null)
       (score :col-type bigint :col-default 0 :accessor user-score))
      (:metaclass dao-class)
      (:keys name))

In this case the name of the users will be treated as the primary key and the
database table is assume to be users.

Now look at a slightly more complex example.

    (defclass country ()
      ((id :col-type integer :col-identity t :accessor id)
       (name :col-type text :col-unique t :initarg :country :accessor country)
       (region-id :col-type integer :col-references ((regions id)) :initarg :region-id
          :accessor region-id))
        (:metaclass dao-class)
      (:table-name countries))

In this example we have an id column which is specified to be an identity column.
Postgresql will automatically generate a sequence of of integers and this will
be the primary key.

We have a name column which is specified as unique and is not null.

We have a region-id column which references the id column in the regions table.
This is a foreign key constraint and Postgresql will not accept inserting a country
into the database unless there is an existing region with an id that matches this
number. Postgresql will also not allow deleting a region if there are countries
that reference that region's id. If we wanted Postgresql to delete countries when
regions are deleted, that column would be specified as:

   (region-id :col-type integer :col-references ((regions id) :cascade)
     :initarg :region-id :accessor region-id)

Now you can see why the double parens.

We also specify that the table name is not 'country' but 'countries'. (Some style guides
recommend that table names be plural and references to rows be singular.)

When inheriting from DAO classes, a subclass' set of columns also contains
all the columns of its superclasses. The primary key for such a class is the
union of its own keys and all the keys from its superclasses. Classes
inheriting from DAO classes should probably always use the dao-class metaclass
themselves.

When a DAO is created with make-instance, the :fetch-defaults keyword argument
can be passed, which, when T, will cause a query to fetch the default values
for all slots that refers to columns with defaults and were not bound through
initargs. In some cases, such as serial columns, which have an implicit default,
this will not work. You can work around this by creating your own sequence,
e.g. 'my_sequence', and defining a (:nextval \"my_sequence\") default.

Finally, DAO class slots can have an option :ghost t to specify them as ghost
slots. These are selected when retrieving instances, but not written when
updating or inserting, or even included in the table definition. The only known
use for this to date is for creating the table with (oids=true), and specify a
slot like this:

    (oid :col-type integer :ghost t :accessor get-oid)"))

(defgeneric dao-keys (class)
  (:documentation "Returns list of slot names that are the primary key of DAO
class. Explicit keys takes priority over col-identity which takes priority
over col-primary-key.

This is likely interesting if you have primary keys which are composed of
more than one slot. Pay careful attention to situations where the primary key
not only has more than one column, but they are actually in a different order
than they are in the database table itself. Obviously the table needs to have
been defined. You can provide a quoted class-name or an instance of a dao."))

(defmethod dao-keys :before ((class dao-class))
  (unless (class-finalized-p class)
    #+postmodern-thread-safe
    (unless (class-finalized-p class)
      (bordeaux-threads:with-lock-held (*class-finalize-lock*)
        (unless (class-finalized-p class)
          (finalize-inheritance class))))
    #-postmodern-thread-safe
    (finalize-inheritance class)))

(defgeneric find-primary-key-column (class)
  (:documentation "Loops through a class's column definitions and returns
the first column name that has bound either col-identity or col-primary-key.
Returns a symbol."))

(defmethod find-primary-key-column ((class dao-class))
  (loop for x in (dao-column-slots class) do
            (if (or (slot-boundp x 'col-identity)
                  (slot-boundp x 'col-primary-key))
                (return (slot-definition-name x)))))

(defmethod find-primary-key-column ((class symbol))
  (loop for x in (dao-column-slots (find-class class)) do
            (if (or (slot-boundp x 'col-identity)
                  (slot-boundp x 'col-primary-key))
                (return (slot-definition-name x)))))

(defmethod find-primary-key-column (dao)
  (loop for x in (dao-column-slots (class-of dao)) do
            (if (or (slot-boundp x 'col-identity)
                  (slot-boundp x 'col-primary-key))
                (return (slot-definition-name x)))))

(defmethod validate-superclass ((class dao-class) (super-class standard-class))
  t)

(defmethod dao-keys ((class-name symbol))
  (dao-keys (find-class class-name)))

(defmethod dao-keys (dao)
  (mapcar #'(lambda (slot)
              (slot-value dao slot))
          (dao-keys (the dao-class (class-of dao)))))

(defun dao-column-slots (class)
  "Enumerate the slots in a class that refer to table rows."
  (cond ((closer-mop:classp class)
         (mapcar 'slot-column
          (remove-if-not (lambda (x) (typep x 'effective-column-slot))
                         (closer-mop:class-slots class))))
        ((symbolp class)
         (mapcar 'slot-column
          (remove-if-not (lambda (x) (typep x 'effective-column-slot))
                         (closer-mop:class-slots (find-class class)))))
        ((typep class 'dao-class)
         (mapcar 'slot-column
          (remove-if-not (lambda (x) (typep x 'effective-column-slot))
                         (closer-mop:class-slots (class-of class)))))
        (t nil)))

(defun dao-column-fields (class)
  "Returns a list of symbols of the names of the slots in a class."
  (mapcar 'slot-definition-name (dao-column-slots class)))

(defun dao-table-name (class)
  "Get the name of the table associated with the given DAO class (or symbol naming
such a class)."
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
            (if (symbolp (car table-name))
                (car table-name)
                (intern (car table-name))))
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
  (when (not (dao-keys class))
    (setf (slot-value class 'effective-keys)
          (list (find-primary-key-column class))))
  (build-dao-methods class))

(defclass direct-column-slot (standard-direct-slot-definition)
  ((col-type :initarg :col-type :reader column-type)
   (col-default :initarg :col-default :reader column-default)
   (col-identity :initarg :col-identity :reader column-identity)
   (col-unique :initarg :col-unique :reader column-unique)
   (col-collate :initarg :col-collate :reader column-collate)
   (col-primary-key :initarg :col-primary-key :reader column-primary-key)
   (col-interval :initarg :col-interval :reader column-interval)
   (col-check :initarg :col-check :reader column-check)
   (col-references :initarg :col-references :reader column-references)
   (col-export :initarg :col-export :initform nil :reader column-export)
   (col-import :initarg :col-import :initform nil :reader column-import)
   (ghost :initform nil :initarg :ghost :reader ghost)
   (sql-name :reader slot-sql-name))
  (:documentation "Type of slots that refer to database columns."))

(defmethod shared-initialize :after ((slot direct-column-slot) slot-names
                                     &key col-type col-default
                                       (col-name nil col-name-p) &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-value slot 'sql-name) (to-sql-name
                                     (if col-name-p
                                         col-name
                                         (slot-definition-name slot))
                                     s-sql:*escape-sql-names-p* t))
  ;; The default for nullable columns defaults to :null.
  (when (and (null col-default) (consp col-type) (eq (car col-type) 'or)
             (member 'db-null col-type) (= (length col-type) 3))
    (setf (slot-value slot 'col-default) :null)))

(defmethod direct-slot-definition-class ((class dao-class) &key column col-type
                                         &allow-other-keys)
  "Slots that have a :col-type option are column-slots."
  (if (or column col-type)
      (find-class 'direct-column-slot)
      (call-next-method)))

(defparameter *direct-column-slot* nil
  "This is used to communicate the fact that a slot is a column to
  effective-slot-definition-class.")

(defclass effective-column-slot (standard-effective-slot-definition)
  ((direct-slot :initform *direct-column-slot* :reader slot-column)))

(defmethod compute-effective-slot-definition ((class dao-class)
                                              name
                                              direct-slot-definitions)
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
  (:documentation "Test whether a row with the same primary key as the given
dao exists in the database. Will also return NIL when any of the key slots in
the object are unbound."))

(defgeneric insert-dao (dao)
  (:documentation "Insert the given dao into the database. Column slots of the
object which are unbound implies the database defaults. Hence, if these
columns has no defaults defined in the database, the the insertion of the dao
will be failed. (This feature only works on PostgreSQL 8.2 and up.)"))

(defgeneric update-dao (dao)
  (:documentation "Update the representation of the given dao in the database
to the values in the object. This is not defined for tables that do not have
any non-primary-key columns. Raises an error when no row matching the dao
exists."))

(defgeneric delete-dao (dao)
  (:documentation "Delete the given dao from the database."))

(defgeneric upsert-dao (dao)
  (:documentation "Like save-dao or save-dao/transaction but using a different
method that doesn't involve a database exception. This is safe to use both in
and outside a transaction, though it's advisable to always do it in a
transaction to prevent a race condition. The way it works is:

If the object contains unbound slots, we call insert-dao directly, thus the
behavior is like save-dao.

Otherwise we try to update a record with the same primary key. If the
PostgreSQL returns a non-zero number of rows updated it treated as the
record is already exists in the database, and we stop here.

If the PostgreSQL returns a zero number of rows updated, it treated as the
record does not exist and we call insert-dao.

The race condition might occur at step 3 if there's no transaction: if UPDATE
returns zero number of rows updated and another thread inserts the record at
that moment, the insertion implied by step 3 will fail.

Note, that triggers and rules may affect the number of inserted or updated
rows returned by PostgreSQL, so zero or non-zero number of affected rows may
not actually indicate the existence of record in the database.

This method returns two values: the DAO object and a boolean (T if the object
was inserted, NIL if it was updated).

IMPORTANT: This is not the same as insert on conflict (sometimes called an upsert)
in Postgresq. An upsert in Postgresql terms is an insert with a fallback of updating
the row if the insert key conflicts with an already existing row. An upsert-dao
in Postmodern terms is the reverse. First you try updating an existing object. If
there is no existing object to oupdate, then you insert a new object."))

(defgeneric get-dao (type &rest args)
  (:method ((class-name symbol) &rest args)
    (let ((class (find-class class-name)))
      (unless (class-finalized-p class)
        #+postmodern-thread-safe
        (unless (class-finalized-p class)
          (bordeaux-threads:with-lock-held (*class-finalize-lock*)
            (unless (class-finalized-p class)
              (finalize-inheritance class))))
        #-postmodern-thread-safe
        (finalize-inheritance class-name))
      (when (not (dao-keys class)) (error "Class ~a has no key slots."
                                          (class-name class)))
      (apply 'get-dao class-name args)))
  (:documentation "Select the DAO object from the row that has the given primary
key values, or NIL if no such row exists. Objects created by this function will have
initialize-instance called on them (after loading in the values from the
database) without any arguments ― even :default-initargs are skipped.
The same goes for select-dao and query-dao."))

(defgeneric make-dao (type &rest args &key &allow-other-keys)
  (:method ((class-name symbol) &rest args &key &allow-other-keys)
    (let ((class (find-class class-name)))
      (apply 'make-dao class args)))
  (:method ((class dao-class) &rest args &key &allow-other-keys)
    (unless (class-finalized-p class)
      #+postmodern-thread-safe
      (unless (class-finalized-p class)
        (bordeaux-threads:with-lock-held (*class-finalize-lock*)
          (unless (class-finalized-p class)
            (finalize-inheritance class))))
      #-postmodern-thread-safe
      (finalize-inheritance class))
    (let ((instance (apply #'make-instance class args)))
      (insert-dao instance)))
  (:documentation "Combines make-instance with insert-dao. Make the instance of
the given class and insert it into the database, returning the created instance."))

(defmacro define-dao-finalization (((dao-name class) &rest keyword-args) &body body)
  "Create an :around-method for make-dao. The body is executed in a lexical
environment where dao-name is bound to a freshly created and inserted DAO. The
representation of the DAO in the database is then updated to reflect changes
that body might have introduced. Useful for processing values of slots with the
type serial, which are unknown before insert-dao."
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
  creation. An example would be creating a dao object with unbounded slots.
Fetch-defaults could then be used to fetch the default values from the database
and bind the unbound slots which have default values. E.g.

  (let ((dao (make-instance 'test-data :a 23)))
      (pomo:fetch-defaults dao))

Returns dao if there were unbound slots with default values, nil otherwise."))

(defun %eval (code)
  (funcall (compile nil `(lambda () ,code))))

(defun set-to-class (class)
  "Take an imput that may be a symbol, actual class or dao object and returns the actual class."
  (cond ((symbolp class)
         (find-class class))
        ((closer-mop:classp class)
         class)
        ((typep (class-of class) 'dao-class)
         (class-of class))
        (t nil)))

(defun col-type-text-p (column-slot)
  "Returns t if a column-slot has text as a type. Could be text or (or text db-null)"
  (when (and column-slot (typep column-slot 'direct-column-slot))
    (let ((col-type (column-type column-slot)))
      (cond ((and (listp col-type) (member 'text col-type))
             t)
            ((eq col-type 'text)
             t)
            (t nil)))))

(defun find-col-type (class column-name)
  "Returns the col-type for a class and a column-name.
The column name must be a symbol"
  (setf class (set-to-class class))
  (when class
    (let ((column-slot (find-dao-column-slot class column-name)))
     (if column-slot
         (column-type column-slot)
         nil))))

;; Note: Export functions need only take a value parameter and return
;; a value parameter

(defun find-export-function (class column-name)
  "Returns the export function, if any, for a class and a column-name.
Column name must be a symbol"
  (setf class (set-to-class class))
  (when (stringp column-name) (setf column-name (from-sql-name column-name)))
  (when class
    (let ((column-slot (find-dao-column-slot class column-name)))
     (if column-slot
         (column-export column-slot)
         nil))))

(defun collect-export-functions (class)
  "Collects the export functions for a class (if any) and returns a list of lists of form
 (sql_name_of_field . export-function)"
  (setf class (set-to-class class))
  (loop for x in (dao-column-slots class)
        when (column-export x)
          collect
          (cons (slot-sql-name x) (fdefinition (column-export x)))))

;; Note: Import functions need to take a dao, a slot-name
;; and the imported value

(defun find-import-function (class column-name)
  "Returns the import function, if any, for a class and a column-name.
Column name must be a symbol"
  (setf class (set-to-class class))
  (when class
    (when (stringp column-name)
      (setf column-name (field-name-to-slot-name class column-name)))
    (when column-name
      (let ((col-slot (find-dao-column-slot class column-name)))
       (if col-slot
           (column-import col-slot)
           nil)))))

(defun collect-import-functions (class)
  "Collects the import functions for a class (if any) and returns a list of lists of form
 (sql_name_of_field . export-function)"
  (setf class (set-to-class class))
  (loop for x in (dao-column-slots class)
        when (column-import x)
          collect
          (cons (slot-sql-name x) (fdefinition (column-import x)))))

(defun find-dao-column-slot (class column-name)
  "Given a class and a symbol returns the dao-column-slot class for the column
named by that symbol (not the sql_column_name). Column name can be a symbol or
a string."
  (cond ((symbolp column-name)
         (find (string-downcase (symbol-name column-name))
               (dao-column-slots class)
               :key #'slot-definition-name-as-string
               :test 'equal))
        ((stringp column-name)
         (find (string-downcase column-name)
               (dao-column-slots class)
               :key #'slot-definition-name-as-string
               :test 'equal))
        (t nil)))

(defun slot-definition-name-as-string (slot)
  "Given a dao slot, this returns a downcased string of the name of a slot
without the package name for use in find functions."
  (string-downcase (symbol-name (closer-mop:slot-definition-name slot))))

(defun field-name-to-slot-name (class field-name)
  "Takes a Postgresql column name and tries to match it to a dao slot name symbol.
This is trying to deal with the hyphens and underscores problem where
Postgresql table field names cannot use hyphens but that is normal CL
practice. The slot name symbol will be the full package::slot-name. field-name
must be a string."
  (let ((slot (find field-name
                         (dao-column-slots class)
                         :key #'slot-definition-name-as-string
                         :test 'equal)))
    (if slot
        (closer-mop:slot-definition-name slot)
        (progn
          (setf field-name (cl-ppcre:regex-replace-all "_" field-name "-"))
          (setf slot (find field-name
                         (dao-column-slots class)
                         :key #'slot-definition-name-as-string
                         :test 'equal))
          (if slot
              (closer-mop:slot-definition-name slot)
              nil)))))

(defun build-dao-methods (class)
  "Synthesise a number of methods for a newly defined DAO class.
\(Done this way because some of them are not defined in every
situation, and each of them needs to close over some pre-computed
values. Notes for future maintenance: Fields are the slot names
in a dao class. Field-sql-name returns the col-name for the
postgresql table, which may or may not be the same as the slot
names in the class and also may have no relation to the initarg
or accessor or reader.)"
  (setf (slot-value class 'column-map)
        (mapcar (lambda (s) (cons (slot-sql-name s) (slot-definition-name s)))
                (dao-column-slots class)))

  (%eval
   `(let* ((fields (dao-column-fields ,class))
           (key-fields (dao-keys ,class))
           (ghost-slots (remove-if-not 'ghost (dao-column-slots ,class)))
           (ghost-fields (mapcar 'slot-definition-name ghost-slots))
           (value-fields (remove-if (lambda (x) (or (member x key-fields)
                                                    (member x ghost-fields)))
                                    fields))
           (table-name (dao-table-name ,class)))
      (labels ((field-sql-name (field)
                 (make-symbol (car (find field (slot-value ,class 'column-map)
                                         :key #'cdr :test #'eql))))
               (test-fields (fields)
                 `(:and ,@(loop :for field :in fields
                                :collect (list := (field-sql-name field) '$$))))
               (set-fields (fields)
                 (loop :for field :in fields
                       :append (list (field-sql-name field) '$$)))
               (slot-values (object &rest slots)
                 (loop :for slot :in (apply 'append slots)
                       :collect
                       (if (and (slot-boundp object slot)
                                (find-export-function object slot))
                           (funcall (find-export-function object slot)
                                     (slot-value object slot))
                           (slot-value object slot)))))
        ;; When there is no primary key, a lot of methods make no sense.
        (when key-fields
          (let ((tmpl (sql-template `(:select (:exists (:select t :from ,table-name
                                                        :where ,(test-fields
                                                                 key-fields)))))))
            (defmethod dao-exists-p ((object ,class))
              (and (every (lambda (s) (slot-boundp object s)) key-fields)
                   (query (apply tmpl (slot-values object key-fields)) :single))))

          ;; When all values are primary keys, updating makes no sense.
          (when value-fields
            (let ((update-tmpl (sql-template `(:update ,table-name
                                        :set ,@(set-fields value-fields)
                                        :where ,(test-fields key-fields)))))
              (defmethod update-dao ((object ,class))
                (when (zerop (execute (apply update-tmpl
                                             (slot-values object value-fields
                                                          key-fields))))
                  (error "Updated row does not exist."))
                object)
              ;; upsert in Postgresql terms is an insert with a fallback of updating
              ;; the row if the insert key conflicts with an already existing row
              ;; Historically an upsert-dao in Postmodern terms is the reverse
              ;; updating an existing object and inserting a new object if there
              ;; is no existing object to update.
              (defmethod upsert-dao ((object ,class))
                (handler-case
                    (if (zerop (execute (apply update-tmpl
                                               (slot-values object value-fields
                                                            key-fields))))
                        (values (insert-dao object) t)
                        (values object nil))
                  (unbound-slot ()
                    (values (insert-dao object) t))))))

          (let ((del-tmpl (sql-template `(:delete-from ,table-name
                                      :where ,(test-fields key-fields)))))
            (defmethod delete-dao ((object ,class))
              (execute (apply del-tmpl (slot-values object key-fields)))))

          (let ((get-tmpl (sql-template `(:select * :from ,table-name
                                      :where ,(test-fields key-fields)))))
            (defmethod get-dao ((type (eql (class-name ,class))) &rest keys)
              (car (exec-query *database* (apply get-tmpl keys)
                               (dao-row-reader ,class))))))

        (defmethod insert-dao ((object ,class))
          (let (bound unbound)
            (loop :for field :in fields
                  :do
                     (if (slot-boundp object field)
                          (push field bound)
                          (push field unbound)))
            (let* ((fields (remove-if (lambda (x) (member x ghost-fields))
                                      bound))
                   (query (sql-compile
                           `(:insert-into ,table-name
                             :set ,@(loop for field in fields
                                          collect (field-sql-name field)
                                          collect
                                          (if (and (slot-boundp object field)
                                                   (find-export-function object field))
                                              (funcall (find-export-function object field)
                                                       (slot-value object field))
                                              (slot-value object field)))
                             ,@(when unbound (cons :returning
                                                   (mapcar #'field-sql-name
                                                           unbound))))))
                   (returned
                     (query query :row)))
              (when unbound
                (loop :for value :in returned
                      :for field :in unbound
                      :do (setf (slot-value object field) value)))))
          object)

        (let* ((defaulted-slots (remove-if-not
                                 (lambda (x)
                                   (slot-boundp x 'col-default))
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
                    (loop :for value :in (query
                                          (sql-compile (cons :select defaults))
                                          :list)
                          :for slot-name :in names
                          :do (setf (slot-value object slot-name) value))))
                object)
              (defmethod fetch-defaults ((object ,class))
                nil)))

        (defmethod shared-initialize :after ((object ,class) slot-names
                                             &key (fetch-defaults nil)
                                             &allow-other-keys)
          (declare (ignore slot-names))
          (when fetch-defaults
            (fetch-defaults object)))))))

(defmacro with-column-writers ((&rest defs) &body body)
  "Provides control over the way get-dao, select-dao, and query-dao read values
from the database. This is not commonly needed, but can be used to reduce the
amount of queries a system makes. writers should be a list of alternating
column names (strings or symbols) and writers, where writers are either
symbols referring to a slot in the objects, or functions taking two arguments ―
an instance and a value ― which can be used to somehow store the value in the
new instance. When any DAO-fetching function is called in the body, and
columns matching the given names are encountered in the result, the writers
are used instead of the default behaviour (try and store the value in the slot
that matches the column name).

An example of using this is to add some non-column slots to a DAO class, and
use query-dao within a with-column-writers form to pull in extra information
about the objects, and immediately store it in the new instances."
  `(let ((*custom-column-writers*
           (append (list ,@(loop :for (field writer) :on defs :by #'cddr
                                 :collect `(cons (to-sql-name ,field nil) ,writer)))
                   *custom-column-writers*)))
     ,@body))

(defun dao-from-fields (class column-map query-fields
                        result-next-field-generator-fn)
  (let ((instance (allocate-instance class)))
    (loop :for field :across query-fields
          :for writer := (cdr (assoc (field-name field)
                                     column-map
                                     :test #'string=))
          :do
             (etypecase writer
               (null (if *ignore-unknown-columns*
                         (funcall result-next-field-generator-fn field)
                         (error "No slot named ~a in class ~a. DAO out of sync with table, or incorrect query used."
                                (field-name field) (class-name class))))
               (symbol (setf (slot-value instance writer)
                             (funcall result-next-field-generator-fn field)))
               (function (let ((import-function-symbol
                                 (find-import-function instance (field-name field))))
                           (cond ((and import-function-symbol
                                    (eq writer
                                        (fdefinition import-function-symbol)))
                               (setf (slot-value instance (field-name-to-slot-name
                                                           class (field-name field)))
                                     (funcall import-function-symbol
                                              (funcall result-next-field-generator-fn field))))
                              ((and import-function-symbol
                                    (not (functionp import-function-symbol)))
                               (setf (slot-value instance (field-name-to-slot-name
                                                           class (field-name field)))
                                     (funcall (fdefinition import-function-symbol)
                                              (funcall result-next-field-generator-fn field))))
                              ((and import-function-symbol
                                    (functionp import-function-symbol))
                               (setf (slot-value instance (field-name-to-slot-name
                                                           class (field-name field)))
                                     (funcall import-function-symbol
                                              (funcall result-next-field-generator-fn field))))
                              (t
                               (funcall writer instance
                                        (funcall result-next-field-generator-fn field))))))))
    (initialize-instance instance)
    instance))

(defun dao-row-reader (class)
  "Defines a row-reader for objects of a given class. Note that query fields are the
Postgresql column names, not the dao slot names."
  (row-reader (query-fields)
    (let ((column-map (append *custom-column-writers*
                              (collect-import-functions class)
                              (dao-column-map class))))
      (loop :while (next-row)
            :collect (dao-from-fields class column-map query-fields #'next-field)))))


(defun save-dao (dao)
  "Tries to insert the given dao using insert-dao. If the dao has unbound slots,
those slots will be updated and bound by default data triggered by the
database. If this raises a unique key violation error, it tries to update it by
using update-dao instead. In this case, if the dao has unbound slots, updating
will fail with an unbound slots error.

Be aware that there is a possible race condition here ― if some other process
deletes the row at just the right moment, the update fails as well. Returns a
boolean telling you whether a new row was inserted.

This function is unsafe to use inside of a transaction ― when a row with the
given keys already exists, the transaction will be aborted. Use
save-dao/transaction instead in such a situation.

See also: upsert-dao."
  (handler-case
      (progn (insert-dao dao) t)
    (cl-postgres-error:unique-violation ()
      (update-dao dao)
      nil)
    (cl-postgres-error:columns-error ()
      (update-dao dao)
      nil)))

(defun save-dao/transaction (dao)
  "The transaction safe version of save-dao. Tries to insert the given dao using
insert-dao. If this raises a unique key violation error, it tries to update it
by using update-dao instead. If the dao has unbound slots, updating will fail
with an unbound slots error. If the dao has unbound slots, those slots will be
updated and bound by default data triggered by the database.

Acts exactly like save-dao, except that it protects its attempt to insert the
object with a rollback point, so that a failure will not abort the transaction.

See also: upsert-dao."
  (handler-case
      (with-savepoint save-dao/transaction (insert-dao dao) t)
    (cl-postgres-error:unique-violation ()
      (update-dao dao)
      nil)
    (cl-postgres-error:columns-error ()
      (update-dao dao)
      nil)))

(defun query-dao% (type query row-reader &rest args)
  (let ((class (find-class type)))
    (unless (class-finalized-p class)
      #+postmodern-thread-safe
      (unless (class-finalized-p class)
        (bordeaux-threads:with-lock-held (*class-finalize-lock*)
          (unless (class-finalized-p class)
            (finalize-inheritance class))))
      #-postmodern-thread-safe
      (finalize-inheritance class))
    (if args
        (progn
          (prepare-query *database* "" query)
          (exec-prepared *database* "" args row-reader))
        (exec-query *database* query row-reader))))

(defmacro query-dao (type query &rest args)
  "Execute the given query (which can be either a string or an S-SQL expression)
and return the result as DAOs of the given type. If the query contains
placeholders ($1, $2, etc) their values can be given as extra arguments. The
names of the fields returned by the query must either match slots in the DAO
class, or be bound through with-column-writers."
  `(query-dao% ,type ,(real-query query) (dao-row-reader (find-class ,type)) ,@args))

(defmacro dao-row-reader-with-body ((type type-var) &body body)
  (let ((fields (gensym))
        (column-map (gensym)))
    `(row-reader (,fields)
       (let ((,column-map (append *custom-column-writers*
                                  (dao-column-map (find-class ,type)))))
         (loop :while (next-row)
               :do (let ((,type-var
                           (dao-from-fields (find-class ,type)
                                            ,column-map
                                            ,fields
                                            #'next-field)))
                     ,@body))))))

(defmacro do-query-dao (((type type-var) query) &body body)
  "Like query-dao, but iterates over the results rather than returning them.
For each matching DAO, body is evaluated with type-var bound to the instance.

Example:

    (do-query-dao (('user user)
        (:order-by
           (:select '* :from 'user :where (:> 'score 10000))
         'name))
  (pushnew user high-scorers))"

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
  "Select DAO objects for the rows in the associated table for which the given
test (either an S-SQL expression or a string) holds. When sorting arguments
are given, which can also be S-SQL forms or strings, these are used to sort
the result.

(Note that, if you want to sort, you have to pass the test argument.)

    (select-dao 'user (:> 'score 10000) 'name)"

  `(query-dao% ,type (sql ,(generate-dao-query type test ordering))
               (dao-row-reader (find-class ,type))))

(defmacro do-select-dao (((type type-var) &optional (test t)
                          &rest ordering)
                         &body body)
  "Like select-dao, but iterates over the results rather than returning them.
For each matching DAO, body is evaluated with type-var bound to the DAO
instance.

Example:

(do-select-dao (('user user) (:> 'score 10000) 'name)
  (pushnew user high-scorers))"
  `(query-dao% ,type (sql ,(generate-dao-query type test ordering))
               (dao-row-reader-with-body (,type ,type-var)
                 ,@body)))

(defun list-to-column (col-type)
  "If a col-type is a list, alist or plist, will set the Postgresql column to text."
  (if (member col-type '(list alist plist))
      'text
    col-type))

(defun dao-table-definition (table)
  "Given a DAO class, or the name of one, this will produce an SQL query
string with a definition of the table. This is just the bare simple definition,
so if you need any extra indices or or constraints, you'll have to write your
own queries to add them, in which case look to s-sql's create-table function."
  (unless (typep table 'dao-class)
    (setf table (find-class table)))
  (unless (class-finalized-p table)
    #+postmodern-thread-safe
    (unless (class-finalized-p table)
      (bordeaux-threads:with-lock-held (*class-finalize-lock*)
        (unless (class-finalized-p table)
          (finalize-inheritance table))))
    #-postmodern-thread-safe
    (finalize-inheritance table))
  (sql-compile
   `(:create-table ,(dao-table-name table)
                   ,(loop :for slot :in (dao-column-slots table)
                          :unless (ghost slot)
                            :collect `(,(slot-definition-name slot)
                                       :type ,(list-to-column (column-type slot))
                                       ,@(cond ((slot-boundp slot 'col-identity)
                                                `(:primary-key "generated always as identity"))
                                               ((slot-boundp slot 'col-primary-key)
                                                `(:primary-key t)))
                                       ,@(when (slot-boundp slot 'col-unique)
                                           `(:unique t))
                                       ,@(when (slot-boundp slot 'col-collate)
                                           `(:collate ,(column-collate slot)))
                                       ,@(when (slot-boundp slot 'col-interval)
                                           `(:interval ,(column-interval slot)))
                                       ,@(when (slot-boundp slot 'col-check)
                                           `(:check ,(column-check slot)))
                                       ,@(when (slot-boundp slot 'col-references)
                                           `(:references ,(column-references slot)))
                                       ,@(when (slot-boundp slot 'col-default)
                                           `(:default ,(column-default slot)))))
                   ,@(when (and (dao-keys table) (not (find-primary-key-column table)))
                       `((:primary-key ,@(dao-keys table)))))))
