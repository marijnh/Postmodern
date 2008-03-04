(in-package :postmodern)

(defclass dao-class (standard-class)
  ((keys :initarg :keys :initform nil :reader dao-keys)
   (table-name :reader table-name)
   (row-reader :reader dao-row-reader))
  (:documentation "Metaclass for database-access-object classes."))

(defmethod validate-superclass ((class dao-class) (super-class standard-class))
  t)

(defun dao-slots (class)
  "Enumerate the slots in a class that refer to table rows."
  (remove-if-not (lambda (x) (typep x 'dao-slot)) (class-direct-slots class)))
(defun dao-fields (class)
  (mapcar 'slot-definition-name (dao-slots class)))

(defmethod shared-initialize :after ((class dao-class) slot-names &key table-name &allow-other-keys)
  (declare (ignore slot-names))
  (unless (every (lambda (x) (member x (dao-fields class))) (dao-keys class))
    (error "Class ~A has a key that is not also a slot." (class-name class)))
  ;; Default the table name to the class name if it is not given,
  ;; remove it from the list form if it was given.
  (setf (slot-value class 'table-name)
        (cond ((not table-name) (class-name class))
              ((symbolp (car table-name)) (car table-name))
              (t (intern (car table-name)))))
  (build-row-reader class)
  (build-dao-methods class))


(defclass dao-slot (standard-direct-slot-definition)
  ((col-type :initarg :col-type :reader slot-col-type)
   (col-default :initarg :col-default :reader slot-col-default)
   (sql-name :reader slot-sql-name))
  (:documentation "Type of slots that refer to database columns."))

(defmethod shared-initialize :after ((slot dao-slot) slot-names &key row-type row-default &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-value slot 'sql-name) (to-sql-name (slot-definition-name slot)))
  (when (and (null row-default) (consp row-type) (eq (car row-type) 'or)
             (member 'db-null row-type) (= (length row-type) 3))
    (setf (slot-value slot 'row-default) :null)))

(defmethod direct-slot-definition-class ((class dao-class) &key col-type &allow-other-keys)
  "Slots that have a :col-type option are dao-slots."
  (if col-type (find-class 'dao-slot) (call-next-method)))

(defun build-row-reader (class)
  "Initialize the row-reader for this table to a reader that collects
instances of the associated class and initializes their slots to the
values from the query."
  (let* ((fields (dao-slots class))
         (n-dao-fields (length fields)))
    (flet ((relevant-field (probable-field name)
                             (or (and (string= (slot-sql-name probable-field) name) probable-field)
                                 (find-if (lambda (field) (string= (slot-sql-name field) name)) fields)
                                 (error "Field ~A does not exist in table class ~A." name (class-name class)))))
      (setf (slot-value class 'row-reader)
            (row-reader (query-fields)
              (assert (= (length query-fields) n-dao-fields))
              (loop :while (next-row)
                    :collect
                    (let ((instance (allocate-instance class)))
                      (loop :for query-field :across query-fields
                            :for dao-field :in fields
                            :do (setf (slot-value instance (slot-definition-name
                                                            (relevant-field dao-field (field-name query-field))))
                                      (next-field query-field)))
                      instance)))))))

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
(defgeneric get-dao (type &rest args)
  (:documentation "Get the object corresponding to the given primary
  key, or return nil if it does not exist."))

(defgeneric fetch-defaults (object)
  (:documentation "Used to fetch the default values of an object on
  creation."))

(defclass target-class () ()
  (:documentation "A dummy class that is used to specialise DAO
  methods on -- see build-dao-methods."))

(defun build-dao-methods (class)
  "Synthesise a number of methods for a newly defined DAO class.
\(Done this way because some of them are not defined in every
situation, and each of them needs to close over some pre-computed
values.)"
  (let* ((fields (dao-fields class))
         (key-fields (dao-keys class))
         (value-fields (remove-if (lambda (x) (member x key-fields)) fields))
         (table-name (table-name class)))
    ;; This is a hack -- the MOP does not define a practical way to
    ;; dynamically add methods to a generic, but the specialised-on
    ;; class is determined when the defmethod is evaluated, so setting
    ;; target-class to our class will cause the methods to be
    ;; specialised on the correct class.
    (setf (find-class 'target-class) class)
    (flet ((test-fields (fields)
             `(:and ,@(loop :for field :in fields :collect (list := field '$$))))
           (set-fields (fields)
             (loop :for field :in fields :append (list field '$$)))
           (slot-values (object &rest slots)
             (loop :for slot :in (apply 'append slots) :collect (slot-value object slot))))

      ;; When there is no primary key, a lot of methods make no sense.
      (when key-fields
        (let ((tmpl (sql-template `(:select (:exists (:select t :from ,table-name
                                                      :where ,(test-fields key-fields)))))))
          (defmethod dao-exists-p ((object target-class))
            (and (every (lambda (s) (slot-boundp object s)) key-fields)
                 (query (apply tmpl (slot-values object key-fields)) :single))))
  
        ;; When all values are primary keys, updating makes no sense.
        (when value-fields
          (let ((tmpl (sql-template `(:update ,table-name :set ,@(set-fields value-fields)
                                      :where ,(test-fields key-fields)))))
            (defmethod update-dao ((object target-class))
              (execute (apply tmpl (slot-values object value-fields key-fields))))))
  
        (let ((tmpl (sql-template `(:delete-from ,table-name :where ,(test-fields key-fields)))))
          (defmethod delete-dao ((object target-class))
            (execute (apply tmpl (slot-values object key-fields)))))
  
        (let ((tmpl (sql-template `(:select * :from ,table-name :where ,(test-fields key-fields)))))
          (defmethod get-dao ((type (eql (class-name class))) &rest keys)
            (car (exec-query *database* (apply tmpl keys) (dao-row-reader class))))))

      (defmethod insert-dao ((object target-class))
        (let (bound unbound)
          (loop :for field :in fields
                :do (if (slot-boundp object field)
                        (push field bound)
                        (push field unbound)))
          (let* ((values (mapcan (lambda (x) (list x (slot-value object x))) bound))
                 (returned (query (sql-compile `(:insert-into ,table-name
                                                 :set ,@values
                                                 ,@(when unbound (cons :returning unbound))))
                                  :row)))
            (when unbound
              (loop :for value :in returned
                    :for field :in unbound
                    :do (setf (slot-value object field) value))))))

      (let* ((defaulted-slots (remove-if-not (lambda (x) (slot-boundp x 'col-default)) (dao-slots class)))
             (defaulted-names (mapcar 'slot-definition-name defaulted-slots))
             (default-values (mapcar 'slot-col-default defaulted-slots)))
        (if defaulted-slots
            (defmethod fetch-defaults ((object target-class))
              (let (names defaults)
                ;; Gather unbound slots and their default expressions.
                (loop :for slot-name :in defaulted-names
                      :for default :in default-values
                      :do (unless (slot-boundp object slot-name)
                            (push slot-name names)
                            (push default defaults)))
                ;; If there are any unbound, defaulted slots, fetch their content.
                (when names
                  (loop :for value :in (query (sql-compile (cons :select defaults)))
                        :for slot-name :in names
                        :do (setf (slot-value object slot-name) value)))))
            (defmethod fetch-defaults ((object target-class))
              (declare (ignore object)))))

      (defmethod shared-initialize :after ((object target-class) slot-names &key (fetch-defaults nil) &allow-other-keys)
        (declare (ignore slot-names))
        (when fetch-defaults
          (fetch-defaults object))))))

(defun query-dao% (type query)
  (exec-query *database* query (dao-row-reader (find-class type))))

(defmacro query-dao (type query)
  "Execute a query and return the result as daos of the given type.
The fields returned by the query must match the slots of the dao, both
by type and by name."
  `(query-dao% ,type ,(real-query query)))

(defmacro select-dao (type &optional (test t) &rest ordering)
  "Select daos for the rows in its table for which the given test
holds, order them by the given criteria."
  (flet ((check-string (x)
           (if (stringp x) `(:raw ,x) x)))
    (let* ((type-name (gensym))
           (query `(:select '* :from (table-name (find-class ,type-name))
                    :where ,(check-string test))))
      (when ordering
        (setf query `(:order-by ,query ,@(mapcar #'check-string ordering))))
      `(let ((,type-name ,type))
         (query-dao% ,type-name (sql ,query))))))

(defun dao-table-definition (table)
  "Generate the appropriate CREATE TABLE query for this class."
  (unless (typep table 'dao-class)
    (setf table (find-class table)))
  (sql-compile
   `(:create-table ,(table-name table)
                   ,(loop :for slot :in (dao-slots table)
                       :if (typep slot 'dao-slot)
                       :collect `(,(slot-definition-name slot) :type ,(slot-col-type slot)
                                   ,@(when (slot-boundp slot 'col-default) `(:default ,(slot-col-default slot)))))
                   ,@(when (dao-keys table)
                       `((:primary-key ,@(dao-keys table)))))))

;;; Copyright (c) Marijn Haverbeke
;;;
;;; This software is provided 'as-is', without any express or implied
;;; warranty. In no event will the authors be held liable for any
;;; damages arising from the use of this software.
;;;
;;; Permission is granted to anyone to use this software for any
;;; purpose, including commercial applications, and to alter it and
;;; redistribute it freely, subject to the following restrictions:
;;;
;;; 1. The origin of this software must not be misrepresented; you must
;;;    not claim that you wrote the original software. If you use this
;;;    software in a product, an acknowledgment in the product
;;;    documentation would be appreciated but is not required.
;;;
;;; 2. Altered source versions must be plainly marked as such, and must
;;;    not be misrepresented as being the original software.
;;;
;;; 3. This notice may not be removed or altered from any source
;;;    distribution.
