;; TODO regular slots, think about inheritance

(in-package :postmodern)

(defclass dao-class (standard-class)
  ((keys :initarg :keys :initform (error "No primary key defined for DAO class.")
         :reader dao-keys)
   (table-name :initarg :table-name :reader table-name)
   (row-reader :reader dao-row-reader)))

(defmethod validate-superclass ((class dao-class) (super-class standard-class))
  t)

(defun dao-slots (class)
  (remove-if-not (lambda (x) (typep x 'dao-slot)) (class-direct-slots class)))
(defun dao-fields (class)
  (mapcar 'slot-definition-name (dao-slots class)))
(defun dao-table (object)
  (table-name (class-of object)))

(defmethod shared-initialize :after ((class dao-class) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (unless (every (lambda (x) (member x (dao-fields class))) (dao-keys class))
    (error "Class ~A has a key that is not also a slot." (class-name class)))
  (setf (slot-value class 'table-name)  
        (if (slot-boundp class 'table-name)
            (let ((name (car (table-name class))))
              (if (symbolp name) name (intern name)))
            (class-name class)))
  (build-row-reader class)
  (build-dao-methods class))


(defclass dao-slot (standard-direct-slot-definition)
  ((default :initarg :default :accessor dao-slot-default)
   (row-type :initarg :row-type :accessor slot-row-type)
   (sql-name :accessor slot-sql-name)))

(defmethod initialize-instance :after ((slot dao-slot) &key &allow-other-keys)
  (setf (slot-sql-name slot) (to-sql-name (slot-definition-name slot))))

(defmethod direct-slot-definition-class ((class dao-class) &key row-type &allow-other-keys)
  (if row-type 'dao-slot (call-next-method)))

(defun build-row-reader (class)
  "Initialize the row-reader for this table to a reader that collects
instances of the associated class and initializes their slots to the
values from the query."
  (let* ((fields (dao-slots class))
         (n-fields (length fields)))
    (flet ((relevant-field (probable-field name)
                             (or (and (string= (slot-sql-name probable-field) name) probable-field)
                                 (find-if (lambda (field) (string= (slot-sql-name field) name)) fields)
                                 (error "Field ~A does not exist in table class ~A." name (class-name class)))))
      (setf (slot-value class 'row-reader)
            (row-reader (query-fields)
              (assert (= (length query-fields) n-fields))
              (loop :while (next-row)
                    :collect
                    (let ((instance (allocate-instance class)))
                      (loop :for query-field :across query-fields
                            :for dao-field :in fields
                            :do (setf (slot-value instance (slot-definition-name
                                                            (relevant-field dao-field (field-name query-field))))
                                      (next-field query-field)))
                      instance)))))))

(defgeneric dao-exists-p (object))
(defgeneric update-dao (object))
(defgeneric insert-dao (object))
(defgeneric delete-dao (object))
(defgeneric fetch-defaults (object))
(defgeneric get-dao (type &rest keys))

(defmacro deftable (name templates fields &rest options)
  "Store a table structure under the given name, in the given
templates, or in the nil template when no template names are given.
Fields are defclass-like slot declarations which must at least have a
:type declared. Supported options are: everything that defclass
supports, :auto-id to give the table an id column and a sequence for
creating its ids, :class-name for the name of a dao for this table, if
any, and :indices for the indices that should be created for this
table. Indices can be single fields or lists of fields. The first
index is used as primary key (:auto-id adds an index on the id)."
  (let* ((class (second (assoc :class-name options)))
         (auto-id (second (assoc :auto-id options)))
         (raw-indices (mapcar (lambda (i) (if (consp i) i (list i))) (cdr (assoc :indices options))))
         (indices (mapcar (lambda (i) (if (eq (car i) :unique) (cdr i) i)) raw-indices))
         (index-unique (mapcar (lambda (i) (eq (car i) :unique)) raw-indices))
         (auto-id-sequence-name (intern (format nil "~A-ID-SEQ" name) :keyword)))
    (labels ((filter-options (options)
               (remove-if (lambda (option)
                            (member (car option) '(:class-name :auto-id :indices)))
                          options))
             (primary-key-test (object-binding)
               `(:and ,@(mapcar (lambda (field) `(:= ',field (slot-value ,object-binding ',field)))
                                (car indices))))
             (set-fields (object-binding fields)
               (loop :for field :in fields
                     :append `(',field (slot-value ,object-binding ',field)))))
      (when (eq auto-id t)
        (setf auto-id 'id))
      (when auto-id
        (push `(,auto-id :type integer :initarg ,(intern (symbol-name auto-id) :keyword) :accessor get-id)
              fields)
        (push (list auto-id) indices)
        (push t index-unique))
      (unless indices
        (error "No primary index defined for table ~A." name))
      (setf (car index-unique) t)
      `(progn
        ,(when class
           `(defclass ,class () ,fields ,@(filter-options options)))
        (let* ((table (make-instance 'table :name ',name :class-name ',class
                                     :fields (mapcar 'extract-field ',fields)
                                     :id-sequence ,(when auto-id auto-id-sequence-name)
                                     :indices (list ,@(mapcar (lambda (i u) `(make-instance 'index :fields ',i :unique-p ,u))
                                                              indices index-unique))))
               (row-reader ,(if class `(initialize-row-reader table))))
          (declare (ignorable row-reader))
          ,@(mapcar (lambda (sch) `(setf (table ',name ',sch) table)) (or templates (list nil)))
          ,@(when class 
              `(,@(when auto-id
                    `((defmethod initialize-instance :after ((dao ,class) &key defer-id &allow-other-keys)
                        (when (and (not (slot-boundp dao ',auto-id)) (not defer-id))
                          (setf (slot-value dao ',auto-id) (sequence-next ',auto-id-sequence-name))))))
                (defmethod dao-exists-p ((dao ,class))
                  (and ,(if auto-id `(slot-boundp dao ',auto-id) t)
                       (query (:select (:exists (:select 1 :from ',name :where ,(primary-key-test 'dao))))
                              :single)))
                (defmethod insert-dao ((dao ,class))
                  ,@(when auto-id
                      `((unless (slot-boundp dao ',auto-id)
                          (setf (slot-value dao ',auto-id) (sequence-next ',auto-id-sequence-name)))))
                  (execute (:insert-into ',name :set ,@(set-fields 'dao (mapcar 'car fields)))))
                (defmethod update-dao ((dao ,class))
                  (execute (:update ',name
                                    :set ,@(set-fields 'dao (remove-if (lambda (f) (member f (car indices)))
                                                                       (mapcar 'car fields)))
                                    :where ,(primary-key-test 'dao))))
                (defmethod delete-dao ((dao ,class))
                  (execute (:delete-from ',name :where ,(primary-key-test 'dao))))
                (defmethod query-dao% ((type (eql ',class)) query)
                  (exec-query *database* query row-reader))
                (defmethod dao-table ((type (eql ',class)))
                  ',name)
                (defmethod get-dao ((type (eql ',class)) &rest args)
                  (car (exec-query *database*
                                   (sql (:select '* :from ',name :where
                                                 (:and ,@(mapcar (lambda (key) `(:= ',key (pop args)))
                                                                 (car indices)))))
                                   row-reader)))))
          (values))))))

(defun build-dao-methods (class)
  (let* ((key-fields (dao-keys class))
         (value-fields (remove-if (lambda (x) (member x key-fields)) (dao-fields class)))
         (table-name (table-name class)))
    ;; Cheat
    (setf (find-class 'target-class) class)
    (flet ((test-fields (fields)
             `(:and ,@(loop :for field :in fields :collect (list := field '$$))))
           (set-fields (fields)
             (loop :for field :in fields :append (list field '$$)))
           (slot-values (object &rest slots)
             (loop :for slot :in (apply 'append slots) :collect (slot-value object slot))))

      (let ((tmpl (sql-template `(:select (:exists (:select t :from ,table-name
                                                    :where ,(test-fields key-fields)))))))
        (defmethod dao-exists-p ((object target-class))
          (query (apply tmpl (slot-values object key-fields)) :single)))
      (let ((tmpl (sql-template `(:update ,table-name :set ,@(set-fields value-fields)
                                  :where ,(test-fields key-fields)))))
        (defmethod update-dao ((object target-class))
          (execute (apply tmpl (slot-values object value-fields key-fields)))))

      (defmethod insert-dao ((object target-class))
        (execute (sql-compile
                  `(:insert-into ,table-name :set
                     ,@(loop :for field :in (dao-fields class) :if (slot-boundp object field)
                             :append (list field (slot-value object field)))))))

      (let ((tmpl (sql-template `(:delete-from ,table-name :where ,(test-fields key-fields)))))
        (defmethod delete-dao ((object target-class))
          (execute (apply tmpl (slot-values object key-fields)))))

      (let ((defaulted (remove-if-not (lambda (x) (slot-boundp x 'default)) (dao-slots class))))
        (if defaulted
            (let ((query (sql-compile `(:select ,@(mapcar 'dao-slot-default defaulted)))))
              (defmethod fetch-defaults ((object target-class))
                (loop :for value :in (query query :row)
                      :for slot :in defaulted
                      :do (setf (slot-value object (slot-definition-name slot)) value))))
            (defmethod fetch-defaults ((object target-class))
              (declare (ignore object)))))

      (let ((tmpl (sql-template `(:select * :from ,table-name :where ,(test-fields key-fields)))))
        (defmethod get-dao ((type (eql (class-name class))) &rest keys)
          (car (exec-query *database* (apply tmpl keys) (dao-row-reader class)))))

      (defmethod initialize-instance :after ((object target-class) &key defer-defaults &allow-other-keys)
        (unless defer-defaults
          (fetch-defaults object))))))

(defun query-dao% (type query)
  (exec-query *database* query (dao-row-reader (find-class type))))

(defmacro query-dao (type query)
  "Execute a query and return the result as daos of the given type.
The fields returned by the query must match the slots of the dao, both
in type and in order."
  `(query-dao% ,type ,(real-query query)))

(defmacro select-dao (type &optional (test t) &rest ordering)
  "Select daos for the rows in its table for which the given test
holds."
  (let* ((type-name (gensym))
         (query `(:select '* :from (dao-table ,type-name)
                  :where ,(if (stringp test) `(:raw ,test) test))))
    (when ordering
      (setf query `(:order-by ,query ,@ordering)))
    `(let ((,type-name ,type))
      (query-dao% ,type-name (sql (:select '* :from (dao-table ,type-name)
                                            :where ,(if (stringp test) `(:raw ,test) test)))))))

(defun save-dao (dao)
  "Save a dao: update it when it already exists, insert it otherwise."
  (if (dao-exists-p dao)
      (update-dao dao)
      (insert-dao dao)))

(defun dao-create-table (table)
  "Create a defined table and its indices in the database."
  (setf table (find-class table))
  (execute
   (sql-compile
    `(:create-table ,(table-name table)
                    ,(loop :for slot :in (dao-slots table)
                           :if (typep slot 'dao-slot)
                           :collect `(,(slot-definition-name slot) :type ,(slot-row-type slot)
                                       ,@(when (slot-boundp slot 'default) `(:default ,(dao-slot-default slot)))))
                    (:primary-key ,@(dao-keys table))))))

(defun dao-drop-table (table)
  "Drop a defined table."
  (execute (:drop-table (table-name (find-class table)))))

(defun dao-reset-table (table)
  "Remove and re-create a defined table."
  (dao-drop-table table)
  (dao-create-table table))

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
