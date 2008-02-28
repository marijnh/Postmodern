(in-package :postmodern)

(defclass table-field ()
  ((name :initarg :name :accessor table-field-name)
   (sql-name :initarg :sql-name :accessor table-field-sql-name)
   (type :initarg :type :accessor table-field-type))
  (:documentation "Representation of a table field."))

(defclass table ()
  ((name :initarg :name :accessor table-name)
   (class-name :initarg :class-name :accessor table-class-name)
   (fields :initarg :fields :accessor table-fields)
   (indices :initarg :indices :accessor table-indices)
   (id-sequence :initarg :id-sequence :accessor table-id-sequence))
  (:documentation "Representation of a table."))

(defclass index ()
  ((fields :initarg :fields :reader index-fields)
   (unique-p :initarg :unique-p :reader index-unique-p)))

(defparameter *templates* (make-hash-table)
  "Location for storing database templates.")

(defun template (name)
  (gethash name *templates*))
(defun (setf template) (value name)
  (setf (gethash name *templates*) value))
(defmacro dotemplate ((table-name template) &body body)
  (let ((ignored (gensym)))
    `(maphash (lambda (,table-name ,ignored)
                (declare (ignore ,ignored))
                ,@body)
      (or (template ,template)
       (error "Template ~A does not exists." ,template)))))

(defun table (name &optional template)
  "Get the named table inside the named template."
  (let ((template-table (template template)))
    (if template-table
        (gethash name template-table)
        nil)))
(defun (setf table) (value name &optional template)
  (let ((template-table (template template)))
    (unless template-table
        (setf template-table (make-hash-table)
              (template template) template-table))
    (setf (gethash name template-table) value)))

(defun extract-field (slot)
  "Transform a list of defclass-style slot declarations to a list of
table-field objects."
  (make-instance 'table-field :name (car slot) :sql-name (to-sql-name (car slot))
                 :type (or (getf (cdr slot) :type)
                           (error "No type specified for slot ~A." (car slot)))))

(defun initialize-row-reader (table)
  "Initialize the row-reader for this table to a reader that collects
instances of the associated class and initializes their slots to the
values from the query."
  (let ((table-fields (table-fields table))
        (n-fields (length (table-fields table)))
        (class-name (table-class-name table)))
    (row-reader (query-fields)
      (assert (= (length query-fields) n-fields))
      (loop :while (next-row)
            :collect
            (let ((instance (allocate-instance (find-class class-name))))
              (flet ((relevant-field (probable-field name)
                       (or (and (string= (table-field-sql-name probable-field) name)
                                probable-field)
                           (find-if (lambda (field) (string= (table-field-sql-name field) name))
                                    table-fields)
                           (error "Field ~A does not exist in defined table ~A." name (table-name table)))))
                (loop :for query-field :across query-fields
                      :for table-field :in table-fields
                      :do (setf (slot-value instance (table-field-name (relevant-field table-field 
                                                                                       (field-name query-field))))
                                (next-field query-field)))
                instance))))))

(defgeneric dao-exists-p (dao)
  (:documentation "Return a boolean indicating whether the given dao
exists in the database."))
(defgeneric insert-dao (dao)
  (:documentation "Insert the given dao into the database."))
(defgeneric update-dao (dao)
  (:documentation "Update the dao's representation in the database
with the values in the given object."))
(defgeneric delete-dao (dao)
  (:documentation "Delete the given dao from the database."))
(defgeneric query-dao% (type query)
  (:documentation "Execute the given query, and convert the result
into daos of the given type."))
(defgeneric dao-table (type)
  (:documentation "Get the name of the table associated with the given
dao type."))
(defgeneric get-dao (type &rest args)
  (:documentation "Get the dao corresponding to the given primary key,
or return nil if it does not exist."))
(defgeneric get-id (dao)
  (:documentation "Get the id for a dao that refers to a table that
has :auto-ids."))

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
                  ,(when (remove-if (lambda (f) (member f (car indices)))
                                                                       (mapcar 'car fields))
                    `(execute (:update ',name
                                      :set ,@(set-fields 'dao (remove-if (lambda (f) (member f (car indices)))
                                                                         (mapcar 'car fields)))
                                      :where ,(primary-key-test 'dao)))))
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

(defun save-dao (dao)
  "Save a dao: update it when it already exists, insert it otherwise."
  (if (dao-exists-p dao)
      (update-dao dao)
      (insert-dao dao)))

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
      (query-dao% ,type-name (sql ,query)))))

(defun table! (table template)
  "Lookup a table, complain if it does not exist."
  (or (table table template)
      (error "No table name ~A found in template ~A." table template)))
  
(defun next-id (table &optional template)
  "Get the next id for a table that has :auto-ids."
  (sequence-next (or (table-id-sequence (table! table template))
                     (error "Table ~A in template ~A does not have an id sequence." table template))))

(defun create-table (table &optional template)
  "Create a defined table and its indices in the database."
  (let ((table (table! table template)))
    (flet ((index-name (fields)
             (format nil "~A~{_~A~}_index" (to-sql-name (table-name table))
                     (mapcar #'to-sql-name (index-fields fields)))))
      (execute
       (sql-compile
        `(:create-table ,(table-name table)
          ,(mapcar (lambda (field)
                     (list (table-field-name field) :type (table-field-type field)))
                    (table-fields table))
          (:primary-key ,@(index-fields (car (table-indices table)))))))
      (dolist (index (cdr (table-indices table)))
        (execute (sql-compile `(,(if (index-unique-p index) :create-unique-index :create-index)
                                 (:raw ,(index-name index))
                                :on ,(table-name table)
                                :fields ,@(index-fields index)))))
      (when (table-id-sequence table)
        (execute (:create-sequence (table-id-sequence table)))))))

(defun drop-table (table &optional template)
  "Drop a defined table."
  (let ((table (table! table template)))
    (execute (sql (:drop-table (table-name table))))
    (when (table-id-sequence table)
      (execute (:drop-sequence (table-id-sequence table))))))

(defun reset-table (table &optional template)
  "Remove and re-create a defined table."
  (drop-table table template)
  (create-table table template))

(defun create-template (&optional template)
  "Create all the tables in a template \(default is the nil
template)."
  (dotemplate (table template)
    (create-table table template)))

(defun clear-template (&optional template)
  "Drop all the tables in a template."
  (dotemplate (table template)
    (drop-table table template)))

;;; Copyright (c) 2006 Marijn Haverbeke & Streamtech
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
