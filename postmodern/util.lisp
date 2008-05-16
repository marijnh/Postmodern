(in-package :postmodern)

(defun to-identifier (name)
  "Used to allow both strings and symbols as identifier - converts
symbols to string with the S-SQL rules."
  (if (stringp name)
      name
      (to-sql-name name)))

(defun sequence-next (sequence)
  "Shortcut for getting the next value from a sequence."
  (query (:select (:nextval (to-identifier sequence))) :single))

(defmacro make-list-query (relkind)
  "Helper macro for the functions that list tables, sequences, and
views."
  `(sql (:select 'relname :from 'pg-catalog.pg-class
         :inner-join 'pg-catalog.pg-namespace :on (:= 'relnamespace 'pg-namespace.oid)
         :where (:and (:= 'relkind ,relkind)
                 (:not-in 'nspname (:set "pg_catalog" "pg_toast"))
                 (:pg-catalog.pg-table-is-visible 'pg-class.oid)))))

(defmacro make-exists-query (relkind name)
  "Helper macro for the functions that check whether an object
exists."
  `(sql (:select (:exists (:select 1 :from 'pg-catalog.pg-class
                                   :where (:and (:= 'relkind ,relkind)
                                                (:= 'relname (to-identifier ,name))))))))

(defun list-tables (&optional strings-p)
  "Return a list of the tables in a database. Turn them into keywords
if strings-p is not true."
  (let ((result (query (make-list-query "r") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))
(defun table-exists-p (table)
  "Check whether a table exists. Takes either a string or a symbol for
the table name."
  (query (make-exists-query "r" table) :single))

(defun list-sequences (&optional strings-p)
  "Return a list of the sequences in a database. Turn them into
keywords if strings-p is not true."
  (let ((result (query (make-list-query "S") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))
(defun sequence-exists-p (sequence)
  "Check whether a sequence exists. Takes either a string or a symbol
for the sequence name."
  (query (make-exists-query "S" sequence) :single))

(defun list-views (&optional strings-p)
  "Return a list of the views in a database. Turn them into keywords
if strings-p is not true."
  (let ((result (query (make-list-query "v") :column)))
    (if strings-p result (mapcar 'from-sql-name result))))
(defun view-exists-p (view)
  "Check whether a view exists. Takes either a string or a symbol for
the view name."
  (query (make-exists-query "v" view) :single))

(defun table-description (table)
  "Return a list of (name type null-allowed) lists for the fields of a
table."
  (query (:select 'attname 'typname (:not 'attnotnull)
                  :from 'pg-catalog.pg-attribute
                  :inner-join 'pg-catalog.pg-type :on (:= 'pg-type.oid 'atttypid)
                  :inner-join 'pg-catalog.pg-class :on (:and (:= 'pg-class.oid 'attrelid)
                                                             (:= 'pg-class.relname (to-identifier table)))
                  :where (:> 'attnum 0))))

(defclass transaction-handle ()
  ((open-p :initform t :accessor transaction-open-p)
   (connection :initform *database* :reader transaction-connection))
  (:documentation "Simple box type for storing the status and the
associated database connection of a transaction. When open-p is nil,
the transaction has been aborted or committed."))

(defmacro with-transaction ((&optional name) &body body)
  "Execute the body within a database transaction, committing when the
body exits normally, and aborting otherwise. An optional name can be
given to the transaction, which can be used to force a commit or abort
before the body unwinds."
  (let ((name (or name (gensym))))
    `(let ((,name (make-instance 'transaction-handle)))
      (execute "BEGIN")
      (unwind-protect
           (prog1 (progn ,@body)
             (commit-transaction ,name))
        (abort-transaction ,name)))))

(defun abort-transaction (transaction)
  "Immediately abort an open transaction."
  (when (transaction-open-p transaction)
    (let ((*database* (transaction-connection transaction)))
      (execute "ABORT"))
    (setf (transaction-open-p transaction) nil)))

(defun commit-transaction (transaction)
  "Immediately commit an open transaction."
  (when (transaction-open-p transaction)
    (let ((*database* (transaction-connection transaction)))
      (execute "COMMIT"))
    (setf (transaction-open-p transaction) nil)))


(defclass savepoint-handle (transaction-handle)
  ((name :initform (error "Savepoint name is not provided.")
         :initarg :name :reader savepoint-name)
   (open-p :initform t :accessor savepoint-open-p)
   (connection :initform *database* :reader savepoint-connection))
  (:documentation "Simple box type for storing the state and the
associated database connection of a savepoint."))

(defmacro with-savepoint (name &body body)
  "Execute the body within a savepoint, releasing savepoint when the
body exits normally, and rolling back otherwise. NAME is both the
variable that can be used to release or rolled back before the body
unwinds, and the SQL name of the savepoint."
  `(let ((,name (make-instance 'savepoint-handle :name (to-sql-name ',name))))
     (execute (format nil "SAVEPOINT ~A" (savepoint-name ,name)))
     (unwind-protect (prog1 (progn ,@body)
                       (release-savepoint ,name))
       (rollback-savepoint ,name))))

(defun rollback-savepoint (savepoint)
  "Immediately roll back a savepoint, aborting it results."
  (when (savepoint-open-p savepoint)
    (let ((*database* (savepoint-connection savepoint)))
      (execute (format nil "ROLLBACK TO SAVEPOINT ~A"
                       (savepoint-name savepoint))))
    (setf (savepoint-open-p savepoint) nil)))

(defun release-savepoint (savepoint)
  "Immediately release a savepoint, commiting its results."
  (when (savepoint-open-p savepoint)
    (let ((*database* (savepoint-connection savepoint)))
      (execute (format nil "RELEASE SAVEPOINT ~A"
                           (savepoint-name savepoint))))
    (setf (transaction-open-p savepoint) nil)))

(defun coalesce (&rest args)
  (some (lambda (x) (if (eq x :null) nil x)) args))
