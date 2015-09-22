(in-package :postmodern)

(defparameter *transaction-level* 0)
(defparameter *current-logical-transaction* nil)

(defclass transaction-handle ()
  ((open-p :initform t :accessor transaction-open-p)
   (connection :initform *database* :reader transaction-connection)
   (commit-hooks :initform nil :accessor commit-hooks)
   (abort-hooks :initform nil :accessor abort-hooks))
  (:documentation "Simple box type for storing the status and the
associated database connection of a transaction. When open-p is nil,
the transaction has been aborted or committed. commit-hooks and
abort-hooks hold lists of functions (which should require no
arguments) to be executed at commit and abort time, respectively."))

(defun call-with-transaction (body)
  (let ((transaction (make-instance 'transaction-handle)))
    (execute "BEGIN")
    (unwind-protect
         (multiple-value-prog1
             (let ((*transaction-level* (1+ *transaction-level*))
                   (*current-logical-transaction* transaction))
               (funcall body transaction))
           (commit-transaction transaction))
      (abort-transaction transaction))))

(defmacro with-transaction ((&optional name) &body body)
  "Execute the body within a database transaction, committing when the
body exits normally, and aborting otherwise. An optional name can be
given to the transaction, which can be used to force a commit or abort
before the body unwinds."
  (let ((transaction-name (or name (gensym "anonymous-transaction"))))
    `(call-with-transaction (lambda (,transaction-name)
                              (declare (ignorable ,transaction-name)) ,@body))))

(defun abort-transaction (transaction)
  "Immediately abort an open transaction."
  (when (transaction-open-p transaction)
    (let ((*database* (transaction-connection transaction)))
      (execute "ABORT"))
    (setf (transaction-open-p transaction) nil)
    (mapc #'funcall (abort-hooks transaction))))

(defun commit-transaction (transaction)
  "Immediately commit an open transaction."
  (when (transaction-open-p transaction)
    (let ((*database* (transaction-connection transaction)))
      (execute "COMMIT"))
    (setf (transaction-open-p transaction) nil)
    (mapc #'funcall (commit-hooks transaction))))


(defclass savepoint-handle (transaction-handle)
  ((name :initform (error "Savepoint name is not provided.")
         :initarg :name :reader savepoint-name)
   (open-p :initform t :accessor savepoint-open-p)
   (connection :initform *database* :reader savepoint-connection))
  (:documentation "Simple box type for storing the state and the
associated database connection of a savepoint."))

(defun call-with-savepoint (name body)
  (let ((savepoint (make-instance 'savepoint-handle :name (to-sql-name name))))
    (execute (format nil "SAVEPOINT ~A" (savepoint-name savepoint)))
    (unwind-protect
         (multiple-value-prog1
             (let ((*transaction-level* (1+ *transaction-level*))
                   (*current-logical-transaction* savepoint))
               (funcall body savepoint))
           (release-savepoint savepoint))
      (rollback-savepoint savepoint))))

(defmacro with-savepoint (name &body body)
  "Execute the body within a savepoint, releasing savepoint when the
body exits normally, and rolling back otherwise. NAME is both the
variable that can be used to release or rolled back before the body
unwinds, and the SQL name of the savepoint."
  `(call-with-savepoint ',name (lambda (,name) (declare (ignorable ,name)) ,@body)))

(defun rollback-savepoint (savepoint)
  "Immediately roll back a savepoint, aborting it results."
  (when (savepoint-open-p savepoint)
    (let ((*database* (savepoint-connection savepoint)))
      (execute (format nil "ROLLBACK TO SAVEPOINT ~A"
                       (savepoint-name savepoint))))
    (setf (savepoint-open-p savepoint) nil)
    (mapc #'funcall (abort-hooks savepoint))))

(defun release-savepoint (savepoint)
  "Immediately release a savepoint, commiting its results."
  (when (savepoint-open-p savepoint)
    (let ((*database* (savepoint-connection savepoint)))
      (execute (format nil "RELEASE SAVEPOINT ~A"
                       (savepoint-name savepoint))))
    (setf (transaction-open-p savepoint) nil)
    (mapc #'funcall (commit-hooks savepoint))))

(defun call-with-logical-transaction (name body)
  (if (zerop *transaction-level*)
      (call-with-transaction body)
      (call-with-savepoint name body)))

(defmacro with-logical-transaction ((&optional (name nil name-p)) &body body)
  "Executes the body within a with-transaction (if no transaction is
already in progress) or a with-savepoint (if one is), binding the
transaction or savepoint to NAME (if supplied)"
  (let* ((effective-name (if name-p
                             name
                             (gensym)))
         (effective-body (if name-p
                             `(lambda (,name) ,@body)
                             `(lambda (,effective-name)
                                (declare (ignore ,effective-name))
                                ,@body))))
    `(call-with-logical-transaction ',effective-name ,effective-body)))

(defmethod abort-logical-transaction ((savepoint savepoint-handle))
  (rollback-savepoint savepoint))

(defmethod abort-logical-transaction ((transaction transaction-handle))
  (abort-transaction transaction))

(defmethod commit-logical-transaction ((savepoint savepoint-handle))
  (commit-transaction savepoint))

(defmethod commit-logical-transaction ((transaction transaction-handle))
  (commit-transaction transaction))

(defun call-with-ensured-transaction (thunk)
  (if (zerop *transaction-level*)
      (with-transaction () (funcall thunk))
      (funcall thunk)))

(defmacro ensure-transaction (&body body)
  "Executes body within a with-transaction form if and only if no
transaction is already in progress."
  `(call-with-ensured-transaction (lambda () ,@body)))
