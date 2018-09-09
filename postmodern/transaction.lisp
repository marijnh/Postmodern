(in-package :postmodern)

(defparameter *transaction-level* 0)
(defparameter *current-logical-transaction* nil)
(defparameter *isolation-level* :read-committed-rw)

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

(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun isolation-level-p (item)
       "Checks whether a variable is a valid isolation-level keyword."
       (and item (member item '(:read-committed-rw :read-committed-ro
                                :repeatable-read-rw :repeatable-read-ro
                                :serializable)))))

(defun begin-transaction (&optional (isolation-level *isolation-level*))
  (cond
    ((eq isolation-level :read-committed-rw)
     "BEGIN TRANSACTION ISOLATION LEVEL READ COMMITTED READ WRITE")
    ((eq isolation-level :read-committed-ro)
     "BEGIN TRANSACTION ISOLATION LEVEL READ COMMITTED READ ONLY")
    ((eq isolation-level :repeatable-read-rw)
     "BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ WRITE")
    ((eq isolation-level :repeatable-read-ro)
     "BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY")
    ((eq isolation-level :serializable)
     "BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE READ WRITE")
    (t "BEGIN TRANSACTION ISOLATION LEVEL READ COMMITTED READ WRITE")))

(defun call-with-transaction (body &optional (isolation-level *isolation-level*))
  (let ((transaction (make-instance 'transaction-handle)))
    (execute (begin-transaction isolation-level))
    (unwind-protect
         (multiple-value-prog1
             (let ((*transaction-level* (1+ *transaction-level*))
                   (*current-logical-transaction* transaction))
               (funcall body transaction))
           (commit-transaction transaction))
      (abort-transaction transaction))))

(defmacro with-transaction ((&optional name isolation-level) &body body)
  "Execute the body within a database transaction, committing when the
body exits normally, and aborting otherwise. An optional name and/or
isolation-level can be given to the transaction. The name can be used to
force a commit or abort before the body unwinds. The isolation-level
will set the isolation-level used by the transaction."
  (let ((transaction-name (or (when (not (isolation-level-p name))
                                  name)
                              (gensym "anonymous-transaction"))))
    (when (and name (isolation-level-p name))
      (setf isolation-level name))
    `(call-with-transaction (lambda (,transaction-name)
                              (declare (ignorable ,transaction-name)) ,@body)
                            ,isolation-level)))

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

(defun call-with-logical-transaction (name body &optional (isolation-level *isolation-level*))
  (if (zerop *transaction-level*)
      (call-with-transaction body isolation-level)
      (call-with-savepoint name body)))

(defmacro with-logical-transaction ((&optional (name nil name-p)
                                       (isolation-level *isolation-level* isolation-level-p))
                                    &body body)
  "Executes the body within a with-transaction (if no transaction is
already in progress) or a with-savepoint (if one is), binding the
transaction or savepoint to NAME (if supplied)"
  (let* ((effective-name (if (and name-p (not (isolation-level-p name)))
                             name
                             (gensym)))
         (effective-body (if (and name-p (not (isolation-level-p name)))
                             `(lambda (,name) ,@body)
                             `(lambda (,effective-name)
                                (declare (ignore ,effective-name))
                                ,@body)))
         (effective-isolation-level (cond ((and isolation-level-p (isolation-level-p isolation-level))
                                           isolation-level)
                                          ((and name-p (isolation-level-p name))
                                           name)
                                          (t isolation-level))))
    `(call-with-logical-transaction ',effective-name ,effective-body ,effective-isolation-level)))

(defmethod abort-logical-transaction ((savepoint savepoint-handle))
  (rollback-savepoint savepoint))

(defmethod abort-logical-transaction ((transaction transaction-handle))
  (abort-transaction transaction))

(defmethod commit-logical-transaction ((savepoint savepoint-handle))
  (release-savepoint savepoint))

(defmethod commit-logical-transaction ((transaction transaction-handle))
  (commit-transaction transaction))

(defun call-with-ensured-transaction (thunk &optional (isolation-level *isolation-level*))
  (if (zerop *transaction-level*)
      (with-transaction (nil isolation-level) (funcall thunk))
      (funcall thunk)))

(defmacro ensure-transaction (&body body)
  "Executes body within a with-transaction form if and only if no
transaction is already in progress."
  `(call-with-ensured-transaction (lambda () ,@body)))

(defmacro ensure-transaction-with-isolation-level (isolation-level &body body)
  "Executes body within a with-transaction form if and only if no
transaction is already in progress. This adds the ability to specify an isolatin
level other than the current default"
  `(call-with-ensured-transaction (lambda () ,@body) ,isolation-level))
