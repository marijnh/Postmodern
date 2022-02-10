;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

(defparameter *current-logical-transaction* nil
  "This is bound to the current transaction-handle or savepoint-handle instance
representing the innermost open logical transaction.")

(defgeneric abort-hooks (obj)
  (:documentation "An accessor for the transaction or savepoint's list of abort
hooks, each of which should be a function with no required arguments. These
functions will be executed when a transaction is aborted or a savepoint rolled
back (whether via a non-local transfer of control or explicitly by either
abort-transaction or rollback-savepoint)."))

(defgeneric abort-logical-transaction (obj)
  (:documentation "Roll back the given logical transaction, regardless of
whether it is an actual transaction or a savepoint."))

(defgeneric commit-hooks (obj)
  (:documentation "An accessor for the transaction or savepoint's list of commit
hooks, each of which should be a function with no required arguments. These
functions will be executed when a transaction is committed or a savepoint
released."))

(defgeneric commit-logical-transaction (obj)
  (:documentation "Commit the given logical transaction, regardless of whether
it is an actual transaction or a savepoint."))

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

(defun retry-transaction (&optional condition)
  "Invokes the retry-transaction restart, if found."
  (let ((restart (find-restart 'retry-transaction condition)))
    (if (null restart)
        (error "Attempting to invoke-restart RETRY-TRANSACTION but no such restart is active. Are you in a transaction block?")
        (invoke-restart restart))))

(defun call-with-transaction (body &optional (isolation-level *isolation-level*))
  (tagbody start
     (restart-case
         (let ((transaction (make-instance 'transaction-handle)))
           (execute (begin-transaction isolation-level))
           (unwind-protect
                (return-from call-with-transaction
                  (multiple-value-prog1
                      (let ((*transaction-level* (1+ *transaction-level*))
                            (*current-logical-transaction* transaction))
                        (funcall body transaction))
                    (commit-transaction transaction)))
             (abort-transaction transaction)))
       (retry-transaction ()
         :report "Retry the current transaction."
         (go start)))))

(defmacro with-transaction ((&optional name isolation-level) &body body)
  "Execute the given body within a database transaction, committing it when the
body exits normally, and aborting otherwise. An optional name and/or
isolation-level can be given to the transaction. The name can be used to force
a commit or abort before the body unwinds. The isolation-level will set the
isolation-level used by the transaction.

You can specify the following isolation levels in postmodern transactions:

- :read-committed-rw (read committed with read and write)
- :read-committed-ro (read committed with read only)
- :repeatable-read-rw (repeatable read with read and write)
- :repeatable-read-ro (repeatable read with read only)
- :serializable (serializable with reand and write)

Sample usage where george is just the name given to the transaction (not quoted
or a string) and ... simply indicates other statements would be expected here:

    (with-transaction ()
      (execute (:insert-into 'test-data :set 'value 77))
      ...)

    (with-transaction (george)
      (execute (:insert-into 'test-data :set 'value 22))
      ...)

    (with-transaction (george :read-committed-rw)
      (execute (:insert-into 'test-data :set 'value 33))
      (query (:select '* :from 'test-data))
      ...)

    (with-transaction (:serializable)
      (execute (:insert-into 'test-data :set 'value 44))
      ...)

Further discussion of transactions and isolation levels can found in the
isolation notes file in the doc folder."
  (let ((transaction-name (or (when (not (isolation-level-p name))
                                name)
                              (gensym "anonymous-transaction"))))
    (when (and name (isolation-level-p name))
      (setf isolation-level name))
    `(call-with-transaction (lambda (,transaction-name)
                              (declare (ignorable ,transaction-name)) ,@body)
                            ,isolation-level)))

(defun abort-transaction (transaction)
  "Roll back the given transaction to the beginning, but the transaction
block is still active. Thus calling abort-transaction in the middle of a
transaction does not end the transaction. Any subsequent statements will still
be executed. Per the Postgresql documentation: ABORT rolls back the current
transaction and causes all the updates made by the transaction to be discarded.
This command is identical in behavior to the standard SQL command ROLLBACK, and
is present only for historical reasons."
  (when (transaction-open-p transaction)
    (let ((*database* (transaction-connection transaction)))
      (execute "ABORT"))
    (setf (transaction-open-p transaction) nil)
    (mapc #'funcall (abort-hooks transaction))))

(defun rollback-transaction (transaction)
  "Roll back the given transaction to the beginning, but the transaction
block is still active. Thus calling abort-transaction in the middle of a
transaction does not end the transaction. Any subsequent statements will still
be executed. Per the Postgresql documentation: this rolls back the current
transaction and causes all the updates made by the transaction to be discarded."
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
  "Can only be used within a transaction. Establishes a savepoint with the given
name at the start of body, and binds the same name to a handle for that savepoint.
The body is executed and, at the end of body, the savepoint is released, unless a
condition is thrown, in which case it is rolled back. Execute the body within a
savepoint, releasing savepoint when the body exits normally, and rolling back
otherwise. NAME is both the variable that can be used to release or rolled back
before the body unwinds, and the SQL name of the savepoint.

An example might look like this:

(defun test12 (x &optional (y nil))
  (with-logical-transaction (lt1 :read-committed-rw)
    (execute (:insert-into 'test-data :set 'value 0))
    (with-savepoint sp1
      (execute (:insert-into 'test-data :set 'value 1))
      (if (< x 0)
          (rollback-savepoint sp1)
          (release-savepoint sp1)))
    (with-savepoint sp2
      (execute (:insert-into 'test-data :set 'value 2))
      (with-savepoint sp3
        (execute (:insert-into 'test-data :set 'value 3))
        (if (> x 0)
            (rollback-savepoint sp3)
            (release-savepoint sp3))
        (when y (rollback-savepoint sp2)))
      (if (= x 0)
          (rollback-savepoint sp2)
          (release-savepoint sp2)))
    (when (string= y \"abrt\")
      (abort-transaction lt1))))"

  `(call-with-savepoint ',name (lambda (,name)
                                 (declare (ignorable ,name)) ,@body)))

(defun rollback-savepoint (savepoint)
  "Immediately roll back a savepoint, aborting the results."
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

(defun call-with-logical-transaction (name body
                                      &optional (isolation-level *isolation-level*))
  (if (zerop *transaction-level*)
      (call-with-transaction body isolation-level)
      (call-with-savepoint name body)))

(defmacro with-logical-transaction ((&optional (name nil name-p)
                                       (isolation-level *isolation-level*
                                                        isolation-level-p))
                                    &body body)
  "Executes body within a with-transaction form if no transaction is currently
in progress, otherwise simulates a nested transaction by executing it
within a with-savepoint form. The transaction or savepoint is bound to name
if one is supplied. The isolation-level will set the isolation-level used by
the transaction.

You can specify the following isolation levels in postmodern transactions:

- :read-committed-rw (read committed with read and write)
- :read-committed-ro (read committed with read only)
- :repeatable-read-rw (repeatable read with read and write)
- :repeatable-read-ro (repeatable read with read only)
- :serializable (serializable with reand and write)

Sample usage where george is just the name given to the transaction (not
quoted or a string) and ... simply indicates other statements would be
expected here:


  (with-logical-transaction ()
    (execute (:insert-into 'test-data :set 'value 77))
    ...)

  (with-logical-transaction (george)
    (execute (:insert-into 'test-data :set 'value 22))
    ...)

  (with-logical-transaction (george :read-committed-rw)
    (execute (:insert-into 'test-data :set 'value 33))
    ...)

  (with-logical-transaction (:serializable)
    (execute (:insert-into 'test-data :set 'value 44))
    ...)"
  (let* ((effective-name (if (and name-p (not (isolation-level-p name)))
                             name
                             (gensym)))
         (effective-body (if (and name-p (not (isolation-level-p name)))
                             `(lambda (,name) ,@body)
                             `(lambda (,effective-name)
                                (declare (ignore ,effective-name))
                                ,@body)))
         (effective-isolation-level (cond ((and isolation-level-p
                                                (isolation-level-p isolation-level))
                                           isolation-level)
                                          ((and name-p (isolation-level-p name))
                                           name)
                                          (t isolation-level))))
    `(call-with-logical-transaction ',effective-name ,effective-body
                                    ,effective-isolation-level)))

(defmethod abort-logical-transaction ((savepoint savepoint-handle))
  (rollback-savepoint savepoint))

(defmethod abort-logical-transaction ((transaction transaction-handle))
  (abort-transaction transaction))

(defmethod commit-logical-transaction ((savepoint savepoint-handle))
  (release-savepoint savepoint))

(defmethod commit-logical-transaction ((transaction transaction-handle))
  (commit-transaction transaction))

(defun call-with-ensured-transaction (thunk
                                      &optional (isolation-level *isolation-level*))
  (if (zerop *transaction-level*)
      (with-transaction (nil isolation-level) (funcall thunk))
      (funcall thunk)))

(defmacro ensure-transaction (&body body)
  "Ensures that body is executed within a transaction, but does not begin a new
transaction if one is already in progress."
  `(call-with-ensured-transaction (lambda () ,@body)))

(defmacro ensure-transaction-with-isolation-level (isolation-level &body body)
  "Executes body within a with-transaction form if and only if no transaction is
already in progress. This adds the ability to specify an isolation level other
than the current default"
  `(call-with-ensured-transaction (lambda () ,@body) ,isolation-level))
