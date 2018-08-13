(in-package :postmodern)

(defparameter *transaction-level* 0)
(defparameter *current-logical-transaction* nil)
(defvar *transaction-mode* "")

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
    (execute (format nil "BEGIN ~A" *transaction-mode*)) ; 4) This one line change
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
  (release-savepoint savepoint))

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

;;;; The following code is copyright Gregory Tod and used
;;;; with permission under an MIT license.
;;;; See https://github.com/gtod/postgres-json

;;;; Postmodern transactions with isolation levels and RO, RW
;;;; settings plus serialization failure handling.

;;; The following adds support for setting the isolation level of a
;;; Postmodern transaction, and setting the read only or read write
;;; status of that transaction at the same time. See
;;; http://www.postgresql.org/docs/9.4/static/sql-set-transaction.html

;;; There is also support for a rudimentary retry loop to catch the
;;; cl-postgres-error:serialization-failure conditions that may arise
;;; when using 'repeatable read' or 'serializable' isolation levels.
;;; Since 'read committed' is the default isolation level, many
;;; Postgres users will never have seen such failures.  Postgres-JSON
;;; requires either of the above isolation levels because of, say,
;;; SUPERSEDE keeping a full history under the covers...

;;; A small change to Postmodern is required to support these
;;; additions, see postgres/postmodern.lisp

;;;; Public specials

(alexandria:define-constant +serializable-rw+
    "isolation level serializable read write" :test 'string=
    :documentation "START TRANSACTION string to set Postgres 'Serializable' isolation level and read/write.")

(alexandria:define-constant +repeatable-read-rw+
  "isolation level repeatable read read write" :test 'string=
  :documentation "START TRANSACTION string to set Postgres 'Repeatable read' isolation level and read/write."  )

(alexandria:define-constant +read-committed-ro+
  "isolation level read committed read only" :test 'string=
  :documentation "START TRANSACTION string to set Postgres 'Read committed' isolation level, which is the default, and read only.")

(alexandria:define-constant +read-committed-rw+
  "isolation level read committed read write" :test 'string=
  :documentation "START TRANSACTION string to set Postgres 'Read committed' isolation level, which is the default, and read write.")

(defvar *pgj-default-isolation-level* '+repeatable-read-rw+
  "The isolation level, a symbol, to use for WITH-MODEL-TRANSACTION.
For models that maintain history can only be +REPEATABLE-READ-RW+ or
+SERIALIZABLE-RW+.  For models without history could conceivably be
+READ-COMMITTED-RW+.")

;;;; Implementation

;; This should be thread safe as it it only ever bound local to a
;; specific thread.
(defvar *top-isolation-level* nil
  "When we start the first transaction in a nested group, bind this
to the isolation level requested.")

;; By abuse of notation I am referring to the combination of isolation
;; level _and_ read only/read write settings as just 'isolation
;; level'.  For calculating the congruence of nested transactions the
;; true isolation level is paramount but clearly you can't nest a RW
;; level inside a RO one...
(defvar *isolation-levels-hierarchy*
  '(+read-committed-ro+ +read-committed-rw+ +repeatable-read-rw+ +serializable-rw+)
  "A list of symbols for string constants which set Postgres isolation
levels, and read only or read/write settings.  Nested transactions must
be started with an isolation level to the left of, or at the same
level as, the top level in the nested group.")

(defun potential-serialization-failure-p (isolation-level)
  "Certain isolation levels require client handling of the
cl-postgres-error:serialization-failure condition.  This function
returns true if ISOLATION-LEVEL, a symbol, is such an isolation
level."
  (member isolation-level '(+repeatable-read-rw+ +serializable-rw+)))

(defun isolation-level-position (isolation-level)
  "What POSITION does ISOLATION-LEVEL, a symbol, hold in the
*ISOLATION-LEVELS-HIERARCHY*?"
  (position isolation-level *isolation-levels-hierarchy*))

(defun nestable-isolation-level-p (top-isolation-level isolation-level)
  "Can you nest a new (virtual) transaction with ISOLATION-LEVEL, a
symbol, inside a (true) transaction with TOP-ISOLATION-LEVEL?"
  (<= (isolation-level-position isolation-level)
      (isolation-level-position top-isolation-level)))

(define-condition incompatible-transaction-setting (error)
  ((transaction-name :initarg :transaction-name :reader transaction-name)
   (original :initarg :original :reader original)
   (current :initarg :current :reader current))
  (:report (lambda (condition stream)
             (format stream "You cannot nest the transaction named ~A with isolation level ~A
inside a transaction with isolation level ~A."
                     (transaction-name condition)
                     (current condition)
                     (original condition))))
  (:documentation "Signaled for a nested invocation of
WITH-ENSURED-TRANSACTION-LEVEL or WITH-LOGICAL-TRANSACTION-LEVEL
inside a previous invocation with an incongruent isolation level."))

(defun check-isolation-level (label isolation-level)
  (unless (nestable-isolation-level-p *top-isolation-level* isolation-level)
    (error 'incompatible-transaction-setting
           :transaction-name label
           :original *top-isolation-level*
           :current isolation-level)))

(defun transaction-thunk (transaction body)
  `(lambda (,transaction)
    (declare (ignorable ,transaction))
    ,@body))

;;;; Transactions proper in CALL-WITH style.
;;;; Semi public - some clients may need/want these

(defun call-with-transaction-level (label isolation-level thunk)
  (log:debug "Starting transaction ~A" label)
  ;; See postgres/postmodern.lisp for our proposed mods to Postmodern
  (let ((pomo:*transaction-mode* (symbol-value isolation-level))
        (*top-isolation-level* isolation-level))
    (multiple-value-prog1 (pomo:call-with-transaction thunk)
      (log:debug "Completing transaction ~A" label))))

(defun call-with-logical-transaction-level (label isolation-level thunk)
  (multiple-value-prog1
      (if *top-isolation-level*
          (progn
            (check-isolation-level label isolation-level)
            (log:debug "Nesting logical transaction ~A" label)
            (pomo:call-with-logical-transaction label thunk))
          (call-with-transaction-level label isolation-level thunk))
    (log:debug "Completing logical transaction ~A" label)))

(defun call-with-ensured-transaction-level (label isolation-level thunk)
  (multiple-value-prog1
      (if *top-isolation-level*
          (progn
            (check-isolation-level label isolation-level)
            (log:trace "Nesting ensured transaction ~A" label)
            ;; There is no "real" transaction to abort, so don't pass in label
            (funcall thunk nil))
          (progn
            (log:trace "Starting ensured transaction ~A" label)
            (call-with-transaction-level label isolation-level thunk)))
    (log:trace "Completing ensured transaction ~A" label)))

(defun call-with-retry-serialization-failure (label thunk)
  (if *serialization-failure-sleep-times*
      (dolist (sleep *serialization-failure-sleep-times* (funcall thunk))
        (log:trace "In retry serial loop with sleep: ~A" sleep)
        (handler-case
            (return (funcall thunk))
          (cl-postgres-error:serialization-failure ()
            (log:debug "Handle serialization failure of ~A.  Sleeping around: ~A" label sleep)
            (unless (zerop sleep) ; Do not wait the first time through
              (sleep (+ sleep (/ (random 2000) 1000)))))))
      (funcall thunk)))

;;;; Public macro interface

;;; These have the same name but with
;;; a -level suffix.  The do the same thing but allow specification of
;;; an isolation level.

(defmacro with-transaction-level ((name isolation-level) &body body)
  "Unilaterally evaluate BODY inside a Postmodern WITH-TRANSACTION
form with Postgres 'transaction mode' set to the symbol-value of
ISOLATION-LEVEL, a symbol.  The symbol NAME is bound to the Postmodern
`transaction-handle' and may be used in calls to Postmodern's
abort-transaction and commit-transaction."
  `(call-with-transaction-level ',name ',isolation-level
                                ,(transaction-thunk name body)))

(defmacro with-logical-transaction-level ((name isolation-level) &body body)
  "Similar to Postmodern's WITH-LOGICAL-TRANSACTION but start any top
level transaction with Postgres 'transaction mode' set to the
symbol-value of ISOLATION-LEVEL.  The symbol NAME is bound to the
Postmodern `transaction-handle' and may be used in calls to
Postmodern's abort-transaction and commit-transaction.  The condition
`incompatible-transaction-setting' will be signaled for incongruent
nested isolation levels."
  `(call-with-logical-transaction-level ',name ',isolation-level
                                        ,(transaction-thunk name body)))

(defmacro ensure-transaction-level ((isolation-level) &body body)
  "Similar to Postmodern's ENSURE-TRANSACTION but start any top level
transaction with Postgres 'transaction mode' set to the symbol-value
of ISOLATION-LEVEL.  The condition `incompatible-transaction-setting'
will be signaled for incongruent nested isolation levels."
  (let ((label (gensym "TRAN")))
    `(call-with-ensured-transaction-level ',label ',isolation-level
                                          ,(transaction-thunk label body))))
