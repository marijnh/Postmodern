;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

(defparameter *allow-overwriting-prepared-statements* t
  "When set to t, ensured-prepared will overwrite prepared statements having
the same name if the query statement itself in the postmodern meta connection
is different than the query statement provided to ensure-prepared.")

(defun ensure-prepared (connection id query &optional (overwrite nil))
  "Make sure a statement has been prepared for this connection. If overwrite is
set to t (not the default), it will overwrite the existing query of the same
name."
  (let ((meta (connection-meta connection)))
    (unless (and (gethash id meta)
                 (if overwrite
                     (equal (gethash id meta) query)
                     t))
      (setf (gethash id meta) query)
      (prepare-query connection id query))))

(let ((next-id 0))
  (defun next-statement-id ()
    "Provide unique statement names."
    (incf next-id)
    (with-standard-io-syntax (format nil "STATEMENT_~A" next-id))))

(defun generate-prepared (function-form name query format)
  "Helper function for the following two macros. Note that it will attempt to
automatically reconnect if database-connection-error, or admin-shutdown. It
will reset any prepared statements triggering an invalid-sql-statement-name
error. The generated function will overwrite old prepared statements triggering
a duplicate-prepared-statement error and will pre-emptively overwrite an existing
prepared statement of the same name the first time generate-prepared is called
for this function name. Subsequent calls to the generated function will not
overwrite unless postgresql throws a duplicate-prepared-statement error."
  (destructuring-bind (reader result-form) (reader-for-format format)
    (let ((base `(exec-prepared *database* statement-id params ,reader)))
      `(let ((statement-id ,(string name))
             (query ,(real-query query)))
         (,@function-form (&rest params)
                          (handler-bind
                              ((postmodern:database-connection-error
                                (lambda (msg1)
                                  (format *error-output* "~%Database-connection-error ~a~%" msg1)
                                  (invoke-restart :reconnect))))
                            (handler-bind ((cl-postgres-error:admin-shutdown
                                            (lambda (msg2)
                                              (declare (ignore msg2))
                                              (invoke-restart :reconnect))))
                              (cl-postgres::with-reconnect-restart *database*
                                (handler-bind
                                    ((cl-postgres-error:invalid-sql-statement-name #'pomo:reset-prepared-statement)
                                     (cl-postgres-error:duplicate-prepared-statement #'pomo:reset-prepared-statement))
                                  (if overwrite
                                      (progn
                                        (setf overwrite nil)
                                        (ensure-prepared *database* statement-id query t))
                                    (ensure-prepared *database* statement-id query overwrite))
                                  (,result-form ,base))))))))))

(defmacro prepare (query &optional (format :rows))
  "Wraps a query into a function that will prepare it once for a
connection, and then execute it with the given parameters. The query
should contain a placeholder \($1, $2, etc) for every parameter."
  `(let ((overwrite t))
     ,(generate-prepared '(lambda) (next-statement-id) query format)))

(defmacro defprepared (name query &optional (format :rows))
  "Like prepare, but gives the function a name instead of returning
it. The name should not be a string but may be quoted."
  (when (consp name) (setf name (s-sql::dequote name)))
  `(let ((overwrite t))
     ,(generate-prepared `(defun ,name) name query format)))

(defmacro defprepared-with-names (name (&rest args)
				  (query &rest query-args)
				  &optional (format :rows))
  "Like defprepared, but with lambda list for statement arguments."
  (let ((prepared-name (gensym "PREPARED")))
    `(let ((,prepared-name (prepare ,query ,format)))
       (declare (type function ,prepared-name))
       (defun ,name ,args
	       (funcall ,prepared-name ,@query-args)))))

(defun prepared-statement-exists-p (name)
  "Returns t if the prepared statement exists in the current postgresql
session, otherwise nil."
  (if (query (:select 'name
                   :from 'pg-prepared-statements
                   :where (:= 'name (string-upcase name)))
             :single)
      t
      nil))

(defun list-prepared-statements (&optional (names-only nil))
  "Syntactic sugar. A query that lists the prepared statements
in the session in which the function is run. If the optional
names-only parameter is set to t, it will only return a list
of the names of the prepared statements."
  (if names-only
      (alexandria:flatten (query "select name from pg_prepared_statements"))
      (query "select * from pg_prepared_statements" :alists)))

(defun drop-prepared-statement (name &key (location :both) (database *database*) (remove-function t))
  "Prepared statements are stored both in the meta slot in the postmodern
connection and in postgresql session information. In the case of prepared
statements generated with defprepared, there is also a lisp function with
the same name.

If you know the prepared statement name, you can delete the prepared statement
from both locations (the default behavior), just from postmodern by passing
:postmodern to the location key parameter or just from postgresql by passing
:postgresql to the location key parameter.

If you pass the name 'All' as the statement name, it will
delete all prepared statements.

The default behavior is to also remove any lisp function of the same name.
This behavior is controlled by the remove-function key parameter."
  (when (symbolp name) (setf name (string name)))
  (check-type name string)
  (check-type location keyword)
  (setf name (string-upcase name))
  (when database
    (cond ((eq location :both)
           (cond ((equal name "ALL")
                  (maphash #'(lambda (x y)
                               (declare (ignore y))
                               (remhash x (connection-meta database))
                               (when (and remove-function (find-symbol (string-upcase x))
                                 (fmakunbound (find-symbol (string-upcase x))))))
                           (connection-meta database))
                 (clrhash (connection-meta database))
                 (query "deallocate ALL"))
                (t
                 (remhash name (connection-meta database))
                 (handler-case
                     (query (format nil "deallocate ~:@(~S~)" name))
                   (cl-postgres-error:invalid-sql-statement-name ()
                     (format t "Statement does not exist ~a~%" name)))
                 (when (and remove-function (find-symbol (string-upcase name)))
                   (fmakunbound (find-symbol (string-upcase name)))))))
         ((eq location :postmodern)
          (if (equal name "ALL")
              (maphash #'(lambda (x y)
                           (declare (ignore y))
                               (remhash x (connection-meta database))
                               (when (and remove-function (find-symbol (string-upcase x)))
                                 (fmakunbound (find-symbol (string-upcase x)))))
                           (connection-meta database))
            (progn
              (remhash (string-upcase name)
                       (connection-meta database))
              (when (and remove-function (find-symbol (string-upcase name)))
                   (fmakunbound (find-symbol (string-upcase name)))))))
         ((eq location :postgresql)
          (cond ((equal name "ALL")
                 (query "deallocate ALL"))
                (t (handler-case
                       (query (format nil "deallocate ~:@(~S~)" name))
                     (cl-postgres-error:invalid-sql-statement-name ()
                       (format t "Statement does not exist ~a~%" name)))))))))

(defun list-postmodern-prepared-statements (&optional (names-only nil))
  "List the prepared statements that postmodern has put in the meta slot in
the connection. It will return a list of alists of form:
  ((:NAME . \"SNY24\")
  (:STATEMENT . \"(SELECT name, salary FROM employee WHERE (city = $1))\")
  (:PREPARE-TIME . #<TIMESTAMP 25-11-2018T15:36:43,385>)
  (:PARAMETER-TYPES . \"{text}\") (:FROM-SQL).

If the names-only parameter is set to t, it will only return a list of
the names of the prepared statements."
  (if names-only
      (alexandria:hash-table-keys (postmodern::connection-meta *database*))
      (alexandria:hash-table-alist (postmodern::connection-meta *database*))))

(defun find-postgresql-prepared-statement (name)
  "Returns the specified named prepared statement (if any) that postgresql
has for this session."
  (query (:select 'statement
                  :from 'pg-prepared-statements
                  :where (:= 'name (string-upcase name)))
         :single))

(defun find-postmodern-prepared-statement (name)
  "Returns the specified named prepared statement (if any) that postmodern has put in
the meta slot in the connection."
  (gethash (string-upcase name) (postmodern::connection-meta *database*)))

(defun reset-prepared-statement (condition)
  "If you have received an invalid-prepared-statement error or a prepared-statement
already exists error but the prepared statement is still in the meta slot in
the postmodern connection, try to regenerate the prepared statement at the
database connection level and restart the connection."
  (let* ((name (pomo:database-error-extract-name condition))
         (statement (find-postmodern-prepared-statement name))
         (pid (write-to-string (first (cl-postgres::connection-pid *database*)))))
    (setf (cl-postgres::connection-available *database*) t)
    (when statement
      (cl-postgres::with-reconnect-restart *database*
        (terminate-backend pid))
      (cl-postgres:prepare-query *database* name statement)
      (invoke-restart 'reset-prepared-statement))))

(defun get-pg-backend-pid ()
  "Get the process id used by postgresql for this connection."
  (query "select pg_backend_pid()" :single))

(defun get-pid-from-postmodern ()
  "Get the process id used by postgresql for this connection,
but get it from the postmodern connection parameters."
  (gethash "pid" (pomo::connection-parameters *database*)))

(defun cancel-backend (pid &optional (database *database*))
  "Polite way of terminating a query at the database (as opposed to calling close-database).
Slower than (terminate-backend pid) and does not always work."
    (let ((database-name (cl-postgres::connection-db database))
          (user (cl-postgres::connection-user database))
          (password (cl-postgres::connection-password database))
          (host (cl-postgres::connection-host database))
          (port (cl-postgres::connection-port database))
          (use-ssl (cl-postgres::connection-use-ssl database)))
    (with-connection `(,database-name ,user ,password ,host :PORT ,port :USE-SSL ,use-ssl)
      (query "select pg_cancel_backend($1);" pid))))

(defun terminate-backend (pid &optional (database *database*))
  "Less polite way of terminating at the database (as opposed to calling close-database).
Faster than (cancel-backend pid) and more reliable."
  (let ((database-name (cl-postgres::connection-db database))
        (user (cl-postgres::connection-user database))
        (password (cl-postgres::connection-password database))
        (host (cl-postgres::connection-host database))
        (port (cl-postgres::connection-port database))
        (use-ssl (cl-postgres::connection-use-ssl database)))
    (with-connection `(,database-name ,user ,password ,host :PORT,port :USE-SSL ,use-ssl)
      (query "select pg_terminate_backend($1);" pid))))
