;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

(define-condition mismatched-parameter-types (error)
  ((prepared-statement-types :initarg prepared-statement-types
                             :reader prepared-statement-types)
   (parameter-types :initarg parameter-types
                    :reader parameter-types))
  (:report (lambda (condition stream)
             (format stream "Parameter types ~a do not match prepared statement parameter types ~a"
                     (parameter-types condition)
                     (prepared-statement-types condition)))))

(defun ensure-prepared (connection id query &optional (overwrite nil) (params nil))
  "Make sure a statement has been prepared for this connection. If overwrite is
set to t (not the default), it will overwrite the existing query of the same
name. Reminder that the meta info is a list of query, params."
  (let ((meta (connection-meta connection)))
    (unless (and (gethash id meta)
                 (if overwrite
                     (equal (first (gethash id meta)) query)
                     t))
        (progn
          (setf (gethash id meta) (list query params))
          (prepare-query connection id query params)))))

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
                                   (format *error-output*
                                           "~%Database-connection-error ~a~%"
                                           msg1)
                                   (invoke-restart :reconnect))))
                            (handler-bind ((cl-postgres-error:admin-shutdown
                                             (lambda (msg2)
                                               (declare (ignore msg2))
                                               (invoke-restart :reconnect))))
                              (cl-postgres::with-reconnect-restart *database*
                                (handler-bind
                                    ((cl-postgres-error:invalid-sql-statement-name
                                       #'pomo:reset-prepared-statement)
                                     (cl-postgres-error:duplicate-prepared-statement
                                       #'pomo:reset-prepared-statement))
                                  (cond (overwrite
                                         (setf overwrite nil)
                                         (drop-prepared-statement statement-id :remove-function nil)
                                         (ensure-prepared *database* statement-id
                                                          query t params))
                                        ((not (connection-use-binary *database*))
                                         (ensure-prepared *database* statement-id
                                                          query overwrite params))
                                        (t
                                         (let ((prepared-statement-param-types
                                                 (cl-postgres:parameter-list-types
                                                  (second
                                                   (find-postmodern-prepared-statement
                                                    statement-id))))
                                               (param-types
                                                 (cl-postgres::parameter-list-types params)))
                                           (if (equal prepared-statement-param-types
                                                      param-types)
                                               (ensure-prepared *database* statement-id
                                                                query overwrite params)
                                               (error
                                                (make-condition 'mismatched-parameter-types
                                                                :prepared-statement-types
                                                                prepared-statement-param-types
                                                                :parameter-types
                                                                param-types))))))
                                  (,result-form ,base))))))))))

(defmacro prepare (query &optional (format :rows))
  "Wraps a query into a function that can be used as the interface to a prepared
statement. The given query (either a string or an S-SQL form) may contain
placeholders, which look like $1, $2, etc. The resulting function takes one
argument for every placeholder in the query, executes the prepared query, and
returns the result in the format specified. (Allowed formats are the same as for
query.)

For queries that have to be run very often, especially when they are complex,
it may help performance since the server only has to plan them once. See the
http://www.postgresql.org/docs/current/static/sql-prepare.html
in the PostgreSQL manual for details.

In some cases, the server will complain about not being able to deduce the type
of the arguments in a statement. In that case you should add type
declarations (either with the PostgreSQL's CAST SQL-conforming syntax or
historical :: syntax, or with S-SQL's :type construct) to help it out.

Note that it will attempt to automatically reconnect if database-connection-error,
or admin-shutdown. It will reset prepared statements triggering an
invalid-sql-statement-name error. It will overwrite old prepared statements
triggering a duplicate-prepared-statement error."
  `(let ((overwrite ,*allow-overwriting-prepared-statements*))
     ,(generate-prepared '(lambda) (next-statement-id) query format)))

(defmacro defprepared (name query &optional (format :rows))
  "This is another macro variant of prepare. It is like prepare, but gives the
function a name which now becomes a top-level function for the prepared
statement. The name should not be a string but may be quoted."
  (when (consp name) (setf name (s-sql::dequote name)))
  `(let ((overwrite ,*allow-overwriting-prepared-statements*))
     ,(generate-prepared `(defun ,name) name query format)))

(defmacro defprepared-with-names (name (&rest args)
                                  (query &rest query-args)
                                  &optional (format :rows))
  "Like defprepared, but allows to specify names of the function arguments in a
lambda list as well as arguments supplied to the query."
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
              :where (:= 'name '$1))
             (string-upcase name) :single)
      t
      nil))

(defun list-prepared-statements (&optional (names-only nil))
  "This is syntactic sugar. A query that lists the prepared statements in the
session in which the function is run. It will return a list of alists of form:
  ((:NAME . \"SNY24\")
  (:STATEMENT . \"(SELECT name, salary FROM employee WHERE (city = $1))\")
  (:PREPARE-TIME . #<TIMESTAMP 25-11-2018T15:36:43,385>)
  (:PARAMETER-TYPES . \"{text}\") (:FROM-SQL).

If the optional names-only parameter is
set to t, it will only return a list of the names of the prepared statements."
  (if names-only
      (alexandria:flatten (query "select name from pg_prepared_statements"))
      (query "select * from pg_prepared_statements" :alists)))


(defun list-postmodern-prepared-statements (&optional (names-only nil))
  "List the prepared statements that postmodern has put in the meta slot in
the connection.

If the names-only parameter is set to t, it will only return a list of
the names of the prepared statements."
  (if names-only
      (alexandria:hash-table-keys (postmodern::connection-meta *database*))
      (alexandria:hash-table-alist (postmodern::connection-meta *database*))))

(defun find-postgresql-prepared-statement (name)
  "Returns the specified named prepared statement (if any) that postgresql has
for this session."
  (query (:select 'statement
          :from 'pg-prepared-statements
          :where (:= 'name '$1))
         (string-upcase name) :single))

(defun find-postgresql-prepared-statement-by-query (query)
  "Returns the name of prepared statement (if any) postgresql has
for this session that matches the query ."
  (query (:select 'name
          :from 'pg-prepared-statements
          :where (:= 'statement '$1))
         query))

(defun find-postmodern-prepared-statement (name)
  "Returns the specified named prepared statement (if any) that postmodern has
put in the meta slot in the connection. Note that this is the statement itself,
not the name."
  (gethash (string-upcase name)
           (postmodern::connection-meta *database*)))

(defun drop-prepared-statement (name &key (location :both) (database *database*)
                                       (remove-function t))
  "The statement name can be a string or quoted symbol.

Prepared statements are stored both in the meta slot in the postmodern
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
                               (when (and remove-function
                                          (find-symbol (string-upcase x))
                                          (fmakunbound (find-symbol (string-upcase x))))))
                           (connection-meta database))
                  (clrhash (connection-meta database))
                  (query "deallocate ALL"))
                 (t
                  (remhash name (connection-meta database))
                  (handler-case
                      (query (format nil "deallocate ~:@(~S~)" name))
                    (cl-postgres-error:invalid-sql-statement-name ()
                      (format nil "Statement does not exist in either postmodern or postgresql or may be in process of creation ~a~%" name)))
                  (when (and remove-function (find-symbol (string-upcase name)))
                    (fmakunbound (find-symbol (string-upcase name)))))))
          ((eq location :postmodern)
           (if (equal name "ALL")
               (progn
                 (maphash #'(lambda (x y)
                             (declare (ignore y))
                             (remhash x (connection-meta database))
                             (when (and remove-function
                                        (find-symbol (string-upcase x)))
                               (fmakunbound (find-symbol (string-upcase x)))))
                         (connection-meta database)))
               (progn
                 (remhash (string-upcase name)
                          (connection-meta database))
                 (when (and remove-function (find-symbol (string-upcase name)))
                   (fmakunbound (find-symbol (string-upcase name)))))))
          ((eq location :postgresql)
           (cond ((equal name "ALL")
                  (query "deallocate ALL"))
                 (t
                  (handler-case
                        (query (format nil "deallocate ~:@(~S~)" name))
                      (cl-postgres-error:invalid-sql-statement-name ()
                        (format nil "Statement does not exist in postgresql or is being created ~a~%"
                                name)))))))))

(defun reset-prepared-statement (condition &optional params)
  "If you have received an invalid-prepared-statement error or a
prepared-statement already exists error but the prepared statement is still in
the meta slot in the postmodern connection, this will try to regenerate the
prepared statement at the database connection level and restart the connection."
  (let* ((name (pomo:database-error-extract-name condition))
         (statement (find-postmodern-prepared-statement name))
         (pid (write-to-string (first (cl-postgres::connection-pid *database*)))))
    (setf (cl-postgres::connection-available *database*) t)
    (when statement
      (cl-postgres::with-reconnect-restart *database*
        (terminate-backend pid))
      (cl-postgres:prepare-query *database* name (first statement)
                                 (if params
                                     params
                                     (second statement)))
      (invoke-restart 'reset-prepared-statement))))

(defun get-pid ()
  "Get the process id used by postgresql for this connection."
  (query "select pg_backend_pid()" :single))

(defun get-pid-from-postmodern ()
  "Get the process id used by postgresql for this connection,
but get it from the postmodern connection parameters."
  (gethash "pid" (pomo::connection-parameters *database*)))

(defun cancel-backend (pid &optional (database *database*))
  "Polite way of terminating a query at the database (as opposed to calling
close-database). Slower than (terminate-backend pid) and does not always work."
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
