(in-package :postmodern)

(defun ensure-prepared (connection id query)
  "Make sure a statement has been prepared for this connection.
If there is a query by the same name in the connection meta slot
and that query is different than the query parameter, the query
in the connection meta slot will be replaced by the new query."
  (let ((meta (connection-meta connection)))
    (unless (and (gethash id meta) (equal (gethash id meta) query))
      (setf (gethash id meta) query)
      (prepare-query connection id query))))

(let ((next-id 0))
  (defun next-statement-id ()
    "Provide unique statement names."
    (incf next-id)
    (with-standard-io-syntax (format nil "STATEMENT_~A" next-id))))

(defun generate-prepared (function-form name query format)
  "Helper function for the following two macros."
  (destructuring-bind (reader result-form) (reader-for-format format)
    (let ((base `(exec-prepared *database* statement-id params ,reader)))
      `(let ((statement-id ,(string name))
             (query ,(real-query query)))
         (,@function-form (&rest params)
            (handler-bind ((cl-postgres-error:admin-shutdown
                            (lambda (msg)
                              (declare (ignore msg))
                  (invoke-restart :reconnect))))
               (cl-postgres::with-reconnect-restart *database*
                  (handler-bind
                      ((cl-postgres-error:invalid-sql-statement-name #'pomo:reset-prepared-statement))
                    (cl-postgres::with-reconnect-restart *database*
                  (handler-bind
                     ((cl-postgres-error:duplicate-prepared-statement #'pomo:overwrite-prepared-statement))
                     (ensure-prepared *database* statement-id query)
                     (,result-form ,base)))))))))))
#|
(defun generate-prepared (function-form name query format)
  "Helper function for the following two macros."
  (destructuring-bind (reader result-form) (reader-for-format format)
    (let ((base `(exec-prepared *database* statement-id params ,reader)))
      `(let ((statement-id ,(string name))
             (query ,(real-query query)))
         (,@function-form (&rest params)
                          (ensure-prepared *database* statement-id query)
                          (,result-form ,base))))))
|#
(defmacro prepare (query &optional (format :rows))
  "Wraps a query into a function that will prepare it once for a
connection, and then execute it with the given parameters. The query
should contain a placeholder \($1, $2, etc) for every parameter."
  (generate-prepared '(lambda) (next-statement-id) query format))

(defmacro defprepared (name query &optional (format :rows))
  "Like prepare, but gives the function a name instead of returning
it. The name should not be quoted or a string."
  (generate-prepared `(defun ,name) name query format))

(defun prepared-statement-exists-p (statement-name)
  "Returns t if the prepared statement exists in the current postgresql
session, otherwise nil."
  (if (query (:select 'name
                   :from 'pg-prepared-statements
                   :where (:= 'name (string-upcase statement-name)))
             :single)
      t
      nil))

(defun list-prepared-statements ()
  "Syntactic sugar. A query that lists the prepared statements
in the session in which the function is run."
  (query "select * from pg_prepared_statements" :alists))

(defun drop-prepared-statement (statement-name &key (location :both) (database *database*))
  "Prepared statements are stored both in the meta slot in the postmodern
connection and in postgresql session information. If you know the prepared
statement name, you can delete the prepared statement from both locations (the
default behavior), just from postmodern (passing :postmodern to the location
key parameter) or just from postgresql (passing :postgresql to the location
key parameter). If you pass the name 'All' as the statement name, it will
delete all prepared statements."
  (check-type statement-name string)
  (check-type location keyword)
  (setf statement-name (string-upcase statement-name))
  (cond ((eq location :both)
         (when (or (equal statement-name "ALL")
                   (prepared-statement-exists-p statement-name))
           (if (equal statement-name "ALL")
               (progn
                 (clrhash (connection-meta database))
                 (query "deallocate ALL"))
               (progn
                 (remhash statement-name (connection-meta database))
                 (query (format nil "deallocate ~:@(~S~)" statement-name))))))
        ((eq location :postmodern)
         (if (equal statement-name "ALL")
             (clrhash (connection-meta database))
             (remhash (string-upcase statement-name) (connection-meta database))))
        ((eq location :postgresql)
         (cond ((equal statement-name "ALL")
                (query "deallocate ALL"))
               ((prepared-statement-exists-p statement-name)
                (query (format nil "deallocate ~:@(~S~)" statement-name)))
               (t nil)))))

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
  "If you have received an invalid-prepared-statement error but the prepared
statement is still in the meta slot in the postmodern connection,
try to regenerate the prepared statement at the database connection level
and restart the connection."
  (let* ((name (pomo:database-error-extract-name condition))
         (statement (find-postmodern-prepared-statement name))
         (pid (write-to-string (first (cl-postgres::connection-pid *database*)))))
    (setf (cl-postgres::connection-available *database*) t)
    (cl-postgres::with-reconnect-restart *database*
      (terminate-backend pid))
    (cl-postgres:prepare-query *database* name statement)
    (invoke-restart 'reset-prepared-statement)))

(defun overwrite-prepared-statement (condition)
  "If you have received an invalid-prepared-statement error but the prepared
statement is still in the meta slot in the postmodern connection,
try to regenerate the prepared statement at the database connection level
and restart the connection."
  (let* ((name (pomo:database-error-extract-name condition))
         (statement (find-postmodern-prepared-statement name))
         (pid (write-to-string (first (cl-postgres::connection-pid *database*)))))
    (setf (cl-postgres::connection-available *database*) t)
    (cl-postgres::with-reconnect-restart *database*
      (terminate-backend pid))
    (cl-postgres:prepare-query *database* name statement)
    (invoke-restart 'overwrite-prepared-statement)))

(defun get-pid ()
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
        (host (cl-postgres::connection-host database)))
    (with-connection `(,database-name ,user ,password ,host)
      (query "select pg_cancel_backend($1);" pid))))

(defun terminate-backend (pid &optional (database *database*))
  "Less polite way of terminating at the database (as opposed to calling close-database).
Faster than (cancel-backend pid) and more reliable."
  (let ((database-name (cl-postgres::connection-db database))
        (user (cl-postgres::connection-user database))
        (password (cl-postgres::connection-password database))
        (host (cl-postgres::connection-host database)))
    (with-connection `(,database-name ,user ,password ,host)
      (query "select pg_terminate_backend($1);" pid))))
