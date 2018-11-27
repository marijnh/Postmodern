(in-package :postmodern)

(defun ensure-prepared (connection id query)
  "Make sure a statement has been prepared for this connection."
  (let ((meta (connection-meta connection)))
    (unless (gethash id meta)
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
                          (ensure-prepared *database* statement-id query)
                          (,result-form ,base))))))

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
  "Returns t if the prepared statement exists in the current postgresql session."
  (query (:select 'name
                  :from 'pg-prepared-statements
                  :where (:= 'name (string-upcase statement-name)))))

(defun list-prepared-statements ()
  "Syntactic sugar. A query that lists the prepared statements
in the session in which the function is run."
  (query (:select '* :from 'pg-prepared-statements) :alists))

(defun drop-prepared-statement (statement-name &optional (database *database*))
  "If you know the prepared statement name, this will call
postgresql to deallocate the prepared statement"
  (check-type statement-name string)
  (when (gethash (string-upcase statement-name) (connection-meta database))
    (remhash (string-upcase statement-name) (connection-meta database))
    (query (format nil "deallocate \"~a\"" (string-upcase statement-name)))))

(defun list-postmodern-prepared-statements ()
  "List the prepared statements that postmodern has put in the meta slot in
the connection. It will return a list of alists of form:
  ((:NAME . \"SNY24\")
  (:STATEMENT . \"(SELECT name, salary FROM employee WHERE (city = $1))\")
  (:PREPARE-TIME . #<TIMESTAMP 25-11-2018T15:36:43,385>)
  (:PARAMETER-TYPES . \"{text}\") (:FROM-SQL)"
  (alexandria:hash-table-alist (postmodern::connection-meta *database*)))

(defun find-postmodern-prepared-statement (name)
  "Returns the named prepared statement (if any) that postmodern has put in
the meta slot in the connection."
  (gethash name (postmodern::connection-meta *database*)))

(defun get-pid ()
  "Get the process id used by postgresql for this connection"
  (query "select pg_backend_pid()" :single))

(defun cancel-backend (pid)
  "Polite way of terminating a query at the database (as opposed to calling close-database).
Slower than (terminate-backend pid)"
  (query "select pg_cancel_backend($1);" pid))

(defun terminate-backend (pid)
  "Less polite way of terminating at the database (as opposed to calling close-database).
Faster than (cancel-backend pid)"
  (query "select pg_terminate_backend($1);" pid))
