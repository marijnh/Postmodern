;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

(defvar *connection-pools* (make-hash-table :test 'equal)
  "Maps pool specifiers to lists of pooled connections.")

(defparameter *database* nil
  "Special holding the current database. Most functions and macros operating
on a database assume this contains a connected database.")

(defclass pooled-database-connection (database-connection)
  ((pool-type :initarg :pool-type :accessor connection-pool-type))
  (:documentation "Type for database connections that are pooled.
Stores the arguments used to create it, so different pools can be
distinguished."))

(defun connect (database-name user-name password host &key (port 5432) pooled-p
                                                        (use-ssl *default-use-ssl*)
                                                        (use-binary nil)
                                                        (service "postgres")
                                                        (application-name ""))
  "Create a new database connection for the given user and the database. Port
will default to 5432, which is where most PostgreSQL servers are running. If
pooled-p is T, a connection will be taken from a pool of connections of this
type, if one is available there, and when the connection is disconnected it
will be put back into this pool instead. use-ssl can be :no, :yes, or :try,
as in open-database, and defaults to the value of *default-use-ssl*."
  (cond (pooled-p
         (let ((type (list database-name user-name password host port use-ssl
                           application-name use-binary)))
           (or (get-from-pool type)
               (let ((connection (cl-postgres:open-database database-name user-name
                                                            password host port
                                                            use-ssl
                                                            service application-name
                                                            use-binary)))
                 #-genera (change-class connection 'pooled-database-connection
                                        :pool-type type)
                 #+genera (progn
                            (change-class connection 'pooled-database-connection)
                            (setf (slot-value connection 'pool-type) type))
                 connection))))
        (t (cl-postgres:open-database database-name user-name password host port
                                      use-ssl service application-name
                                      use-binary))))

(defun connected-p (database)
  "Returns a boolean indicating whether the given connection is still connected
to the server."
  (cl-postgres:database-open-p database))

(defun connect-toplevel (database-name user-name password host
                         &key (port 5432) (use-ssl *default-use-ssl*) (application-name "")
                           use-binary)
  "Bind the *database* to a new connection. Use this if you only need one
connection, or if you want a connection for debugging from the REPL."
  (when (and *database* (connected-p *database*))
    (restart-case (error "Top-level database already connected.")
      (replace () :report "Replace it with a new connection."
        (disconnect-toplevel))
      (leave () :report "Leave it." (return-from connect-toplevel nil))))
  (setf *database* (connect database-name user-name password host
                            :port port :use-ssl use-ssl :application-name application-name
                            :use-binary use-binary))
  (values))

(defgeneric disconnect (database)
  (:method ((connection database-connection))
    (close-database connection))
  (:documentation "Disconnects a normal database connection, or moves a pooled
connection into the pool."))

(defgeneric reconnect (database)
  (:method ((database database-connection))
    (reopen-database database)
    (when *schema-path*
      (let ((path *schema-path*))
        (set-search-path path))))
  (:method ((connection pooled-database-connection))
    (error "Can not reconnect a pooled database."))
  (:documentation "Reconnect a disconnected database connection. This is not
allowed for pooled connections ― after they are disconnected they might be in
use by some other process, and should no longer be used."))

(defun disconnect-toplevel ()
  "Disconnect *database*."
  (when (and *database* (connected-p *database*))
    (disconnect *database*))
  (setf *database* nil))

(defun call-with-connection (spec thunk)
  "The functional backend to with-connection. Binds *database* to a new connection
as specified by spec, which should be a list that connect can be applied to, and
runs the zero-argument function given as second argument in the new environment.
When the function returns or throws, the new connection is disconnected."
  (let ((*database* (apply #'connect spec)))
    (unwind-protect (funcall thunk)
      (disconnect *database*))))

(defmacro with-connection (spec &body body)
  "Evaluates the body with *database* bound to a connection as specified by spec,
which should be list that connect can be applied to."
  `(let ((*database* (apply #'connect ,spec)))
     (unwind-protect (progn ,@body)
       (disconnect *database*))))

#+postmodern-thread-safe
(defvar *pool-lock*
  (bordeaux-threads:make-lock "connection-pool-lock")
  "A lock to prevent multiple threads from messing with the connection
pool at the same time.")

(defmacro with-pool-lock (&body body)
  "Aquire a lock for the pool when evaluating body \(if thread support
is present)."
  #+postmodern-thread-safe
  `(bordeaux-threads:with-lock-held (*pool-lock*) ,@body)
  #-postmodern-thread-safe
  `(progn ,@body))

(defun get-from-pool (type)
  "Get a database connection from the specified pool, returns nil if
no connection was available."
  (with-pool-lock
    (pop (gethash type *connection-pools*))))

(defmethod disconnect ((connection pooled-database-connection))
  "Add the connection to the corresponding pool, or drop it when the
pool is full."
  (macrolet ((the-pool ()
               '(gethash (connection-pool-type connection) *connection-pools* ())))
    (when (database-open-p connection)
      (with-pool-lock
        (if (or (not *max-pool-size*) (< (length (the-pool)) *max-pool-size*))
            (push connection (the-pool))
            (call-next-method))))
    (values)))

(defun clear-connection-pool ()
  "Disconnect and remove all connections in the connection pools."
  (with-pool-lock
    (maphash
     (lambda (type connections)
       (declare (ignore type))
       (dolist (conn connections)
         (close-database conn)))
     *connection-pools*)
    (setf *connection-pools* (make-hash-table :test 'equal))
    (values)))
