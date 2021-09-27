;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

(defgeneric connection-port (cl)
  (:method ((cl t)) nil))

(defgeneric connection-db (cl)
  (:method ((cl t)) nil))

(defgeneric connection-parameters (obj)
  (:documentation "This method returns a mapping (string to string) containing
all the configuration parameters for the connection."))

(defclass database-connection ()
  ((host :initarg :host :reader connection-host)
   (port :initarg :port :reader connection-port)
   (database :initarg :db :reader connection-db)
   (user :initarg :user :reader connection-user)
   (password :initarg :password :reader connection-password)
   (use-ssl :initarg :ssl :reader connection-use-ssl)
   (use-binary :initarg :binary :accessor connection-use-binary :initform nil)
   (service :initarg :service :accessor connection-service)
   (application-name :initarg :application-name :accessor connection-application-name)
   (socket :initarg :socket :accessor connection-socket)
   (meta :initform nil)
   (available :initform t :accessor connection-available)
   (parameters :accessor connection-parameters)
   (timestamp-format :accessor connection-timestamp-format))
  (:default-initargs :application-name "postmodern-default")
  (:documentation "Representation of a database connection. Contains
login information in order to be able to automatically re-establish a
connection when it is somehow closed."))

(defun get-postgresql-version (connection)
  "Retrieves the version number of the connected postgresql database as a
string. Some installations of Postgresql add additional information after the base
version number, so hopefully this gets rid of the unwanted info."
  (first
   (split-sequence:split-sequence #\space
                                  (gethash "server_version"
                                           (connection-parameters connection)))))

(defun postgresql-version-at-least (desired-version connection)
  "Takes a postgresql version number which should be a string with the major and
minor versions separated by a period e.g. '12.2' or '9.6.17'. Checks against the
connection understanding of the running postgresql version and returns t if the
running version is the requested version or newer."
  (when (numberp desired-version)
    (setf desired-version (write-to-string desired-version)))
  (flet ((validate-input (str)
           (unless (or (not (stringp desired-version))
                       (= 0 (length str)))
             (every (lambda (char)
                      (or (digit-char-p char)
                          (eq char #\.)))
                    str)))
         (string-version-to-integer-list (string-version)
           (mapcar #'parse-integer
                   (split-sequence:split-sequence #\.
                                                  string-version
                                                  :remove-empty-subseqs t)))
         (convert-to-integer (split-versions)
           (apply #'+
                  (loop for x in split-versions counting x into y collect
                                                                  (/ (* x 10000)
                                                                     (expt 100 y))))))
    (when (validate-input desired-version)
      (let ((current-version (string-version-to-integer-list
                              (get-postgresql-version connection))))
        (setf desired-version (string-version-to-integer-list desired-version))
        (when (>= (convert-to-integer current-version)
                  (convert-to-integer desired-version))
          t)))))

(defun connection-meta (connection)
  "This method provides access to a hash table that is associated with the
current database connection, and is used to store information about the
prepared statements that have been parsed for this connection."
  (or (slot-value connection 'meta)
      (let ((meta-data (make-hash-table :test 'equal)))
        (setf (slot-value connection 'meta) meta-data)
        meta-data)))

(defun connection-pid (connection)
  "Retrieves a list consisting of the pid and the secret-key from the
connection, not from the database itself. These are needed for cancelling
connections and error processing with respect to prepared statements."
  (list (gethash "pid" (slot-value connection 'parameters))
        (gethash "secret-key" (slot-value connection 'parameters))))

(defun use-binary-parameters (db-connection param)
  "Accepts a database connection and nil or t. The default for cl-postgres/Postmodern
is pass parameters to Postgresql as text (not in binary format). This is how it has
been since the beginning of Postmodern and the default is set this way in order to
avoid breaking existing user code. You can set Postmodern to pass integer, float
or boolean parameters to Postgresql in binary format on a connection basis when
the connection is created or you can use this function to change the existing connection
to use or not use binary parameter passing."
  (setf (connection-use-binary db-connection) param))

(defun database-open-p (connection)
  "Returns a boolean indicating whether the given connection is currently
connected."
  (and (connection-socket connection)
       (open-stream-p (connection-socket connection))))

(defun open-database (database user password host
                      &optional (port 5432) (use-ssl :no)
                        (service "postgres") (application-name "postmodern-default")
                        (use-binary nil))
  "Create and open a connection for the specified server, database, and user.
use-ssl may be :no, :try, :yes, or :full; where :try means 'if the server
supports it'. :require uses provided ssl certificate with no verification.
:yes only verifies that the server cert is issued by a trusted CA,
but does not verify the server hostname. :full 'means expect a CA-signed cert
for the supplied host name' and verify the server hostname. When it is anything
but :no, you must have the CL+SSL package loaded to initiate the connection.

On SBCL and Clozure CL, the value :unix may be passed for host, in order to
connect using a Unix domain socket instead of a TCP socket."
  (check-type database string)
  (check-type user string)
  (check-type password (or null string))
  (check-type host (or string (eql :unix)) "a string or :unix")
  (check-type port (integer 1 65535) "an integer from 1 to 65535")
  (check-type use-ssl (member :no :try :require :yes :full) ":no, :try, :require, :yes or :full")
  (let ((conn (make-instance 'database-connection :host host :port port
                                                  :user user :password password
                                                  :socket nil :db database
                                                  :ssl use-ssl
                                                  :binary use-binary
                                                  :service service
                                                  :application-name application-name))
        (connection-attempts 0))
    (initiate-connection conn connection-attempts)
    conn))

#+(and (or cl-postgres.features:sbcl-available ccl allegro) unix)
(progn
  (defparameter *unix-socket-dir*
    #-(or freebsd darwin) "/var/run/postgresql/"
    #+(or darwin freebsd) "/tmp/"
    "Directory where the Unix domain socket for PostgreSQL be found.")

  (defun unix-socket-path (base-dir port)
    (unless (char= #\/ (aref base-dir (1- (length base-dir))))
      (setf base-dir (concatenate 'string base-dir "/")))
    (format nil "~a.s.PGSQL.~a" base-dir port))

  #+cl-postgres.features:sbcl-available
  (defun unix-socket-connect (path)
    (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
      (sb-bsd-sockets:socket-connect sock path)
      (sb-bsd-sockets:socket-make-stream
       sock :input t :output t :element-type '(unsigned-byte 8))))

  #+ccl (setf ccl:*default-socket-character-encoding* :utf-8)
  #+ccl
  (defun unix-socket-connect (path)
    (ccl:make-socket :type :stream
                     :address-family :file
                     :format :binary
                     :remote-filename path))

  #+allegro
  (defun unix-socket-connect (path)
    (socket:make-socket :type :stream
                        :address-family :file
                        :format :binary
                        :remote-filename path)))

#+cl-postgres.features:sbcl-available
(defun get-host-address (host)
  "Returns valid IPv4 or IPv6 address for the host."
  ;; get all IPv4 and IPv6 addresses as a list
  (let* ((host-ents (multiple-value-list (sb-bsd-sockets:get-host-by-name host)))
         ;; remove protocols for which we don't have an address
         (addresses (remove-if-not #'sb-bsd-sockets:host-ent-address host-ents)))
    ;; Return the first one or nil,
    ;; but actually, it shouln't return nil, because
    ;; get-host-by-name should signal NAME-SERVICE-ERROR condition
    ;; if there isn't any address for the host.
    (first addresses)))


#+cl-postgres.features:sbcl-available
(defun inet-socket-connect (host port)
  (let* ((host-ent (get-host-address host))
         (sock (make-instance
                #+cl-postgres.features:sbcl-ipv6-available
                (ecase (sb-bsd-sockets:host-ent-address-type host-ent)
                  (2  'sb-bsd-sockets:inet-socket)
                  (10 'sb-bsd-sockets:inet6-socket))

                #-cl-postgres.features:sbcl-ipv6-available
                'sb-bsd-sockets:inet-socket

                :type :stream :protocol :tcp))
         (address (sb-bsd-sockets:host-ent-address host-ent)))
    (sb-bsd-sockets:socket-connect sock address port)
    (sb-bsd-sockets:socket-make-stream
     sock :input t :output t :buffering :full :element-type '(unsigned-byte 8))))

#+ccl
(defun inet-socket-connect (host port)
  (when (and (stringp host)
             (string= host "localhost"))
    (setf host "127.0.0.1")) ;this corrects a strange ccl error we are seeing in certain scram authentication situations
  (ccl:make-socket :format :binary
                   :remote-host host
                   :remote-port port))

#+allegro
(defun inet-socket-connect (host port)
  (socket:make-socket :remote-host host
                      :remote-port port
                      :format :binary
                      :type :stream))

(defun initiate-connection (conn &optional (connection-attempts 0))
  "Check whether a connection object is connected, try to connect it
if it isn't."
  (flet ((add-restart (err)
           (restart-case (error (wrap-socket-error err))
             (:reconnect () :report "Try again."
               (progn (incf connection-attempts)
                      (initiate-connection conn connection-attempts)))))
         (assert-unix ()
           #+unix t
           #-unix (error "Unix sockets only available on Unix (really)")))
    (handler-case
        (let ((socket #-(or allegro cl-postgres.features:sbcl-available ccl)
                      (usocket:socket-stream
                       (usocket:socket-connect (connection-host conn)
                                               (connection-port conn)
                                               :element-type '(unsigned-byte 8)))
                      #+(or allegro cl-postgres.features:sbcl-available ccl)
                      (cond
                        ((equal (connection-host conn) :unix)
                         (assert-unix)
                         (unix-socket-connect (unix-socket-path *unix-socket-dir*
                                                                (connection-port conn))))
                        ((and (stringp (connection-host conn))
                              (char= #\/ (aref (connection-host conn) 0)))
                         (assert-unix)
                         (unix-socket-connect (unix-socket-path (connection-host conn)
                                                                (connection-port conn))))
                        ((and (pathnamep (connection-host conn))
                              (eql :absolute (pathname-directory (connection-host conn))))
                         (assert-unix)
                         (unix-socket-connect (unix-socket-path (namestring (connection-host conn))
                                                                (connection-port conn))))
                        (t
                         (inet-socket-connect (connection-host conn)
                                              (connection-port conn)))))
              (finished nil)
              (*connection-params* (make-hash-table :test 'equal)))
          (setf (connection-parameters conn) *connection-params*)
          (unwind-protect
               (setf socket (handler-case
                                (authenticate socket conn)
                              (cl-postgres-error:protocol-violation (err)
                                (setf finished t)
                                (ensure-socket-is-closed socket)
                                ;; If we settled on a single logging library, I
                                ;; would suggest logging this kind of situation
                                ;; with at least the following data
                                ;; (database-error-message err)
                                ;; (database-error-detail err)
                                (incf connection-attempts)
                                (when (< connection-attempts
                                         *retry-connect-times*)
                                  (initiate-connection conn
                                                       connection-attempts))))
                     (connection-timestamp-format conn)
                     (if (string= (gethash "integer_datetimes"
                                           (connection-parameters conn)) "on")
                         :integer :float)
                     (connection-socket conn) socket
                     finished t)
            (unless finished
              (ensure-socket-is-closed socket)))
          (maphash (lambda (id query-param-list)
                     (prepare-query conn id
                                    (first query-param-list)
                                    (second query-param-list)))
                   (connection-meta conn)))
      #-(or allegro cl-postgres.features:sbcl-available ccl)
      (usocket:socket-error (e) (add-restart e))
      #+ccl (ccl:socket-error (e) (add-restart e))
      #+allegro(excl:socket-error (e) (add-restart e))
      #+cl-postgres.features:sbcl-available(sb-bsd-sockets:socket-error (e) (add-restart e))
      #+cl-postgres.features:sbcl-available(sb-bsd-sockets:name-service-error (e) (add-restart e))
      (stream-error (e) (add-restart e))))
  (values))

(defun reopen-database (conn &optional (connection-attempts 0))
  "Re-establish a database connection for a previously closed connection object.
(Calling this on a connection that is still open is harmless.)"
  (loop :while (not (database-open-p conn))
        :repeat *retry-connect-times*
        :do
           (initiate-connection conn connection-attempts)))

(defun ensure-connection (conn &optional (connection-attempts 0))
  "Used to make sure a connection object is connected before doing anything
with it."
  (unless conn
    (error "No database connection selected."))
  (unless (database-open-p conn)
    (restart-case (error 'database-connection-lost
                         :message "Connection to database server lost.")
      (:reconnect () :report "Try to reconnect."
        (loop :while (not (database-open-p conn))
              :repeat *retry-connect-times*
              :do
                 (initiate-connection conn connection-attempts))))))

(defun close-database (connection)
  "Close a database connection. It is advisable to call this on connections when
you are done with them. Otherwise the open socket will stick around until it is
garbage collected, and no one will tell the database server that we are done
with it."
  (when (database-open-p connection)
    (terminate-connection (connection-socket connection)))
  (values))

(defmacro using-connection (connection &body body)
  "This is used to prevent a row-reader from recursively calling some
query function. Because the connection is still returning results from
the previous query when a row-reading is being executed, starting
another query will not work as expected \(or at all, in general). This
might also raise an error when you are using a single database
connection from multiple threads, but you should not do that at all.
Also binds *timestamp-format* and *connection-params*, which might be
needed by the code interpreting the query results."
  (let ((connection-name (gensym)))
    `(let* ((,connection-name ,connection)
            (*timestamp-format* (connection-timestamp-format ,connection-name))
            (*connection-params* (connection-parameters ,connection-name)))
       (when (not (connection-available ,connection-name))
         (error 'database-error
                :message "This connection is still processing another query."))
       (setf (connection-available ,connection-name) nil)
       (unwind-protect (progn ,@body)
         (setf (connection-available ,connection-name) t)))))

(defmacro with-reconnect-restart (connection &body body)
  "When, inside the body, an error occurs that breaks the connection
socket, a condition of type database-connection-error is raised,
offering a :reconnect restart."
  (let ((connection-name (gensym))
        (body-name (gensym))
        (retry-name (gensym)))
    `(let ((,connection-name ,connection))
       (ensure-connection ,connection-name)
       (labels ((,body-name ()
                  (handler-case (progn ,@body)
                    (stream-error (e)
                      (cond ((eq (connection-socket ,connection-name)
                                 (stream-error-stream e))
                             (ensure-socket-is-closed (connection-socket
                                                       ,connection-name)
                                                      :abort t)
                             (,retry-name (wrap-socket-error e)))
                            (t (error e))))
                    (cl-postgres-error:server-shutdown (e)
                      (ensure-socket-is-closed (connection-socket
                                                ,connection-name)
                                               :abort t)
                      (,retry-name e))))
                (,retry-name (err)
                  (restart-case (error err)
                    (:reconnect () :report "Try to reconnect"
                      (reopen-database ,connection-name)
                      (,body-name)))))
         (,body-name)))))

(defun wait-for-notification (connection)
  "This function blocks until asynchronous notification is received on the
connection. Return the channel string, the payload and notifying pid as
multiple values. The PostgreSQL LISTEN command must be used to enable listening
for notifications."
  (block nil
    (with-reconnect-restart connection
      (handler-bind ((postgresql-notification
                       (lambda (c)
                         (return (values (postgresql-notification-channel c)
                                         (postgresql-notification-payload c)
                                         (postgresql-notification-pid c))))))
        (message-case (connection-socket connection))))))

(defun exec-query (connection query &optional (row-reader 'ignore-row-reader))
  "Sends the given query to the given connection, and interprets the results
 (if there are any) with the given row-reader. If the database returns
information about the amount of rows affected, this is returned as a second
value."
  (check-type query string)
  (with-reconnect-restart connection
    (using-connection connection
      (send-query (connection-socket connection) query row-reader))))

(defun prepare-query (connection name query &optional parameters)
  "Parse and plan the given query, and store it with Postgresql under the given name.
Note that prepared statements are per-connection, so they can only be executed
through the same connection that prepared them. Also note that while the Postmodern package
will also stored the prepared query in the connection-meta slot of the connection, but
cl-postgres prepare-query does not. If the name is an empty string, Postgresql will not
store it as a reusable query. To make this useful in cl-postgres while
(connection-use-binary connection) is true, you need to pass a list of parameters with
the same type as you will be using when you call (exec-prepared).

For example:

    (prepare-query connection \"test6\" \"select $1, $2\" '(1 T))
    (exec-prepared connection \"test6\" '(42 nil) 'list-row-reader)"

  (check-type query string)
  (check-type name string)
  (with-reconnect-restart connection
    (using-connection connection
                      (send-parse (connection-socket connection) name query parameters
                                  (connection-use-binary connection))
      (values))))

(defun unprepare-query (connection name)
  "Close the prepared query given by name by closing the session connection.
Does not remove the query from the meta slot in connection. This is not the same as
keeping the connection open and sending Postgresql query to deallocate the named
prepared query."
  (check-type name string)
  (with-reconnect-restart connection
    (using-connection connection
                      (send-close (connection-socket connection) name)
                      (values))))

(defun find-postgresql-prepared-query (connection name)
  "Returns a list of (name, query, parameters) for a named prepared query.
Note the somewhat similar Postmodern version (find-postgresql-prepared-statement name) only
returns the query, not the parameters or name."
  (let* ((prepared-queries
          (exec-query connection
                      "select name, statement, parameter_types from pg_prepared_statements"
                      'list-row-reader))
         (query (find name prepared-queries :key 'first :test 'equal))
         (len (if (and (stringp (third query)))
                  (length (third query))
                  0)))
    (when query (setf (third query) (subseq (third query) 1 (decf len))))
    query))

(defun exec-prepared (connection name parameters
                      &optional (row-reader 'ignore-row-reader))
  "Execute the prepared statement by the given name. Parameters should be given
as a list. Each value in this list should be of a type that to-sql-string has
been specialised on. (Byte arrays will be passed in their binary form, without
being put through to-sql-string.) The result of the executing the statement, if
any, is interpreted by the given row reader, and returned. Again, the number or
affected rows is optionally returned as a second value.
row-reader to the result."
  (check-type name string)
  (check-type parameters list)
  (with-reconnect-restart connection
    (using-connection connection
      (handler-case
          (send-execute (connection-socket connection)
                        name parameters row-reader (connection-use-binary connection))
        (cl-postgres-error::invalid-byte-sequence (error)
          (error "~A ~%Did you specify the types of parameters to be passed when
you created the prepared statement? This error typically happens in this context
when you are passing parameters to a prepared statement in a binary format but
Postgresql is expecting the parameters to be in text format." error))))))

;; A row-reader that returns a list of (field-name . field-value)
;; alist for the returned rows.
(def-row-reader alist-row-reader (fields)
  (loop :while (next-row)
        :collect (loop :for field :across fields
                       :collect (cons (field-name field)
                                      (next-field field)))))

;; Row-reader that returns a list of lists.
(def-row-reader list-row-reader (fields)
  (loop :while (next-row)
        :collect (loop :for field :across fields
                       :collect (next-field field))))

;; Row-reader that returns a vector of vectors.
(def-row-reader vector-row-reader (fields)
  (let ((rows (make-array 1 :adjustable t :fill-pointer 0)))
    (loop :for row = (make-array (length fields))
          :while (next-row)
          :do (progn
                (loop :for field :across fields
                      :for idx :upfrom 0
                      :do (setf (aref row idx) (next-field field)))
                (vector-push-extend row rows)))
    rows))

;; Row-reader that discards the query results.
(def-row-reader ignore-row-reader (fields)
  (loop :while (next-row)
        :do (loop :for field :across fields
                  :do
                     (next-field field)))
  (values))
