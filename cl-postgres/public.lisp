(in-package :cl-postgres)

(defclass database-connection ()
  ((host :initarg :host :reader connection-host)
   (port :initarg :port :reader connection-port)
   (database :initarg :db :reader connection-db)
   (user :initarg :user :reader connection-user)
   (password :initarg :password :reader connection-password)
   (use-ssl :initarg :ssl :reader connection-use-ssl)
   (socket :initarg :socket :accessor connection-socket)
   (meta :initform nil)
   (available :initform t :accessor connection-available)
   (parameters :accessor connection-parameters)
   (timestamp-format :accessor connection-timestamp-format))
  (:documentation "Representatino of a database connection. Contains
login information in order to be able to automatically re-establish a
connection when it is somehow closed."))

(defun connection-meta (connection)
  "Retrieves the meta field of a connection, the primary purpose of
which is to store information about the prepared statements that
exists for it."
  (or (slot-value connection 'meta)
      (let ((meta-data (make-hash-table)))
        (setf (slot-value connection 'meta) meta-data)
        meta-data)))

(defun database-open-p (connection)
  "Returns a boolean indicating whether the given connection is
currently connected."
  (and (connection-socket connection)
       (open-stream-p (connection-socket connection))))

(defun parse-connection-string (connection-string &key (kind :guess))
  "Attempts to parse CONNECTION-STRING as per the libpq connection
string spec; Only extracts those options that cl-postgres can deal
with (so only things specific to the underlynig protocol, not things
like keepalives_count which affect how libpq implements the protocol).

Returns a list that can be applied to open-database. In other words:

  database user password host port use-ssl

use-ssl will be either :no :yes :try, user, password and port may be nil.

If KIND is :GUESS the function will look at the first characters of
CONNECTION-STRING and try to guess if it should be parsed as a
key/value string or a URI. KIND can be :URI or :KEY-VALUE to force one
specific interpretation of the connection-string.

See also: http://www.postgresql.org/docs/9.2/static/libpq-connect.html#LIBPQ-CONNSTRING"
  (ecase kind
    (:uri 
     (parse-connection-string/uri connection-string))
    (:key-value
     (parse-connection-string/key-value connection-string))
    (:guess
     (if (or (alexandria:starts-with-subseq "postgres:" connection-string)
             (alexandria:starts-with-subseq "postgresql:" connection-string))
         (parse-connection-string connection-string :kind :uri)
         (parse-connection-string connection-string :kind :key-value)))))

(defun parse-connection-string/uri (uri)
  (let ((start 0))
    (labels ((looking-at (regexp)
               (multiple-value-bind (match groups)
                   (cl-ppcre:scan-to-strings (cl-ppcre:create-scanner `(:sequence :start-anchor ,(cl-ppcre:parse-string regexp)))
                                             uri :start start)
                 (when match
                   (incf start (length match))
                   (coerce groups 'list))))
             (schema ()
               (first (looking-at "(postgres|postgresql):")))
             (auth ()
               (looking-at "//(?:([^/:]+)(?::([^/]+))?@)?"))
             (endpoint ()
               (looking-at "([^/]*)(?::(\d+))?/"))
             (dbname ()
               (first (looking-at "([^?]*)")))
             (query-params ()
               (looking-at "\\?(.*)")))
      (let* ((schema (schema))
             (auth (auth))
             (username (if (first auth)
                           (do-urlencode:urldecode (first auth))
                           nil))
             (password (if (second auth)
                           (do-urlencode:urldecode (second auth))
                           nil))
             (endpoint (endpoint))
             (host (do-urlencode:urldecode (first endpoint)))
             (port (if (second endpoint)
                       (parse-integer (do-urlencode:urldecode (second endpoint)))
                       nil))
             (dbname (do-urlencode:urldecode (dbname)))
             (query-params (first (query-params))))
        (assert (member schema '("postgres" "postgresql") :test #'string=)
                (uri)
                "Unknown schema ~S in ~S." schema uri)
        (loop
           for name=value in (cl-ppcre:split "&" query-params :sharedp t)
           for (%name %value) = (cl-ppcre:split "=" name=value :limit 2)
           for name = (do-urlencode:urldecode %name)
           for value = (do-urlencode:urldecode %value)
           collect (cons name value) into properties
           finally (return (compute-open-database-parameters
                            (list* (cons "dbname" dbname)
                                   (cons "user" username)
                                   (cons "password" password)
                                   (cons "host" host)
                                   (cons "port" port)
                                   properties))))))))

(defun parse-connection-string/key-value (string)
  (let ((position 0))
    (labels ((looking-at (regexp)
               (multiple-value-bind (match groups)
                   (cl-ppcre:scan-to-strings (cl-ppcre:create-scanner `(:sequence :start-anchor ,(cl-ppcre:parse-string regexp)))
                                             string :start position)
                 (when match
                   (incf position (length match))
                   (if (and groups (< 0 (length groups)))
                       (aref groups 0)
                       t))))

             (property ()
               (let (key value)
                 (setf key (key))
                 (equal)
                 (setf value (value))
                 (cons key value)))

             (key ()
               (looking-at "\\s*([^=\\s]+)"))
             
             (equal ()
               (looking-at "\\s*=\\s*"))
             
             (value ()
               (if (looking-at "'")
                   (looking-at "(.*?)(?<!\\\\)'")
                   (looking-at "(\\w+)"))))

      (loop
         while (< position (length string))
         collect (property) into properties
         finally (return (compute-open-database-parameters properties))))))

(defun compute-open-database-parameters (properties)
  (let (host port dbname username password use-ssl)
    (loop
       for (name . value) in properties
       do (cond
            ((string= name "host")     (setf host value))
            ((string= name "hostaddr") (setf host value))
            ((string= name "port")     (setf port (if value
                                                      (parse-integer value)
                                                      nil)))
            ((string= name "dbname")   (setf dbname value))
            ((string= name "user")     (setf username value))
            ((string= name "password") (setf password value))
            ((string= name "ssl")
             (cond
               ((string= "true" value) (setf use-ssl :yes))
               ((string= "false" value) (setf use-ssl :no))
               (t (error "Unknown ssl value ~S." value))))
            ((string= name "sslmode")
             (cond
               ((member value '("disable") :test #'string=)
                (setf use-ssl :no))
               ((member value '("allow" "prefer") :test #'string=)
                (setf use-ssl :try))
               ((member value '("require" "verify-ca" "verify-full") :test #'string=)
                (setf use-ssl :yes))))))
    (list dbname username password host port use-ssl)))

(defun open-database (database user password host &optional (port 5432) (use-ssl :no))
  "Create and connect a database object. use-ssl may be :no, :yes, or :try."
  (check-type database string)
  (check-type user string)
  (check-type password (or null string))
  (check-type host (or string (eql :unix)) "a string or :unix")
  (check-type port (integer 1 65535) "an integer from 1 to 65535")
  (check-type use-ssl (member :no :yes :try) ":no, :yes, or :try")
  (let ((conn (make-instance 'database-connection :host host :port port :user user
                             :password password :socket nil :db database :ssl use-ssl)))
    (initiate-connection conn)
    conn))

#+(and sbcl unix)
(defun sb-unix-socket-connect (socket-dir port)
  (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream))
        (addr (format nil "~a.s.PGSQL.~a" socket-dir port)))
    (sb-bsd-sockets:socket-connect sock addr)
    (sb-bsd-sockets:socket-make-stream
     sock :input t :output t :element-type '(unsigned-byte 8))))

#+sbcl
(defun sb-socket-connect (port host)
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket
                             :type :stream :protocol :tcp))
        (host (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host))))
    (sb-bsd-sockets:socket-connect sock host port)
    (sb-bsd-sockets:socket-make-stream
     sock :input t :output t :buffering :full :element-type '(unsigned-byte 8))))

(defun initiate-connection (conn)
  "Check whether a connection object is connected, try to connect it
if it isn't."
  (flet ((add-restart (err)
           (restart-case (error (wrap-socket-error err))
             (:reconnect () :report "Try again." (initiate-connection conn)))))
    (handler-case
        (let ((socket #-(or allegro sbcl)
                      (usocket:socket-stream
                       (usocket:socket-connect (connection-host conn)
                                               (connection-port conn)
                                               :element-type '(unsigned-byte 8)))
                      #+allegro
                      (socket:make-socket :remote-host (connection-host conn)
                                          :remote-port (connection-port conn)
                                          :format :binary)
                      #+sbcl
                      (if (and (stringp (connection-host conn))
                               (char= #\/ (aref (connection-host conn) 0)))
                          #+unix(sb-unix-socket-connect (connection-host conn) (connection-port conn))
                          #-unix(error "Unix sockets only available on Unix (really)")
                          (sb-socket-connect (connection-port conn)
                                             (connection-host conn))))
              (finished nil)
              (*connection-params* (make-hash-table :test 'equal)))
          (setf (slot-value conn 'meta) nil
                (connection-parameters conn) *connection-params*)
          (unwind-protect
               (setf socket (authenticate socket (connection-user conn)
                                          (connection-password conn) (connection-db conn)
                                          (connection-use-ssl conn))
                     (connection-timestamp-format conn)
                     (if (string= (gethash "integer_datetimes" (connection-parameters conn)) "on")
                         :integer :float)
                     (connection-socket conn) socket
                     finished t)
            (unless finished
              (ensure-socket-is-closed socket))))
      #-(or allegro sbcl)(usocket:socket-error (e) (add-restart e))
      #+allegro(excl:socket-error (e) (add-restart e))
      #+sbcl(sb-bsd-sockets:socket-error (e) (add-restart e))
      (stream-error (e) (add-restart e))))
    (values))

(defun reopen-database (conn)
  "Reconnect a disconnected database connection."
  (unless (database-open-p conn)
    (initiate-connection conn)))

(defun ensure-connection (conn)
  "Used to make sure a connection object is connected before doing
anything with it."
  (unless conn
    (error "No database connection selected."))
  (unless (database-open-p conn)
    (restart-case (error 'database-connection-lost :message "Connection to database server lost.")
      (:reconnect () :report "Try to reconnect." (initiate-connection conn)))))

(defun close-database (connection)
  "Gracefully disconnect a database connection."
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
        (error 'database-error :message "This connection is still processing another query."))
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
                   (cond ((eq (connection-socket ,connection-name) (stream-error-stream e))
                          (ensure-socket-is-closed (connection-socket ,connection-name) :abort t)
                          (,retry-name (wrap-socket-error e)))
                         (t (error e))))
                 (cl-postgres-error:server-shutdown (e)
                   (ensure-socket-is-closed (connection-socket ,connection-name) :abort t)
                   (,retry-name e))))
             (,retry-name (err)
               (restart-case (error err)
                 (:reconnect () :report "Try to reconnect"
                             (reopen-database ,connection-name)
                             (,body-name)))))
      (,body-name)))))

(defun exec-query (connection query &optional (row-reader 'ignore-row-reader))
  "Execute a query string and apply the given row-reader to the
result."
  (check-type query string)
  (with-reconnect-restart connection
    (using-connection connection
      (send-query (connection-socket connection) query row-reader))))

(defun prepare-query (connection name query)
  "Prepare a query string and store it under the given name."
  (check-type query string)
  (check-type name string)
  (with-reconnect-restart connection
    (using-connection connection
      (send-parse (connection-socket connection) name query)
      (values))))

(defun exec-prepared (connection name parameters &optional (row-reader 'ignore-row-reader))
  "Execute a previously prepared query with the given parameters,
apply a row-reader to the result."
  (check-type name string)
  (check-type parameters list)
  (with-reconnect-restart connection
    (using-connection connection
      (send-execute (connection-socket connection)
                    name parameters row-reader))))

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
                  :do (next-field field)))
  (values))
