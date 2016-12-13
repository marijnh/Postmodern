(in-package :cl-postgres)

(defclass database-connection ()
  ((host :initarg :host :reader connection-host)
   (port :initarg :port :reader connection-port)
   (database :initarg :db :reader connection-db)
   (user :initarg :user :reader connection-user)
   (password :initarg :password :reader connection-password)
   (use-ssl :initarg :ssl :reader connection-use-ssl)
   (service :initarg :service :accessor connection-service)
   (socket :initarg :socket :accessor connection-socket)
   (meta :initform nil)
   (available :initform t :accessor connection-available)
   (parameters :accessor connection-parameters)
   (timestamp-format :accessor connection-timestamp-format))
  (:documentation "Representation of a database connection. Contains
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

(defun open-database (database user password host &optional (port 5432) (use-ssl :no) (service "postgres"))
  "Create and connect a database object. use-ssl may be :no, :yes, or :try."
  (check-type database string)
  (check-type user string)
  (check-type password (or null string))
  (check-type host (or string (eql :unix)) "a string or :unix")
  (check-type port (integer 1 65535) "an integer from 1 to 65535")
  (check-type use-ssl (member :no :yes :try) ":no, :yes, or :try")
  (let ((conn (make-instance 'database-connection :host host :port port :user user
                             :password password :socket nil :db database :ssl use-ssl
                             :service service)))
    (initiate-connection conn)
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
  (ccl:make-socket :format :binary
                   :remote-host host
                   :remote-port port))

#+allegro
(defun inet-socket-connect (host port)
  (socket:make-socket :remote-host host
                      :remote-port port
                      :format :binary
                      :type :stream))

(defun initiate-connection (conn)
  "Check whether a connection object is connected, try to connect it
if it isn't."
  (flet ((add-restart (err)
           (restart-case (error (wrap-socket-error err))
             (:reconnect () :report "Try again." (initiate-connection conn))))
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
                         (unix-socket-connect (unix-socket-path *unix-socket-dir* (connection-port conn))))
                        ((and (stringp (connection-host conn))
                              (char= #\/ (aref (connection-host conn) 0)))
                         (assert-unix)
                         (unix-socket-connect (unix-socket-path (connection-host conn) (connection-port conn))))
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
          (setf (slot-value conn 'meta) nil
                (connection-parameters conn) *connection-params*)
          (unwind-protect
               (setf socket (authenticate socket conn)
                     (connection-timestamp-format conn)
                     (if (string= (gethash "integer_datetimes" (connection-parameters conn)) "on")
                         :integer :float)
                     (connection-socket conn) socket
                     finished t)
            (unless finished
              (ensure-socket-is-closed socket))))
      #-(or allegro cl-postgres.features:sbcl-available ccl)(usocket:socket-error (e) (add-restart e))
      #+ccl (ccl:socket-error (e) (add-restart e))
      #+allegro(excl:socket-error (e) (add-restart e))
      #+cl-postgres.features:sbcl-available(sb-bsd-sockets:socket-error (e) (add-restart e))
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

(defun wait-for-notification (connection)
  "Perform a blocking wait for asynchronous notification. Return the
channel string, the payload and notifying pid as multiple values."
  (block nil
    (with-reconnect-restart connection
      (handler-bind ((postgresql-notification
                      (lambda (c)
                        (return (values (postgresql-notification-channel c)
                                        (postgresql-notification-payload c)
                                        (postgresql-notification-pid c))))))
        (message-case (connection-socket connection))))))

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

(defun unprepare-query (connection name)
  "Close the prepared query given by name."
  (check-type name string)
  (with-reconnect-restart connection
    (using-connection connection
      (send-close (connection-socket connection) name)
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
