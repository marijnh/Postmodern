(in-package :cl-postgres)

;; For more information about the PostgreSQL scocket protocol, see
;; http://www.postgresql.org/docs/current/interactive/protocol.html

(defmacro define-message (name id (&rest arglist) &body parts)
  "This macro synthesizes a function to send messages of a specific
type. It takes care of the plumbing -- calling writer functions on a
stream, keeping track of the length of the message -- so that the
message definitions themselves stay readable."
  (let ((writers nil)
        (socket (gensym))
        (strings ())
        (base-length 4)
        (extra-length ()))
    (setf writers
          (mapcar (lambda (part)
                    (let ((name (gensym)))
                      (ecase (first part)
                        (uint
                         (incf base-length (second part))
                         `(,(integer-writer-name (second part) nil) ,socket ,(third part)))
                        (string
                         (push `(,name ,(second part)) strings)
                         (incf base-length 1) ;; The null terminator
                         (push `(enc-byte-length ,name) extra-length)
                         `(write-str ,socket ,name))
                        (bytes
                         (push `(,name ,(second part)) strings)
                         (push `(length ,name) extra-length)
                         `(write-bytes ,socket ,name)))))
                  parts))
    (push `(write-uint4 ,socket (+ ,base-length ,@extra-length))
          writers)
    (when id
      (push `(write-uint1 ,socket ,(char-code id)) writers))
    `(defun ,name ,(cons socket arglist)
      (declare (type stream ,socket)
               #.*optimize*)
      (let ,strings ,@writers))))

;; Try to enable SSL for a connection.
(define-message ssl-request-message nil ()
  (uint 4 80877103))

;; Sends the initial message and sets a few parameters.
(define-message startup-message nil (user database)
  (uint 4 196608) ;; Identifies protocol 3.0
  (string "user")
  (string user)
  (string "database")
  (string database)
  (string "client_encoding")
  (string *client-encoding*)
  (uint 1 0)) ;; Terminates the parameter list

;; Identify a user with a plain-text password.
(define-message plain-password-message #\p (password)
  (string password))

(defun bytes-to-hex-string (bytes)
  "Convert an array of 0-255 numbers into the corresponding string of
\(lowercase) hex codes."
  (declare (type (vector (unsigned-byte 8)) bytes)
           #.*optimize*)
  (let ((digits #.(coerce "0123456789abcdef" 'simple-base-string))
        (result (make-string (* (length bytes) 2) :element-type 'base-char)))
    (loop :for byte :across bytes
	  :for pos :from 0 :by 2
	  :do (setf (char result pos) (aref digits (ldb (byte 4 4) byte))
		    (char result (1+ pos)) (aref digits (ldb (byte 4 0) byte))))
    result))

(defun md5-password (password user salt)
  "Apply the hashing that PostgreSQL expects to a password."
  (declare (type string user password)
           (type (vector (unsigned-byte 8)) salt)
           #.*optimize*)
  (flet ((md5-and-hex (sequence)
           (bytes-to-hex-string (md5:md5sum-sequence sequence))))
    (let* ((pass1 (md5-and-hex (concatenate 'string password user)))
           (pass2 (md5-and-hex (concatenate '(vector (unsigned-byte 8) *) (enc-string-bytes pass1) salt))))
      (concatenate 'string "md5" pass2))))

;; Identify a user with an MD5-hashed password.
(define-message md5-password-message #\p (password user salt)
  (string (md5-password password user salt)))

;; Send a query, the simple way.
(define-message query-message #\Q (query)
  (string query))

;; Parse a query
(define-message simple-parse-message #\P (query)
  (uint 1 0) ;; Name of the prepared statement
  (string query)
  (uint 2 0)) ;; Parameter types

;; Parse a query, giving it a name.
(define-message parse-message #\P (name query)
  (string name)
  (string query)
  (uint 2 0))

(defun formats-to-bytes (formats)
  "Formats have to be passed as arrays of 2-byte integers, with 1
indicating binary and 0 indicating plain text."
  (declare (type vector formats)
           #.*optimize*)
  (let* ((result (make-array (* 2 (length formats))
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (loop :for format :across formats
          :for pos :from 1 :by 2
          :do (when format (setf (elt result pos) 1)))
    result))

;; Bind the unnamed prepared query, asking for the given result
;; formats.
(define-message simple-bind-message #\B (formats)
  (uint 1 0) ;; Name of the portal
  (uint 1 0) ;; Name of the prepared statement
  (uint 2 0) ;; Number of parameter format specs
  (uint 2 0) ;; Number of parameter specifications
  (uint 2 (length formats)) ;; Number of result format specifications
  (bytes (formats-to-bytes formats))) ;; Result format

;; This one was a bit too complex to put into define-message format,
;; so it does everything by hand.
(defun bind-message (socket name result-formats parameters)
  "Bind a prepared statement, ask for the given formats, and pass the
given parameters, that can be either string or byte vector.
\(vector \(unsigned-byte 8)) parameters will be sent as binary data, useful
for binding data for binary long object columns."
  (declare (type stream socket)
           (type string name)
           (type vector result-formats)
           (type list parameters)
           #.*optimize*)
  (let* ((n-params (length parameters))
         (param-formats (make-array n-params :element-type 'fixnum))
         (param-sizes (make-array n-params :element-type 'fixnum))
         (param-values (make-array n-params))
         (n-result-formats (length result-formats)))
    (declare (type (unsigned-byte 16) n-params n-result-formats))
    (loop :for param :in parameters
          :for i :from 0
          :do (flet ((set-param (format size value)
                       (setf (aref param-formats i) format
                             (aref param-sizes i) size
                             (aref param-values i) value)))
                (cond ((eq param :null)
                       (set-param 0 0 nil))
                      ((typep param '(vector (unsigned-byte 8)))
                       (set-param 1 (length param) param))
                      (t
                       (unless (typep param 'string)
                         (setf param (to-sql-string param)))
                       (set-param 0 (enc-byte-length param) param)))))
    (write-uint1 socket #.(char-code #\B))
    (write-uint4 socket (+ 12
                           (enc-byte-length name)
                           (* 6 n-params)   ;; Input formats and sizes
                           (* 2 n-result-formats)
                           (loop :for size :of-type fixnum :across param-sizes
                                 :sum size)))
    (write-uint1 socket 0)                  ;; Name of the portal
    (write-str socket name)                 ;; Name of the prepared statement
    (write-uint2 socket n-params)           ;; Number of parameter format specs
    (loop :for format :across param-formats ;; Param formats (text/binary)
          :do (write-uint2 socket format))
    (write-uint2 socket n-params)           ;; Number of parameter specifications
    (loop :for param :across param-values
          :for size :across param-sizes
          :do (write-int4 socket (if param size -1))
          :do (when param
                (if (typep param '(vector (unsigned-byte 8)))
                    (write-sequence param socket)
                    (enc-write-string param socket))))
    (write-uint2 socket n-result-formats)   ;; Number of result formats
    (loop :for format :across result-formats ;; Result formats (text/binary)
          :do (write-uint2 socket (if format 1 0)))))

;; Describe the anonymous portal, so we can find out what kind of
;; result types will be passed.
(define-message simple-describe-message #\D ()
  (uint 1 #.(char-code #\S)) ;; This is a statement describe
  (uint 1 0)) ;; Name of the portal

;; Describe a named portal.
(define-message describe-prepared-message #\D (name)
  (uint 1 #.(char-code #\S)) ;; This is a statement describe
  (string name))

;; Execute a bound statement.
(define-message simple-execute-message #\E ()
  (uint 1 0) ;; Name of the portal
  (uint 4 0)) ;; Max amount of rows (0 = all rows)

;; Flush the sent messages, force server to start responding.
(define-message flush-message #\H ())

;; For re-synchronizing a socket.
(define-message sync-message #\S ())

;; Tell the server we are about to close the connection.
(define-message terminate-message #\X ())

;; To get out of the copy-in protocol.
(define-message copy-done-message #\c ())

;; ** MCNA Addition ** -- this line and everything below it was added by MCNA to
;; support bulk copying.
(defun copy-data-message (socket data)
  (declare (type stream socket)
    (optimize (speed 3) (safety 0) (space 1) (debug 1)
      (compilation-speed 0)))
  (write-uint1 socket 100)
  (write-uint4 socket (+ 4 (length data)))
  (enc-write-string data socket))

(define-message copy-fail-message #\f (reason)
  (string reason))
