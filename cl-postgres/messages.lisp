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
                         (push `(#.*string-byte-length* ,name) extra-length)
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

;; Sends the initial message and sets a few parameters.
(define-message startup-message nil (user database)
  (uint 4 196608) ;; Identifies protocol 3.0
  (string "user")
  (string user)
  (string "database")
  (string database)
  (string "client_encoding")
  (string #.*client-encoding*)
  (uint 1 0)) ;; Terminates the parameter list

;; Identify a user with a plain-text password.
(define-message plain-password-message #\p (password)
  (string password))

(defun bytes-to-hex-string (bytes)
  "Convert an array of 0-255 numbers into the corresponding string of
\(lowercase) hex codes."
  (let ((digits "0123456789abcdef")
	(result (make-string (* (length bytes) 2))))
    (loop :for byte :across bytes
	  :for pos :from 0 :by 2
	  :do (setf (char result pos) (aref digits (ldb (byte 4 4) byte))
		    (char result (1+ pos)) (aref digits (ldb (byte 4 0) byte))))
    result))

(defun md5-password (password user salt)
  "Apply the hashing that PostgreSQL expects to a password."
  (flet ((md5-and-hex (sequence)
	   (bytes-to-hex-string (md5:md5sum-sequence sequence))))
    (let* ((pass1 (md5-and-hex (concatenate 'string password user)))
           (pass2 (md5-and-hex (concatenate '(vector (unsigned-byte 8) *) (#.*string-bytes* pass1) salt))))
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
         (n-result-formats (length result-formats)))
    (declare (type (unsigned-byte 16) n-params n-result-formats))
    (loop for param :in parameters
          for idx :from 0
          do (if (or (eq param :null)
                     (eq param nil))
                 (progn
                   (setf (aref param-formats idx) 0)
                   (setf (aref param-sizes idx) 0))
                 (etypecase param
                   ((vector (unsigned-byte 8))
                    (setf (aref param-formats idx) 1)
                    (setf (aref param-sizes idx) (length param)))
                   (string
                    (setf (aref param-formats idx) 0)
                    (setf (aref param-sizes idx) (#.*string-byte-length* param))))))
    (write-uint1 socket #.(char-code #\B))
    (write-uint4 socket (+ 12
                           (length name)
                           (* 2 n-params)   ;; Input formats
                           (* 2 n-result-formats)
                           (* 4 n-params)
                           (loop :for size :of-type fixnum :across param-sizes
                                 :sum size)))
    (write-uint1 socket 0)                  ;; Name of the portal
    (write-str socket name)                 ;; Name of the prepared statement
    (write-uint2 socket n-params)           ;; Number of parameter format specs
    (loop :for format :across param-formats ;; Param formats (text/binary)
          :do (write-uint2 socket format))
    (write-uint2 socket n-params)           ;; Number of parameter specifications
    (loop :for param :in parameters
          :for size :of-type fixnum :across param-sizes
          :do (if (or (eq param :null)
                      (eq param nil))
                  (write-int4 socket -1)   ;; -1 size means SQL NULL
                  (progn
                    (write-uint4 socket size)
                    (etypecase param
                      ((vector (unsigned-byte 8)) (write-sequence param socket))
                      (string (#.*write-string* param socket))))))
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

;;; Copyright (c) 2006 Marijn Haverbeke
;;;
;;; This software is provided 'as-is', without any express or implied
;;; warranty. In no event will the authors be held liable for any
;;; damages arising from the use of this software.
;;;
;;; Permission is granted to anyone to use this software for any
;;; purpose, including commercial applications, and to alter it and
;;; redistribute it freely, subject to the following restrictions:
;;;
;;; 1. The origin of this software must not be misrepresented; you must
;;;    not claim that you wrote the original software. If you use this
;;;    software in a product, an acknowledgment in the product
;;;    documentation would be appreciated but is not required.
;;;
;;; 2. Altered source versions must be plainly marked as such, and must
;;;    not be misrepresented as being the original software.
;;;
;;; 3. This notice may not be removed or altered from any source
;;;    distribution.
