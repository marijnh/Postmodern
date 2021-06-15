;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

;; For more information about the PostgreSQL scocket protocol, see
;; http://www.postgresql.org/docs/current/interactive/protocol.html
;;
;; Postgresql Scram Documentation is at
;; https://www.postgresql.org/docs/current/protocol.html
;; https://www.postgresql.org/docs/current/protocol-overview.html
;; https://www.postgresql.org/docs/current/protocol-flow.html
;; https://www.postgresql.org/docs/current/sasl-authentication.html
;; https://www.postgresql.org/docs/current/protocol-message-types.html
;; https://www.postgresql.org/docs/current/protocol-message-formats.html
;;

;; Scram Functions following the specifications here:
;; RFC 5802 https://tools.ietf.org/html/rfc5802
;; RFC 7677 https://tools.ietf.org/html/rfc7677

;;  From RFC 7677
;;  This is a simple example of a SCRAM-SHA-256 authentication exchange
;;  when the client doesn't support channel bindings.  The username
;;    'user' and password 'pencil' are being used.
;;
;;   C: n,,n=user,r=rOprNGfwEbeRWgbNEkqO
;;   S: r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,
;;      s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096
;;   C: c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,
;;      p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ=
;;   S: v=6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4=

;; Reminder if the string has an '=' sign at the end, it is probably encoded in
;; base64 and the equal signs are there for padding.
;; Reminder (cl-base64:string-to-base64-string "abcde") "YWJjZGU="
;; (cl-base64:base64-string-to-usb8-array
;;   (cl-base64:string-to-base64-string "abcde"))
;; #(97 98 99 100 101)

;; (cl-postgres-trivial-utf-8:utf-8-bytes-to-string
;;   (cl-base64:base64-string-to-usb8-array
;;     (cl-base64:string-to-base64-string "abcde")))
;; "abcde"
;;  ":=": The variable on the left-hand side represents the octet
;;        string resulting from the expression on the right-hand side.

;;   o  "+": Octet string concatenation.

;;     o  "[ ]": A portion of an expression enclosed in "[" and "]" may not
;;        be included in the result under some circumstances.  See the
;;        associated text for a description of those circumstances.

;;     o  Normalize(str): Apply the SASLprep profile [RFC4013] of the
;;        "stringprep" algorithm [RFC3454] as the normalization algorithm to
;;        a UTF-8 [RFC3629] encoded "str".  The resulting string is also in
;;        UTF-8.  When applying SASLprep, "str" is treated as a "stored
;;        strings", which means that unassigned Unicode codepoints are
;;        prohibited (see Section 7 of [RFC3454]).  Note that
;;        implementations MUST either implement SASLprep or disallow use of
;;        non US-ASCII Unicode codepoints in "str".

;;     o  HMAC(key, str): Apply the HMAC keyed hash algorithm (defined in
;;        [RFC2104]) using the octet string represented by "key" as the key
;;        and the octet string "str" as the input string.  The size of the
;;        result is the hash result size for the hash function in use.  For
;;        example, it is 20 octets for SHA-1 (see [RFC3174]).

;;     o  H(str): Apply the cryptographic hash function to the octet string
;;        "str", producing an octet string as a result.  The size of the
;;        result depends on the hash result size for the hash function in
;;        use.

;;     o  XOR: Apply the exclusive-or operation to combine the octet string
;;        on the left of this operator with the octet string on the right of
;;        this operator.  The length of the output and each of the two
;;        inputs will be the same for this use.

;;     o  Hi(str, salt, i):

;;       U1   := HMAC(str, salt + INT(1))
;;       U2   := HMAC(str, U1)
;;       ...
;;       Ui-1 := HMAC(str, Ui-2)
;;       Ui   := HMAC(str, Ui-1)

;;       Hi := U1 XOR U2 XOR ... XOR Ui

;;        where "i" is the iteration count, "+" is the string concatenation
;;        operator, and INT(g) is a 4-octet encoding of the integer g, most
;;        significant octet first.

;;        Hi() is, essentially, PBKDF2 [RFC2898] with HMAC() as the
;;        pseudorandom function (PRF) and with dkLen == output length of
;;        HMAC() == output length of H().

;;     SaltedPassword  := Hi(Normalize(password), salt, i)
;;     ClientKey       := HMAC(SaltedPassword, "Client Key")
;;     StoredKey       := H(ClientKey)
;;     AuthMessage     := client-first-message-bare + "," +
;;                        server-first-message + "," +
;;                        client-final-message-without-proof
;;     ClientSignature := HMAC(StoredKey, AuthMessage)
;;     ClientProof     := ClientKey XOR ClientSignature
;;     ServerKey       := HMAC(SaltedPassword, "Server Key")
;;     ServerSignature := HMAC(ServerKey, AuthMessage)


;;     Messages

;;     RFC 5802 names four consecutive messages between server and client:

;;     client-first -  The client-first message consists of a gs2-cbind-flag,
;;                     the desired username, and a randomly generated client
;;                     nonce cnonce.
;;     server-first -  The server appends to this client nonce its own nonce
;;                     snonce, and adds it to the server-first message, which
;;                     also contains a salt used by the server for salting the
;;                     user's password hash, and an iteration count indicator it
;;     client-final -  After that the client sends the client-final message,
;;                     which contains c-bind-input, the concatenation of the
;;                     client and the server nonce, and cproof.
;;     server-final -  The communication closes with the server-final message,
;;                     which contains the server proof sproof.

;;     Salted password - The salted password spassword is calculated as follows:

;;         spassword = Hi(password, salt, iterations)

;;     where Hi(p,s,i) is defined as PBKDF2 (HMAC, p, s, i, output length of H).

;;     Proofs - The client and the server prove to each other they have the same
;;              Auth variable, consisting of:

;;         Auth = client-first, server-first, client-final-without-proof

;;     The proofs are calculated as follows:

;;         ckey = HMAC(spassword, 'Client Key')
;;         skey = HMAC(spassword, 'Server Key')

;;         cproof = ckey XOR HMAC(H(ckey), Auth)
;;         sproof = HMAC(skey, Auth)

;;     where the XOR operation is applied to byte strings of the same length,
;;     H(ckey) is a normal hash of ckey. 'Client Key' and 'Server Key' are
;;     verbatim strings.

;;     Stored password - The stored password is equal to H(ckey). In the
;;                       algorithm above, the client proves knowledge of ckey,
;;                       which is then hashed and compared against what is
;;                       stored on the server.

;;     For every user, the server only has to store the username, H(ckey), skey,
;;     salt, and it, but not the clear text password itself.


(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))
(deftype index () `(integer 0 ,array-total-size-limit))

(declaim (ftype (function (index) octet-vector) make-octet-vector)
         (inline make-octet-vector))
(defun make-octet-vector (len)
  (make-array (the index len)
              :element-type 'octet
              :initial-element 0))

;; pjb suggestion. Also
;;sabra: errors such as Array index -8 out of bounds for #(0 … 0) .  may be cryptic.  Better provide a condition with an error message such: vector too long for pad-octet-vector.
(defun pad-octet-vector (vector &optional (desired-length 32))
  "Takes an octet-vector and, if it is shorter than the SIZE parameter,
pads it to the SIZE parameter by adding 0 entries at the beginning."
  (let ((length (length vector)))
    (if (= desired-length length)
        vector
        (let ((result (make-octet-vector desired-length)))
          (replace result vector :start1 (- desired-length length))))))
#|
;; no-defun-allowed + beach + flip214 suggestion
(defun pad-octet-vector (vector &key (desired-length 32) (padding 0))
  (let ((vector-length (length vector)))
    (if (>= vector-length length)
        vector
        (replace (make-array desired-length :initial-element padding)
                 vector
                 :start1 (- desired-length vector-length)))))
|#
(defun gen-client-nonce (&optional (nonce-length 32))
  "Generate a random alphanumeric nonce with a default length of 32."
  (let* ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         (chars-length 62)
         (client-nonce (make-string nonce-length))
         (crypto:*prng* (crypto:make-prng :os :seed :random)))
    (dotimes (i nonce-length)
      (setf (aref client-nonce i)
            (aref chars
                  (crypto:strong-random chars-length))))
    client-nonce))

(defun gen-client-initial-response (user-name client-nonce)
  (when (not user-name) (setf user-name ""))
  (format nil "n,,n=~a,r=~a" (saslprep-normalize user-name) client-nonce))

(defun gen-salted-password (password server-salt iterations
                            &key (digest :sha256) (salt-type :byte-array))
  "Takes an password (must be an ascii string) and server salt (by default
presumed byte-array but can be set for :string or :hex) and an integer
iterations. Digest is presumed to be :sha256 but can be set to other valid
ironclad digests. returns a byte-array"
  (case salt-type
    (:string (setf server-salt
                   (ironclad:ascii-string-to-byte-array server-salt)))
    (:byte-array t)
    (:base64-string (setf server-salt (cl-base64:base64-string-to-usb8-array
                                       server-salt)))
    (:hex (setf server-salt (ironclad:hex-string-to-byte-array server-salt)))
    (t (cerror "Please enter valid salt-type"
               "unknown salt-type in gen-salted-password")))
  (ironclad:pbkdf2-hash-password
   (ironclad:ascii-string-to-byte-array (saslprep-normalize password))
   :salt server-salt
   :digest digest
   :iterations iterations))

(defun split-server-response (response)
  "Takes an array of bytes which are encoded in base64,  It should return a list
of three alists of the form:
 ((\"r\" . \"odaUyoz0GpB5GxXLfe2Y8SVjZEosREsxzxhtXY1jiNebxJlohG8IRD1v\")
 (\"s\" . \"HV25Sl/1VAUF7k+Ddv42dQ==\") (\"i\" . \"4096\") where \"r\" is the
 server nonce,
 \"s\" is a base64 encoded salt and \"i\" is the number of iterations for the
 hash digest.

 We do not use split-sequence building the cons cell because the equal sign can
 appear in the nonce or salt itself."
  (loop :for x
          :in (split-sequence:split-sequence #\,
                                             (clp-utf8:utf-8-bytes-to-string
                                              response))
        :collect (let ((split-on (position #\= x)))
                   (cons (subseq x 0 split-on)
                         (subseq x (1+ split-on))))))

(defun validate-server-nonce (server-nonce client-nonce)
  "checks whether the server-nonce begins with the client-nonce. Both need to be
normal strings."
  (when (not (= 0 (search client-nonce server-nonce)))
    (error 'protocol-violation
           :message "Client-nonce not found at beginning of server-nonce")))

(defun parse-scram-server-first-response (response client-nonce
                                          &key (response-type
                                                :base64-usb8-array))
  "Takes a server response and a client-nonce. If the server response is not in
the form of an array of bytes which are encoded in base64, the response type
must be specified as either :base64-string or :utf8-string. The client-nonce
should be a normal utf8 string.

It returns the server-response as a normal string, the server-provided-salt as a
normal string, and the server-iterations as an integer"
  (cond ((eq response-type :base64-usb8-array) nil)
        ((eq response-type :utf8-string)
         (setf response
               (cl-base64:base64-string-to-usb8-array
                (cl-base64:string-to-base64-string response))))
        ((eq response-type :base64-string)
         (setf response (cl-base64:base64-string-to-usb8-array response)))
        (t (cerror "Invalid response type for parse-scram-server-first-response.
Must be one of :base64-usb8-array, :utf8-string or :base64-string"
                   response-type)))

  (let* ((split-response (split-server-response response))
         (server-nonce (cdr (assoc "r" split-response :test 'equal)))
         (server-nonce-validated (validate-server-nonce server-nonce
                                                        client-nonce))
         (server-salt (cdr (assoc "s" split-response :test 'equal)))
         (server-iterations (parse-integer (cdr (assoc "i" split-response
                                                       :test 'equal))))
         (num-of-split (length split-response)))
    (when (not (= 3 num-of-split))
      (error 'protocol-error
             :message (format nil "There was an error in parsing the server
response. Parsing had ~a results instead of 3" num-of-split)))
    (values server-nonce server-salt server-iterations)))

(defun gen-client-key (salted-password
                       &optional (message "Client Key") (sha-method :sha256))
  "Returns a byte array"
  (when (stringp salted-password)
    (setf salted-password (ironclad:ascii-string-to-byte-array salted-password)))
  (ironclad:hmac-digest
   (ironclad:update-hmac
    (ironclad:make-hmac salted-password sha-method)
    (ironclad:ascii-string-to-byte-array message))))

(defun gen-stored-key (client-key)
  (ironclad:digest-sequence :sha256 client-key))

(defun gen-auth-message (client-initial-response server-response
                         final-message-part1)
  "Currently assumes all parameters are normal strings"
  (format nil "~a,~a,~a"
          (if (and (search "n,," client-initial-response)
                   (= 0 (search "n,," client-initial-response)))
              (subseq client-initial-response 3)
              client-initial-response)
          server-response
          final-message-part1))

(defun gen-client-signature (stored-key auth-message
                             &optional (sha-method :sha256))
  (ironclad:hmac-digest
   (ironclad:update-hmac
    (ironclad:make-hmac stored-key sha-method)
    (ironclad:ascii-string-to-byte-array auth-message))))

(defun gen-client-proof (client-key client-signature)
  "The eventual client-proof needs to be base64 encoded"
  (let* ((int (logxor (ironclad:octets-to-integer client-key)
                      (ironclad:octets-to-integer client-signature)))
         (octet-arry (ironclad:integer-to-octets int)))
    (pad-octet-vector octet-arry 32)))

(defun get-server-key (salted-password &optional (message "Server Key"))
  (gen-client-signature salted-password message))

(defun get-server-signature (server-key auth-message)
  (gen-client-signature server-key auth-message))

(defun gen-final-message-part-1 (server-nonce)
  "Assumes the server-nonce is a utf8 string"
  (format nil "c=biws,r=~a" server-nonce))

(defun gen-final-message (final-message-part1 client-proof)
  "Assuming client-proof is in a usb8 array, returns client-proof as part of the
final message as a base64 string"
  (format nil "~a,p=~a" final-message-part1
          (cl-base64:usb8-array-to-base64-string client-proof)))

(defun aggregated-gen-final-client-message (user-name client-nonce
                                            server-message password
                                            &key (response-type
                                                  :base64-usb8-array)
                                              (salt-type :base64-string))
  "Takes a user-name, a client-nonce, a server response and a password. If the
server response is not in the form of an array of bytes which are encoded in
base64, the response type must be specified as either :base64-string or
:utf8-string. The client-nonce should be a normal utf8 string.
It returns the server-response as a normal string, the server-provided-salt as
a normal string, and the server-iterations as an integer.

The allowed response-types are :base64-string, :base64-usb8-array and
:utf8-string."
  (multiple-value-bind (server-nonce server-salt server-iterations)
      (parse-scram-server-first-response server-message client-nonce
                                         :response-type response-type)
    (let* ((final-message-part-1 (gen-final-message-part-1 server-nonce))
           (client-initial-response
             (gen-client-initial-response user-name client-nonce))
           (salted-password
             (gen-salted-password password server-salt server-iterations
                                  :salt-type salt-type))
           (client-key (gen-client-key salted-password))
           (stored-key (gen-stored-key client-key))
           (auth-message
             (gen-auth-message client-initial-response server-message
                               final-message-part-1))
           (client-signature (gen-client-signature stored-key auth-message))
           (client-proof (gen-client-proof client-key client-signature))
           (final-client-message
             (gen-final-message final-message-part-1 client-proof))
           (server-key (get-server-key salted-password))
           (server-signature (cl-base64:usb8-array-to-base64-string
                              (get-server-signature server-key auth-message))))
      (values final-client-message server-signature))))
