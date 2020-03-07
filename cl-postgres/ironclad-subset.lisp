;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

(defclass discrete-logarithm-group ()
  ((p :initarg :p :reader group-pval)
   (q :initarg :q :reader group-qval)
   (g :initarg :g :reader group-gval)))


;;; converting from integers to octet vectors

(defun octets-to-integer (octet-vec &key (start 0) end (big-endian t) n-bits)
  (declare (type (simple-array (unsigned-byte 8) (*)) octet-vec)
           (optimize (speed 3) (space 0) (safety 1) (debug 0)))
  (let ((end (or end (length octet-vec))))
    (multiple-value-bind (n-bits n-bytes)
        (let ((size (- end start)))
          (if n-bits
              (values n-bits (min (ceiling n-bits 8) size))
              (values (* 8 size) size)))
      (let ((sum (if big-endian
                     (loop with sum = 0
                           for i from (- end n-bytes) below end
                           do (setf sum (+ (ash sum 8) (aref octet-vec i)))
                           finally (return sum))
                     (loop for i from start below (+ start n-bytes)
                           for j from 0 by 8
                           sum (ash (aref octet-vec i) j)))))
        (ldb (byte n-bits 0) sum)))))

(defun integer-to-octets (bignum &key n-bits (big-endian t))
  (declare (optimize (speed 3) (space 0) (safety 1) (debug 0)))
  (let* ((n-bits (or n-bits (integer-length bignum)))
         (bignum (ldb (byte n-bits 0) bignum))
         (n-bytes (ceiling n-bits 8))
         (octet-vec (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) octet-vec))
    (if big-endian
        (loop for i from (1- n-bytes) downto 0
              for index from 0
              do (setf (aref octet-vec index) (ldb (byte 8 (* i 8)) bignum))
              finally (return octet-vec))
        (loop for i from 0 below n-bytes
              for byte from 0 by 8
              do (setf (aref octet-vec i) (ldb (byte 8 byte) bignum))
              finally (return octet-vec)))))

(defun maybe-integerize (thing)
  (etypecase thing
    (integer thing)
    ((simple-array (unsigned-byte 8) (*)) (octets-to-integer thing))))


(defun byte-array-to-hex-string (vector &key (start 0) end (element-type 'base-char))
  "Return a string containing the hexadecimal representation of the
subsequence of VECTOR between START and END.  ELEMENT-TYPE controls
the element-type of the returned string."
  (declare (type (vector (unsigned-byte 8)) vector)
           (type fixnum start)
           (type (or null fixnum) end)
           (optimize (speed 3) (safety 1)))
  (let* ((end (or end (length vector)))
         (length (- end start))
         (hexdigits #.(coerce "0123456789abcdef" 'simple-base-string)))
    (loop with string = (ecase element-type
                          ;; so that the compiler optimization can jump in
                          (base-char (make-string (* length 2)
                                                  :element-type 'base-char))
                          (character (make-string (* length 2)
                                                  :element-type 'character)))
       for i from start below end
       for j from 0 below (* length 2) by 2
       do (let ((byte (aref vector i)))
            (declare (optimize (safety 0)))
            (setf (aref string j)
                  (aref hexdigits (ldb (byte 4 4) byte))
                  (aref string (1+ j))
                  (aref hexdigits (ldb (byte 4 0) byte))))
       finally (return string))))


(defun hex-string-to-byte-array (string &key (start 0) (end nil))
  "Parses a substring of STRING delimited by START and END of
hexadecimal digits into a byte array."
  (declare (type string string))
  (let* ((end (or end (length string)))
         (length (/ (- end start) 2))
         (key (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) key))
    (flet ((char-to-digit (char)
             (or (position char "0123456789abcdef" :test #'char-equal)
                 (error 'ironclad-error
                        :format-control "~A is not a hex digit"
                        :format-arguments (list char)))))
      (loop for i from 0
            for j from start below end by 2
            do (setf (aref key i)
                     (+ (* (char-to-digit (char string j)) 16)
                        (char-to-digit (char string (1+ j)))))
         finally (return key)))))

(defun ascii-string-to-byte-array (string &key (start 0) end)
  "Convert STRING to a (VECTOR (UNSIGNED-BYTE 8)).  It is an error if
STRING contains any character whose CHAR-CODE is greater than 255."
  (declare (type string string)
           (type fixnum start)
           (type (or null fixnum) end)
           (optimize (speed 3) (safety 1)))
  (let* ((length (length string))
         (vec (make-array length :element-type '(unsigned-byte 8)))
         (end (or end length)))
    (loop for i from start below end do
          (let ((byte (char-code (char string i))))
            (unless (< byte 256)
              (error 'ironclad-error
                     :format-control "~A is not an ASCII character"
                     :format-arguments (list (char string i))))
            (setf (aref vec i) byte))
          finally (return vec))))

(declaim (notinline byte-array-to-hex-string
                    hex-string-to-byte-array
                    ascii-string-to-byte-array))

(defgeneric digest-length (digest)
  (:documentation "Return the number of bytes in a digest generated by DIGEST."))

(defdigest sha256 :digest-length 32 :block-length 64)

(defclass hmac (mac)
  ((inner-digest :reader inner-digest :initarg :inner-digest)
   (outer-digest :reader outer-digest :initarg :outer-digest)))

(defmethod print-object ((mac hmac) stream)
  (print-unreadable-object (mac stream :type nil :identity t)
    (format stream "HMAC(~A)" (type-of (inner-digest mac)))))

(defun make-hmac (key digest-name)
  (make-instance 'hmac :key key
                 :inner-digest (make-digest digest-name)
                 :outer-digest (make-digest digest-name)))

(defun update-hmac (hmac sequence &key (start 0) (end (length sequence)))
  (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
  (update-digest (inner-digest hmac) sequence :start start :end end)
  hmac)

(defun hmac-digest (hmac &key buffer (buffer-start 0))
  (let* ((x (copy-digest (inner-digest hmac)))
         (inner-hash (produce-digest x :digest buffer :digest-start buffer-start)))
    (copy-digest (outer-digest hmac) x)
    (update-digest x inner-hash :digest buffer :digest-start buffer-start)
    (produce-digest x :digest buffer :digest-start buffer-start)))

(defun ub32ref/be (vector offset)
  (declare (type simple-octet-vector vector)
           (type index offset))
  #+(and ccl ironclad-assembly x86-64)
  (%ub32ref/be vector offset)

  #+(and ecl ironclad-assembly little-endian)
  (ffi:c-inline (vector offset)
                (t :unsigned-int)
                :uint32-t
                "{
uint32_t n = *((uint32_t *) ((#0)->array.self.b8 + #1));
uint32_t r = (n << 24)
           | ((n & 0xff00) << 8)
           | ((n >> 8) & 0xff00)
           | (n >> 24);
@(return 0) = r;
}"
                :side-effects nil)

  #+(and sbcl big-endian)
  (sb-sys:sap-ref-32 (sb-sys:vector-sap vector) offset)

  #+(and sbcl ironclad-assembly (or x86 x86-64))
  (swap32 (sb-sys:sap-ref-32 (sb-sys:vector-sap vector) offset))

  #-(or (and ccl ironclad-assembly x86-64)
        (and ecl ironclad-assembly little-endian)
        (and sbcl big-endian)
        (and sbcl ironclad-assembly (or x86 x86-64)))
  (dpb (ub16ref/be vector offset)
       (byte 16 16)
       (ub16ref/be vector (+ offset 2))))

(defmethod derive-key ((kdf pbkdf2) passphrase salt iteration-count key-length)
  (pbkdf2-derive-key (kdf-digest kdf) passphrase salt iteration-count key-length))



(defun pbkdf2-hash-password (password &key (salt (make-random-salt))
                                           (digest 'sha256)
                                           (iterations 1000))
  "Given a PASSWORD as a byte vector, a SALT as a byte
vector (MAKE-RANDOM-SALT is called to generate a random salt if none
is provided), a digest function (SHA256 by default), and a number of
iterations (1000), returns the PBKDF2-derived hash of the
password (byte vector) as the first value, and the SALT (byte vector)
as the second value."
  (values (pbkdf2-derive-key digest password salt iterations (digest-length digest))
          salt))

(defun pbkdf2-derive-key (digest passphrase salt iteration-count key-length)
  (check-type iteration-count (integer 1 *))
  (check-type key-length (integer 1 *))
  (loop with count = 1
     with hmac = (make-hmac passphrase digest)
     with hmac-length = (digest-length digest)
     with key = (make-array key-length :element-type '(unsigned-byte 8)
                            :initial-element 0)
     with key-position = 0
     with count-buffer = (make-array 4 :element-type '(unsigned-byte 8))
     with hmac-out = (make-array hmac-length :element-type '(unsigned-byte 8))
     while (plusp key-length)
     do (let ((size (min hmac-length key-length)))
          (reinitialize-instance hmac :key passphrase)
          (update-hmac hmac salt)
          (setf (ub32ref/be count-buffer 0) count)
          (update-hmac hmac count-buffer)
          (hmac-digest hmac :buffer hmac-out)
          (xor-block size hmac-out 0 key key-position key key-position)
          (loop for i from 1 below iteration-count
             do
               (reinitialize-instance hmac :key passphrase)
               (update-hmac hmac hmac-out)
               (hmac-digest hmac :buffer hmac-out)
               (xor-block size hmac-out 0 key key-position key key-position)
             finally
               (decf key-length size)
               (incf key-position size)
               (incf count)))
     finally (return key)))

(defgeneric digest-sequence (digest-spec sequence &rest args &key start end digest digest-start)
  (:documentation "Return the digest of the subsequence of SEQUENCE
specified by START and END using the algorithm DIGEST-NAME.  For CMUCL
and SBCL, SEQUENCE can be any vector with an element-type
of (UNSIGNED-BYTE 8); for other implementations, SEQUENCE must be a
(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).

If DIGEST is provided, the digest will be placed into DIGEST starting at
DIGEST-START.  DIGEST must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
An error will be signaled if there is insufficient room in DIGEST."))


(define-compiler-macro digest-sequence (&whole form &environment env
                                               name sequence &rest keys)
  (declare (ignore env))
  (maybe-expand-one-shot-call form 'digest-sequence name sequence keys))

(defmethod digest-sequence ((digest-name symbol) sequence &rest kwargs)
  (apply #'digest-sequence (make-digest digest-name) sequence kwargs))

(defmethod digest-sequence (state sequence &key (start 0) end
                            digest (digest-start 0))
  #+(or cmu sbcl)
  (locally
      (declare (type (vector (unsigned-byte 8)) sequence) (type index start))
    ;; respect the fill-pointer
    (let ((end (or end (length sequence))))
      (declare (type index end))
      (#+cmu lisp::with-array-data
       #+sbcl sb-kernel:with-array-data ((data sequence) (real-start start) (real-end end))
        (declare (ignore real-end))
        (update-digest state data
                       :start real-start :end (+ real-start (- end start))))))
  #-(or cmu sbcl)
  (let ((real-end (or end (length sequence))))
    (update-digest state sequence
                   :start start :end (or real-end (length sequence))))
  (produce-digest state :digest digest :digest-start digest-start))

;;; These four functions represent the common interface for digests in
;;; other crypto toolkits (OpenSSL, Botan, Python, etc.).  You obtain
;;; some state object for a particular digest, you update it with some
;;; data, and then you get the actual digest.  Flexibility is the name
;;; of the game with these functions.
(defun make-digest (digest-name &rest keys &key &allow-other-keys)
  "Return a digest object which uses the algorithm DIGEST-NAME."
  (typecase digest-name
    (symbol
     (let ((name (massage-symbol digest-name)))
       (if (digestp name)
           (apply (the function (get name '%make-digest)) keys)
           (error 'unsupported-digest :name digest-name))))
    (t
     (error 'type-error :datum digest-name :expected-type 'symbol))))
