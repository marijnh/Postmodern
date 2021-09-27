;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: POSTMODERN; -*-
(in-package :postmodern)

;;; Timestamp library utilities
(defun simple-date-date-string (item)
  "If the item is a simple-date:date instance, return a string in the form of
yyyy-mm-dd"
  (when (find-package 'simple-date)
    (when (typep item (find-symbol "DATE" :simple-date))
      (multiple-value-bind (year month day)
          (funcall (find-symbol "DECODE-DATE" :simple-date) item)
        (format nil "~a-~a-~a" year month day)))))

(defun simple-date-timestamp-string (item)
  "If the item is a simple-date:timestamp instance, return a string in the
form of yyyy-mm-dd hh:mm:ss:ms"
  (when (find-package 'simple-date)
    (when (typep item (find-symbol "TIMESTAMP" :simple-date))
      (multiple-value-bind (year month day hour minute second millisecond)
          (funcall (find-symbol "DECODE-TIMESTAMP" :simple-date) item)
        (format nil "~a-~a-~a ~a:~a:~a:~a" year month day hour minute
                second millisecond)))))

(defun simple-date-time-of-day-string (item)
  "If the item is a simple-date:timestamp instance, return a string in the
form of yyyy-mm-dd hh:mm:ss:ms"
  (when (find-package 'simple-date)
    (when (typep item (find-symbol "TIME-OF-DAY" :simple-date))
      (multiple-value-bind (hour minute second millisecond)
          (funcall (find-symbol "DECODE-TIME-OF-DAY" :simple-date) item)
        (format nil "~a:~a:~a:~a"  hour minute second millisecond)))))

(defun simple-date-interval-string (item)
  "If the item is a simple-date:timestamp instance, return a string in the
form of P3Y6M4DT12H30M5,3S represents a duration of three years, six months,
four days, twelve hours, thirty minutes, five seconds and 300 milliseconds
following ISO 8601. Note that milliseconds are a decimal fraction added to
seconds and the separator is a comma, not a full stop. If the interval does not
have milliseconds or the milliseconds = 0, that decimal fraction will not be
added."
  (when (find-package 'simple-date)
    (when (typep item (find-symbol "INTERVAL" :simple-date))
      (multiple-value-bind (year month day hour minute second millisecond)
          (funcall (find-symbol "DECODE-INTERVAL" :simple-date) item)
        (if (and millisecond (/= millisecond 0))
            (format nil "P~aY~aM~aDT~aH~aM~a,~S" year month day hour minute
                    second (/ millisecond 100))
            (format nil "P~aY~aM~aDT~aH~aM~aS" year month day hour minute
                       second))))))

(defun local-time-timestamp-string (item)
  "If the item is a simple-date:timestamp instance, return a string in the
form of yyyy-mm-dd hh:mm:ss:ms"
  (when (find-package 'local-time)
    (when (typep item (find-symbol "TIMESTAMP" :local-time))
      (format nil "{~a}"(funcall (find-symbol "FORMAT-TIMESTRING" :local-time) nil item)))))

;;;; Borrowed from cl-json but modified to deal with timestamps, intervals
;;;; and other specific objects
;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; Copyright (c) 2008 Hans Hübner (marked parts)
;;;; All rights reserved.
;;;; MIT License
;;; Symbols

(defun json-intern (string)
  "Intern STRING in the current *JSON-SYMBOLS-PACKAGE*."
  (intern string (or *json-symbols-package* *package*)))

(define-condition unknown-symbol-error (parse-error)
  ((datum :accessor unknown-symbol-error-datum :initarg :datum))
  (:documentation
   "Signalled by safe-json-intern when a symbol that is
not already interned in *json-symbols-package* is found.")
  (:report
   (lambda (condition stream)
     (format stream
             "SAFE-JSON-INTERN only allows previously interned symbols, ~A is not interned in *json-symbols-package*"
             (unknown-symbol-error-datum condition)))))

(defun unknown-symbol-error (string)
  (error 'unknown-symbol-error :datum string))

(defun safe-json-intern (string)
  "The default json-intern is not safe. Interns of many
unique symbols could potentially use a lot of memory.
An attack could exploit this by submitting something that is passed
through that has many very large, unique symbols. This version
is safe in that respect because it only allows symbols that already
exist."
  (or (find-symbol string
                   (or *json-symbols-package* *package*))
      (unknown-symbol-error string)))

(defun map-slots (function object)
  "Call FUNCTION on the name and value of every bound slot in OBJECT."
  (loop for slot in (class-slots (class-of object))
    for slot-name = (slot-definition-name slot)
    if (slot-boundp object slot-name)
      do (funcall function slot-name (slot-value object slot-name))))

(define-condition unencodable-value-error (type-error)
  ((context :accessor unencodable-value-error-context :initarg :context))
  (:documentation
   "Signalled when a datum is passed to ENCODE-JSON (or another
encoder function) which actually cannot be encoded.")
  (:default-initargs :expected-type t)
  (:report
   (lambda (condition stream)
     (with-accessors ((datum type-error-datum)
                      (context unencodable-value-error-context))
         condition
       (format stream
               "Value ~S is not of a type which can be encoded~@[ by ~A~]."
               datum context)))))

(defun unencodable-value-error (value &optional context)
  "Signal an UNENCODABLE-VALUE-ERROR."
  (error 'unencodable-value-error :datum value :context context))

(defmacro with-substitute-printed-representation-restart ((object stream)
                                                          &body body)
  "Establish a SUBSTITUTE-PRINTED-REPRESENTATION restart for OBJECT
and execute BODY."
  `(restart-case (progn ,@body)
     (substitute-printed-representation ()
       (let ((repr (with-output-to-string (s)
                     (write ,object :stream s :escape nil)
                     nil)))
         (write-json-string repr ,stream)))))

(defgeneric encode-json (object &optional stream)
  (:documentation "Write a JSON representation of OBJECT to STREAM and
return NIL."))

(defun encode-json-to-string (object)
  "Return the JSON representation of OBJECT as a string."
  (with-output-to-string (stream)
    (encode-json object stream)))

(defmethod encode-json (anything &optional (stream *json-output*))
  "signal an error which the user can correct by choosing to encode the
string which is the printed representation of the OBJECT."
  (declare (ignore stream))
  (unencodable-value-error anything 'encode-json))

(defmethod encode-json ((nr number) &optional (stream *json-output*))
  "Write the JSON representation of the number NR to STREAM (or to
*JSON-OUTPUT*)."
  (write-json-number nr stream))

(defmethod encode-json ((s string) &optional (stream *json-output*))
  "Write the JSON representation of the string S to STREAM (or to
*JSON-OUTPUT*)."
  (write-json-string s stream))

(defmethod encode-json ((c character) &optional (stream *json-output*))
  "JSON does not define a character type, we encode characters as Strings."
  (encode-json (string c) stream))

(defmethod encode-json ((s symbol) &optional (stream *json-output*))
  "Write the JSON representation of the symbol S to STREAM (or to
*JSON-OUTPUT*).  If S is boolean, a boolean literal is written.
Otherwise, the name of S is passed to *LISP-IDENTIFIER-NAME-TO-JSON*
and the result is written as String."
  (let ((mapped (car (rassoc s +json-lisp-symbol-tokens+))))
    (if mapped
        (progn (write-string mapped stream) nil)
        (let ((s (funcall *lisp-identifier-name-to-json* (symbol-name s))))
          (write-json-string s stream)))))

;;; The code below is from Hans Hübner's YASON (with modifications).

(defun next-aggregate-member (context stream)
  "Between two members of an Object or Array, print a comma separator."
  (if (not (eq context *json-aggregate-context*))
      (error "Member encoder used ~:[outside any~;in inappropriate~] ~
              aggregate environment"
             *json-aggregate-context*))
  (prog1 *json-aggregate-first*
    (unless *json-aggregate-first*
      (write-char #\, stream))
    (setq *json-aggregate-first* nil)))

(defmacro with-aggregate ((context begin-char end-char
                           &optional (stream '*json-output*))
                          &body body)
  "Run BODY to encode a JSON aggregate type, delimited by BEGIN-CHAR
and END-CHAR."
  `(let ((*json-aggregate-context* ',context)
         (*json-aggregate-first* t))
     (declare (special *json-aggregate-context* *json-aggregate-first*))
     (write-char ,begin-char ,stream)
     (unwind-protect (progn ,@body)
       (write-char ,end-char ,stream))))

(defmacro with-array ((&optional (stream '*json-output*)) &body body)
  "Open a JSON Array, run BODY, then close the Array.  Inside the BODY,
AS-ARRAY-MEMBER or ENCODE-ARRAY-MEMBER should be called to encode
Members of the Array."
  `(with-aggregate (array #\[ #\] ,stream) ,@body))

(defmacro as-array-member ((&optional (stream '*json-output*))
                           &body body)
  "BODY should be a program which encodes exactly one JSON datum to
STREAM.  AS-ARRAY-MEMBER ensures that the datum is properly formatted
as a Member of an Array, i. e. separated by comma from any preceding
or following Member."
  `(progn
     (next-aggregate-member 'array ,stream)
     ,@body))

(defun encode-array-member (object &optional (stream *json-output*))
  "Encode OBJECT as the next Member of the innermost JSON Array opened
with WITH-ARRAY in the dynamic context.  OBJECT is encoded using the
ENCODE-JSON generic function, so it must be of a type for which an
ENCODE-JSON method is defined."
  (next-aggregate-member 'array stream)
  (encode-json object stream)
  object)

(defun stream-array-member-encoder (stream
                                    &optional (encoder #'encode-json))
  "Return a function which takes an argument and encodes it to STREAM
as a Member of an Array.  The encoding function is taken from the
value of ENCODER (default is #'ENCODE-JSON)."
  (lambda (object)
    (as-array-member (stream)
      (funcall encoder object stream))))

(defmacro with-object ((&optional (stream '*json-output*)) &body body)
  "Open a JSON Object, run BODY, then close the Object.  Inside the BODY,
AS-OBJECT-MEMBER or ENCODE-OBJECT-MEMBER should be called to encode
Members of the Object."
  `(with-aggregate (object #\{ #\} ,stream) ,@body))

(defmacro as-object-member ((key &optional (stream '*json-output*))
                             &body body)
  "BODY should be a program which writes exactly one JSON datum to
STREAM.  AS-OBJECT-MEMBER ensures that the datum is properly formatted
as a Member of an Object, i. e. preceded by the (encoded) KEY and
colon, and separated by comma from any preceding or following Member."
  `(progn
     (next-aggregate-member 'object ,stream)
     (let ((key (encode-json-to-string ,key)))
       (if (char= (aref key 0) #\")
           (progn (write-string key ,stream) nil)
           (encode-json key ,stream)))
     (write-char #\: ,stream)
     ,@body))

(defun encode-object-member (key value
                             &optional (stream *json-output*))
  "Encode KEY and VALUE as a Member pair of the innermost JSON Object
opened with WITH-OBJECT in the dynamic context.  KEY and VALUE are
encoded using the ENCODE-JSON generic function, so they both must be
of a type for which an ENCODE-JSON method is defined.  If KEY does not
encode to a String, its JSON representation (as a string) is encoded
over again."
  (as-object-member (key stream)
    (encode-json value stream))
  value)

(defun stream-object-member-encoder (stream
                                     &optional (encoder #'encode-json))
  "Return a function which takes two arguments and encodes them to
STREAM as a Member of an Object (String : Value pair)."
  (lambda (key value)
    (as-object-member (key stream)
      (funcall encoder value stream))))

;;; End of YASON code.


;;; You can use the streaming encoder above, or
;;; two differnet types of sexp based encoders below

(defun encode-json-list-guessing-encoder (s stream)
  "Write the JSON representation of the list S to STREAM (or to
*JSON-OUTPUT*).  If S is not encodable as a JSON Array, try to encode
it as an Object (per ENCODE-JSON-ALIST)."
  (restart-case
      (handler-bind ((unencodable-value-error
                       (lambda (e)
                         (with-accessors ((datum type-error-datum)) e
                           (if (and (consp datum)
                                    (ignore-errors (every #'consp s)))
                               (invoke-restart 'try-as-alist)
                               (error e)))))
                     (type-error
                       (lambda (e)
                         (declare (ignore e))
                         (unencodable-value-error s 'encode-json))))
        (write-string
         (with-output-to-string (temp)
           (with-array (temp)
             (mapcar (stream-array-member-encoder temp) s)))
         stream))
    (try-as-alist ()
      (encode-json-alist s stream)))
  (values))

(defun json-bool (value)
  "Intended for the JSON-EXPLICT-ENCODER. Converts a non-nil value
to a value (:true) that creates a json true value when used in the
explict encoder. Or (:false)."
  (if value
      '(:true)
      '(:false)))

(defun json-or-null (value)
  "Intended for the JSON-EXPLICT-ENCODER. Returns a non-nil value
as itself, or a nil value as a json null-value"
  (or value '(:null)))

(defun encode-json-list-explicit-encoder (s stream)
  (handler-bind ((type-error
                  (lambda (e)
                    (declare (ignore e))
                    (unencodable-value-error s 'encode-json))))
    (ecase (car s)
      (:json (mapcar (lambda (str) (write-string str stream))
                     (cdr s)))
      (:true (write-json-chars "true" stream))
      (:false (write-json-chars "false" stream))
      (:null (write-json-chars "null" stream))
      ((:list :array)
       (with-array (stream)
         (mapcar (stream-array-member-encoder stream)
                 (cdr s))))
      (:object (if (consp (cadr s))
                  (encode-json-alist (cdr s) stream)
                  (encode-json-plist (cdr s) stream)))
      (:alist (encode-json-alist (cdr s) stream))
      (:plist (encode-json-plist (cdr s) stream)))
    nil))

(defun use-guessing-encoder ()
  (setf *json-list-encoder-fn* 'encode-json-list-guessing-encoder))

(defun use-explicit-encoder ()
  (setf *json-list-encoder-fn* 'encode-json-list-explicit-encoder))

(defmacro with-local-encoder (&body body)
  `(let (*json-list-encoder-fn*)
     (declare (special *json-list-encoder-fn*))
     ,@body))

(defmacro with-guessing-encoder  (&body body)
  `(with-local-encoder (use-guessing-encoder)
     ,@body))

(defmacro with-explicit-encoder  (&body body)
  `(with-local-encoder (use-explicit-encoder)
     ,@body))

(defmethod encode-json ((s list) &optional (stream *json-output*))
  "Write the JSON representation of the list S to STREAM (or to
*JSON-OUTPUT*), using one of the two rules specified by
first calling USE-GUESSING-ENCODER or USE-EXPLICIT-ENCODER.
The guessing encoder: If S is a list encode S as a JSON Array, if
S is a dotted list encode it as an Object (per ENCODE-JSON-ALIST).
The explicit decoder: If S is a list, the first symbol defines
the encoding:
If (car S) is 'TRUE return a JSON true value.
If (car S) is 'FALSE return a JSON false value.
If (car S) is 'NULL return a JSON null value.
If (car S) is 'JSON princ the strings in (cdr s) to stream
If (car S) is 'LIST or 'ARRAY encode (cdr S) as a a JSON Array.
If (car S) is 'OBJECT encode (cdr S) as A JSON Object,
interpreting (cdr S) either as an A-LIST or a P-LIST."
  (funcall *json-list-encoder-fn* s stream))

(defmethod encode-json ((s sequence) &optional (stream *json-output*))
  "Write the JSON representation (Array) of the sequence S (not an
alist) to STREAM (or to *JSON-OUTPUT*)."
  (with-array (stream)
    (map nil (stream-array-member-encoder stream) s)))

(defmethod encode-json ((h hash-table) &optional (stream *json-output*))
  "Write the JSON representation (Object) of the hash table H to
STREAM (or to *JSON-OUTPUT*)."
  (with-object (stream)
    (maphash (stream-object-member-encoder stream) h)))

#+postmodern-use-mop
(defmethod encode-json ((o standard-object)
                        &optional (stream *json-output*))
  "Check to see if the object is a local-time:timestamp, or a simple-date
timestamp, date or interval. Is so, handle those. If not then
Write the JSON representation (Object) of the CLOS object O to
STREAM (or to *JSON-OUTPUT*)."
  (cond ((typep o (find-symbol "TIMESTAMP" :simple-date))
         (encode-json (simple-date-timestamp-string o) stream))
        ((typep o (find-symbol "TIME-OF-DAY" :simple-date))
         (encode-json (simple-date-time-of-day-string o) stream))
        ((typep o (find-symbol "DATE" :simple-date))
         (encode-json (simple-date-date-string o) stream))
        ((typep o (find-symbol "INTERVAL" :simple-date))
         (encode-json (simple-date-interval-string o) stream))
        ((typep o (find-symbol "TIMESTAMP" :local-time))
         (encode-json (local-time-timestamp-string o) stream))
        (t (with-object (stream)
             (map-slots (stream-object-member-encoder stream) o)))))

(defun encode-json-alist (alist &optional (stream *json-output*))
  "Write the JSON representation (Object) of ALIST to STREAM (or to
*JSON-OUTPUT*).  Return NIL."
  (write-string
   (with-output-to-string (temp)
     (with-object (temp)
       (loop
          with bindings = alist
          do (if (listp bindings)
                 (if (endp bindings)
                     (return)
                     (let ((binding (pop bindings)))
                       (if (consp binding)
                           (destructuring-bind (key . value) binding
                             (encode-object-member key value temp))
                           (unless (null binding)
                             (unencodable-value-error
                              alist 'encode-json-alist)))))
                 (unencodable-value-error alist 'encode-json-alist)))))
   stream)
  nil)

(defun encode-json-alist-to-string (alist)
  "Return the JSON representation (Object) of ALIST as a string."
  (with-output-to-string (stream)
    (encode-json-alist alist stream)))

(defun encode-json-plist (plist &optional (stream *json-output*))
  "Write the JSON representation (Object) of PLIST to STREAM (or to
*JSON-OUTPUT*).  Return NIL."
  (write-string
   (with-output-to-string (temp)
     (with-object (temp)
       (loop
          with properties = plist
          do (if (listp properties)
                 (if (endp properties)
                     (return)
                     (let ((indicator (pop properties)))
                       (if (and (listp properties)
                                (not (endp properties)))
                           (encode-object-member
                            indicator (pop properties) temp)
                           (unencodable-value-error
                            plist 'encode-json-plist))))
                 (unencodable-value-error plist 'encode-json-plist)))))
   stream)
  nil)

(defun encode-json-plist-to-string (plist)
  "Return the JSON representation (Object) of PLIST as a string."
  (with-output-to-string (stream)
    (encode-json-plist plist stream)))

(defun write-json-string (s stream)
  "Write a JSON representation (String) of S to STREAM."
  (write-char #\" stream)
  (if (stringp s)
      (write-json-chars s stream)
      (encode-json s stream))
  (write-char #\" stream)
  nil)

(defun write-json-chars (s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (loop for ch across s
        for code = (char-code ch)
        with special
        if (setq special (car (rassoc ch '((#\" . #\")
                                           (#\\ . #\\)
                                           (#\/ . #\/)
                                           (#\b . #\Backspace)
                                           (#\f . #\)
                                           (#\n . #\Newline)
                                           (#\r . #\Return)
                                           (#\t . #\Tab)
                                           (#\u . (4 . 16))))))
          do (write-char #\\ stream) (write-char special stream)
        else if (< #x1f code #x7f)
               do (write-char ch stream)
        else
          do (let* ((special '#.(rassoc-if #'consp '((#\" . #\")
                                                     (#\\ . #\\)
                                                     (#\/ . #\/)
                                                     (#\b . #\Backspace)
                                                     (#\f . #\)
                                                     (#\n . #\Newline)
                                                     (#\r . #\Return)
                                                     (#\t . #\Tab)
                                                     (#\u . (4 . 16))))))
               (destructuring-bind (esc . (width . radix)) special
                 (format stream "\\~C~V,V,'0R" esc radix width code)))))

(eval-when (:compile-toplevel)
  (if #-clisp (subtypep 'long-float 'single-float)
      #+clisp (subtypep 'long-float 'float)
        ;; only one float type
        (pushnew :cl-json-only-one-float-type *features*)
        ;; else -- we check here only for the case where there are two
        ;; float types, single- and double- --- we don't consider the
        ;; "only single and short" case.  Could be added if necessary.
        (progn
          (when #-clisp (subtypep 'single-float 'short-float)
                #+clisp (subtypep 'float 'short-float)
            (pushnew :cl-json-single-float-is-subsumed *features*))
          (when (subtypep 'long-float 'double-float)
            (pushnew :cl-json-double-float-is-subsumed *features*)))))

(defun write-json-number (nr stream)
  "Write the JSON representation of the number NR to STREAM."
  (typecase nr
    (integer (format stream "~d" nr))
    (real (let ((*read-default-float-format*
                 (etypecase nr
                   (short-float 'short-float)
                   #-clisp (rational 'single-float)
                   #+clisp (rational 'float)
                   #-(or cl-json-single-float-is-subsumed
                         cl-json-only-one-float-type)
                   (single-float 'single-float)
                   #-(or cl-json-double-float-is-subsumed
                         cl-json-only-one-float-type)
                   (double-float 'double-float)
                   #-cl-json-only-one-float-type
                   (long-float 'long-float))))
            (format stream "~f" nr)))
    (t (unencodable-value-error nr 'write-json-number))))


;;; First a simpler version, see testcase json-object-simplified-camel-case
;;; for difference with the ordinary came-case-to-lisp
(defun simplified-camel-case-to-lisp (camel-string)
 "Insert - between lowercase and uppercase chars.
Ignore _ + * and several consecutive uppercase."
 (declare (string camel-string))
 (let ((*print-pretty* nil))
   (with-output-to-string (result)
     (loop for c across camel-string
           with last-was-lowercase
           when (and last-was-lowercase
                     (upper-case-p c))
             do (princ "-" result)
           if (lower-case-p c)
             do (setf last-was-lowercase t)
           else
             do (setf last-was-lowercase nil)
           do (princ (char-upcase c) result)))))


(defun camel-case-split (string)
  "Assume STRING is in camel case, and split it into largest possible
``homogenous'' parts.  A homogenous part consists either a) of
upper-case alphabetic chars; or b) of lower-case alphabetic chars with
an optional initial upper-case; or c) of decimal digits; or d) of a
single non-alphanumeric char.  The return value is a list of
pairs (CATEGORY . PART) where CATEGORY is one of the keywords :UPPER,
:UPPER-1, :LOWER, :NUMERIC, :MIXED, and PART is a substring of
STRING."
  (let ((length (length string)))
    (macrolet ((shift-part (e new-cat &optional subst-cat)
                 `(prog1 (if b (cons ,(or subst-cat 'cat)
                                     (subseq string b ,e)))
                    (setq b ,e cat ,new-cat))))
      (loop for i from 0 to length
         with cat = nil and b = nil
         if (= i length)
           if (shift-part i nil) collect it end
         else if (let ((c (aref string i)))
                   (cond
                     ((upper-case-p c)
                      (case cat
                        ((:upper-1 :upper) (setq cat :upper) nil)
                        (t (shift-part i :upper-1))))
                     ((lower-case-p c)
                      (case cat
                        (:upper-1 (setq cat :mixed) nil)
                        (:upper (let ((subst-cat
                                       (if (> (- i b) 2) :upper :upper-1)))
                                  (shift-part (1- i) :mixed subst-cat)))
                        ((:numeric :punct nil) (shift-part i :lower))))
                     ((digit-char-p c)
                      (if (not (eql cat :numeric))
                          (shift-part i :numeric)))
                     (t (shift-part i :punct))))
           collect it))))

(defun camel-case-transform-all-caps (parts
                                      &optional cat-before from-numeric)
  "Take a list of PARTS (as returned by CAMEL-CASE-SPLIT) and
transform it into a string with Lisp-style hyphenation, assuming that
some initial portion of it does not contain :MIXED parts."
  (if (endp parts)
      (cond (from-numeric (throw 'all-caps nil))
            ((eql cat-before :punct) nil)
            (t '("+")))
      (destructuring-bind ((cat . part) . rest) parts
        (case cat
          ((:lower :mixed) (throw 'all-caps nil))
          (:punct
           (let ((transformed (if (string= part "_") "-" part)))
             (if (or from-numeric (eql cat-before :punct))
                 (cons transformed (camel-case-transform-all-caps rest cat))
                 (let ((transformed-rest
                        (catch 'all-caps
                          (camel-case-transform-all-caps rest cat))))
                   (if transformed-rest
                       (cons transformed transformed-rest)
                       (list* "+"
                              (if (string= part "_") "--" part)
                              (camel-case-transform rest cat)))))))
          ((:upper :upper1)
           (cons part (camel-case-transform-all-caps rest cat nil)))
          (t (cons part (camel-case-transform-all-caps
                         rest cat from-numeric)))))))

(defun camel-case-transform (parts &optional (cat-before :punct))
  "Take a list of PARTS (as returned by CAMEL-CASE-SPLIT) and
transform it into a string with Lisp-style hyphenation, assuming that
some initial portion of it does not contain :UPPER parts."
  (if (endp parts)
      '("")
      (destructuring-bind ((cat . part) . rest) parts
        (case cat
          (:upper
           (if (eql cat-before :punct)
               (let ((transformed-rest
                      (catch 'all-caps
                        (camel-case-transform-all-caps rest cat))))
                 (if transformed-rest
                     (list* "+" part transformed-rest)
                     (list* "+" part "+" (camel-case-transform rest cat))))
               (list* "-+" part "+" (camel-case-transform rest cat))))
          (:upper-1
           (case cat-before
             (:punct
              (let ((transformed-rest
                     (catch 'all-caps
                       (camel-case-transform-all-caps rest cat))))
                (if transformed-rest
                    (list* "+" part transformed-rest)
                    (list* "*" part (camel-case-transform rest cat)))))
             (:numeric (list* "-*" part (camel-case-transform rest cat)))
             (t (list* "-" part (camel-case-transform rest cat)))))
          (:numeric
           (case cat-before
             (:punct
              (let ((transformed-rest
                     (catch 'all-caps
                       (camel-case-transform-all-caps rest cat t))))
                (if transformed-rest
                    (list* "+" part transformed-rest)
                    (cons part (camel-case-transform rest cat)))))
             (t (list* "-" part (camel-case-transform rest cat)))))
          (:mixed
           (list* (case cat-before (:punct "*") (:numeric "-*") (t "-"))
                  (string-upcase part)
                  (camel-case-transform rest cat)))
          (:lower
           (list* (if (eql cat-before :punct) "" "-")
                  (string-upcase part)
                  (camel-case-transform rest cat)))
          (:punct
           (cons (if (string= part "_") "--" part)
                 (camel-case-transform rest cat)))))))

(defun camel-case-to-lisp (string)
  "Take a camel-case string and convert it into a string with
Lisp-style hyphenation."
  (apply #'concatenate 'string
         (camel-case-transform (camel-case-split string))))

(defun lisp-to-camel-case (string)
  "Take a string with Lisp-style hyphentation and convert it to camel
case.  This is an inverse of CAMEL-CASE-TO-LISP."
  (loop with i = 0 and l = (length string)
     with cc-string = (make-string l) and cc-i = 0
     with init = t and cap = nil and all-caps = nil
     while (< i l)
     do (let ((c (aref string i)))
          (unless (case c
                    (#\* (if init (setq cap t)))
                    (#\+ (cond
                           (all-caps (setq all-caps nil init t))
                           (init (setq all-caps t))))
                    (#\- (progn
                           (setq init t)
                           (cond
                             ((or all-caps
                                  (and (< (1+ i) l)
                                       (char= (aref string (1+ i)) #\-)
                                       (incf i)))
                              (setf (aref cc-string cc-i) #\_)
                              (incf cc-i))
                             (t (setq cap t))))))
            (setf (aref cc-string cc-i)
                  (if (and (or cap all-caps) (alpha-char-p c))
                      (char-upcase c)
                      (char-downcase c)))
            (incf cc-i)
            (setq cap nil init nil))
          (incf i))
     finally (return (subseq cc-string 0 cc-i))))
