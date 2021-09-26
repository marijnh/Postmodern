;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

(defparameter *timestamp-format* :unbound
  "This is used to communicate the format \(integer or float) used for
timestamps and intervals in the current connection, so that the
interpreters for those types know how to parse them.")

(defparameter *sql-readtable* (make-hash-table)
  "The exported special var holding the current read table, a hash
mapping OIDs to instances of the type-interpreter class that contain
functions for retreiving values from the database in text, and
possible binary, form.

For simple use, you will not have to touch this, but it is possible that code
within a Lisp image requires different readers in different situations, in which
case you can create separate read tables.")

(defun interpret-as-text (stream size)
  "This interpreter is used for types that we have no specific
interpreter for -- it just reads the value as a string. \(Values of
unknown types are passed in text form.)"
  (enc-read-string stream :byte-length size))

(defclass type-interpreter ()
  ((oid :initarg :oid :accessor type-interpreter-oid)
   (use-binary :initarg :use-binary :accessor type-interpreter-use-binary)
   (binary-reader :initarg :binary-reader :accessor type-interpreter-binary-reader)
   (text-reader :initarg :text-reader :accessor type-interpreter-text-reader))
  (:documentation "Information about type interpreter for types coming
  back from the database. use-binary is either T for binary, nil for
  text, or a function of no arguments to be called to determine if
  binary or text should be used. The idea is that there will always be
  a text reader, there may be a binary reader, and there may be times
  when one wants to use the text reader."))

(defun interpreter-binary-p (interp)
  "If the interpreter's use-binary field is a function, call it and
return the value, otherwise, return T or nil as appropriate."
  (let ((val (type-interpreter-use-binary interp)))
    (typecase val
      (function (funcall val))
      (t val))))

(defun interpreter-reader (interp)
  "Determine if we went the text or binary reader for this type
interpreter and return the appropriate reader."
  (if (interpreter-binary-p interp)
      (type-interpreter-binary-reader interp)
      (type-interpreter-text-reader interp)))

(let ((default-interpreter (make-instance 'type-interpreter
                                          :oid :default
                                          :use-binary nil
                                          :text-reader #'interpret-as-text)))
  (defun get-type-interpreter (oid)
    "Returns a type-interpreter containing interpretation rules for
this type."
    (gethash oid *sql-readtable* default-interpreter)))

(defun set-sql-reader (oid function &key (table *sql-readtable*) binary-p)
  "Define a new reader for a given type. table defaults to *sql-readtable*.
The reader function should take a single argument, a string, and transform
that into some kind of equivalent Lisp value. When binary-p is true, the reader
function is supposed to directly read the binary representation of the value.
In most cases this is not recommended, but if you want to use it: provide a
function that takes a binary input stream and an integer (the size of the
value, in bytes), and reads the value from that stream. Note that reading
less or more bytes than the given size will horribly break your connection."
  (assert (integerp oid))
  (if function
      (setf (gethash oid table)
            (make-instance
             'type-interpreter
             :oid oid
             :use-binary binary-p
             :binary-reader
             (when binary-p function)
             :text-reader
             (if binary-p
                 'interpret-as-text
                 (lambda (stream size)
                   (funcall function
                            (enc-read-string stream :byte-length size))))))
      (remhash oid table))
  table)

(defmacro binary-reader (fields &body value)
  "A slightly convoluted macro for defining interpreter functions. It
allows two forms. The first is to pass a single type identifier, in
which case a value of this type will be read and returned directly.
The second is to pass a list of lists containing names and types, and
then a body. In this case the names will be bound to values read from
the socket and interpreted as the given types, and then the body will
be run in the resulting environment. If the last field is of type
bytes, string, or uint2s, all remaining data will be read and
interpreted as an array of the given type."
  (let ((stream-name (gensym))
        (size-name (gensym))
        (length-used 0))
    (flet ((read-type (type &optional modifier)
             (ecase type
               (bytes `(read-bytes ,stream-name (- ,size-name ,length-used)))
               (string `(enc-read-string ,stream-name
                                         :byte-length (- ,size-name ,length-used)))
               (uint2s `(let* ((size (/ (- ,size-name ,length-used) 2))
                               (result
                                 (make-array size
                                             :element-type '(unsigned-byte 16)
                                             :initial-element 0)))
                          (dotimes (i size)
                            (setf (elt result i) (read-uint2 ,stream-name)))
                          result))
               (int (assert (integerp modifier))
                (incf length-used modifier)
                `(,(integer-reader-name modifier t) ,stream-name))
               (uint (assert (integerp modifier))
                (incf length-used modifier)
                `(,(integer-reader-name modifier nil) ,stream-name)))))
      `(lambda (,stream-name ,size-name)
         (declare (type stream ,stream-name)
                  (type integer ,size-name)
                  (ignorable ,size-name))
         ,(if (consp fields)
              (progn
                `(let ,(loop :for field :in fields
                            :collect `(,(first field)
                                       ,(apply #'read-type (cdr field))))
                  ,@value))
              (read-type fields (car value)))))))

(defmacro define-interpreter (oid name fields &body value)
  "Shorthand for defining binary readers."
  (declare (ignore name)) ;; Names are there just for clarity
  `(set-sql-reader ,oid (binary-reader ,fields ,@value) :binary-p t))

(define-interpreter oid:+char+ "char" int 1)
(define-interpreter oid:+int2+ "int2" int 2)
(define-interpreter oid:+int4+ "int4" int 4)
(define-interpreter oid:+int8+ "int8" int 8)

(define-interpreter oid:+oid+ "oid" uint 4)

(define-interpreter oid:+bool+ "bool" ((value int 1))
  (if (zerop value) nil t))

(define-interpreter oid:+bytea+ "bytea" bytes)
(define-interpreter oid:+text+ "text" string)
(define-interpreter oid:+bpchar+ "bpchar" string)
(define-interpreter oid:+varchar+ "varchar" string)

(define-interpreter oid:+json+ "json" string)
(define-interpreter oid:+jsonb+ "jsonb" ((version int 1)
                                         (content string))
  (unless (= 1 version)
    (warn "Unexpected JSONB version: ~S." version))
  content)

(defun read-row-value (stream size)
  (declare (type stream stream)
           (type integer size))
  (let ((num-fields (read-uint4 stream)))
    (loop for i below num-fields
          collect (let ((oid (read-uint4 stream))
                        (size (read-int4 stream)))
                    (declare (type (signed-byte 32) size))
                    (if #-abcl (eq size -1)
                        #+abcl (eql size -1)
                        :null
                        (funcall (interpreter-reader (get-type-interpreter oid))
                                 stream size))))))

;; "row" types
(defparameter *read-row-values-as-binary* nil
  "Controls whether row values (as in select row(1, 'foo') ) should be
  received from the database in text or binary form. The default value
  is nil, specifying that the results be sent back as text. Set this
  to t to cause the results to be read as binary.")

(set-sql-reader oid:+record+ #'read-row-value
                :binary-p (lambda ()
                            *read-row-values-as-binary*))

(defmacro with-binary-row-values (&body body)
  "Helper macro to locally set *read-row-values-as-binary* to t while
executing body so that row values will be returned as binary."
  `(let ((*read-row-values-as-binary* t))
     ,@body))

(defmacro with-text-row-values (&body body)
  "Helper macro to locally set *read-row-values-as-binary* to nil while
executing body so that row values will be returned as t."
  `(let ((*read-row-values-as-binary* nil))
     ,@body))

(defun read-binary-bits (stream size)
  (declare (type stream stream)
           (type integer size))
  (let ((byte-count (- size 4))
        (bit-count (read-uint4 stream)))
    (let ((bit-bytes (read-bytes stream byte-count))
          (bit-array (make-array (list bit-count)
                                 :element-type 'bit
                                 :initial-element 0)))
      (loop for i below bit-count
            do (let ((cur-byte (ash i -3))
                     (cur-bit (ldb (byte 3 0) i)))
                 (setf (aref bit-array i)
                       (ldb (byte 1 (logxor cur-bit 7)) (aref bit-bytes cur-byte)))))
      bit-array)))

(set-sql-reader oid:+bit+ #'read-binary-bits :binary-p t)
(set-sql-reader oid:+varbit+ #'read-binary-bits :binary-p t)

(defun read-binary-array-value (stream size)
  (declare (type stream stream)
           (type integer size))
  (let ((num-dims (read-uint4 stream))
        (has-null (read-uint4 stream))
        (element-type (read-uint4 stream)))
    (cond
      ((zerop num-dims)
       ;; Should we return nil or a (make-array nil) when num-dims is
       ;; 0? Returning nil for now.
       nil)
      (t
       (let* ((array-dims
                (loop for i below num-dims
                      collect (let ((dim (read-uint4 stream))
                                    (lb (read-uint4 stream)))
                                (declare (ignore lb))
                                dim)))
              (num-items (reduce #'* array-dims)))
         (let ((results (make-array array-dims)))
           (loop for i below num-items
                 do (let ((size (read-int4 stream)))
                      (declare (type (signed-byte 32) size))
                      (setf (row-major-aref results i)
                            (if #-abcl (eq size -1)
                                #+abcl (eql size -1)
                                :null
                                (funcall
                                 (interpreter-reader
                                  (get-type-interpreter element-type))
                                 stream size)))))
           results))))))

(dolist (oid (list oid:+bool-array+
                   oid:+bytea-array+
                   oid:+char-array+
                   oid:+name-array+ ;; (internal PG type)
                   oid:+int2-array+
                   oid:+int4-array+
                   oid:+text-array+
                   oid:+bpchar-array+
                   oid:+varchar-array+
                   oid:+int8-array+
                   oid:+point-array+
                   oid:+lseg-array+
                   oid:+box-array+
                   oid:+float4-array+
                   oid:+float8-array+
                   oid:+oid-array+
                   oid:+timestamp-array+
                   oid:+date-array+
                   oid:+time-array+
                   oid:+timestamptz-array+
                   oid:+interval-array+
                   oid:+bit-array+
                   oid:+varbit-array+
                   oid:+numeric-array+))
  (set-sql-reader oid #'read-binary-array-value :binary-p t))

;; record arrays
;;
;; NOTE: need to treat this separately because if we want
;; the record (row types) to come back as text, we have to read the
;; array value as text.
(set-sql-reader oid:+record-array+ #'read-binary-array-value
                :binary-p (lambda () *read-row-values-as-binary*))

(define-interpreter oid:+point+ "point" ((point-x-bits uint 8)
                                         (point-y-bits uint 8))
  (list (cl-postgres-ieee-floats:decode-float64 point-x-bits)
        (cl-postgres-ieee-floats:decode-float64 point-y-bits)))

(define-interpreter oid:+lseg+ "lseg" ((point-x1-bits uint 8)
                                       (point-y1-bits uint 8)
                                       (point-x2-bits uint 8)
                                       (point-y2-bits uint 8))
  (list (list (cl-postgres-ieee-floats:decode-float64 point-x1-bits)
              (cl-postgres-ieee-floats:decode-float64 point-y1-bits))
        (list (cl-postgres-ieee-floats:decode-float64 point-x2-bits)
              (cl-postgres-ieee-floats:decode-float64 point-y2-bits))))

(define-interpreter oid:+box+ "box" ((point-x1-bits uint 8)
                                     (point-y1-bits uint 8)
                                     (point-x2-bits uint 8)
                                     (point-y2-bits uint 8))
  (list (list (cl-postgres-ieee-floats:decode-float64 point-x1-bits)
              (cl-postgres-ieee-floats:decode-float64 point-y1-bits))
        (list (cl-postgres-ieee-floats:decode-float64 point-x2-bits)
              (cl-postgres-ieee-floats:decode-float64 point-y2-bits))))

(define-interpreter oid:+float4+ "float4" ((bits uint 4))
  (cl-postgres-ieee-floats:decode-float32 bits))
(define-interpreter oid:+float8+ "float8" ((bits uint 8))
  (cl-postgres-ieee-floats:decode-float64 bits))

;; Numeric types are rather involved. I got some clues on their
;; structure from
;; http://archives.postgresql.org/pgsql-interfaces/2004-08/msg00000.php
(define-interpreter oid:+numeric+ "numeric"
  ((length uint 2)
   (weight int 2)
   (sign int 2)
   (dscale int 2)
   (digits uint2s))
  (declare (ignore dscale))
  (let ((total (loop :for i :from (1- length) :downto 0
                     :for scale = 1 :then (* scale #.(expt 10 4))
                     :summing (* scale (elt digits i))))
        (scale (- length weight 1)))
    (unless (zerop sign)
      (setf total (- total)))
    (/ total (expt 10000 scale))))

;; Since date and time types are the most likely to require custom
;; readers, there is a hook for easily adding binary readers for them.

(defun set-date-reader (f table)
  (set-sql-reader oid:+date+ (binary-reader ((days int 4))
                                            (funcall f days))
                  :table table
                  :binary-p t))

(defun interpret-usec-bits (bits)
  "Decode a 64 bit time-related value based on the timestamp format
used. Correct for sign bit when using integer format."
  (ecase *timestamp-format*
    (:float (round (* (cl-postgres-ieee-floats:decode-float64 bits) 1000000)))
    (:integer (if (logbitp 63 bits)
                  (dpb bits (byte 63 0) -1)
                  bits))))

(defun set-interval-reader (f table)
  (set-sql-reader oid:+interval+
                  (binary-reader ((usec-bits uint 8) (days int 4)
                                  (months int 4))
                                 (funcall f months days
                                          (interpret-usec-bits usec-bits)))
                  :table table
                  :binary-p t))

(defun set-usec-reader (oid f table)
  (set-sql-reader oid
                  (binary-reader ((usec-bits uint 8))
                                 (funcall f (interpret-usec-bits usec-bits)))
                  :table table
                  :binary-p t))

;; Public interface for adding date/time readers

(defun set-sql-datetime-readers (&key date timestamp timestamp-with-timezone
                                   interval time
                                   (table *sql-readtable*))
  "Since there is no widely recognised standard way of representing dates and
times in Common Lisp, and reading these from string representation is clunky
and slow, this function provides a way to easily plug in binary readers for
the date, time, timestamp, and interval types. It should be given functions
with the following signatures:

- :date (days)

Where days is the amount of days since January 1st, 2000.

- :timestamp (useconds)

Timestamps have a microsecond resolution. Again, the zero point is the start
of the year 2000, UTC.

- :timestamp-with-timezone

Like :timestamp, but for values of the 'timestamp with time zone' type (which
PostgreSQL internally stores exactly the same as regular timestamps).

- :time (useconds)

Refers to a time of day, counting from midnight.

- :interval (months days useconds)

An interval is represented as several separate components. The reason that days
and microseconds are separated is that you might want to take leap seconds into
account.



Defaults are provided as follows:
  #'default-date-reader
  #'default-timestamp-reader
  #'default-interval-reader
  #'default-time-reader

e.g.
(defun make-temp-postgres-query-requiring-unix-timestamps ()
  (flet ((temp-timestamp-reader (useconds-since-2000)
            (- (+ +start-of-2000+ (floor useconds-since-2000 1000000))
              (encode-universal-time 0 0 0 1 1 1970 0))))
    (set-sql-datetime-readers
      :date #'temp-timestamp-reader)
    (let ((query (make-postgres-query-requiring-unix-timestamps))
      (set-sql-datetime-readers
        :date #'default-timestamp-reader)
      query))))
"
  (when date (set-date-reader date table))
  (when timestamp (set-usec-reader oid:+timestamp+ timestamp table))
  (when timestamp-with-timezone (set-usec-reader oid:+timestamptz+
                                                 timestamp-with-timezone table))
  (when interval (set-interval-reader interval table))
  (when time (set-usec-reader oid:+time+ time table))
  table)

;; Provide meaningful defaults for the date/time readers.

(defconstant +start-of-2000+ (encode-universal-time 0 0 0 1 1 2000 0))
(defconstant +seconds-in-day+ (* 60 60 24))

(defun default-date-reader (days-since-2000)
  (+ +start-of-2000+ (* days-since-2000 +seconds-in-day+)))

(defun default-timestamp-reader (useconds-since-2000)
  (+ +start-of-2000+ (floor useconds-since-2000 1000000)))

(defun default-interval-reader (months days useconds)
  (multiple-value-bind (sec us) (floor useconds 1000000)
    `((:months ,months) (:days ,days) (:seconds ,sec)
      (:useconds ,us))))

(defun default-time-reader (usecs)
   (multiple-value-bind (seconds usecs)
       (floor usecs 1000000)
     (multiple-value-bind (minutes seconds)
         (floor seconds 60)
       (multiple-value-bind (hours minutes)
           (floor minutes 60)
         `((:hours ,hours) (:minutes ,minutes) (:seconds ,seconds)
           (:microseconds ,usecs))))))

(set-sql-datetime-readers
  :date #'default-date-reader
  :timestamp #'default-timestamp-reader
  :timestamp-with-timezone #'default-timestamp-reader
  :interval #'default-interval-reader
  :time #'default-time-reader)

;; Readers for a few of the array types

(defun read-array-value (transform)
  (declare #.*optimize*)
  (lambda (value)
    (declare (type string value))
    (let ((pos 0))
      (declare (type fixnum pos))
      (labels ((readelt ()
                 (case (char value pos)
                   (#\" (interpret
                         (with-output-to-string (out)
                           (loop :with escaped := nil
                                 :for ch := (char value (incf pos)) :do
                                   (when (and (char= ch #\") (not escaped))
                                     (return))
                                   (setf escaped (and (not escaped)
                                                      (char= ch #\\)))
                                   (unless escaped (write-char ch out)))
                           (incf pos))))
                   (#\{ (incf pos)
                    (unless (char= (char value pos) #\})
                      (loop :for val := (readelt) :collect val :into vals :do
                        (let ((next (char value pos)))
                          (incf pos)
                          (ecase next (#\,) (#\} (return vals)))))))
                   (t (let ((start pos))
                        (loop :for ch := (char value pos) :do
                          (when (or (char= ch #\,) (char= ch #\}))
                            (return (interpret (subseq value start pos))))
                          (incf pos))))))
               (interpret (word)
                 (if (string= word "NULL")
                     :null
                     (funcall transform word))))
        (let* ((arr (readelt))
               (dim (if arr (loop :for x := arr :then (car x) :while (consp x)
                                  :collect (length x))
                        '(0))))
          (make-array dim :initial-contents arr))))))

;; Working with tables.

(defun copy-sql-readtable (&optional (table *sql-readtable*))
  "Copies a given readtable."
  (let ((new-table (make-hash-table)))
    (maphash (lambda (oid interpreter) (setf (gethash oid new-table)
                                             interpreter))
             table)
    new-table))

(defparameter *default-sql-readtable* (copy-sql-readtable *sql-readtable*)
  "A copy of the default readtable that client code can fall back
  on.")

(defun default-sql-readtable ()
  "Returns the default readtable, containing only the readers defined by
CL-postgres itself."
  *default-sql-readtable*)
