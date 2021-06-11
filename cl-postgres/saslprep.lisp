;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

(defvar *printable-ascii-chars* '(#\  #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\*
                                  #\+ #\, #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5
                                  #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\? #\@
                                  #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K
                                  #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V
                                  #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_ #\` #\a
                                  #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l
                                  #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w
                                  #\x #\y #\z #\{ #\| #\} #\~))

(define-condition bad-char-error (error)
  ((message
    :initarg :message
    :accessor bad-char-error-message
    :initform nil
    :documentation "Text message indicating what went wrong with the validation.")
   (value
    :initarg :value
    :accessor bad-char-error-value
    :initform nil
    :documentation "The value of the field for which the error is signalled.")
   (normalization-form
    :initarg :normalization-form
    :accessor bad-char-error-normalization-form
    :initform nil
    :documentation "The normalization form for the error was signalled.")))

(defmethod print-object ((object bad-char-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[L~A ~]~S~@[: ~S~]"
            (bad-char-error-normalization-form object)
            (bad-char-error-message object)
            (bad-char-error-value object))))

(defun bad-char-error (message &key value normalization-form)
  (error 'bad-char-error
         :message message
         :value value
         :normalization-form normalization-form))

(defun char-printable-ascii-p (ch)
  "Returns t if the char is printable ascii."
  (member ch *printable-ascii-chars*))

(defun string-printable-ascii-p (str)
  "Returns t if every character in the string is printable ascii."
  (every #'char-printable-ascii-p str))

(defun code-point-printable-ascii-p (int)
  "Returns t if the int is a printable ascii code-point."
  (and (>= int 32)
       (<= int 126)))

(defun char-mapped-to-nothing-p (chr)
  "Returns t if the character should be mapped to nothing per RFC 3454
Table B.1 and RFC 4013"
  (when (not (or (characterp chr) (integerp chr)))
    (bad-char-error "Passing unknown type data to char-mapped-to-nothing-p"
                    :value chr))
  (let ((chr-code-point (if (integerp chr) (coerce chr 'fixnum)
                            (char-code chr))))
    (declare (optimize speed)
             (integer chr-code-point))
    (or (member chr-code-point '(#x00AD #x1806 #x200B #x2060 #xFEFF #x034F
                                 #x180B #x180C #x180D #x200C #x200D))
        (and (>= chr-code-point #xFE00) (<= chr-code-point #xFE0F)))))

(defun char-mapped-to-space-p (chr)
  "If character is mapped to space per RFC 3454 Table C.1.2 and RFC 4013, then
return t, else nil"
  (when (not (or (characterp chr) (integerp chr)))
    (bad-char-error "Passing unknown type data to char-mapped-to-space-p"
                    :value chr))
  (let ((chr-code-point (if (integerp chr) (coerce chr 'fixnum)
                            (char-code chr))))
    (declare (optimize speed)
             (integer chr-code-point))
    (or (member chr-code-point '(#x00A0 #x1680  #x202F #x205F #x3000))
        (and (>= chr-code-point #x2000) (<= chr-code-point #x200B)))))

(defun string-mapped-to-nothing (str)
  "Reads a string and removes any character that should be mapped to nothing per
RFC 3454 and RFC 4013."
  (let ((s1 (coerce str 'simple-vector))
        (lst nil))
    (loop for x across s1 counting x into y do
      (cond ((char-mapped-to-nothing-p x))
            ((characterp x)
             (push x lst))
            (t (return-from string-mapped-to-nothing))))
    (setf lst (nreverse lst))
    (format nil "~{~A~}" lst)))

(defun string-mapped-to-space (str)
  "Reads a string and converts any character which should be mapped to a space
pre RFC 3454 and RFC 4013 to a space."
  (let ((s1 (coerce str 'simple-vector)))
    (loop for x across s1 for y from 0 do
      (when (char-mapped-to-space-p x)
        (setf (aref s1 y) #\Space)))
    (coerce s1 'string)))

(defun saslprep-normalize (str &optional (form :nfkc))
  "Scans string. If any character should be mapped to nothing, it eliminates
that character. If any character is not printable ascii, it returns nil. If
every character remaining after eliminations is printable ascii, it returns the
printable-ascii string. It then calls (uax-15:normalize str form) to normalize
the string based on the provided unicode form, defaulting to :nfkc."
  (when (string-printable-ascii-p str)
    (return-from saslprep-normalize str))
  (setf str (string-mapped-to-nothing str))
  (setf str (string-mapped-to-space str))
  (setf str (uax-15:normalize str form)))
