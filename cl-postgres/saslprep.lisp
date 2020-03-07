;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

;; For more information about the PostgreSQL scocket protocol, see
;; http://www.postgresql.org/docs/current/interactive/protocol.html

;; Scram Functions following the specifications here:
;; RFC 5802 https://tools.ietf.org/html/rfc5802
;; RFC 7677 https://tools.ietf.org/html/rfc7677


(defun char-mapped-to-nothing-p (chr)
  "Returns t if the character should be mapped to nothing per RFC 3454 Table B.1 and RFC 4013"
;  (gethash ch hsh)
  (when (not (or (characterp chr) (integerp chr)))
    (bad-char-error "Passing unknown type data to char-mapped-to-nothing-p" :value chr))
  (let ((chr-code-point (if (integerp chr) (coerce chr 'fixnum) (char-code chr))))
    (declare (optimize speed)
             (integer chr-code-point))
    (if (or (member chr-code-point '(#x00AD #x1806 #x200B #x2060 #xFEFF #x034F #x180B #x180C #x180D #x200C #x200D))
                    (and (>= chr-code-point #xFE00) (<= chr-code-point #xFE0F)))
        t
        nil)))

(defun char-mapped-to-space-p (chr)
  "If character is mapped to space per RFC 3454 Table C.1.2 and RFC 4013, then return t, else nil"
  (when (not (or (characterp chr) (integerp chr)))
    (bad-char-error "Passing unknown type data to char-mapped-to-space-p" :value chr))
  (let ((chr-code-point (if (integerp chr) (coerce chr 'fixnum) (char-code chr))))
    (declare (optimize speed)
             (integer chr-code-point))
    (if (or (member chr-code-point '(#x00A0 #x1680  #x202F #x205F #x3000))
            (and (>= chr-code-point #x2000) (<= chr-code-point #x200B)))
      t
      nil)))

(defun string-mapped-to-nothing (str)
  "Reads a string and removes any character that should be mapped to nothing per RFC 3454 and RFC 4013."
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
  "Reads a string and converts any character which should be mapped to a space pre RFC 3454 and RFC 4013 to a space."
  (let ((s1 (coerce str 'simple-vector)))
    (loop for x across s1 counting x into y do
         (when (char-mapped-to-space-p x)
           (setf (aref s1 (- y 1)) #\Space)))
    (coerce s1 'string)))

(defun saslprep-normalize (str &optional (form :nfkc))
  "Scans string. If any character should be mapped to nothing, it eliminates that character. If any character is not printable ascii, it returns nil. If every character remaining after eliminations is printable ascii, it returns the printable-ascii string. "
  (when (string-printable-ascii-p str)
    (return-from saslprep-normalize str))
  (setf str (string-mapped-to-nothing str))
  (setf str (string-mapped-to-space str))
  (setf str (uax-15:normalize str form)))
