(in-package :cl-postgres)

;; Ascii variants of some trivial-utf-8 functions.
(when (eq *read-string* 'read-ascii-string)
  (defun read-ascii-string (stream &key null-terminated byte-length)
    "Read an ascii-string from a byte stream, until either a null byte
is reached or the given amount of bytes have been read."
    (declare (type stream stream)
             (type fixnum byte-length)
             #.*optimize*)
    (let ((bytes-read 0)
          (string (make-array 64 :element-type 'character
                              :adjustable t :fill-pointer 0)))
      (loop
       (when (and byte-length (>= bytes-read byte-length))
         (return))
       (let ((next-char (read-byte stream)))
         (incf bytes-read)
         (when (and null-terminated (eq next-char 0))
           (return))
         (vector-push-extend (code-char next-char) string)))
      string))

  (defun ascii-string-bytes (string)
    "Convert an ascii string to an array of octets."
    (map '(vector (unsigned-byte 8) *) 'char-code string))

  (defun write-ascii-string (string stream)
    "Write an ascii string to a stream."
    (declare (type stream stream)
             (type string string)
             #.*optimize*)
    (loop :for char :of-type character :across string
          :do (write-byte (char-code char) stream))))

;; These are used to synthesize reader and writer names for integer
;; reading/writing functions when the amount of bytes and the
;; signedness is known. Both the macro that creates the functions and
;; some macros that use them create names this way.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun integer-reader-name (bytes signed)
    (intern (format nil "READ-~AINT~A" (if signed "" "U") bytes)))
  (defun integer-writer-name (bytes signed)
    (intern (format nil "WRITE-~AINT~A" (if signed "" "U") bytes))))

(defmacro integer-reader (bytes signed)
  "Create a function to read integers from a binary stream."
  (let* ((bits (* bytes 8))
         (return-form (if signed
                          `(if (logbitp ,(1- bits) result)
                            (dpb result (byte ,(1- bits) 0) -1)
                            result)
                          `result)))
    `(defun ,(integer-reader-name bytes signed) (socket)
      (declare (type stream socket)
               #.*optimize*)
      ,(if (= bytes 1)
           `(let ((result (the fixnum (read-byte socket))))
             (declare (type (unsigned-byte 8) result))
             ,return-form)
           `(let ((result 0))
             (declare (type (unsigned-byte ,bits)))
             ,@(loop :for byte :from (1- bytes) :downto 0
                     :collect `(setf (ldb (byte 8 ,(* 8 byte)) result)
                                (the fixnum (read-byte socket))))
             ,return-form)))))

(defmacro integer-writer (bytes) ;; No signed version needed for now
  "Create a function to write integers to a binary stream."
  (let ((bits (* 8 bytes)))
    `(defun ,(integer-writer-name bytes nil) (socket value)
      (declare (type stream socket)
               (type (unsigned-byte ,bits) value)
               #.*optimize*)
      ,@(if (= bytes 1)
            `((write-byte value socket))
            (loop :for byte :from (1- bytes) :downto 0
                  :collect `(write-byte (ldb (byte 8 ,(* byte 8)) value)
                             socket)))
      (values))))

;; All the instances of the above that we need.

(integer-reader 1 t)
(integer-reader 1 nil)
(integer-reader 2 t)
(integer-reader 2 nil)
(integer-reader 4 t)
(integer-reader 4 nil)
(integer-reader 8 t)
(integer-reader 8 nil)

(integer-writer 1)
(integer-writer 2)
(integer-writer 4)

(defun write-bytes (socket bytes)
  "Write a byte-array to a stream."
  (declare (type stream socket)
           (type (simple-array (unsigned-byte 8)) bytes)
           #.*optimize*)
  (write-sequence bytes socket))

(defun write-str (socket string)
  "Write a null-terminated string to a stream \(encoding it when UTF-8
support is enabled.)."
  (declare (type stream socket)
           (type string string)
           #.*optimize*)
  (#.*write-string* string socket)
  (write-uint1 socket 0))

(defun read-bytes (socket length)
  "Read a byte array of the given length from a stream."
  (declare (type stream socket)
           (type fixnum length)
           #.*optimize*)
  (let ((result (make-array length :element-type '(unsigned-byte 8))))
    (read-sequence result socket)
    result))

(defun read-str (socket)
  "Read a null-terminated string from a stream. Takes care of encoding
when UTF-8 support is enabled."
  (declare (type stream socket)
           #.*optimize*)
  (#.*read-string* socket :null-terminated t))

(defun skip-bytes (socket length)
  "Skip a given number of bytes in a binary stream."
  (declare (type stream socket)
           (type (unsigned-byte 32) length)
           #.*optimize*)
  (dotimes (i length)
    (read-byte socket)))

(defun skip-str (socket)
  "Skip a null-terminated string."
  (declare (type stream socket)
           #.*optimize*)
  (loop :for char :of-type fixnum = (read-byte socket)
        :until (zerop char)))

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
