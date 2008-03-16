(in-package :postmodern)

(defun ensure-prepared (connection id query)
  "Make sure a statement has been prepared for this connection."
  (let ((meta (connection-meta connection)))
    (unless (gethash id meta)
      (setf (gethash id meta) t)
      (prepare-query connection (symbol-name id) query))))

(let ((next-id 0))
  (defun next-statement-id ()
    "Provide unique statement names."
    (incf next-id)
    (intern (format nil "STATEMENT-~A" next-id) :keyword)))

(defun generate-prepared (function-form query format)
  "Helper macro for the following two functions."
  (destructuring-bind (reader result-form) (or (cdr (assoc format *result-styles*))
                                               (error "~S is not a valid result style." format))
    (let ((base `(exec-prepared *database* (symbol-name statement-id)
                  (mapcar 'to-sql-string params) ',reader)))
      `(let ((statement-id (next-statement-id)))
        (,@function-form (&rest params)
          (ensure-prepared *database* statement-id ,(real-query query))
         (,result-form ,base))))))

(defmacro prepare (query &optional (format :rows))
  "Wraps a query into a function that will prepare it once for a
connection, and then execute it with the given parameters. The query
should contain a placeholder \($1, $2, etc) for every parameter."
  (generate-prepared '(lambda) query format))

(defmacro defprepared (name query &optional (format :rows))
  "Like perpare, but gives the function a name instead of returning
it."
  (generate-prepared `(defun ,name) query format))

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
