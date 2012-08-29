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
    (intern (with-standard-io-syntax (format nil "STATEMENT-~A" next-id)) :keyword)))

(defun generate-prepared (function-form query format)
  "Helper macro for the following two functions."
  (destructuring-bind (reader result-form) (reader-for-format format)
    (let ((base `(exec-prepared *database* (symbol-name statement-id) params ,reader)))
      `(let ((statement-id (next-statement-id))
             (query ,(real-query query)))
        (,@function-form (&rest params)
          (ensure-prepared *database* statement-id query)
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

(defmacro defprepared-with-names (name (&rest args)
				  (query &rest query-args)
				  &optional (format :rows))
  "Like defprepared, but with lambda list for statement arguments."
  (let ((prepared-name (gensym "STATEMENT")))
    `(progn
       (defprepared ,prepared-name ,query ,format)
       (defun ,name ,args
	 (,prepared-name ,@query-args)))))
