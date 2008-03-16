(defpackage :s-sql-system
  (:use :common-lisp :asdf))
(in-package :s-sql-system)

(defsystem :s-sql
  :depends-on (:cl-postgres)
  :components 
  ((:module :s-sql
    :components ((:file "s-sql")))))
