(defsystem "s-sql"
  :depends-on ("cl-postgres")
  :components
  ((:module "s-sql"
    :components ((:file "s-sql")))))
