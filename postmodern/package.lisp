(defpackage :postmodern
  (:use #-postmodern-use-mop :common-lisp
        #+postmodern-use-mop :closer-common-lisp
        :s-sql :cl-postgres)
  (:nicknames :pomo)

  #+postmodern-use-mop
  (:export
   #:dao-class #:dao-exists-p #:dao-keys #:query-dao #:select-dao #:get-dao
   #:with-column-writers
   #:insert-dao #:update-dao #:save-dao #:save-dao/transaction #:delete-dao
   #:dao-table-name #:dao-table-definition
   #:\!dao-def)
   
  (:export 
   #:connect #:disconnect #:reconnect
   #:call-with-connection #:with-connection
   #:*database* #:connected-p #:database-connection
   #:connect-toplevel #:disconnect-toplevel
   #:clear-connection-pool #:*max-pool-size* #:*default-use-ssl*
   #:query #:execute #:doquery
   #:prepare #:defprepared
   #:sequence-next #:list-sequences #:sequence-exists-p
   #:list-tables #:table-exists-p #:table-description
   #:list-views #:view-exists-p
   #:with-transaction #:commit-transaction #:abort-transaction
   #:with-savepoint #:rollback-savepoint #:release-savepoint
   #:db-null #:coalesce

   #:deftable #:*table-name* #:*table-symbol*
   #:create-table #:create-all-tables #:create-package-tables
   #:\!index #:\!unique-index #:\!foreign

   ;; Reduced S-SQL interface
   #:sql #:sql-compile #:sql-fmt
   #:smallint #:bigint #:numeric #:real #:double-precision
   #:bytea #:text #:varchar
   #:*escape-sql-names-p* #:sql-escape-string #:register-sql-operators
   #:sql-error

   ;; Condition type from cl-postgres
   #:database-error #:database-error-message #:database-error-code
   #:database-error-detail #:database-error-query #:database-error-cause
   #:database-connection-error))

(in-package :postmodern)
