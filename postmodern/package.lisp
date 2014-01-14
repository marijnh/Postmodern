(defpackage :postmodern
  (:use #-postmodern-use-mop :common-lisp
        #+postmodern-use-mop :closer-common-lisp
        :s-sql :cl-postgres)
  (:nicknames :pomo)

  #+postmodern-use-mop
  (:export
   #:dao-class #:dao-exists-p #:dao-keys #:query-dao #:select-dao #:get-dao #:do-query-dao #:do-select-dao
   #:with-column-writers
   #:insert-dao #:update-dao #:save-dao #:save-dao/transaction #:upsert-dao #:delete-dao #:make-dao
   #:define-dao-finalization
   #:dao-table-name #:dao-table-definition
   #:\!dao-def #:*ignore-unknown-columns*)

  (:export
   #:connect #:disconnect #:reconnect
   #:call-with-connection #:with-connection
   #:*database* #:connected-p #:database-connection
   #:connect-toplevel #:disconnect-toplevel
   #:clear-connection-pool #:*max-pool-size* #:*default-use-ssl*
   #:query #:execute #:doquery
   #:prepare #:defprepared #:defprepared-with-names
   #:sequence-next #:list-sequences #:sequence-exists-p
   #:list-tables #:table-exists-p #:table-description
   #:list-views #:view-exists-p
   #:*current-logical-transaction* #:with-transaction #:commit-transaction #:abort-transaction
   #:with-savepoint #:rollback-savepoint #:release-savepoint
   #:with-logical-transaction #:ensure-transaction
   #:abort-hooks #:commit-hooks
   #:db-null #:coalesce

   #:deftable #:*table-name* #:*table-symbol*
   #:create-table #:create-all-tables #:create-package-tables
   #:\!index #:\!unique-index #:\!foreign #:\!unique
   #:create-schema #:drop-schema #:list-schemata
   #:with-schema #:schema-exist-p #:set-search-path #:get-search-path

   ;; Reduced S-SQL interface
   #:sql #:sql-compile
   #:smallint #:bigint #:numeric #:real #:double-precision
   #:bytea #:text #:varchar
   #:*escape-sql-names-p* #:sql-escape-string #:sql-escape #:register-sql-operators
   #:sql-error

   ;; Condition type from cl-postgres
   #:database-error #:database-error-message #:database-error-code
   #:database-error-detail #:database-error-query #:database-error-cause
   #:database-connection-error #:database-error-constraint-name))

(in-package :postmodern)
