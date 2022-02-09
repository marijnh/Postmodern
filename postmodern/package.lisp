;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER; -*-
(defpackage :postmodern
  (:use #-postmodern-use-mop :common-lisp
        #+postmodern-use-mop :closer-common-lisp
        :s-sql :cl-postgres)
  (:nicknames :pomo)

  #+postmodern-use-mop
  (:export
   #:dao-class #:dao-exists-p #:dao-keys #:query-dao #:select-dao #:get-dao
   #:fetch-defaults
   #:do-query-dao #:do-select-dao
   #:with-column-writers
   #:insert-dao #:update-dao #:save-dao #:save-dao/transaction #:upsert-dao
   #:delete-dao #:make-dao
   #:define-dao-finalization
   #:dao-table-name #:dao-table-definition
   #:\!dao-def #:*ignore-unknown-columns*
   #:class-finalized-p
   #:finalize-inheritance
   #:find-dao-column-slot
   #:col-type-text-p
   #:find-col-type)

  (:export
   #:connect
   #:disconnect
   #:reconnect
   #:call-with-connection
   #:with-connection
   #:*database*
   #:connected-p
   #:database-connection
   #:connect-toplevel
   #:disconnect-toplevel
   #:change-toplevel-database
   #:clear-connection-pool
   #:*max-pool-size*
   #:*default-use-ssl*
   #:list-connections
   #:connection-use-binary
   #:use-binary-parameters
   #:query
   #:execute
   #:doquery
   #:parse-queries
   #:read-queries
   #:execute-file
   #:prepare
   #:defprepared
   #:defprepared-with-names
   #:*current-logical-transaction*
   #:*isolation-level*
   #:with-transaction
   #:commit-transaction
   #:commit-logical-transaction
   #:abort-transaction
   #:rollback-transaction
   #:abort-logical-transaction
   #:with-savepoint
   #:rollback-savepoint
   #:release-savepoint
   #:with-logical-transaction
   #:ensure-transaction
   #:ensure-transaction-with-isolation-level
   #:abort-hooks
   #:commit-hooks
   #:retry-transaction
   #:deftable
   #:*table-name*
   #:*table-symbol*
   #:create-table
   #:create-all-tables
   #:create-package-tables
   #:\!index
   #:\!unique-index
   #:\!foreign
   #:\!unique
   #:set-search-path
   #:get-search-path
   #:get-database-comment
   #:encode-json-to-string
   #:load-uuid-extension

   ;; Prepared Statement Functions
   #:*allow-overwriting-prepared-statements*
   #:*enforce-parameter-types*
   #:prepared-statement-exists-p
   #:list-prepared-statements
   #:drop-prepared-statement
   #:list-postmodern-prepared-statements
   #:find-postmodern-prepared-statement
   #:find-postgresql-prepared-statement
   #:reset-prepared-statement
   #:get-pid
   #:cancel-backend
   #:terminate-backend
   #:get-pid-from-postmodern

   ;; Reduced S-SQL interface
   #:sql #:sql-compile
   #:smallint #:bigint #:numeric #:real #:double-precision
   #:serial #:serial8
   #:bytea #:text #:varchar
   #:*escape-sql-names-p*
   #:sql-escape-string
   #:sql-escape
   #:register-sql-operators
   #:sql-error
   #:from-sql-name
   #:to-sql-name
   #:db-null

   ;; Condition type from cl-postgres
   #:database-error
   #:database-error-message
   #:database-error-code
   #:database-error-detail
   #:database-error-query
   #:database-error-cause
   #:database-connection-error
   #:database-error-constraint-name
   #:database-error-extract-name

   ;; Utility Functions
   ;; columns
   #:list-columns
   #:list-columns-with-types
   #:column-exists-p
   #:rename-column
   ;; constraints
   #:list-all-constraints
   #:describe-constraint
   #:describe-foreign-key-constraints
   ;; database-management
   #:create-database
   #:database-version
   #:postgresql-version
   #:current-database
   #:num-records-in-database
   #:database-exists-p
   #:database-size
   #:drop-database
   #:list-databases
   #:list-templates
   #:list-available-collations
   #:list-database-access-rights
   #:find-comments

   ;; extensions
   #:list-available-extensions
   #:list-installed-extensions
   ;; functions
   #:list-database-functions
   ;; indices
   #:list-indices
   #:index-exists-p
   #:create-index
   #:drop-index
   #:list-table-indices
   #:list-indexed-column-and-attributes
   #:list-index-definitions
   ;; keys
   #:list-foreign-keys
   #:list-unique-or-primary-constraints
   #:find-primary-key-info
   #:find-primary-key-column
   ;; roles
   #:list-roles
   #:list-role-permissions
   #:role-exists-p
   #:create-role
   #:drop-role
   #:alter-role-search-path
   #:change-password
   #:grant-role-permissions
   #:grant-readonly-permissions
   #:grant-editor-permissions
   #:grant-admin-permissions
   #:revoke-all-on-table
   #:list-role-accessible-databases

   #:list-database-users ;deprecated

   ;; schemas
   #:list-schemata ;
   #:list-schemas
   #:create-schema
   #:drop-schema
   #:with-schema
   #:schema-exists-p
   #:get-schema-comment
   ;; sequences
   #:sequence-next
   #:list-sequences
   #:sequence-exists-p
   #:create-sequence
   #:drop-sequence
   ;; tables
   #:list-tables
   #:list-all-tables
   #:table-exists-p
   #:table-description
   #:table-description-plus
   #:table-description-menu
   #:list-table-sizes
   #:table-size
   #:list-tables-in-schema
   #:drop-table
   #:get-table-oid
   #:get-table-comment
   #:get-column-comments
   #:get-column-comment
   #:get-all-table-comments
   #:rename-table
   #:list-check-constraints
   ;; tablespaces
   #:list-tablespaces
   ;; triggers
   #:describe-triggers
   #:list-triggers
   #:list-detailed-triggers
   ;; util
   #:add-comment
   #:find-comments
   #:list-available-types
   #:cache-hit-ratio
   #:bloat-measurement
   #:unused-indexes
   #:check-query-performance
   #:coalesce
   #:split-fully-qualified-tablename
   #:postgres-array-string-to-list
   #:postgres-array-string-to-array
   #:valid-sql-identifier-p
   ;; views
   #:list-views #:view-exists-p
   #:describe-views))

(in-package :postmodern)
