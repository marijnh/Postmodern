(defpackage :postmodern
  (:use #-postmodern-use-mop :common-lisp
        #+postmodern-use-mop :closer-common-lisp
        :s-sql :cl-postgres)
  (:nicknames :pomo)

  #+postmodern-use-mop
  (:export
   #:dao-class #:dao-exists-p #:query-dao #:select-dao #:get-dao
   #:insert-dao #:update-dao #:delete-dao
   #:dao-table-name #:dao-table-definition)
   
  (:export 
   #:connect #:disconnect #:reconnect
   #:call-with-connection #:with-connection
   #:*database* #:connected-p #:database-connection
   #:connect-toplevel #:disconnect-toplevel
   #:clear-connection-pool
   #:query #:execute #:doquery
   #:prepare #:defprepared
   #:sequence-next #:list-sequences #:sequence-exists-p
   #:list-tables #:table-exists-p #:table-description
   #:list-views #:view-exists-p
   #:with-transaction #:commit-transaction #:abort-transaction
   #:with-savepoint #:rollback-savepoint #:release-savepoint
   #:db-null #:coalesce

   ;; Reduced S-SQL interface
   #:sql #:sql-compile
   #:smallint #:bigint #:numeric #:real #:double-precision
   #:bytea #:text #:varchar
   #:*escape-sql-names-p* #:sql-escape-string #:register-sql-operators

   ;; Condition type from cl-postgres
   #:database-error #:database-error-message #:database-error-code
   #:database-error-detail #:database-error-query #:database-error-cause
   #:database-connection-error))

(in-package :postmodern)

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
