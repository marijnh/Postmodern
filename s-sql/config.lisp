;;; -*- Mode: Lisp; Base: 10; Syntax: ANSI-Common-Lisp; Package: S-SQL; -*-
(in-package :s-sql)

;; Converting between symbols and SQL strings.

(defparameter *postgres-reserved-words*
  (let ((words (make-hash-table :test 'equal)))
    (dolist (word '("all" "analyse" "analyze" "and" "any" "array" "as" "asc"
                    "asymmetric" "authorization" "between" "binary" "both"
                    "case" "cast" "check" "collate" "column" "concurrently"
                    "constraint" "create" "cross" "current-catalog"
                    "current-date" "current-role" "current-schema"
                    "current-time" "current-timestamp" "current-user" "default"
                    "deferrable" "desc" "distinct" "do" "else" "end" "except"
                    "false" "fetch" "filter"  "for" "foreign" "freeze" "from"
                    "full" "grant" "group" "having" "ilike" "in" "initially"
                    "inner" "intersect" "into" "is" "isnull" "join" "lateral"
                    "leading" "left" "like" "limit" "localtime" "localtimestamp"
                    "natural" "new" "not" "notnull" "nowait" "null" "off"
                    "offset" "old" "on" "only" "or" "order" "outer" "overlaps"
                    "placing" "primary" "references" "returning" "right"
                    "select" "session-user" "Share" "similar" "some" "symmetric"
                    "table" "then" "to" "trailing" "true" "union" "unique"
                    "user" "using" "variadic" "verbose" "when" "where" "window"
                    "with"))
      (setf (gethash word words) t))
    words)
  "A set of all PostgreSQL's reserved words, for automatic escaping. Probably
not a good idea to use these words as identifiers anyway.")

(defparameter *escape-sql-names-p* :auto
  "Determines whether double quotes are added around column, table, and **
function names in queries. Valid values:

- T, in which case every name is escaped,
- NIL, in which case no name is escaped,
- :auto, which causes only reserved words to be escaped, or.
- :literal which is the same as :auto except it has added consequence in
  to-sql-name (see below).

The default value is :auto.

Be careful when binding this with let and such ― since a lot of SQL compilation
tends to happen at compile-time, the result might not be what you expect. Mixed
case sensitivity is not currently well supported. Postgresql itself will
downcase unquoted identifiers. This will be revisited in the future if
requested.")

(defvar *downcase-symbols* t
  "When converting symbols to strings, whether to downcase the symbols is set
here. The default is to downcase symbols.")

(defparameter *standard-sql-strings* nil
  "Indicate whether S-SQL will use standard SQL strings (just use ''
  for #\'), or backslash-style escaping. Setting this to NIL is always
  safe, but when the server is configured to allow standard
  strings (parameter 'standard_conforming_strings' is 'on'), the noise
  in queries can be reduced by setting this to T.")
