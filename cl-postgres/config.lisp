;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

(defparameter *silently-truncate-ratios* t "Given a ratio, a stream and a
digital-length-limit, if *silently-truncate-ratios* is true,
will return a potentially truncated ratio. If false and the digital-length-limit
is reached, it will throw an error noting the loss of precision and offering to
continue or reset *silently-truncate-ratios* to true. Code contributed by
Attila Lendvai.")

(defparameter *query-log* nil "When debugging, it can be helpful to inspect the
queries that are being sent to the database. Set this variable to an output
stream value (*standard-output*, for example) to have CL-postgres log every
query it makes.")
(defparameter *query-callback* 'log-query "When profiling or debugging, the
*query-log* may not give enough information, or reparsing its output may not be
feasible. This variable may be set to a designator of function taking two
arguments. This function will be then called after every query, and receive
query string and internal time units (as in (CL:GET-INTERNAL-REAL-TIME)) spent
in query as its arguments.

Default value of this variable is 'LOG-QUERY, which takes care of *QUERY-LOG*
processing. If you provide custom query callback and wish to keep *QUERY-LOG*
functionality, you will have to call LOG-QUERY from your callback function")

(defvar *retry-connect-times* 5
  "How many times do we try to connect again. Borrowed from pgloader")

(defvar *retry-connect-delay* 0.5
  "How many seconds to wait before trying to connect again. Borrowed from
pgloader")

(defparameter *on-evidence-of-man-in-the-middle-attack* :error
  "If Postmodern sees evidence of an attempted man-in-the-middle attack,
what should Postmodern do? Acceptable values are :error, :warn or :ignore")
