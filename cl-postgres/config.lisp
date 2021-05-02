;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :cl-postgres)

(defparameter *use-binary-parameters* nil
  "Postmodern defaults to passing query parameters as text. If Postgresql does not have a table column which would allow it to determine the appropriate data type and you do not specify the date type in the query, Postgresql treats the parameter as text. In other words:

    (query \"select $1\" 1 :single)
    \"1\"

   (query \"select $1::integer\" 1 :single)
   1

This parameter provides an optional setting which will cause Postmodern to pass parameters to Postgresql in binary format if that format is available for that datatype. Currently this means int2, int4, int8, float, double-float (except clisp) and boolean. Rational numbers continue to be passed as text.

The default for cl-postgres/Postmodern is to continue to pass parameters to Postgresql as text (not in binary format) in order to avoid breaking existing user code. If you want to pass parameters to Postgresql in binary format, you can either:

    (setf cl-postgres:*use-binary-parameters* t)

or use postmodern's use-binary-parameters function:

    (pomo:use-binary-parameters t)")

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
