;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES.FEATURES; -*-
(in-package :cl-postgres.features)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package 'sb-bsd-sockets)
    (pushnew 'sbcl-available *features*)

    (when (find-symbol "INET6-SOCKET" 'sb-bsd-sockets)
      (pushnew 'sbcl-ipv6-available *features*))))
