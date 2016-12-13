(defpackage :cl-postgres.features
  (:use :common-lisp)
  (:export #:sbcl-available
           #:sbcl-ipv6-available))
(in-package :cl-postgres.features)


(eval-when (:compile-toplevel :load-toplevel)
  (when (find-package 'sb-bsd-sockets)
    (pushnew 'sbcl-available *features*)

    (when (find-symbol "INET6-SOCKET" 'sb-bsd-sockets)
      (pushnew 'sbcl-ipv6-available *features*))))
