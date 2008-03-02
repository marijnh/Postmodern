(in-package :cl-postgres)

(defparameter *client-encoding* "UNICODE")

(setf (fdefinition 'enc-byte-length)  (fdefinition 'trivial-utf-8:utf-8-byte-length)
      (fdefinition 'enc-write-string) (fdefinition 'trivial-utf-8:write-utf-8-bytes)
      (fdefinition 'enc-read-string)  (fdefinition 'trivial-utf-8:read-utf-8-string)
      (fdefinition 'enc-string-bytes) (fdefinition 'trivial-utf-8:string-to-utf-8-bytes))
