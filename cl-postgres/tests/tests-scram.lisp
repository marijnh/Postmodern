;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES-TESTS; -*-
(in-package :cl-postgres-tests)

(def-suite :cl-postgres-scram
    :description "Scram Test suite for cl-postgres"
    :in :cl-postgres)

(in-suite :cl-postgres-scram)

;; From RFC 7677
;;   This is a simple example of a SCRAM-SHA-256 authentication exchange
;;    when the client doesn't support channel bindings.  The username
;;    'user' and password 'pencil' are being used.

;;    C: n,,n=user,r=rOprNGfwEbeRWgbNEkqO

;;    S: r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096

;;    C: c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ=

;;    S: v=6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4=

(test string-to-usb-array
      (is (equalp (cl-base64:base64-string-to-usb8-array
                   (cl-base64:string-to-base64-string "r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096"))
                  #(114 61 102 121 107 111 43 100 50 108 98 98 70 103 79 78 82 118 57 113 107 120
                    100 97 119 76 51 114 102 99 78 72 89 74 89 49 90 86 118 87 86 115 55 106 44
                    115 61 81 83 88 67 82 43 81 54 115 101 107 56 98 102 57 50 44 105 61 52 48 57
                    54))))


(test c-nonce-string-to-byte-array
      (is (equalp (cl-base64:base64-string-to-usb8-array (cl-base64:string-to-base64-string "fyko+d2lbbFgONRv9qkxdawL"))
                  #(102 121 107 111 43 100 50 108 98 98 70 103 79 78 82 118 57 113 107 120 100 97
                    119 76))))

(test ironclad-ascii-string-to-byte-array
      (is (equalp (ironclad:ascii-string-to-byte-array "QSXCR+Q6sek8bf92")
                  #(81 83 88 67 82 43 81 54 115 101 107 56 98 102 57 50))))


(test utf-8-bytes-to-string
      (is (equalp (cl-postgres-trivial-utf-8:utf-8-bytes-to-string (ironclad:ascii-string-to-byte-array "QSXCR+Q6sek8bf92"))
                  "QSXCR+Q6sek8bf92")))

(test make-octet-vector
  (is (equalp (cl-postgres::make-octet-vector 2)
              #(0 0))))

(test pad-octet-vector
  (is (equalp (cl-postgres::pad-octet-vector #(2 4 6))
              #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 4 6)))

  (is (equalp (cl-postgres::pad-octet-vector #(2 4 6) 16)
             #(0 0 0 0 0 0 0 0 0 0 0 0 0 2 4 6)))

  (is (equalp  (cl-postgres::pad-octet-vector #(2 4 6) 8)
             #(0 0 0 0 0 2 4 6))))

(test gen-client-nonce
  (is (stringp (cl-postgres::gen-client-nonce)))
  (is (equal (length (cl-postgres::gen-client-nonce))
             32)))

(test gen-client-initial-response
  (is (string= (cl-postgres::gen-client-initial-response "admin" "TqL5wJvVhC22JeHZKK9BPsVIp778bqm7")
               "n,,n=admin,r=TqL5wJvVhC22JeHZKK9BPsVIp778bqm7")))

(test split-server-response
  (is (equalp (cl-postgres::split-server-response
               #(114 61 102 121 107 111 43 100 50 108 98 98 70 103 79 78 82 118 57 113 107 120
                 100 97 119 76 51 114 102 99 78 72 89 74 89 49 90 86 118 87 86 115 55 106 44
                 115 61 81 83 88 67 82 43 81 54 115 101 107 56 98 102 57 50 44 105 61 52 48 57
                 54))
              '(("r" . "fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j")
                ("s" . "QSXCR+Q6sek8bf92") ("i" . "4096")))))

(test validate-server-nonce
  (is (not (cl-postgres::validate-server-nonce "fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"
                                               "fyko+d2lbbFgONRv9qkxdawL")))
  (signals error (cl-postgres::validate-server-nonce "fyko+d2lbbFgONRv9qkxd3awL3rfcNHYJY1ZVvWVs7"
                                               "fyko+d2lbbFgONRv9qkxdawL")))

(test parse-scram-server-first-response
  (multiple-value-bind (r s i)
      (cl-postgres::parse-scram-server-first-response
       #(114 61 102 121 107 111 43 100 50 108 98 98 70 103 79 78 82 118 57 113 107 120
         100 97 119 76 51 114 102 99 78 72 89 74 89 49 90 86 118 87 86 115 55 106 44
         115 61 81 83 88 67 82 43 81 54 115 101 107 56 98 102 57 50 44 105 61 52 48 57
         54)
       "fyko+d2lbbFgONRv9qkxdawL")
    (is (equal r "fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j"))
    (is (equal s "QSXCR+Q6sek8bf92"))
    (is (eql i 4096))))

(test gen-client-and-stored-keys
  (is (equalp (cl-postgres::gen-client-key
               (cl-postgres::gen-salted-password "pencil" "31f2b148ca94a7e64554" 4096
                                                 :digest :sha256 :salt-type :hex))
              #(49 201 18 138 184 227 228 72 125 143 233 47 101 95 99 204 74 1 45 132 132 171
                244 59 124 60 176 240 196 122 150 162)))
  (is (equalp (cl-postgres::gen-stored-key
               (cl-postgres::gen-client-key
                (cl-postgres::gen-salted-password "pencil" "31f2b148ca94a7e64554" 4096
                                                  :digest :sha256 :salt-type :hex)))
       #(240 250 82 23 183 193 228 224 160 39 197 184 126 115 234 252 91 19 249 237
  179 61 208 119 110 153 228 223 65 224 83 67))))

(test gen-final-message-part-1
  (is (equal (cl-postgres::gen-final-message-part-1 "3rfcNHYJY1ZVvWVs7j")
             "c=biws,r=3rfcNHYJY1ZVvWVs7j")))

(test gen-auth-message
  (is (equal (cl-postgres::gen-auth-message
              "n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"
              "r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096"
              (cl-postgres::gen-final-message-part-1 "3rfcNHYJY1ZVvWVs7j"))
             "n=user,r=fyko+d2lbbFgONRv9qkxdawL,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096,c=biws,r=3rfcNHYJY1ZVvWVs7j")))

(test salting-password
  (let* ((salt-in-hex "74172b96cd9d296b497b")
         (password "pencil")
         (go-hex-string-password "b58fb579cae2a50591a06a807bc0535106f8e1c725ea5ce3b6eb70ca4e2aeb99")
         (salt-as-byte-array (ironclad:hex-string-to-byte-array salt-in-hex)))
    (is (equal (ironclad:byte-array-to-hex-string
                (cl-postgres::gen-salted-password password salt-in-hex 4096 :digest :sha256 :salt-type :hex))
           go-hex-string-password))
    (is (equalp salt-as-byte-array
               #(116 23 43 150 205 157 41 107 73 123)))))

(test computing-client-proof
  (let* ((salt-in-hex "31f2b148ca94a7e64554")
         (user "")
         (password "pencil")
         (salted-password-in-byte-array
           (cl-postgres::gen-salted-password password salt-in-hex 4096
                                             :digest :sha256 :salt-type :hex))
         (client-nonce "MQiVmMEKTBZgNA==")
         (client-key-in-byte-array (cl-postgres::gen-client-key salted-password-in-byte-array))
         (stored-key-in-byte-array (cl-postgres::gen-stored-key
                                    client-key-in-byte-array))
         (client-initial-response (cl-postgres::gen-client-initial-response user client-nonce ))
         (server-initial-response "r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==,s=MfKxSMqUp+ZFVA==,i=4096")
         (server-initial-response-in-byte-array (ironclad:ascii-string-to-byte-array
                                                 server-initial-response))
         (split-server-initial-response
           (cl-postgres::split-server-response server-initial-response-in-byte-array))
         (server-nonce (cdar split-server-initial-response))
         (client-final-message-part-1
           (cl-postgres::gen-final-message-part-1 "MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ=="))
         (auth-message (cl-postgres::gen-auth-message client-initial-response server-initial-response
                                                      client-final-message-part-1))
         (client-signature (cl-postgres::gen-client-signature stored-key-in-byte-array auth-message))
         (client-proof (cl-postgres::gen-client-proof client-key-in-byte-array client-signature)))
    (is (equalp salted-password-in-byte-array
                #(92 147 248 203 42 239 13 95 237 254 246 114 208 17 250 132 171 224 250 166 2
                  149 120 179 73 54 208 167 52 149 139 123)))
    (is (equalp client-key-in-byte-array
                #(49 201 18 138 184 227 228 72 125 143 233 47 101 95 99 204 74 1 45 132 132 171
                  244 59 124 60 176 240 196 122 150 162)))
    (is (equalp stored-key-in-byte-array
                #(240 250 82 23 183 193 228 224 160 39 197 184 126 115 234 252 91 19 249 237
                  179 61 208 119 110 153 228 223 65 224 83 67)))
    (is (equal client-initial-response "n,,n=,r=MQiVmMEKTBZgNA=="))
    (is (equalp (cl-postgres::split-server-response
                 (ironclad:ascii-string-to-byte-array
                  server-initial-response))
                '(("r" . "MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==") ("s" . "MfKxSMqUp+ZFVA==")
                  ("i" . "4096"))))
    (is (equal server-nonce "MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ=="))
    (is (equal client-final-message-part-1
               "c=biws,r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ=="))
    (is (equal auth-message
               "n=,r=MQiVmMEKTBZgNA==,r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==,s=MfKxSMqUp+ZFVA==,i=4096,c=biws,r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ=="))
    (is (equalp client-signature
               #(238 221 3 125 17 11 114 158 227 71 15 22 56 124 63 254 144 33 36 144 221 85
                 86 84 51 246 66 30 235 63 38 201)))
    (is (equalp client-proof
               #(223 20 17 247 169 232 150 214 158 200 230 57 93 35 92 50 218 32 9 20 89 254
                 162 111 79 202 242 238 47 69 176 107)))
    (is (equal (cl-base64:usb8-array-to-base64-string client-proof)
              "3xQR96noltaeyOY5XSNcMtogCRRZ/qJvT8ry7i9FsGs="))))
