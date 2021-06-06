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


;; potential testing help at https://md5calc.com/hash/sha256/,
;; https://coding.tools/base64-encode, https://www.online-toolz.com/tools/text-hex-convertor.php

;; The Following is using SHA1, not SHA256
;; Here is a complete example:
;; Username: user
;; Password: pencil
;; Client generates the initial string "n,,n=user,r=fyko+d2lbbFgONRv9qkxdawL"
;; Server generates the random nonce 3rfcNHYJY1ZVvWVs7j
;; Server replies: r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096
;; The salt (hex): 4125c247e43ab1e93c6dff76
;; Client final message bare: c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j
;; Salted password (hex): 1d96ee3a529b5a5f9e47c01f229a2cb8a6e15f7d
;; Client key (hex): e234c47bf6c36696dd6d852b99aaa2ba26555728



;; Stored key (hex): e9d94660c39d65c38fbad91c358f14da0eef2bd6
;; Auth message: n=user,r=fyko+d2lbbFgONRv9qkxdawL,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096,c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j
;; Client signature (hex): 5d7138c486b0bfabdf49e3e2da8bd6e5c79db613
;; Client proof (hex): bf45fcbf7073d93d022466c94321745fe1c8e13b
;; Server key (hex): 0fe09258b3ac852ba502cc62ba903eaacdbf7d31
;; Server signature (hex): ae617da6a57c4bbb2e0286568dae1d251905b0a4
;; Client final message: c=biws,r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,p=v0X8v3Bz2T0CJGbJQyF0X+HI4Ts=
;; Server final message: v=rmF9pqV8S7suAoZWja4dJRkFsKQ=
;; Server's server signature (hex): ae617da6a57c4bbb2e0286568dae1d251905b0a4

#|
Comparing with Go library tests https://github.com/lib/pq/pull/608/files

Note: hex.DecodeString() in go returns the bytes represented by the hexadecimal string
1. Test salting password
(let* ((salt-in-hex "74172b96cd9d296b497b")
       (password "pencil")
       (go-hex-string-password "b58fb579cae2a50591a06a807bc0535106f8e1c725ea5ce3b6eb70ca4e2aeb99"))
       (equal (ironclad:byte-array-to-hex-string (gen-salted-password "pencil" "74172b96cd9d296b497b" 4096 :digest :sha256 :salt-type :hex))
         "b58fb579cae2a50591a06a807bc0535106f8e1c725ea5ce3b6eb70ca4e2aeb99")
       (equal (salt-as-byte-array (ironclad:hex-string-to-byte-array "74172b96cd9d296b497b"))
              #(116 23 43 150 205 157 41 107 73 123)))

2. Test computing client proof
(let* ((salt-in-hex "31f2b148ca94a7e64554")
       (password "pencil")
       (salted-password-in-byte-array  (gen-salted-password "pencil" "31f2b148ca94a7e64554" 4096 :digest :sha256 :salt-type :hex))
       (client-nonce "MQiVmMEKTBZgNA==") ;; presumably base64 string
       (client-key #(49 201 18 138 184 227 228 72 125 143 233 47 101 95 99 204 74 1 45 132 132 171
                     244 59 124 60 176 240 196 122 150 162))
       (stored-key #(240 250 82 23 183 193 228 224 160 39 197 184 126 115 234 252 91 19 249 237
                     179 61 208 119 110 153 228 223 65 224 83 67))
       (stored-key #(44 60 28 116 166 147 204 191 177 239 14 200 8 14 37 202 252 60 246 48 249 21
  248 172 40 234 161 10 26 203 220 125))
       (client-initial-response "n=,r=MQiVmMEKTBZgNA==")
       (server-initial-response "r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==,s=MfKxSMqUp+ZFVA==,i=4096")
       (server-nonce "MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==") ;;presumably base64 string
       (client-final-message-part-1 "c=biws,r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==")
       (auth-message "n=,r=MQiVmMEKTBZgNA==,r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==,s=MfKxSMqUp+ZFVA==,i=4096,c=biws,r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==")
       (client-signature (gen-client-signature (gen-stored-key (gen-client-key (gen-salted-password "pencil" "31f2b148ca94a7e64554" 4096 :digest :sha256 :salt-type :hex))) "n=,r=MQiVmMEKTBZgNA==,r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==,s=MfKxSMqUp+ZFVA==,i=4096,c=biws,r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ=="))
       (client-signature-calculated #(238 221 3 125 17 11 114 158 227 71 15 22 56 124 63 254 144 33 36 144 221 85
                                      86 84 51 246 66 30 235 63 38 201))
       (client-proof (gen-client-proof (gen-client-key (gen-salted-password "pencil" "31f2b148ca94a7e64554" 4096 :digest :sha256 :salt-type :hex))  (gen-client-signature (gen-stored-key (gen-client-key (gen-salted-password "pencil" "31f2b148ca94a7e64554" 4096 :digest :sha256 :salt-type :hex))) "n=,r=MQiVmMEKTBZgNA==,r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==,s=MfKxSMqUp+ZFVA==,i=4096,c=biws,r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==")))
       (client-proof-calculated-as-bytes #(223 20 17 247 169 232 150 214 158 200 230 57 93 35 92 50 218 32 9 20 89 254
                                           162 111 79 202 242 238 47 69 176 107))
       (client-proof-calculated-as-base64 (cl-base64:usb8-array-to-base64-string #(223 20 17 247 169 232 150 214 158 200 230 57 93 35 92 50 218 32 9 20 89 254
  162 111 79 202 242 238 47 69 176 107))
"3xQR96noltaeyOY5XSNcMtogCRRZ/qJvT8ry7i9FsGs=")
       (go-computed-client-proof "3xQR96noltaeyOY5XSNcMtogCRRZ/qJvT8ry7i9FsGs=") ;; presumably base64 string
       ))


func TestComputeClientProof(t *testing.T) {
	salt, _ := hex.DecodeString()

	salted_password := ComputeSaltedPassword("pencil", salt, 4096)
	auth_message := ComputeAuthMessage(
		[]byte("n=,r=MQiVmMEKTBZgNA=="),
		[]byte("r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ==,s=MfKxSMqUp+ZFVA==,i=4096"),
		[]byte("c=biws,r=MQiVmMEKTBZgNA==8zeUHmzdT2SBnQ=="))

	result_ClientProof := ComputeClientProof(salted_password, auth_message)

	if result_ClientProof !=  {
		t.Errorf("ClientProof was wrong: %s", result_ClientProof)
	}
}

func TestComputeServerSignature(t *testing.T) {
	salt, _ := hex.DecodeString("080f7c0a737897be9f0f")

	salted_password := ComputeSaltedPassword("pencil", salt, 4096)
	auth_message := ComputeAuthMessage(
		[]byte("n=,r=wDIyqexkMXIY7A=="),
		[]byte("r=wDIyqexkMXIY7A==93UKLA23FxSN9Q==,s=CA98CnN4l76fDw==,i=4096"),
		[]byte("c=biws,r=wDIyqexkMXIY7A==93UKLA23FxSN9Q=="))

	result_ServerSignature := ComputeServerSignature(salted_password, auth_message)

	if result_ServerSignature != "IeQ9HCOw5KcB8G3NunvoV9SHHUdNT8YkP/d4FAwd73g=" {
		t.Errorf("ServerSignature was wrong: %s", result_ServerSignature)
	}
}

;; Erlang test https://github.com/epgsql/epgsql
(test erlang
  (let ((user nil)
        (passwd "foobar")
        (client-nonce "9IZ2O01zb9IgiIZ1WJ/zgpJB")
        (server-salt "fs3IXBy7U7+IvVjZ"))
    (is (equal (client-initial-response nil "9IZ2O01zb9IgiIZ1WJ/zgpJB")
               "n,,n=,r=9IZ2O01zb9IgiIZ1WJ/zgpJB"))
    (is (equal (parse-scram-server-first-response
                (cl-base64:base64-string-to-usb8-array
                 (cl-base64:string-to-base64-string "r=9IZ2O01zb9IgiIZ1WJ/zgpJBjx/oIRLs02gGSHcw1KEty3eY,s=fs3IXBy7U7+IvVjZ,i=4096"))
                "9IZ2O01zb9IgiIZ1WJ/zgpJB")
               "9IZ2O01zb9IgiIZ1WJ/zgpJBjx/oIRLs02gGSHcw1KEty3eY"))
    (is  ))
  )




    ServerFirst = <<"r=9IZ2O01zb9IgiIZ1WJ/zgpJBjx/oIRLs02gGSHcw1KEty3eY,s=fs3IXBy7U7+IvVjZ,i=4096">>,
    ClientFinal = <<"c=biws,r=9IZ2O01zb9IgiIZ1WJ/zgpJBjx/oIRLs02gGSHcw1KEty3eY,p=AmNKosjJzS31NTlQ"
                    "YNs5BTeQjdHdk7lOflDo5re2an8=">>,
    ServerFinal = <<"v=U+ppxD5XUKtradnv8e2MkeupiA8FU87Sg8CXzXHDAzw=">>
|#
