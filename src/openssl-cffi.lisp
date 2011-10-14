; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interface to the cryptographic functions in the OpenSSL library
;;;

(cl:defpackage :truledger-openssl
  (:use :cl :cffi :cl-base64)
  (:export
   #:rsa-generate-key
   #:decode-rsa-private-key
   #:encode-rsa-private-key
   #:decode-rsa-public-key
   #:encode-rsa-public-key
   #:rsa-free
   #:sha1
   #:sign
   #:verify
   #:%rsa-privkey-decrypt
   #:%rsa-pubkey-encrypt
   #:rsa-size
   ))

(in-package :truledger-openssl)

(defvar *openssl-process* nil)

(defvar *openssl-lock* (truledger:make-lock "OpenSSL"))

(defmacro with-openssl-lock (() &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-openssl-lock #',thunk))))

(defun call-with-openssl-lock (thunk)
  (let ((process (truledger:current-process)))
    (if (eq process *openssl-process*)
      (funcall thunk)
      (truledger:with-lock-grabbed (*openssl-lock* "OpenSSL Lock")
        (unwind-protect
             (progn
               (setq *openssl-process* process)
               (funcall thunk))
          (setq *openssl-process* nil))))))

;; From cl+ssl
(cffi:define-foreign-library libssl
  (:windows "libssl32.dll")
  (:darwin "libssl.dylib")
  (:openbsd (:or "libssl3.so" "libssl.so.16.0" "libssl.so.15.1"))
  (:unix (:or "libssl.so.0.9.8" "libssl.so" "libssl.so.4" "libssl.so.10"))
  (t (:default "libssl3")))

;; From cl+ssl
(cffi:define-foreign-library libeay32
  (:windows "libeay32.dll"))

(cffi:define-foreign-library libcrypto
  (:darwin (:or "libcrypto.dylib"))
  (:unix (:or "libcrypto.so.0.9.8" "libcrypto.so" "libcrypto.so.4")))

;; This is necessary to successfully start a saved application built
;; with a version of OpenSSL that has libcrypto on a machine whose
;; OpenSSL doesn't have libcrypto, or vice-versa.
(defun clear-eep-addresses ()
  #+ccl
  (let ((libssl (cffi:load-foreign-library 'libssl))
        (libcrypto (ignore-errors (cffi:load-foreign-library 'libcrypto)))
        (cnt 0))
    (when libssl
      (setf libssl (cffi::foreign-library-handle libssl)))
    (when libcrypto
      (setf libcrypto (cffi::foreign-library-handle libcrypto)))
    (maphash (lambda (key eep)
               (declare (ignore key))
               (let ((container (ccl::eep.container eep)))
                 (when (and container
                            (or (eq container libssl)
                                (eq container libcrypto)))
                   (setf (ccl::eep.address eep) nil
                         (ccl::eep.container eep) nil)
                   (incf cnt))))
             ccl::*eeps*)
    (and (> cnt 0) cnt)))

(truledger:add-save-application-function 'clear-eep-addresses)

(defparameter $null (null-pointer))

(defcfun ("OPENSSL_add_all_algorithms_conf" open-ssl-add-all-algorithms) :void
  )

;; This is necessary for reading encrypted private keys
(defun add-all-algorithms ()
  (with-openssl-lock ()
    (open-ssl-add-all-algorithms)))

(defun startup-openssl ()
  (load-foreign-library 'libssl)
  (load-foreign-library 'libeay32)
  ;; libcrypto merged into libssl for recent OpenSSL versions.
  (ignore-errors (load-foreign-library 'libcrypto))
  (open-ssl-add-all-algorithms))

(truledger:add-startup-function 'startup-openssl)
(startup-openssl)

(defconstant $pem-string-rsa "RSA PRIVATE KEY")

(defun d2i-RSAPrivateKey ()
  (foreign-symbol-pointer "d2i_RSAPrivateKey"))

(defcfun ("RSA_free" rsa-free) :void
  (r :pointer))

(defun i2d-RSAPrivateKey ()
  (foreign-symbol-pointer "i2d_RSAPrivateKey"))

;; PHP uses Triple-DES with CBC to encrypt private keys
(defcfun ("EVP_des_ede3_cbc" evp-des-ede3-cbc) :pointer
  )

;; Truledger uses AES-256 with CBC to encrypt private keys
(defcfun ("EVP_aes_256_cbc" evp-aes-256-cbc) :pointer
  )

(defconstant $pem-string-rsa-public "RSA PUBLIC KEY")

(defcfun ("BIO_new" %bio-new) :pointer
  (type :pointer))

(defcfun ("BIO_s_mem" %bio-s-mem) :pointer
  )

(defcfun ("BIO_puts" %bio-puts) :int
  (bp :pointer)
  (buf :pointer))

(defun bio-new-s-mem (&optional string)
  (with-openssl-lock ()
    (let ((res (%bio-new (%bio-s-mem))))
      (when (null-pointer-p res)
        (error "Can't allocate io-mem-buf"))
      (when string
        (with-foreign-strings ((sp string :encoding :latin-1))
          (%bio-puts res sp)))
      res)))

(defmacro with-bio-new-s-mem ((bio &optional string) &body body)
  (let ((thunk (gensym)))
    `(let ((,thunk (lambda (,bio) ,@body)))
       (declare (dynamic-extent ,thunk))
       (call-with-bio-new-s-mem ,string ,thunk))))

(defun call-with-bio-new-s-mem (string thunk)
  (let ((bio (bio-new-s-mem string)))
    (unwind-protect
         (funcall thunk bio)
      (bio-free bio))))

(defcfun ("BIO_ctrl" %bio-ctrl) :long
  (bp :pointer)
  (cmd :long)
  (larg :long)
  (parg :pointer))

(defconstant $BIO-CTRL-INFO 3)

(defun %bio-get-mem-data (bio ptr)
  (%bio-ctrl bio $BIO-CTRL-INFO 0 ptr))  

(defun bio-get-string (bio)
  (with-openssl-lock ()
    (with-foreign-object (p :pointer)
      (let ((len (%bio-get-mem-data bio p)))
        (foreign-string-to-lisp
         (mem-ref p :pointer) :count len :encoding :ascii)))))

(defcfun ("BIO_free" %bio-free) :int
  (a :pointer))

(defun bio-free (bio)
  (with-openssl-lock ()
    (when (eql 0 (%bio-free bio))
      (error "Error freeing bio instance"))))

(defcfun ("PEM_ASN1_read_bio" %pem-asn1-read-bio) :pointer
  (d2i :pointer)
  (name (:pointer :char))
  (bio :pointer)
  (x (:pointer (:pointer :char)))
  (cb :pointer)
  (u :pointer))

(defcfun ("PEM_ASN1_write_bio" %pem-asn1-write-bio) :int
  (i2d (:pointer :int))
  (name (:pointer :char))
  (bio :pointer)
  (x (:pointer :char))
  (enc :pointer)
  (kstr (:pointer :char))
  (klen :int)
  (callback :pointer)
  (u :pointer))

(defun %pem-read-bio-rsa-private-key (bio &optional (x $null) (cb $null) (u $null))
  (with-foreign-strings ((namep $pem-string-rsa :encoding :latin-1))
    (%pem-asn1-read-bio (d2i-RSAPrivateKey) namep bio x cb u)))

(defun %pem-write-bio-rsa-private-key
    (bio x &optional (enc $null) (kstr $null) (klen 0) (cb $null) (u $null))
  (with-foreign-strings ((namep $pem-string-rsa :encoding :latin-1))
    (%pem-asn1-write-bio (i2d-RSAPrivateKey) namep bio x enc kstr klen cb u)))

(defun mem-set-char (val buf &optional (idx 0))
  (setf (mem-ref buf :char idx) val))

(define-condition bad-rsa-key-or-password (simple-error)
  ())

(defun decode-rsa-private-key (string &optional (password ""))
  "Convert a PEM string to an RSA structure. Free it with RSA-FREE.
   Will prompt for the password if it needs it and you provide nil."
  (with-openssl-lock ()
    (with-bio-new-s-mem (bio string)
      (let ((res (if password
                     (with-foreign-strings ((passp password :encoding :latin-1))
                       (prog1 (%pem-read-bio-rsa-private-key bio $null $null passp)
                         (truledger:destroy-password
                          passp (length password) 'mem-set-char)))
                     (%pem-read-bio-rsa-private-key bio))))
        (when (null-pointer-p res)
          (error 'bad-rsa-key-or-password
                 :format-control "Couldn't decode private key from string"))
        res))))

;; Could switch to PEM_write_PKCS8PrivateKey, if PHP compatibility is
;; no longer necessary. It's supposed to be more secure.
(defun encode-rsa-private-key (rsa &optional password)
  "Encode an rsa private key as a PEM-encoded string."
  (with-openssl-lock ()
    (with-bio-new-s-mem (bio)
      (let ((res (if password
                     (with-foreign-strings ((passp password :encoding :latin-1))
                       (prog1 (%pem-write-bio-rsa-private-key
                               bio rsa (evp-aes-256-cbc) passp (length password))
                         (truledger:destroy-password
                          passp (length password) 'mem-set-char)))
                     (%pem-write-bio-rsa-private-key bio rsa))))
        (when (eql res 0)
          (error "Can't encode private key."))
        (bio-get-string bio)))))

(defcfun ("PEM_read_bio_RSA_PUBKEY" %pem-read-bio-rsa-pubkey) :pointer
  (bp :pointer)
  (x :pointer)
  (cb :pointer)
  (u :pointer))

(defcfun ("PEM_write_bio_RSA_PUBKEY" %pem-write-bio-rsa-pubkey) :int
  (bp :pointer)
  (rsa :pointer))

(defun decode-rsa-public-key (string)
  "Convert a PEM-encoded string to an RSA public key.
   You must RSA-FREE the result when you're done with it."
  (with-openssl-lock ()
    (with-bio-new-s-mem (bio string)
      (let ((res (%pem-read-bio-rsa-pubkey bio $null $null $null)))
        (when (null-pointer-p res)
          (error "Couldn't decode public key"))
        res))))

(defun encode-rsa-public-key (rsa)
  "Encode an RSA public or private key to a PEM-encoded public key."
  (with-openssl-lock ()
    (with-bio-new-s-mem (bio)
      (when (eql 0 (%pem-write-bio-rsa-pubkey bio rsa))
        (error "Can't encode RSA public key"))
      (bio-get-string bio))))

;; Return the size in bytes
(defcfun ("RSA_size" rsa-size) :int
  (rsa :pointer))

(defcfun ("RSA_check_key" rsa-check-key) :int
  (rsa :pointer))

(defcfun ("RSA_generate_key" %rsa-generate-key) :pointer
  (bits :int)
  (e :unsigned-long)
  (callback :pointer)
  (cb-arg :pointer))

(defun rsa-generate-key (keylen &optional (exponent 65537))
  "Generate an RSA private key with the given KEYLEN and exponent."
  (with-openssl-lock ()
    (let ((res (%rsa-generate-key keylen exponent $null $null)))
      (when (null-pointer-p res)
        (error "Couldn't generate RSA key"))
      res)))

(defcfun ("SHA1" %sha1) :pointer
  (d :pointer)
  (n :long)
  (md :pointer))

(defun sha1 (string &optional (res-type :hex))
  "Return the sha1 hash of STRING.
   Return a string of hex chars if res-type is :hex, the default,
   a byte-array if res-type is :bytes,
   or a string with 8-bit character values if res-type is :string."
  (check-type res-type (member :hex :bytes :string))
  (with-foreign-pointer (md 20)
    (with-foreign-strings ((d string :encoding :utf-8))
      (with-openssl-lock ()
        (%sha1 d (length string) md)))
    (let* ((byte-array-p (or (eq res-type :hex) (eq res-type :bytes)))
           (res (truledger:copy-memory-to-lisp md 20 byte-array-p)))
      (if (eq res-type :hex)
          (truledger:bin2hex res)
          res))))

(defcfun ("SHA256" %sha256) :pointer
  (d :pointer)
  (n :long)
  (md :pointer))

(defun sha256 (string &optional (res-type :hex))
  "Return the sha256 hash of STRING.
   Return a string of hex chars if res-type is :hex, the default,
   a byte-array if res-type is :bytes,
   or a string with 8-bit character values if res-type is :string."
  (check-type res-type (member :hex :bytes :string))
  (with-foreign-pointer (md 32)
    (with-foreign-strings ((d string :encoding :utf-8))
      (with-openssl-lock ()
        (%sha256 d (length string) md)))
    (let* ((byte-array-p (or (eq res-type :hex) (eq res-type :bytes)))
           (res (truledger:copy-memory-to-lisp md 32 byte-array-p)))
      (if (eq res-type :hex)
          (truledger:bin2hex res)
          res))))

;; Sign and verify
(defcfun ("EVP_PKEY_new" %evp-pkey-new) :pointer)
(defcfun ("EVP_PKEY_free" %evp-pkey-free) :void
  (pkey :pointer))

(defcfun ("EVP_PKEY_set1_RSA" %evp-pkey-set1-rsa) :int
  (pkey :pointer)
  (key :pointer))

(defcfun ("EVP_sha1" %evp-sha1) :pointer)

(defcfun ("EVP_PKEY_size" %evp-pkey-size) :int
  (pkey :pointer))

(defconstant $EVP-MD-CTX-size 32)

(defcfun ("EVP_DigestInit" %evp-sign-init) :int
  (ctx :pointer)
  (type :pointer))

(defcfun ("EVP_DigestUpdate" %evp-sign-update) :int
  (ctx :pointer)
  (d :pointer)
  (cnt :unsigned-int))

(defcfun ("EVP_SignFinal" %evp-sign-final) :int
  (ctx :pointer)
  (sig :pointer)                        ;to EVP_PKEY_size bytes
  (s :pointer)                          ;to int
  (pkey :pointer))

(defcfun ("EVP_VerifyFinal" %evp-verify-final) :int
  (ctx :pointer)
  (sigbuf :pointer)
  (siglen :unsigned-int)
  (pkey :pointer))

(defcfun ("EVP_MD_CTX_cleanup" %evp-md-ctx-cleanup) :int
  (ctx :pointer))

(defmacro with-evp-pkey ((pkey rsa-key &optional public-p) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk (,pkey) ,@body))
       (call-with-evp-pkey #',thunk ,rsa-key ,public-p))))

(defun call-with-evp-pkey (thunk rsa-key public-p)
  (flet ((doit (thunk rsa)
           (let ((pkey (with-openssl-lock () (%evp-pkey-new))))
             (unwind-protect
                  (progn
                    (when (null-pointer-p pkey)
                      (error "Can't allocate private key storage"))
                    (when (eql 0 (with-openssl-lock ()
                                   (%evp-pkey-set1-rsa pkey rsa)))
                      (error "Can't initialize private key storage"))
                    (funcall thunk pkey))
               (unless (null-pointer-p pkey)
                 (with-openssl-lock () (%evp-pkey-free pkey)))))))
    (if public-p
        (truledger:with-rsa-public-key (rsa rsa-key) (doit thunk rsa))
        (truledger:with-rsa-private-key (rsa rsa-key) (doit thunk rsa)))))

(defun sign (data rsa-private-key)
  "Sign the string in DATA with the RSA-PRIVATE-KEY.
   Return the signature BASE64-encoded."
  (check-type data string)
  (with-openssl-lock ()
    (with-evp-pkey (pkey rsa-private-key)
      (let ((type (%evp-sha1)))
        (when (null-pointer-p type)
          (error "Can't get SHA1 type structure"))
        (with-foreign-pointer (ctx $EVP-MD-CTX-size)
          (with-foreign-pointer (sig (1+ (%evp-pkey-size pkey)))
            (with-foreign-pointer (siglen (foreign-type-size :unsigned-int))
              (with-foreign-strings ((datap data :encoding :latin-1))
                (when (or (eql 0 (%evp-sign-init ctx type))
                          (unwind-protect
                               (or (eql 0 (%evp-sign-update
                                           ctx datap (length data)))
                                   (eql 0 (%evp-sign-final ctx sig siglen pkey)))
                            (%evp-md-ctx-cleanup ctx)))
                  (error "Error while signing"))
                ;; Here's the result
                (truledger:base64-encode
                 (truledger:copy-memory-to-lisp
                  sig (mem-ref siglen :unsigned-int) nil))))))))))

(defun verify (data signature rsa-public-key)
  "Verify the SIGNATURE for DATA created by SIGN, using the given RSA-PUBLIC-KEY.
   The private key will work, too."
  (check-type data string)
  (check-type signature string)
  (with-openssl-lock ()
    (with-evp-pkey (pkey rsa-public-key t)
      (let* ((type (%evp-sha1))
             (sig (truledger:base64-decode signature))
             (siglen (length sig)))
        (when (null-pointer-p type)
          (error "Can't get SHA1 type structure"))
        (with-foreign-pointer (ctx $EVP-MD-CTX-size)
          (with-foreign-strings ((datap data :encoding :latin-1)
                                 (sigp sig :encoding :latin-1))
            (when (eql 0 (%evp-sign-init ctx type))
              (error "Can't init ctx for verify"))
            (unwind-protect
                 (progn
                   (when (eql 0 (%evp-sign-update ctx datap (length data)))
                     (error "Can't update ctx for signing"))
                   (let ((res (%evp-verify-final ctx sigp siglen pkey)))
                     (when (eql -1 res)
                       (error "Error in verify"))
                     (not (eql 0 res)))) ; Here's the result
              (%evp-md-ctx-cleanup ctx))))))))

(defcfun ("ERR_get_error" %err-get-error) :unsigned-long)

(defcfun ("ERR_error_string" %err-error-string) :pointer
  (e :unsigned-long)
  (buf :pointer))

(defcfun ("ERR_load_crypto_strings" %err-load-crypto-strings) :void)

(defun get-openssl-errors ()
  (with-openssl-lock ()
    (%err-load-crypto-strings)
    (with-foreign-pointer (buf 120)
      (loop
         for e = (%err-get-error)
         while (not (eql e 0))
         collect (progn
                   (%err-error-string e buf)
                   (foreign-string-to-lisp buf :encoding :latin-1))))))

(defconstant $RSA-PKCS1-PADDING 1)
(defconstant $RSA-PKCS1-PADDING-SIZE 11)

(defcfun ("RSA_public_encrypt" %rsa-public-encrypt) :int
  (flen :int)
  (from :pointer)
  (to :pointer)
  (rsa :pointer)
  (padding :int))

(defcfun ("RSA_private_decrypt" %rsa-private-decrypt) :int
  (flen :int)
  (from :pointer)
  (to :pointer)
  (rsa :pointer)
  (padding :int))

(defun %rsa-pubkey-encrypt (string pubkey &optional
                           (start 0) (end (length string)))
  (assert (>= start 0))
  (assert (<= end (length string)))
  (with-openssl-lock ()
    (let ((flen (- end start)))
      (with-foreign-pointer (from flen)
        (with-foreign-pointer (to (rsa-size pubkey))
          (truledger:copy-lisp-to-memory string from start end)
          (let ((len (%rsa-public-encrypt
                      flen from to pubkey $RSA-PKCS1-PADDING)))
            (when (< len 0)
              (error "Errors from %rsa-public-encrypt: ~s"
                     (get-openssl-errors)))
            (truledger:copy-memory-to-lisp to len nil)))))))

(defun %rsa-privkey-decrypt (string privkey &optional
                            (start 0) (end (length string)))
  (let ((size (rsa-size privkey)))
    (assert (>= start 0))
    (assert (<= end (length string)))
    (assert (= end (+ start size)))
    (with-openssl-lock ()
      (with-foreign-pointer (from size)
        (with-foreign-pointer (to size)
          ;; lisp-string-to-foreign didn't work here,
          ;; likely due to 0 bytes.
          (dotimes (j size)
            (setf (mem-ref from :unsigned-char j)
                  (char-code (aref string (+ start j)))))
          (let ((len (%rsa-private-decrypt
                      size from to privkey $RSA-PKCS1-PADDING)))
            (when (< len 0)
              (error "Errors from %rsa-private-decrypt: ~s"
                     (get-openssl-errors)))
            (truledger:copy-memory-to-lisp to len nil)))))))

;;;
;;; How we get here from crypto-api.lisp
;;;

(defmethod truledger:rsa-generate-key-gf
    ((api (eql :openssl-cffi)) keylen &optional (exponent 65537))
  (rsa-generate-key keylen exponent))

(defmethod truledger:decode-rsa-private-key-gf
    ((api (eql :openssl-cffi)) string &optional (password ""))
  (decode-rsa-private-key string password))

(defmethod truledger:encode-rsa-private-key-gf
    ((api (eql :openssl-cffi)) rsa &optional password)
  (encode-rsa-private-key rsa password))

(defmethod truledger:decode-rsa-public-key-gf
    ((api (eql :openssl-cffi)) string)
  (decode-rsa-public-key string))

(defmethod truledger:encode-rsa-public-key-gf
    ((api (eql :openssl-cffi)) rsa)
  (encode-rsa-public-key rsa))

(defmethod truledger:rsa-free-gf ((api (eql :openssl-cffi)) rsa)
  (rsa-free rsa))

(defmethod truledger:sha1-gf
    ((api (eql :openssl-cffi)) string &optional (res-type :hex))
  (sha1 string res-type))

(defmethod truledger:sha256-gf
    ((api (eql :openssl-cffi)) string &optional (res-type :hex))
  (sha256 string res-type))

(defmethod truledger:sign-gf
    ((api (eql :openssl-cffi)) data rsa-private-key)
  (sign data rsa-private-key))

(defmethod truledger:verify-gf
    ((api (eql :openssl-cffi)) data signature rsa-public-key)
  (verify data signature rsa-public-key))

(defmethod truledger:%rsa-pubkey-encrypt-gf
    ((api (eql :openssl-cffi)) string pubkey &optional
     (start 0) (end (length string)))
  (%rsa-pubkey-encrypt string pubkey start end))

(defmethod truledger:%rsa-privkey-decrypt-gf
    ((api (eql :openssl-cffi)) string pubkey &optional
     (start 0) (end (length string)))
  (%rsa-privkey-decrypt string pubkey start end))

(defmethod truledger:rsa-size-gf ((api (eql :openssl-cffi)) key)
  (rsa-size key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009-2010 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
