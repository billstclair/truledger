; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interface to the cryptographic functions in the OpenSSL library
;;;

(in-package :trubanc)

(defparameter $null (null-pointer))

(defcfun ("OPENSSL_add_all_algorithms_conf" open-ssl-add-all-algorithms) :void
  )

(defparameter *libssl*
  (load-foreign-library '(:default "libssl")))

(defparameter *libcrypto*
  (load-foreign-library '(:default "libcrypto")))

;; This is necessary for reading encrypted private keys
(open-ssl-add-all-algorithms)

;; It's also necessary when staring a saved image.
(add-startup-function 'open-ssl-add-all-algorithms)

(defconstant $pem-string-rsa "RSA PRIVATE KEY")

(defparameter $d2i-RSAPrivateKey
  (foreign-symbol-pointer "d2i_RSAPrivateKey"))

(defcfun ("RSA_free" rsa-free) :void
  (r :pointer))

(defparameter $i2d-RSAPrivateKey
  (foreign-symbol-pointer "i2d_RSAPrivateKey"))

(defcfun ("EVP_des_ede3_cbc" evp-des-ede3-cbc) :pointer
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
  (let ((res (%bio-new (%bio-s-mem))))
    (when (null-pointer-p res)
      (error "Can't allocate io-mem-buf"))
    (when string
      (with-foreign-strings ((sp string :encoding :latin-1))
        (%bio-puts res sp)))
    res))

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
  (with-foreign-object (p :pointer)
    (let ((len (%bio-get-mem-data bio p)))
      (foreign-string-to-lisp
       (mem-ref p :pointer) :count len :encoding :ascii))))

(defcfun ("BIO_free" %bio-free) :int
  (a :pointer))

(defun bio-free (bio)
  (when (eql 0 (%bio-free bio))
    (error "Error freeing bio instance")))

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
    (%pem-asn1-read-bio $d2i-RSAPrivateKey namep bio x cb u)))

(defun %pem-write-bio-rsa-private-key
    (bio x &optional (enc $null) (kstr $null) (klen 0) (cb $null) (u $null))
  (with-foreign-strings ((namep $pem-string-rsa :encoding :latin-1))
    (%pem-asn1-write-bio $i2d-RSAPrivateKey namep bio x enc kstr klen cb u)))

(defun decode-rsa-private-key (string &optional (password ""))
  "Convert a PEM string to an RSA structure. Free it with RSA-FREE.
   Will prompt for the password if it needs it and you provide nil."
  (with-bio-new-s-mem (bio string)
    (let ((res (if password
                   (with-foreign-strings ((passp password :encoding :latin-1))
                     (%pem-read-bio-rsa-private-key bio $null $null passp))
                   (%pem-read-bio-rsa-private-key bio))))
      (when (null-pointer-p res)
        (error "Couldn't encode private key from string"))
      res)))

;; Could switch to PEM_write_PKCS8PrivateKey, if PHP compatibility is
;; no longer necessary. It's supposed to be more secure.
(defun encode-rsa-private-key (rsa &optional password)
  "Encode an rsa private key as a PEM-encoded string."
  (with-bio-new-s-mem (bio)
    (let ((res (if password
                   (with-foreign-strings ((passp password :encoding :latin-1))
                     (%pem-write-bio-rsa-private-key
                      bio rsa (evp-des-ede3-cbc) passp (length password)))
                   (%pem-write-bio-rsa-private-key bio rsa))))
      (when (eql res 0)
        (error "Can't encode private key."))
      (bio-get-string bio))))

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
  (with-bio-new-s-mem (bio string)
    (let ((res (%pem-read-bio-rsa-pubkey bio $null $null $null)))
      (when (null-pointer-p res)
        (error "Couldn't decode public key"))
      res)))

(defun encode-rsa-public-key (rsa)
  "Encode an RSA public or private key to a PEM-encoded public key."
  (with-bio-new-s-mem (bio)
    (when (eql 0 (%pem-write-bio-rsa-pubkey bio rsa))
      (error "Can't encode RSA public key"))
    (bio-get-string bio)))

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
  (let ((res (%rsa-generate-key keylen exponent $null $null)))
    (when (null-pointer-p res)
      (error "Couldn't generate RSA key"))
    res))

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
    (with-foreign-strings ((d string :encoding :latin-1))
      (%sha1 d (length string) md))
    (let* ((byte-array-p (or (eq res-type :hex) (eq res-type :bytes)))
           (res (copy-memory-to-lisp md 20 byte-array-p)))
      (if (eq res-type :hex)
          (bin2hex res)
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

(defmacro with-rsa-public-key ((keyvar key) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk (,keyvar) ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-rsa-public-key #',thunk ,key))))
             
(defun call-with-rsa-public-key (thunk key)
  (if (stringp key)
      (let ((key (decode-rsa-public-key key)))
        (unwind-protect
             (funcall thunk key)
          (rsa-free key)))
      (funcall thunk key)))

(defmacro with-rsa-private-key ((keyvar key) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk (,keyvar) ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-rsa-private-key #',thunk ,key))))
             
(defun call-with-rsa-private-key (thunk key)
  (if (stringp key)
      (let ((key (decode-rsa-private-key key)))
        (unwind-protect
             (funcall thunk key)
          (rsa-free key)))
      (funcall thunk key)))

(defmacro with-evp-pkey ((pkey rsa-key &optional public-p) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk (,pkey) ,@body))
       (call-with-evp-pkey #',thunk ,rsa-key ,public-p))))

(defun call-with-evp-pkey (thunk rsa-key public-p)
  (flet ((doit (thunk rsa)
           (let ((pkey (%evp-pkey-new)))
             (unwind-protect
                  (progn
                    (when (null-pointer-p pkey)
                      (error "Can't allocate private key storage"))
                    (when (eql 0 (%evp-pkey-set1-rsa pkey rsa))
                      (error "Can't initialize private key storage"))
                    (funcall thunk pkey))
               (unless (null-pointer-p pkey)
                 (%evp-pkey-free pkey))))))
    (if public-p
        (with-rsa-public-key (rsa rsa-key) (doit thunk rsa))
        (with-rsa-private-key (rsa rsa-key) (doit thunk rsa)))))

(defun sign (data rsa-private-key)
  "Sign the string in DATA with the RSA-PRIVATE-KEY.
   Return the signature BASE64-encoded."
  (check-type data string)
  (with-evp-pkey (pkey rsa-private-key)
    (let ((type (%evp-sha1)))
      (when (null-pointer-p type)
        (error "Can't get SHA1 type structure"))
      (with-foreign-pointer (ctx $EVP-MD-CTX-size)
        (with-foreign-pointer (sig (%evp-pkey-size pkey))
          (with-foreign-pointer (siglen (foreign-type-size :unsigned-long))
            (with-foreign-strings ((datap data :encoding :latin-1))
              (when (or (eql 0 (%evp-sign-init ctx type))
                        (unwind-protect
                             (or (eql 0 (%evp-sign-update
                                         ctx datap (length data)))
                                 (eql 0 (%evp-sign-final ctx sig siglen pkey)))
                          (%evp-md-ctx-cleanup ctx)))
                (error "Error while signing"))
              ;; Here's the result
              (base64-encode
               (copy-memory-to-lisp
                sig (mem-ref siglen :unsigned-long) nil)))))))))

(defun verify (data signature rsa-public-key)
  "Verify the SIGNATURE for DATA created by SIGN, using the given RSA-PUBLIC-KEY.
   The private key will work, too."
  (check-type data string)
  (check-type signature string)
  (with-evp-pkey (pkey rsa-public-key t)
    (let* ((type (%evp-sha1))
           (sig (base64-decode signature))
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
                   (not (eql 0 res))))  ; Here's the result
            (%evp-md-ctx-cleanup ctx)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http:;;;www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
