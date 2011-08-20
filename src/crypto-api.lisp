; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cryptography API
;;;

(in-package :truledger)

;; :openssl-cffi or :cl-crypto
(defvar *crypto-api*
  :openssl-cffi)

(defun rsa-generate-key (keylen &optional (exponent 65537))
  "Generate an RSA private key with the given KEYLEN and exponent.
  Returns an internal representation of the key, ready for use (called
  an \"RSA structure\" below. The Truledger code doesn't ever go inside
  that representation, just passes it around."
  (rsa-generate-key-gf *crypto-api* keylen exponent))

(defun decode-rsa-private-key (string &optional (password ""))
  "Convert a PEM string to an RSA structure. Free it with RSA-FREE.
   Will prompt for the password if it needs it and you provide nil."
  (decode-rsa-private-key-gf *crypto-api* string password))

(defun encode-rsa-private-key (rsa &optional password)
  "Encode an rsa private key as a PEM-encoded string."
  (encode-rsa-private-key-gf *crypto-api* rsa password))

(defun decode-rsa-public-key (string)
  "Convert a PEM-encoded string to an RSA public key.
   You must RSA-FREE the result when you're done with it."
  (decode-rsa-public-key-gf *crypto-api* string))

(defun encode-rsa-public-key (rsa)
   "Encode an RSA public or private key to a PEM-encoded public key."
   (encode-rsa-public-key-gf *crypto-api* rsa))

(defun rsa-free (rsa)
  "Free a structure returned by decode-rsa-public-key,
  decode-rsa-private-key, or rsa-generate-key. May be a nop for
  garbage collected implementations, but best to overwrite the
  contents, so they don't hang around in RAM."
  (rsa-free-gf *crypto-api* rsa))

(defun sha1 (string &optional (res-type :hex))
  "Return the sha1 hash of STRING.
   Return a string of hex chars if res-type is :hex, the default,
   a byte-array if res-type is :bytes,
   or a string with 8-bit character values if res-type is :string."
  (sha1-gf *crypto-api* string res-type))

(defun sha256 (string &optional (res-type :hex))
  "Return the sha256 hash of STRING.
   Return a string of hex chars if res-type is :hex, the default,
   a byte-array if res-type is :bytes,
   or a string with 8-bit character values if res-type is :string."
  (sha256-gf *crypto-api* string res-type))

(defun sign (data rsa-private-key)
  "Sign the string in DATA with the RSA-PRIVATE-KEY.
   Return the signature BASE64-encoded."
  (sign-gf *crypto-api* data rsa-private-key))

(defun verify (data signature rsa-public-key)
  "Verify the SIGNATURE for DATA created by SIGN, using the given RSA-PUBLIC-KEY.
   The private key will work, too."
  (verify-gf *crypto-api* data signature rsa-public-key))

(defun %rsa-privkey-decrypt (string privkey &optional
                             (start 0) (end (length string)))
  "PRIVKEY is an RSA private key. STRING is a string to decrypt,
   START and END are the region of the string to decrypt.
   (- end start) must be (rsa-size privkey).
   Returns decrypted string."
  (%rsa-privkey-decrypt-gf *crypto-api* string privkey start end))

(defun %rsa-pubkey-encrypt (string pubkey &optional
                            (start 0) (end (length string)))
  "PUBKEY is an RSA public key. STRING is a string to encrypt.
   START & END are the region of STRING to encrypt.
   Region length must be <= (- (rsa-size pubkey) 11).
   Uses PKCS1 padding."
  (%rsa-pubkey-encrypt-gf *crypto-api* string pubkey start end))

(defun rsa-size (key)
  "Return the key length of the public or private key in bytes."
  (rsa-size-gf *crypto-api* key))

;;;
;;; Generic functions for dispatching to plug-in implementations:
;;;
;;;   openssl-cffi.lisp
;;;

(defgeneric rsa-generate-key-gf (api keylen &optional exponent))

(defgeneric decode-rsa-private-key-gf (api string &optional password))

(defgeneric encode-rsa-private-key-gf (api rsa &optional password))

(defgeneric decode-rsa-public-key-gf (api string))

(defgeneric encode-rsa-public-key-gf (api rsa))

(defgeneric rsa-free-gf (api rsa))

(defgeneric sha1-gf (api string &optional res-type))

(defgeneric sha256-gf (api string &optional res-type))

(defgeneric sign-gf (api data rsa-private-key))

(defgeneric verify-gf (api data signature rsa-public-key))

(defgeneric %rsa-privkey-decrypt-gf (api string privkey &optional start end))

(defgeneric %rsa-pubkey-encrypt-gf (api string pubkey &optional start end))

(defgeneric rsa-size-gf (api key))

;;;
;;; Shared functions
;;;

(defun aset (val array idx)
  (setf (aref array idx) val))

(defun destroy-password (buf &optional
                         (len (length buf))
                         (store-fun 'aset))
  "Overwrite a password string with randomness"
  (let* ((s-len (max 8 len))
         (s (urandom-bytes s-len)))
    (dotimes (i len)
      (dotimes (j 8)
        (funcall store-fun (aref s (mod (+ i j) s-len)) buf i)))))

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

(defun pubkey-id (pubkey)
  (sha1 (trim pubkey)))

(defun pubkey-bits (pubkey)
  (with-rsa-public-key (key pubkey)
    (* 8 (rsa-size key))))

(defun privkey-bits (privkey)
  (with-rsa-private-key (key privkey)
    (* 8 (rsa-size key))))

(defconstant $RSA-PKCS1-PADDING-SIZE 11)

(defun pubkey-encrypt (message pubkey)
  "PUBKEY is an RSA public key. MESSAGE is a message to encrypt.
   Returns encrypted message, base64 encoded."
  (with-rsa-public-key (rsa pubkey)
    (let* ((msglen (length message))
           (chars (- (rsa-size rsa) $RSA-PKCS1-PADDING-SIZE))
           (res ""))
      (loop
         for start from 0 below msglen by chars
         for end = (min msglen (+ start chars))
         do
           (dotcat res (%rsa-pubkey-encrypt message rsa start end)))
      (base64-encode res))))

(defun privkey-decrypt (message privkey)
  "PRIVKEY is an RSA private key. MESSAGE is a message to decrypt,
   base64-encoded. Returns decrypted message."
  (with-rsa-private-key (rsa privkey)
    (let* ((size (rsa-size rsa))
           (msg (base64-decode message))
           (msglen (length msg))
           (res ""))
      (loop for start from 0 below msglen by size
         for end = (+ start size)
         do
           (dotcat res (%rsa-privkey-decrypt msg rsa start end)))
      res)))

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
