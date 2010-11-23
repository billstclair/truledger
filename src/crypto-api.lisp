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

(defgeneric rsa-generate-key-gf (api keylen &optional exponent)
  )

(defun decode-rsa-private-key (string &optional (password ""))
  "Convert a PEM string to an RSA structure. Free it with RSA-FREE.
   Will prompt for the password if it needs it and you provide nil."
  (decode-rsa-private-key-gf *crypto-api* string password))

(defgeneric decode-rsa-private-key-gf (api string &optional password)
  )

(defun encode-rsa-private-key (rsa &optional password)
  "Encode an rsa private key as a PEM-encoded string."
  (encode-rsa-private-key-gf *crypto-api* rsa password))

(defgeneric encode-rsa-private-key-gf (api rsa &optional password)
  )

(defun decode-rsa-public-key (string)
  "Convert a PEM-encoded string to an RSA public key.
   You must RSA-FREE the result when you're done with it."
  (decode-rsa-public-key-gf *crypto-api* string))

(defgeneric decode-rsa-public-key-gf (api string)
  )

(defun encode-rsa-public-key (rsa)
   "Encode an RSA public or private key to a PEM-encoded public key."
   (encode-rsa-public-key-gf *crypto-api* rsa))

(defgeneric encode-rsa-public-key-gf (api rsa)
  )

(defun rsa-free (rsa)
  "Free a structure returned by decode-rsa-public-key,
  decode-rsa-private-key, or rsa-generate-key. May be a nop for
  garbage collected implementations, but best to overwrite the
  contents, so they don't hang around in RAM."
  (rsa-free-gf *crypto-api* rsa))

(defgeneric rsa-free-gf (api rsa)
  )

(defun sha1 (string &optional (res-type :hex))
  "Return the sha1 hash of STRING.
   Return a string of hex chars if res-type is :hex, the default,
   a byte-array if res-type is :bytes,
   or a string with 8-bit character values if res-type is :string."
  (sha1-gf *crypto-api* string res-type))

(defgeneric sha1-gf (api string &optional res-type)
  )

(defun sign (data rsa-private-key)
  "Sign the string in DATA with the RSA-PRIVATE-KEY.
   Return the signature BASE64-encoded."
  (sign-gf *crypto-api* data rsa-private-key))

(defgeneric sign-gf (api data rsa-private-key)
  )

(defun verify (data signature rsa-public-key)
  "Verify the SIGNATURE for DATA created by SIGN, using the given RSA-PUBLIC-KEY.
   The private key will work, too."
  (verify-gf *crypto-api* data signature rsa-public-key))

(defgeneric verify-gf (api data signature rsa-public-key)
  )

(defun privkey-decrypt (message privkey)
  "PRIVKEY is an RSA private key. MESSAGE is a message to decrypt,
   base64-encoded. Returns decrypted message."
  (privkey-decrypt-gf *crypto-api* message privkey))

(defgeneric privkey-decrypt-gf (api message privkey)
  )

(defun pubkey-encrypt (message pubkey)
  "PUBKEY is an RSA public key. MESSAGE is a message to encrypt.
   Returns encrypted message, base64 encoded."
  (pubkey-encrypt-gf *crypto-api* message pubkey))

(defgeneric pubkey-encrypt-gf (api message pubkey)
  )

(defun rsa-size (key)
  "Return the key length of the public or private key, the keylen arg
   to the rsa-generate-key call that created its private key."
  (rsa-size-gf *crypto-api* key))

(defgeneric rsa-size-gf (api key)
  )

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

