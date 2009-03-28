(in-package :trubanc)

;; We don't need any of these structures
#||

;; From openssl/evp.h
(defcstruct (evp-pkey :conc-name "EVP-PKEY-")
  (type :int)
  (save-type :int)
  (references :int)
  (rsa :pointer)
  (save-parameters :int)
  (attributes :pointer))
  
;; From openssl/crypto.h
(defcstruct (crypto-ex-data :conc-name "CRYPT-EX-DATA-")
  (sk :pointer)
  (dummy :int))

;; From openssl/rsa.h
;; Doesn't save to a fasl file because of the included structure.
(defcstruct (rsa :conc-name "RSA-")
  (pad :int)
  (version :long)
  (rsa-method :pointer)
  (engine :pointer)
  (n :pointer)
  (e :pointer)
  (d :pointer)
  (p :pointer)
  (q :pointer)
  (dmp1 :pointer)
  (dmq1 :pointer)
  (iqmp :pointer)

  ;; be careful using this if the RSA structure is shared 
  ;; (ex-data crypto-ex-data)
  (sk :pointer)
  (dummy-int)

  (references :int)
  (flags :int)

  ;; Used to cache montgomery values
  (_method_mod_n :pointer)
  (_method_mod_p :pointer)
  (_method_mod_q :pointer)

  ;; all BIGNUM values are actually in the following data, if it is not NULL
  (bignum_data :pointer)
  (blinding :pointer)
)

||#

(defcfun ("fopen" %fopen) :pointer      ;returns a FILE stream
  (file :pointer)                       ;string
  (mode :pointer))                      ;string

(defcfun ("fclose" %fclose) :int         ;returns 0 on success
  (stream :pointer))                    ;a FILE stream

(defcfun ("fread" %fread) :long
  (ptr :pointer)
  (size :long)
  (nitems :long)
  (stream :pointer))

(defun fopen (file mode)
  (with-foreign-strings ((filep (cffi::native-namestring file) :encoding :latin-1)
                         (modep mode))
    (let ((res (%fopen filep modep)))
      (when (null-pointer-p res)
        (error "Failed to open ~s for ~s" file mode))
      res)))

(defun fclose (fp)
  (eql (%fclose fp) 0))

(defmacro with-fopen-file ((fp file &optional mode) &body body)
  (let ((thunk (gensym)))
    `(let ((,thunk (lambda (,fp) ,@body)))
       (declare (dynamic-extent #',thunk))
       (call-with-fopen-file ,file ,mode ,thunk))))

(defun call-with-fopen-file (file mode thunk)
  (let ((fp (fopen file (or mode "r"))))
     (unwind-protect (funcall thunk fp)
       (fclose fp))))

(defun fread-string (fp max-len)
  (with-foreign-pointer (p max-len)
    (let ((cnt (%fread p 1 max-len fp)))
      (foreign-string-to-lisp p :count cnt))))

(defparameter $null (null-pointer))

(defcfun ("OPENSSL_add_all_algorithms_conf" open-ssl-add-all-algorithms) :void
  )

(defcfun ("PEM_ASN1_read" %pem-asn1-read) :pointer
  (d2i :pointer)
  (name (:pointer :char))
  (fp :pointer)                         ;to a FILE stream
  (x (:pointer (:pointer :char)))
  (cb :pointer)
  (u :pointer))

(defcfun ("PEM_ASN1_write" %pem-asn1-write) :int
  (i2d (:pointer :int))
  (name (:pointer :char))
  (fp :pointer)                         ;to a FILE stream
  (x (:pointer :char))
  (enc :pointer)
  (kstr (:pointer :char))
  (klen :int)
  (callback :pointer)
  (u :pointer))

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

(defun %pem-read-rsa-private-key (fp &optional (x $null) (cb $null) (u $null))
  (with-foreign-strings ((namep $pem-string-rsa :encoding :latin-1))
    (%pem-asn1-read $d2i-RSAPrivateKey namep fp x cb u)))

(defun read-rsa-private-key (filename &optional password)
  "Read an RSA private key from a file. Return a pointer to an RSA structure,
   or signal an error. You must rsa-free the result when you're done with it."
  (with-fopen-file (fp filename)
    (let ((res (if password
                   (with-foreign-strings ((passp password :encoding :latin-1))
                     (%pem-read-rsa-private-key fp $null $null passp))
                   (%pem-read-rsa-private-key fp))))
      (when (null-pointer-p res)
        (error "Couldn't load private key from ~s" filename))
      res)))

(defcfun ("RSA_free" rsa-free) :void
  (r :pointer))

(defparameter $i2d-RSAPrivateKey
  (foreign-symbol-pointer "i2d_RSAPrivateKey"))

(defcfun ("EVP_des_ede3_cbc" evp-des-ede3-cbc) :pointer
  )

(defun %pem-write-rsa-private-key
    (fp x &optional (enc $null) (kstr $null) (klen 0) (cb $null) (u $null))
  (with-foreign-strings ((namep $pem-string-rsa :encoding :latin-1))
    (%pem-asn1-write $i2d-RSAPrivateKey namep fp x enc kstr klen cb u)))

;; Could switch to PEM_write_PKCS8PrivateKey, if PHP compatibility is
;; no longer necessary. It's supposed to be more secure.
(defun write-rsa-private-key (rsa filename &optional password)
  "Write RSA to FILE, encrypted with PASSWORD. Returns NIL or
   signals an error."
  (with-fopen-file (fp filename "w")
    (let ((res (if password
                   (with-foreign-strings ((passp password :encoding :latin-1))
                     (%pem-write-rsa-private-key
                      fp rsa (evp-des-ede3-cbc) passp (length password)))
                   (%pem-write-rsa-private-key fp rsa))))
      (when (eql res 0)
        (error "Can't write private key to ~s" filename)))))

(defconstant $pem-string-rsa-public "RSA PUBLIC KEY")

(defparameter $i2d-RSAPublicKey
  (foreign-symbol-pointer "i2d_RSAPublicKey"))

(defun %pem-write-rsa-public-key (fp x)
  (with-foreign-strings ((namep $pem-string-rsa-public :encoding :latin-1))
    (%pem-asn1-write $i2d-RSAPublicKey namep fp x $null $null 0 $null $null)))

(defun write-rsa-public-key (rsa filename)
  (with-fopen-file (fp filename "w")
    (when (eql 0 (%pem-write-rsa-public-key fp rsa))
      (error "Can't write public key to ~s" filename))))

(defparameter $d2i-RSAPublicKey
  (foreign-symbol-pointer "d2i_RSAPublicKey"))

(defun %pem-read-rsa-public-key (fp &optional (x $null))
  (with-foreign-strings ((namep $pem-string-rsa-public :encoding :latin-1))
    (%pem-asn1-read $d2i-RSAPublicKey namep fp x $null $null)))

(defun read-rsa-public-key (filename)
  "Read an RSA public key from a file. Return a pointer to an RSA structure,
   or signal an error. You must rsa-free the result when you're done with it."
  (with-fopen-file (fp filename)
    (let ((res (%pem-read-rsa-public-key fp)))
      (when (null-pointer-p res)
        (error "Couldn't load public key from ~s" filename))
      res)))

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

(defun decode-rsa-private-key (string &optional (password ""))
  "Converts a PEM string to an RSA structure. Free it with rsa-free.
   Will prompt for the password if it needs it and you provide nil."
  (with-bio-new-s-mem (bio string)
    (let ((res (if password
                   (with-foreign-strings ((passp password :encoding :latin-1))
                     (%pem-read-bio-rsa-private-key bio $null $null passp))
                   (%pem-read-bio-rsa-private-key bio))))
      (when (null-pointer-p res)
        (error "Couldn't encode private key from string"))
      res)))

(defun %pem-write-bio-rsa-private-key
    (bio x &optional (enc $null) (kstr $null) (klen 0) (cb $null) (u $null))
  (with-foreign-strings ((namep $pem-string-rsa :encoding :latin-1))
    (%pem-asn1-write-bio $i2d-RSAPrivateKey namep bio x enc kstr klen cb u)))

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

(defun %pem-read-bio-rsa-public-key (bio &optional (x $null))
  (with-foreign-strings ((namep $pem-string-rsa-public :encoding :latin-1))
    (%pem-asn1-read-bio $d2i-RSAPublicKey namep bio x $null $null)))

(defun decode-rsa-public-key (string)
  "Convert a PEM-encoded string to an RSA public key.
   You must rsa-free the result when you're done with it."
  (with-bio-new-s-mem (bio string)
    (let ((res (%pem-read-bio-rsa-public-key bio)))
      (when (null-pointer-p res)
        (error "Couldn't decode public key"))
      res)))

(defun %pem-write-bio-rsa-public-key (bio x)
  (with-foreign-strings ((namep $pem-string-rsa-public :encoding :latin-1))
    (%pem-asn1-write-bio $i2d-RSAPublicKey namep bio x $null $null 0 $null $null)))

(defun encode-rsa-public-key (rsa)
  (with-bio-new-s-mem (bio)
    (when (eql 0 (%pem-write-bio-rsa-public-key bio rsa))
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

(defun rsa-generate-key (keylen &optional (e 65537))
  (let ((res (%rsa-generate-key keylen e $null $null)))
    (when (null-pointer-p res)
      (error "Couldn't generate RSA key"))
    res))

(defcfun ("SHA1" %sha1) :pointer
  (d :pointer)
  (n :long)
  (md :pointer))

(defun copy-memory-to-lisp (pointer len byte-array-p)
  (let ((res (if byte-array-p
                 (make-array len :element-type '(unsigned-byte 8))
                 (make-string len))))
    (dotimes (i len)
      (let ((byte (mem-ref pointer :unsigned-char i)))
        (setf (aref res i)
              (if byte-array-p byte (code-char byte)))))
    res))

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

(defun base64-encode (string)
  (string-to-base64-string string :columns 64))

(defun base64-decode (string)
  (base64-string-to-string string))

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

(defun sign (data rsa-private-key)
  (check-type data string)
  (let ((rsa (if (stringp rsa-private-key)
                 (decode-rsa-private-key rsa-private-key)
                 rsa-private-key))          
        (pkey (%evp-pkey-new))
        (type (%evp-sha1)))
    (unwind-protect
         (progn
           (when (null-pointer-p pkey)
             (error "Can't allocate private key storage"))
           (when (null-pointer-p type)
             (error "Can't get SHA1 type structure"))
           (when (eql 0 (%evp-pkey-set1-rsa pkey rsa))
             (error "Can't initialize private key storage"))
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
                   (base64-encode
                    (copy-memory-to-lisp
                     sig (mem-ref siglen :unsigned-long) nil)))))))
      (unless (null-pointer-p pkey)
        (%evp-pkey-free pkey)))))

(defun verify (data signature rsa-public-key)
  (check-type data string)
  (check-type signature string)
  (let* ((rsa (if (stringp rsa-public-key)
                  (decode-rsa-public-key rsa-public-key)
                  rsa-public-key))          
         (pkey (%evp-pkey-new))
         (type (%evp-sha1))
         (sig (base64-decode signature))
         (siglen (length sig)))
    (unwind-protect
         (progn
           (when (null-pointer-p pkey)
             (error "Can't allocate public key storage"))
           (when (null-pointer-p type)
             (error "Can't get SHA1 type structure"))
           (when (eql 0 (%evp-pkey-set1-rsa pkey rsa))
             (error "Can't initialize public key storage"))
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
                        (not (eql 0 res))))
                 (%evp-md-ctx-cleanup ctx)))))
      (unless (null-pointer-p pkey)
        (%evp-pkey-free pkey)))))
