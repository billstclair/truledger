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

  ;; Used to cache montgomery values */
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
  (with-foreign-strings ((filep (cffi::native-namestring file))
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
  (with-foreign-strings ((namep $pem-string-rsa))
    (%pem-asn1-read $d2i-RSAPrivateKey namep fp x cb u)))

(defun read-rsa-private-key (filename &optional password)
  "Read an RSA private key from a file. Return a pointer to an RSA structure,
   or signal an error. You must rsa-free the result when you're done with it."
  (with-fopen-file (fp filename)
    (let ((res (if password
                   (with-foreign-strings ((passp password))
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
  (with-foreign-strings ((namep $pem-string-rsa))
    (%pem-asn1-write $i2d-RSAPrivateKey namep fp x enc kstr klen cb u)))

;; Could switch to PEM_write_PKCS8PrivateKey, if PHP compatibility is
;; no longer necessary. It's supposed to be more secure.
(defun write-rsa-private-key (rsa filename &optional password)
  "Write RSA to FILE, encrypted with PASSWORD. Returns NIL or
   signals an error."
  (with-fopen-file (fp filename "w")
    (let ((res (if password
                   (with-foreign-strings ((passp password))
                     (%pem-write-rsa-private-key
                      fp rsa (evp-des-ede3-cbc) passp (length password)))
                   (%pem-write-rsa-private-key fp rsa))))
      (when (eql res 0)
        (error "Can't write private key to ~s" filename)))))

(defconstant $pem-string-rsa-public "RSA PUBLIC KEY")

(defparameter $i2d-RSAPublicKey
  (foreign-symbol-pointer "i2d_RSAPublicKey"))

(defun %pem-write-rsa-public-key (fp x)
  (with-foreign-strings ((namep $pem-string-rsa-public))
    (%pem-asn1-write $i2d-RSAPublicKey namep fp x $null $null 0 $null $null)))

(defun write-rsa-public-key (rsa filename)
  (with-fopen-file (fp filename "w")
    (when (eql 0 (%pem-write-rsa-public-key fp rsa))
      (error "Can't write public key to ~s" filename))))

(defparameter $d2i-RSAPublicKey
  (foreign-symbol-pointer "d2i_RSAPublicKey"))

(defun %pem-read-rsa-public-key (fp &optional (x $null))
  (with-foreign-strings ((namep $pem-string-rsa-public))
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
      (with-foreign-strings ((sp string))
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
  (with-foreign-strings ((namep $pem-string-rsa))
    (%pem-asn1-read-bio $d2i-RSAPrivateKey namep bio x cb u)))

(defun decode-rsa-private-key (string &optional (password ""))
  "Converts a PEM string to an RSA structure. Free it with rsa-free.
   Will prompt for the password if it needs it and you provide nil."
  (with-bio-new-s-mem (bio string)
    (let ((res (if password
                   (with-foreign-strings ((passp password))
                     (%pem-read-bio-rsa-private-key bio $null $null passp))
                   (%pem-read-bio-rsa-private-key bio))))
      (when (null-pointer-p res)
        (error "Couldn't encode private key from string"))
      res)))

(defun %pem-write-bio-rsa-private-key
    (bio x &optional (enc $null) (kstr $null) (klen 0) (cb $null) (u $null))
  (with-foreign-strings ((namep $pem-string-rsa))
    (%pem-asn1-write-bio $i2d-RSAPrivateKey namep bio x enc kstr klen cb u)))

;; Could switch to PEM_write_PKCS8PrivateKey, if PHP compatibility is
;; no longer necessary. It's supposed to be more secure.
(defun encode-rsa-private-key (rsa &optional password)
  "Encode an rsa private key as a PEM-encoded string."
  (with-bio-new-s-mem (bio)
    (let ((res (if password
                   (with-foreign-strings ((passp password))
                     (%pem-write-bio-rsa-private-key
                      bio rsa (evp-des-ede3-cbc) passp (length password)))
                   (%pem-write-bio-rsa-private-key bio rsa))))
      (when (eql res 0)
        (error "Can't encode private key."))
      (bio-get-string bio))))

(defun %pem-read-bio-rsa-public-key (bio &optional (x $null))
  (with-foreign-strings ((namep $pem-string-rsa-public))
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
  (with-foreign-strings ((namep $pem-string-rsa-public))
    (%pem-asn1-write-bio $i2d-RSAPublicKey namep bio x $null $null 0 $null $null)))

(defun encode-rsa-public-key (rsa)
  (with-bio-new-s-mem (bio)
    (when (eql 0 (%pem-write-bio-rsa-public-key bio rsa))
      (error "Can't encode RSA public key"))
    (bio-get-string bio)))

(defcfun ("RSA_generate_key" %rsa-generate-key) :pointer
  (num :int)
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

(defun sha1 (string &optional (res-type :hex))
  "Return the sha1 hash of STRING.
   Return a string of hex chars if res-type is :hex, the default,
   a byte-array if res-type is :bytes,
   or a string with 8-bit character values if res-type is :string."
  (check-type res-type (member :hex :bytes :string))
  (with-foreign-pointer (md 20)
    (with-foreign-strings ((d string))
      (%sha1 d (length string) md))
    (let* ((bytes (or (eq res-type :hex) (eq res-type :bytes)))
           (res (if bytes
                    (make-array 20 :element-type '(unsigned-byte 8))
                    (make-string 20))))
      (dotimes (i 20)
        (let ((byte (mem-ref md :unsigned-char i)))
          (setf (aref res i)
                (if bytes byte (code-char byte)))))
      (if (eq res-type :hex)
          (bin2hex res)
          res))))
