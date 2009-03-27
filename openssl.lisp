;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OpenSSL functions
;;;

(in-package #:trubanc)

(defun openssl-trim (string &optional (begin "-----BEGIN"))
  "Return the tail of STRING, starting with BEGIN"
  (let ((pos (search begin string)))
    (if pos
      (subseq string pos)
      string)))

(defmacro run-program-with-streams (program args &key input-string output-p)
  (let ((ins (gensym "INS"))
        (outs (gensym "OUTS")))
    (let ((body `(ccl:run-program
                  ,program ,args
                  ,@(and input-string `(:input ,ins))
                  ,@(and output-p `(:output ,outs)))))
      (when input-string
        (setq body `(with-input-from-string (,ins ,input-string) ,body)))
      (when output-p
        (setq body `(with-output-to-string (,outs) ,body)))
      body)))                            

(defun genrsa (bits)
  "Generate an RSA private key with the given number of BITS."
  (openssl-trim
   (run-program-with-streams "openssl" `("genrsa" ,(format nil "~d" bits))
                             :output-p t)))

(defun pubkey (private-key)
  "Return the public key corresponding to the PRIVATE-KEY"
  (openssl-trim
   (run-program-with-streams "openssl" `("rsa" "-pubout")
                             :input-string private-key
                             :output-p t)))

#+nil
(defun sha1-openssl (string)
  (string-right-trim
   '(#\newline)
   (run-program-with-streams "openssl" `("sha1")
                             :input-string string
                             :output-p t)))

(defun sha1-crypto (string &optional (as-hex-p t))
  (let* ((octets (crypto:ascii-string-to-byte-array string))
         (digest (crypto:digest-sequence :sha1 octets)))
    (if as-hex-p
      (crypto:byte-array-to-hex-string digest)
      (map 'string 'code-char digest))))

(defun sign (string private-key-path)
  (let* ((hash (sha1-crypto string nil))
         (res (run-program-with-streams
               "openssl"
               `("rsautl"
                 "-inkey" ,(namestring (truename private-key-path))
                 "-sign")
               :input-string hash
               :output-p t)))
    (cl-base64:string-to-base64-string res :columns 64)))

(defun verify (string signature public-key-path)
  (let* ((hash (sha1-crypto string nil))
         (sig (cl-base64:base64-string-to-string signature))
         (res (run-program-with-streams
               "openssl"
               `("rsautl"
                 "-inkey" ,(namestring (truename public-key-path))
                 "-pubin"
                 "-verify")
               :input-string sig
               :output-p t)))
    (equal res hash)))

(defparameter *private-key-start* "-----BEGIN RSA PRIVATE KEY-----")
(defparameter *private-key-end*   "-----END RSA PRIVATE KEY-----")
(defparameter *public-key-start*  "-----BEGIN PUBLIC KEY-----")
(defparameter *public-key-end*    "-----END PUBLIC KEY-----")

;; http://luca.ntop.org/Teaching/Appunti/asn1.html
;; Need to do bit strings, tag 3: <unused bits> <byte 0> <byte 1> ...
(defun pem-to-der (string start-str end-str)
  (let ((start (search start-str string))
        (end (search end-str string)))
    (unless (and start end) (error "Not properly PEM encoded"))
    (let* ((key(trim (subseq string (+ start (length start-str)) end))))
      (string-to-octets (cl-base64:base64-string-to-string key)))))

(defun asn-integer (asn)
  (unless (and (listp asn)
               (eql 3 (length asn))
               (eq :universal (first asn))
               (eq :integer (second asn))
               (integerp (third asn)))
    (error "Malformed ingteger ASN"))
  (third asn))

(defun asn-sequence (asn)
  (unless (and (listp asn)
               (eql 3 (length asn))
               (eq :universal (first asn))
               (eq :sequence-or-sequence-of (second asn))
               (listp (third asn)))
    (error "Malformed sequence ASN"))
  (third asn))

;; http://www.rsa.com/rsalabs/node.asp?id=2125
;; ftp://ftp.rsasecurity.com/pub/pkcs/ascii/pkcs-1.asc
(defun load-private-key (string)
  ;; Should really have a template language for these, but there
  ;; are currently only two of them.
  (let* ((asn (load-private-key-asn string))
         (nums (mapcar 'asn-integer (asn-sequence (car asn)))))
    ;; n, e, d, p, q, d mod (p-1), d mod (q-1), (inverse of q) mod p, rest 
    (unless (and (eql 0 (pop nums))
                 (> (length nums) *))
      (error "Malformed private key"))
    (destructuring-bind (modulus
                         public-exponent
                         private-exponent
                         prime1
                         prime2
                         exponent1
                         exponent2
                         coefficient
                         . rest) nums
      (values modulus
              public-exponent
              private-exponent
              prime1
              prime2
              exponent1
              exponent2
              coefficient
              rest))))

(defun load-private-key-asn (string)
  (der-decode (pem-to-der string *private-key-start* *private-key-end*)))

;; Public keys are represented as:
;; ((:UNIVERSAL
;;   :SEQUENCE-OR-SEQUENCE-OF
;;   ((:UNIVERSAL
;;     :SEQUENCE-OR-SEQUENCE-OF
;;     ((:UNIVERSAL :OBJECT-IDENTIFIER NIL 784439383290839892225)
;;      (:UNIVERSAL :NULL "" 0)))
;;    (:UNIVERSAL :BIT-STRING NIL bit-string))))
;; 
;; The bit-string encodes the actual public key:
;; ((:UNIVERSAL
;;   :SEQUENCE-OR-SEQUENCE-OF
;;   ((:UNIVERSAL :INTEGER NIL public-modulus)
;;    (:UNIVERSAL :INTEGER NIL public-exponent))))
;;
;; Need to make der-decode aware of bit-string encoding (unused-bits . octets),
;; where unused-bits is the number of bits unused in the low-order of the
;; last of the octets. Seems to be 0 in the public key encodings.
;; It should also distinguish integer from other types, and encode the others
;; as byte arrays.
(defun load-public-key (string)
  (der-decode (pem-to-der string *public-key-start* *public-key-end*)))

(defparameter *der-classes* #(:universal :application :context-specific :private))
(defparameter *der-constructed* #x20)
(defparameter *der-tag-mask* #x1f)

;; http://www.obj-sys.com/asn1tutorial/node124.html
(defparameter *der-universal-tags*
  '((0 . :reserved-for-BER)
    (1 . :BOOLEAN)
    (2 . :INTEGER)
    (3 . :BIT-STRING)
    (4 . :OCTET-STRING)
    (5 . :NULL)
    (6 . :OBJECT-IDENTIFIER)
    (7 . :Object-Descriptor)
    (8 . :INSTANCE-OF-EXTERNAL)
    (9 . :REAL)
    (10 . :ENUMERATED)
    (11 . :EMBEDDED-PDV)
    (12 . :UTF8-String)
    (13 . :RELATIVE-OID)
    (16 . :SEQUENCE-OR-SEQUENCE-OF)
    (17 . :SET-OR-SET-OF)
    (18 . :Numeric-String)
    (19 . :Printabl-eString)
    (20 . :Teletex-String-or-T61-String)
    (21 . :Videotex-String)
    (22 . :IA5-String)
    (23 . :UTC-Time)
    (24 . :Generalized-Time)
    (25 . :Graphic-String)
    (26 . :Visible-String-or-ISO646-String)
    (27 . :General-String)
    (28 . :Universal-String)
    (29 . :CHARACTER-STRING)
    (30 . :BMP-String)))

(defun printable-string (string)
  (dotimes (i (length string) string)
    (let ((code (char-code (aref string i))))
      (when (or (< code 32) (> code 126))
        (return nil)))))

(defun octets-to-integer (octets)
  (let ((res 0))
    (dotimes (i (length octets) res)
      (setq res (+ (ash res 8) (aref octets i))))))

(defun make-octets (len)
  (make-array len :element-type '(unsigned-byte 8)))

(defun string-to-octets (string)
  (let* ((len (length string))
         (res (make-octets len)))
    (dotimes (i len res)
      (setf (aref res i) (char-code (aref string i))))))

(defun octets-to-string (octets)
  (let* ((len (length octets))
         (res (make-string len)))
    (dotimes (i len res)
      (setf (aref res i) (code-char (aref octets i))))))

(defun makeq (octets)
  (list* 0 (length octets) octets))

(defun deq (q &optional errp)
  (if (>= (car q) (cadr q))
    (and errp (error "End of input"))
    (prog1 (aref (cddr q) (car q))
      (incf (car q)))))

(defun der-decode (octets)
  (loop with q = (makeq octets)
        with res = nil
        for type = (deq q)
          do
       (when (null type) (return (nreverse res)))
       (multiple-value-bind (class constructed tag) (read-der-type type q)
         (let* ((len (read-der-length q))
                (oct (read-der-octets len q)))
           (if constructed
             (push (list class tag (der-decode oct)) res)
             (push (list class
                         tag
                         (cond ((eq tag :integer)
                                (octets-to-integer oct))
                               ((eq tag :bit-string)
                                (let ((unused (aref oct 0))
                                      (oct (subseq oct 1)))
                                  (unless (eql 0 unused)
                                    (error "Can't handle non-zero unused bits in bit-string"))
                                  oct))
                               (t oct)))
                   res))))))

(defun read-der-type (type q)
  (let* ((class (aref *der-classes* (ash type -6)))
         (constructed (logbitp  5 type))
         (tag (logand type *der-tag-mask*)))
    (when (eql tag *der-tag-mask*)
      (setf tag 0)
      (loop for byte = (deq q t)
            do
         (setf tag (+ (ash tag 7) (logand byte #x7f)))
         (unless (logbitp 7 byte) (return))))
    (when (eq class :universal)
      (setq tag (or (cdr (assoc tag *der-universal-tags*)) tag)))
    (values class constructed tag)))

(defun read-der-length (q)
  (let ((len (deq q t)))
    (if (logbitp 7 len)
      (read-der-integer q (logand #x7f len))
      len)))

(defun read-der-octets (len q)
  (let ((res (make-octets len)))
    (dotimes (i (length res) res)
      (setf (aref res i) (deq q t)))))

(defun read-der-integer (q len)
  (let ((res 0))
    (dotimes (i len res)
      (setq res (+ (ash res 8) (deq q t))))))
