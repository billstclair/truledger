;;;; null-cipher.lisp -- the do-nothing cipher

;;; It's not very secure, but it does come in handy to serve as a dummy
;;; cipher in security protocols before ciphers and keys have been
;;; established.

(cl:in-package :crypto)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +null-block-length+ 8)
)

(defclass null (cipher 8-byte-block-mixin)
  ())

(define-block-encryptor null #.+null-block-length+
  (declare (ignore context))
  (unless (and (eq plaintext ciphertext) (= plaintext-start ciphertext-start))
    (replace ciphertext plaintext
             :start1 ciphertext-start :end1 (+ ciphertext-start #.+null-block-length+)
             :start2 plaintext-start :end2 (+ plaintext-start #.+null-block-length+))))

(define-block-decryptor null #.+null-block-length+
  (declare (ignore context))
  (unless (and (eq plaintext ciphertext) (= plaintext-start ciphertext-start))
    (replace plaintext ciphertext
             :start1 plaintext-start :end1 (+ plaintext-start #.+null-block-length+)
             :start2 ciphertext-start :end2 (+ ciphertext-start #.+null-block-length+))))

(defcipher null
  (:encrypt-function null-encrypt-block)
  (:decrypt-function null-decrypt-block)
  (:block-length #.+null-block-length+)
  (:key-length (:variable 1 256 1)))
