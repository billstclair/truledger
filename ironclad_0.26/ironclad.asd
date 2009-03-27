; -*- mode: lisp -*-
(defpackage #:ironclad-system
  (:use :cl :asdf))

(in-package #:ironclad-system)

;;; easy-to-type readmacro for creating s-boxes and the like

(defun array-reader (stream subchar arg)
  (declare (ignore subchar))
  (let ((array-data (read stream nil stream nil))
        (array-element-type `(unsigned-byte ,arg)))
    ;; FIXME: need to make this work for multi-dimensional arrays
    `(make-array ,(length array-data) :element-type ',array-element-type
                :initial-contents ',array-data)))
    
(defparameter *ironclad-readtable*
  (let ((readtable (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\@ #'array-reader readtable)
    readtable))

(defclass ironclad-source-file (cl-source-file) ())
(defclass txt-file (doc-file) ())
(defclass css-file (doc-file) ())

(defmethod source-file-type ((c txt-file) (s module)) "txt")
(defmethod source-file-type ((c css-file) (s module)) "css")

(asdf:defsystem :ironclad
  :version "0.26"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A cryptographic toolkit written in pure Common Lisp"
  :default-component-class ironclad-source-file
  #+sbcl :depends-on #+sbcl (sb-rotate-byte)
  :components ((:static-file "README")
               (:static-file "LICENSE")
               (:static-file "TODO")
               (:static-file "NEWS")
               (:module "doc"
                        :components
                        ((:html-file "ironclad")
                         ;; XXX ASDF bogosity
                         (:txt-file "ironclad-doc")
                         (:css-file "style")))
               (:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "util" :depends-on ("package"))
               (:file "cipher" :depends-on ("package"))
               (:file "macro-utils" :depends-on ("package"))
               (:file "make-cipher" :depends-on ("cipher" "macro-utils"))
               (:file "digest" :depends-on ("package" "common" "macro-utils"))
               (:file "common" :depends-on ("package"))
               #+(or lispworks sbcl openmcl cmu allegro)
               (:file "octet-stream" :depends-on ("common" "make-cipher"))
               (:file "modes" :depends-on ("cipher" "common"))
               ;; public-key cryptography
               (:file "public-key" :depends-on ("package"))
               (:file "dsa" :depends-on ("public-key" "package"))
               (:file "rsa" :depends-on ("public-key" "package"))
               ;; hash functions
               (:file "crc24" :depends-on ("common" "digest"))
               (:file "crc32" :depends-on ("common" "digest"))
               (:file "adler32" :depends-on ("common" "digest"))
               (:file "md2" :depends-on ("common" "digest"))
	       (:file "md4" :depends-on ("common" "digest"))
	       (:file "md5" :depends-on ("common" "digest"))
	       (:file "sha1" :depends-on ("common" "digest"))
               (:file "sha256" :depends-on ("common" "digest"))
	       (:file "ripemd-128" :depends-on ("common" "digest"))
	       (:file "ripemd-160" :depends-on ("common" "digest"))
	       (:file "tiger" :depends-on ("common" "digest"))
               (:file "whirlpool" :depends-on ("common" "digest"))
               (:file "hmac" :depends-on ("common" "digest"))
               (:file "cmac" :depends-on ("common" "digest"))
               ;; block ciphers of various kinds
               (:file "null-cipher" :depends-on ("common" "cipher"))
               (:file "aes" :depends-on ("common" "cipher"))
               (:file "des" :depends-on ("common" "cipher"))
               (:file "blowfish" :depends-on ("common" "cipher"))
               (:file "twofish" :depends-on ("common" "cipher"))
               (:file "idea" :depends-on ("common" "cipher"))
               (:file "misty1" :depends-on ("common" "cipher"))
               (:file "square" :depends-on ("common" "cipher"))
               (:file "rc2" :depends-on ("common" "cipher"))
               (:file "rc5" :depends-on ("common" "cipher"))
               (:file "rc6" :depends-on ("common" "cipher"))
               (:file "tea" :depends-on ("common" "cipher"))
               (:file "xtea" :depends-on ("common" "cipher"))
               (:file "cast5" :depends-on ("common" "cipher"))
               ;; stream ciphers
               (:file "arcfour" :depends-on ("common" "cipher"))
               ;; padding for block ciphers
               (:file "padding" :depends-on ("common"))))

(macrolet ((do-silently (&body body)
             `(handler-bind ((style-warning #'muffle-warning)
                             ;; It's about as fast as we can make it,
                             ;; and a number of the notes relate to code
                             ;; that we're running at compile time,
                             ;; which we don't care about the speed of
                             ;; anyway...
                             #+sbcl (sb-ext:compiler-note #'muffle-warning))
                ,@body)))
(defmethod perform :around ((op compile-op) (c ironclad-source-file))
  (let ((*readtable* *ironclad-readtable*)
        (*print-base* 10)               ; INTERN'ing FORMAT'd symbols
        #+sbcl (sb-ext:*inline-expansion-limit* (max sb-ext:*inline-expansion-limit* 1000))
        #+sbcl (*features* (list* sb-c:*backend-byte-order*
                                  (if (= sb-vm:n-word-bits 32)
                                      :32-bit
                                      :64-bit)
                                  *features*))
        #+cmu (ext:*inline-expansion-limit* (max ext:*inline-expansion-limit* 1000))
        #+cmu (*features* (list* (c:backend-byte-order c:*target-backend*)
                                 (if (= vm:word-bits 32)
                                     :32-bit
                                     :64-bit)
                                 *features*)))
    (do-silently (call-next-method))))

(defmethod perform :around ((op load-op) (c ironclad-source-file))
  (do-silently (call-next-method))))

(defmethod perform :after ((op load-op) (c (eql (find-system :ironclad))))
  (provide :ironclad))


;;; testing

(defclass test-vector-file (static-file)
  ())

(defmethod source-file-type ((c test-vector-file) (s module)) "testvec")

(defpackage :ironclad-tests
  (:nicknames :crypto-tests)
  (:use :cl))

(defmethod perform ((op test-op) (c (eql (find-system :ironclad))))
  (oos 'test-op 'ironclad-tests))

;;; A tester's job is never done!
(defmethod operation-done-p ((op test-op) (c (eql (find-system :ironclad))))
  nil)

(asdf:defsystem ironclad-tests
  :depends-on (ironclad)
  :version "0.5"
  :in-order-to ((test-op (load-op :ironclad-tests)))
  :components ((:file "rt")
               (:file "testfuns" :depends-on ("rt"))
               (:module "test-vectors"
                        :depends-on ("testfuns")
                        :components
                        ((:file "macs")
                         (:file "modes")
                         (:file "ciphers")
                         (:file "digests")
                         (:file "padding")
                         (:file "ironclad")
                         ;; test vectors
                         (:test-vector-file "crc24")
                         (:test-vector-file "crc32")
                         (:test-vector-file "adler32")
                         (:test-vector-file "md2")
                         (:test-vector-file "md4")
                         (:test-vector-file "md5")
                         (:test-vector-file "sha1")
                         (:test-vector-file "sha256")
                         (:test-vector-file "ripemd-128")
                         (:test-vector-file "ripemd-160")
                         (:test-vector-file "tiger")
                         (:test-vector-file "whirlpool")
                         (:test-vector-file "hmac")
                         (:test-vector-file "cmac")
                         ;; block ciphers of various kinds
                         (:test-vector-file "aes")
                         (:test-vector-file "des")
                         (:test-vector-file "3des")
                         (:test-vector-file "blowfish")
                         (:test-vector-file "twofish")
                         (:test-vector-file "idea")
                         (:test-vector-file "misty1")
                         (:test-vector-file "square")
                         (:test-vector-file "rc2")
                         (:test-vector-file "rc5")
                         (:test-vector-file "rc6")
                         (:test-vector-file "tea")
                         (:test-vector-file "xtea")
                         (:test-vector-file "cast5")
                         ;; modes
                         (:test-vector-file "cbc")
                         (:test-vector-file "ctr")
                         (:test-vector-file "ofb")
                         (:test-vector-file "cfb")
                         (:test-vector-file "cfb8")
                         ;; stream ciphers
                         (:test-vector-file "arcfour")))))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :ironclad-tests))))
  nil)

(defmethod perform ((op test-op) (c (eql (find-system :ironclad-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "RTEST")))
      (error "TEST-OP failed for IRONCLAD-TESTS")))
