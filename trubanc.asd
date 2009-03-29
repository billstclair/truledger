; -*- mode: lisp -*-
(in-package #:cl-user)

(asdf:defsystem :trubanc
  :description "An anoymous digitally-signed vault and trading system"
  :author "Bill St. Clair <bill@billstclair.com>"
  :version "0.1"
  :license "Apache"
  :depends-on (cffi cl-base64)
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "ccl")
     (:file "utilities")
     (:file "openssl-cffi")
     (:file "file-locks")
     (:file "fsdb")))))
