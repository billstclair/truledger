; -*- mode: lisp -*-
(in-package #:cl-user)

(asdf:defsystem :trubanc
  :depends-on (cffi cl-base64)
  :version "0.1"
  :author "Bill St. Clair <bill@billstclair.com>"
  :maintainer "Bill St. Clair <bill@billstclair.com>"
  :description "An anoymous digitally-signed vault and trading system"
  :components ((:file "package")
               (:file "ccl" :depends-on ("package"))
               (:file "utilities" :depends-on ("package"))
               (:file "openssl-cffi" :depends-on ("ccl" "package"))
               ))
