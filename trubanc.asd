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
     (:file "openssl-cffi")
     (:file "file-locks")
     (:file "fsdb")
     (:file "bcmath")
     (:file "tokens")
     (:file "parser")
     (:file "utilities")
     (:file "loomrandom")
     (:file "timestamp")
     (:file "server")
     ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009 Bill St. Clair
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
