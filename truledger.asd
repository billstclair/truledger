; -*- mode: lisp -*-
(in-package #:cl-user)

;; Remove this to switch to the new all-lisp crypto code
(pushnew :openssl-cffi *features*)

(asdf:defsystem :truledger
  :description "An anonymous digitally-signed general ledger and trading system"
  :author "Bill St. Clair <bill@billstclair.com>"
  :version "1.1.6"
  :license "Apache"
  :depends-on (cffi cl-base64 hunchentoot drakma cl-smtp split-sequence
               html-template cl-json
               ;; Loaded below with cl-autorepo
               cl-crypto fsdb limited-thread-taskmaster cl-loom
               ;; In systems/ dir
               cybertiggyr-time)
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "ccl")
     (:file "sendmail")
     (:file "utilities")
     (:file "timer")
     (:file "crypto-api")
     #+openssl-cffi
     (:file "openssl-cffi")
     (:file "bcmath")
     (:file "tokens")
     (:file "parser")
     (:file "shared")
     (:file "loomrandom")
     (:file "timestamp")
     (:file "crypto-session")
     (:file "encrypt-note")
     (:file "server")
     (:file "client")
     (:file "backup")
     (:file "web-templates")
     (:file "server-web")
     (:file "toplevel")
     (:file "client-web")
     (:file "loom-client-web")
     (:file "client-json")
     (:file "history")
     (:file "tests")
     ))))

#-windows
(let* ((dir "~/.local/share/common-lisp/source/"))
  (asdf:run-shell-command "mkdir -p ~a" dir)
  (unless (or (find-package :cl-autorepo)
              (ignore-errors (ql:quickload "cl-autorepo")))
    (let ((autorepo-asd (merge-pathnames "cl-autorepo/cl-autorepo.asd" dir))
          (url "https://github.com/billstclair/cl-autorepo"))
    (asdf:run-shell-command "cd ~a;git clone ~a" dir url)
    (load autorepo-asd)
    (ql:quickload "cl-autorepo"))))

#-windows
(flet ((addit (name)
         (cl-autorepo:add-system
          name (format nil "git://github.com/billstclair/~a.git" name) :git)))
  (addit "fsdb")
  (addit "limited-thread-taskmaster")
  (addit "cl-crypto")
  (addit "cl-loom"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009-2012 Bill St. Clair
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
