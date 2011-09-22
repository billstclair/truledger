; -*- mode: lisp -*-
(in-package #:cl-user)

;; Remove this to switch to the new all-lisp crypto code
(pushnew :openssl-cffi *features*)

(asdf:defsystem :truledger
  :description "An anoymous digitally-signed general ledger and trading system"
  :author "Bill St. Clair <bill@billstclair.com>"
  :version "1.0.0"
  :license "Apache"
  :depends-on (cffi cl-base64 cl-who hunchentoot drakma cl-smtp split-sequence
               html-template
               ;; Systems above here come from quicklisp
               ;; System below here are in systems/ dir
               cl-crypto fsdb cybertiggyr-time limited-thread-taskmaster cl-loom)
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
     (:file "server-web")
     (:file "toplevel")
     (:file "web-templates")
     (:file "client-web")
     (:file "loom-client-web")
     (:file "history")
     (:file "tests")
     ))))

#-windows
(unless (or (find-package :cl-autorepo)
            (ignore-errors (ql:quickload "cl-autorepo")))
  (let* ((dir "~/.local/share/common-lisp/source/")
         (autorepo-asd (merge-pathnames "cl-autorepo/cl-autorepo.asd" dir))
         (url "https://github.com/billstclair/cl-autorepo"))
    (asdf:run-shell-command "mkdir -p ~a;cd ~a;git clone ~a" dir dir url)
    (load autorepo-asd)
    (ql:quickload "cl-autorepo")))

#-windows
(progn
(cl-autorepo:add-system
 "cl-crypto" "git://github.com/billstclair/cl-crypto.git" :git)
(cl-autorepo:add-system
 "fsdb" "git://github.com/billstclair/fsdb.git" :git)
(cl-autorepo:add-system
 "limited-thread-taskmaster"
 "git://github.com/billstclair/limited-thread-taskmaster.git"
 :git)
(cl-autorepo:add-system
 "cl-loom" "git://github.com/billstclair/cl-loom.git" :git)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009-2011 Bill St. Clair
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
