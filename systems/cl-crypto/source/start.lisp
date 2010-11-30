(in-package :cl-user)

(defun load-cl-crypto-files ()
  (dolist (file '("packages"
		  "types"
		  "macros"
		  "aes16"
		  "sha1"
                  ;; Need cl-base64 & flexi-streams for this file
                  ;;"strings"
                  ))
    (load file)))

(load-cl-crypto-files)
(use-package :cl-crypto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2010 TSC AG, Postfach 73, CH 6314 Unterageri, Switzerland
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
