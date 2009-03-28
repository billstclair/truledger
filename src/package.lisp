; -*- mode: lisp -*-

(defpackage #:trubanc
  (:use :cl :cffi :cl-base64 :trubanc-loader)
  (:export

   ;; openssl-cffi.lisp
   #:fopen
   #:fclose
   #:with-fopen-file
   #:fread-string

   #:read-rsa-private-key
   #:write-rsa-private-key
   #:read-rsa-public-key
   #:write-rsa-public-key
   #:decode-rsa-private-key
   #:encode-rsa-private-key
   #:decode-rsa-public-key
   #:encode-rsa-public-key
   #:rsa-generate-key
   #:rsa-free
   #:sha1
   #:sign
   #:verify

   ;; utilities.lisp
   #:file-get-contents
   #:hex
   #:trim
   #:bin2hex
   #:hex2bin
   #:copy-memory-to-lisp
   #:base64-encode
   #:base64-decode
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http:;;;www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
