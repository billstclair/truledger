; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encrypted notes
;;;
;;; The format of an encrypted note is:
;;;
;;;  [<id1>:<key1>|<id2>:<key2>,<iv>,<ciphertext>]
;;;
;;; Where the <idN> are the sender and recipient account IDs, the
;;; <keyN> are private-key encrypted password, <iv> is an
;;; initialization vector, and <ciphertext> is the mesage, encrypted
;;; with the passphrase and <iv>.
;;;

(in-package :truledger)

(defun encrypt-note (pubkeydb ids note)
  (check-type pubkeydb fsdb:db)
  (when (equal note "")
    (return-from encrypt-note note))
  (multiple-value-bind (password password-string)
      (new-crypto-session-password)
    (let* ((iv (cl-crypto:generate-iv 16))
           (ivstr (cl-crypto:iv-to-base64 iv))
           (ciphertext (cl-crypto:aes-encrypt-string
                        note password :bits 128 :iv iv))
           (first-id-p t))
      (with-output-to-string (s)
        (princ "[" s)
        (dolist (id (if (listp ids) ids (list ids)))
          (let* ((pubkey (or (fsdb:db-get pubkeydb id)
                             (error "No public key for id: ~s" id)))
                 (key (pubkey-encrypt password-string pubkey)))
            (if first-id-p
                (setf first-id-p nil)
                (princ "|" s))
            (format s "~a:~a" id key)))
        (format s ",~a,~a]" ivstr ciphertext)))))

(defun decrypt-note (id privkey encrypted-note)
  (when (equal encrypted-note "")
    (return-from decrypt-note encrypted-note))
  (destructuring-bind (id-key-string iv ciphertext)
      (parse-square-bracket-string encrypted-note)
    (when (equal ciphertext "")
      (return-from decrypt-note ciphertext))
    (unless (and id-key-string iv ciphertext)
      (error "Malformed encrypted note"))
    (let ((id-key-pairs (split-sequence:split-sequence #\| id-key-string)))
      (dolist (pair-string id-key-pairs
               (error "Note not encrypted to id: ~s" id))
        (destructuring-bind (an-id key)
            (split-sequence:split-sequence #\: pair-string)
          (when (equal an-id id)
            (let* ((password-string (privkey-decrypt key privkey))
                   (password (cl-base64:base64-string-to-usb8-array
                              password-string)))
              (return
                (cl-crypto:aes-decrypt-to-string
                 ciphertext password :bits 128 :iv iv)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2011 Bill St. Clair
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
