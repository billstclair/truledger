; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test code for server.lisp
;;;

(in-package :trubanc)

(defun custmsg (privkey &rest req)
  (signmsg privkey (apply 'simple-makemsg req)))

(defun test-newreq (server &optional (privkey (privkey server)))
  (let* ((msg (process
               server
               (custmsg privkey
                        (privkey-id privkey) $GETREQ (bankid server))))
         (req (unpack-bankmsg server msg $REQ nil $REQ)))
    (bcadd req 1)))

(defun test-gettime (server &optional (privkey (privkey server)))
  (let ((msg (process server
                      (custmsg privkey
                               (privkey-id privkey) $GETTIME
                               (bankid server)
                               (test-newreq server privkey)))))
    (unpack-bankmsg server msg $TIME nil $TIME)))

(defun test-spend (server to-id amount &key
                   (from-key (privkey server))
                   (assetid (tokenid server))
                   (note "Test spend"))
  (let* ((db (db server))
         (from-id (pubkey-id (encode-rsa-public-key from-key)))
         (tokenid (tokenid server))
         (bankid (bankid server))
         (tokens (unless (equal from-id bankid) 2))
         (balmsg (db-get db (asset-balance-key from-id assetid)))
         (bal (unpack-bankmsg server balmsg $ATBALANCE $BALANCE $AMOUNT))
         (newbal (bcsub bal amount))
         (time (test-gettime server from-key))
         (spendmsg (custmsg from-key from-id $SPEND bankid time from-id
                            assetid amount note))
         (balmsg (custmsg from-key to-id $BALANCE bankid time assetid newbal))
         (msg (strcat spendmsg "." balmsg)))
    (when tokens
      tokenid
      (error "Don't handle non-token spends yet"))      
    (process server msg)))

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
