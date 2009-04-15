; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test code for server.lisp
;;;

(in-package :trubanc)

(defun custmsg (privkey &rest req)
  (signmsg privkey (apply 'simple-makemsg req)))

(defun test-newreq (server &optional
                    (privkey (privkey server))
                    (id (privkey-id privkey)))
  (let* ((msg (process
               server
               (custmsg privkey id $GETREQ (bankid server))))
         (req (unpack-bankmsg server msg $REQ nil $REQ)))
    (bcadd req 1)))

(defun test-gettime (server &optional
                     (privkey (privkey server))
                     (id (privkey-id privkey)))
  (let ((msg (process server
                      (custmsg privkey id $GETTIME
                               (bankid server)
                               (test-newreq server privkey)))))
    (unpack-bankmsg server msg $TIME nil $TIME)))

(defun test-spend (server to-id amount &key
                   (from-key (privkey server))
                   (assetid (tokenid server))
                   (note "Test spend"))
  (when (integerp amount) (setq amount (format nil "~d" amount)))
  (let* ((db (db server))
         (from-id (privkey-id from-key))
         (tokenid (tokenid server))
         (bankid (bankid server))
         (tokens (if (equal from-id bankid) 0 2))
         (balmsg (db-get db (asset-balance-key from-id assetid)))
         (bal (unpack-bankmsg server balmsg $ATBALANCE $BALANCE $AMOUNT))
         (newbal (bcsub bal amount (if (equal assetid tokenid) tokens 0)))
         (time (test-gettime server from-key from-id))
         (spendmsg (custmsg from-key from-id $SPEND bankid time to-id
                            assetid amount note))
         (balmsg (custmsg from-key from-id $BALANCE bankid time assetid newbal))
         (msg (strcat spendmsg "." balmsg)))
    (unless (equal from-id bankid)
      (error "Don't handle spends yet from other than the bank"))      
    (process server msg)))

(defun test-register (server privkey &optional name)
  (let* ((pubkey (encode-rsa-public-key privkey))
         (id (privkey-id privkey))
         (msg (apply 'custmsg privkey id $REGISTER (bankid server) pubkey
                     (and name (list name)))))
    (process server msg)))

(defun test-getinbox (server &optional (privkey (privkey server)))
  (let ((id (privkey-id privkey)))
    (process server (custmsg privkey 
                             id
                             $GETINBOX
                             (bankid server)
                             (test-newreq server privkey id)))))
                      

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
