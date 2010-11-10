; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test code for server.lisp
;;;

(in-package :truledger)

(defun custmsg (privkey &rest req)
  (signmsg privkey (apply 'simple-makemsg req)))

(defun test-newreq (server &optional
                    (privkey (privkey server))
                    (id (privkey-id privkey)))
  (let* ((msg (process
               server
               (custmsg privkey id $GETREQ (serverid server))))
         (req (unpack-servermsg server msg $REQ nil $REQ)))
    (bcadd req 1)))

(defun test-gettime (server &optional
                     (privkey (privkey server))
                     (id (privkey-id privkey)))
  (let ((msg (process server
                      (custmsg privkey id $GETTIME
                               (serverid server)
                               (test-newreq server privkey)))))
    (unpack-servermsg server msg $TIME nil $TIME)))

(defun test-getfees (server &optional (privkey (privkey server)))
  (let ((id (privkey-id privkey)))
    (process server (custmsg privkey
                             id
                             $GETFEES
                             (serverid server)
                             (test-newreq server privkey id)))))

(defun test-spend (server to-id amount &key
                   (from-key (privkey server))
                   (assetid (tokenid server))
                   (note "Test spend"))
  (when (integerp amount) (setq amount (format nil "~d" amount)))
  (let* ((db (db server))
         (from-id (privkey-id from-key))
         (tokenid (tokenid server))
         (serverid (serverid server))
         (tokens (if (equal from-id serverid) 0 2))
         (balmsg (db-get db (asset-balance-key from-id assetid)))
         (bal (unpack-servermsg server balmsg $ATBALANCE $BALANCE $AMOUNT))
         (newbal (bcsub bal amount (if (equal assetid tokenid) tokens 0)))
         (time (test-gettime server from-key from-id))
         (spendmsg (custmsg from-key from-id $SPEND serverid time to-id
                            assetid amount note))
         (balmsg (custmsg from-key from-id $BALANCE serverid time assetid newbal))
         (msg (strcat spendmsg "." balmsg)))
    (unless (equal from-id serverid)
      (error "Don't handle spends yet from other than the server"))      
    (process server msg)))

(defun test-register (server privkey &optional name)
  (let* ((pubkey (encode-rsa-public-key privkey))
         (id (privkey-id privkey))
         (msg (apply 'custmsg privkey id $REGISTER (serverid server) pubkey
                     (and name (list name)))))
    (process server msg)))

(defun test-getinbox (server &optional (privkey (privkey server)))
  (let ((id (privkey-id privkey)))
    (process server (custmsg privkey 
                             id
                             $GETINBOX
                             (serverid server)
                             (test-newreq server privkey id)))))

(defun test-getasset (server assetid &optional (privkey (privkey server)))
  (let ((id (privkey-id privkey)))
    (process server (custmsg privkey
                             id
                             $GETASSET
                             (serverid server)
                             (test-newreq server privkey id)
                             assetid))))

(defun test-getoutbox (server &optional (privkey (privkey server)))
  (let ((id (privkey-id privkey)))
    (process server (custmsg privkey
                             id
                             $GETOUTBOX
                             (serverid server)
                             (test-newreq server privkey id)))))

(defun test-getbalance (server &optional acct asset (privkey (privkey server)))
  (let* ((id (privkey-id privkey))
         (req (test-newreq server privkey id))
         (serverid (serverid server))
         (msg (cond (asset (custmsg privkey id $GETBALANCE serverid req acct asset))
                    (acct (custmsg privkey id $GETBALANCE serverid req acct))
                    (t (custmsg privkey id $GETBALANCE serverid req)))))
    (process server msg)))

(defun test-getasset (server assetid &optional (privkey (privkey server)))
  (let ((id (privkey-id privkey)))
    (process server (custmsg privkey
                             id
                             $GETASSET
                             (serverid server)
                             (test-newreq server privkey id)
                             assetid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009-2010 Bill St. Clair
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
