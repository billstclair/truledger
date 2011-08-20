; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Truledger Loom client web server
;;;

(in-package :truledger-client-web)

(defstruct loom-cw
  db
  title
  menu
  body
  version
  session
  passphrase
  servers
  server
  wallets
  wallet)

;; Enter here from truledger::do-loom-web-client
(defun loom-web-server ()
  (let* ((db (make-client-db))
         (cw (loom-web-server-internal db))
         (plist (list :title (loom-cw-title cw)
                      :menu (loom-cw-menu cw)
                      :body (loom-cw-body cw)
                      :version (loom-cw-version cw))))
    (expand-template plist "index.tmpl")))

(defun loom-urlhash-preference (db account-hash)
  (loom-account-preference db account-hash $URLHASH))

(defun (setf loom-urlhash-preference) (value db account-hash)
  (check-type value string)
  (setf (loom-account-preference db account-hash $URLHASH) value))

(defun loom-namehash-preference (db account-hash)
  (loom-account-preference db account-hash $NAMEHASH))

(defun (setf loom-namehash-preference) (value db account-hash)
  (check-type value string)
  (setf (loom-account-preference db account-hash $NAMEHASH) value))

(defun loom-web-server-internal (db)
  (let* ((title "Loom Client")
         (session (get-cookie "session"))
         (cw (make-loom-cw :db db :title title :session session))
         (cmd (parm "cmd")))
    (unless (blankp session)
      (let* ((passphrase (loom-login-with-sessionid db session))
             (account-hash (loom-account-hash db passphrase))
             (urlhash (loom-urlhash-preference db account-hash))
             (servers (loom-account-servers db account-hash)))
        (cond (servers
               (let* ((server (find urlhash servers
                                    :test #'equal
                                    :key #'loom-server-urlhash))
                      (namehash (loom-namehash-preference db account-hash))
                      (wallets (loom-account-wallets db account-hash urlhash))
                      (wallet (find namehash wallets
                                    :test #'equal
                                    :key #'loom-wallet-namehash)))
                 (when (and servers (not server))
                   (setf server (car servers)
                         (loom-urlhash-preference db account-hash)
                         (loom-server-urlhash server)))
                 (when (and wallets (not wallet))
                   (setf wallet (car wallets)
                         (loom-namehash-preference db account-hash)
                         (loom-wallet-namehash wallet)))
                 (setf (loom-cw-passphrase cw) passphrase
                       (loom-cw-servers cw) servers
                       (loom-cw-server cw) server
                       (loom-cw-wallets cw) wallets
                       (loom-cw-wallet cw) wallet)
                 (cond (wallets
                        (loom-do-wallet-command cw cmd))
                       (t (loom-request-initial-wallet cw cmd)))))
              (t (loom-request-initial-server cw cmd)))))
    cw))
                 
(defparameter *loom-menu-plist*
  '(:wallet ("Wallet" "wallet")
    :contacts ("Contacts" "contacts")
    :servers ("Servers" "servers")
    :assets ("Assets" "assets")
    :truledger ("Truledger" ("./"))
    :logout ("Logout" ("./" "logout"))))

(defun make-loom-menu-plist (highlighted-key &rest item-keys)
  (let ((items (loop for key in item-keys
                  for desc = (getf *loom-menu-plist* key)
                  for name = (car desc)
                  for cmd = (cadr desc)
                  for url = (if (listp cmd)
                                (prog1
                                  (car cmd)
                                  (setf cmd (cadr cmd)))
                                "./loom")
                  collect (list :url url
                                :cmd cmd
                                :highlight (eq key highlighted-key)
                                :name name))))
    (list :title "Loom" :items items)))

(defun make-loom-menu (highlighted-key &rest item-keys)
  (let ((plist (apply #'make-loom-menu-plist highlighted-key item-keys)))
    (expand-template plist "menu.tmpl")))

(defparameter *default-loom-server-name*
  "loom.cc")

(defparameter *default-loom-server-url*
  "https://secure.loom.cc/")

(defun loom-request-initial-server (cw cmd)
  (cond ((equal cmd "new-loom-server")
         (new-loom-server cw))
        (t (setf (loom-cw-title cw) "Loom Servers - Truledger Client"
                 (loom-cw-menu cw)
                 (make-loom-menu :servers :servers :truledger :logout)
                 (loom-cw-body cw)
                 (expand-template (list :servername *default-loom-server-name*
                                        :serverurl *default-loom-server-url*)
                                  "loom-servers.tmpl")))))

(defun new-loom-server (cw)
  (with-parms (login-name servername serverurl passphrase walletname private
                          passphrase2 invitation)
    (let* ((errmsg nil)
           (db (loom-cw-db cw))
           (account-passphrase (loom-cw-passphrase cw))
           (servers (loom-cw-servers cw))
           (urlhash (and serverurl (loom-urlhash serverurl)))
           (server (find urlhash servers :test #'equal :key #'loom-server-urlhash))
           (wallets (and server (loom-server-wallets server))))
      (cond ((some #'blankp
                   (list login-name servername serverurl passphrase
                         walletname))
             (setf errmsg "All fields required except Verification and Invitation"))
            ((and server
                  (or (find walletname wallets :test #'equal
                            :key #'loom-wallet-name)
                      (find passphrase wallets :test #'equal
                            :key (lambda (wallet)
                                   (decrypt (loom-wallet-encrypted-passphrase wallet)
                                            account-passphrase)))))
             (setf errmsg "Wallet already exists"))
            ((not (eq (blankp passphrase2) (blankp invitation)))
             (setf errmsg "Verification & Invitation must both be specified"))
            ((and (not (blankp passphrase2))
                  (not (equal passphrase2 passphrase)))
             (setf errmsg "Passphrase doesn't match Verification"))
            (t
             ;; nothing here yet
             ))
      (setf (loom-cw-title cw) "Loom Servers - Truledger Client"
            (loom-cw-menu cw)
            (if (and server wallets)
                (make-loom-menu :wallet :contacts :servers :assets :truledger :logout)
                (make-loom-menu :servers :servers :truledger :logout))
            (loom-cw-body cw)
            (expand-template (list :errmsg errmsg
                                   :login-name login-name
                                   :servername servername
                                   :serverurl serverurl
                                   :passphrase passphrase
                                   :walletname walletname
                                   :private private
                                   :passphrase2 passphrase2
                                   :invitation invitation)
                             "loom-servers.tmpl")))))


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
