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
  session
  passphrase
  account-hash
  servers
  server
  wallets
  wallet)

;; Enter here from truledger::do-loom-web-client
(defun loom-web-server ()
  (catch 'raw-return
    (let* ((db (make-client-db))
           (cw (loom-web-server-internal db))
           (plist (list :title (loom-cw-title cw)
                        :menu (loom-cw-menu cw)
                        :body (loom-cw-body cw))))
      (expand-template plist "index.tmpl"))))

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

(defparameter *loom-cmd-to-function-alist*
  '(("wallet" . loom-do-wallet-command)
    ("contacts" . loom-do-contacts-command)
    ("assets" . loom-do-assets-command)
    ("servers" . loom-do-servers-command)
    ("new-loom-server" . loom-do-new-loom-server)
    ("choose-loom-wallet" . loom-do-choose-loom-wallet)
    ("wallet" . loom-do-wallet-command)
    ))

(defun get-cw-servers (cw)
  (let* ((db (loom-cw-db cw))
         (account-hash (loom-account-hash db (loom-cw-passphrase cw)))
         (urlhash (loom-urlhash-preference db account-hash))
         (servers (loom-account-servers db account-hash t)))
    (when servers
      (let* ((server (find urlhash servers
                                :test #'equal
                                :key #'loom-server-urlhash))
                  (namehash (loom-namehash-preference db account-hash))
                  (wallets (and server (loom-server-wallets server)))
                  (wallet (find namehash wallets
                                :test #'equal
                                :key #'loom-wallet-namehash)))
        (unless server
          (setf server (car servers)
                (loom-urlhash-preference db account-hash)
                (loom-server-urlhash server)))
        (when (and wallets (not wallet))
          (setf wallet (car wallets)
                (loom-namehash-preference db account-hash)
                (loom-wallet-namehash wallet)))
        (setf (loom-cw-servers cw) servers
              (loom-cw-server cw) server
              (loom-cw-wallets cw) wallets
              (loom-cw-wallet cw) wallet))
      servers)))

(defun loom-web-server-internal (db)
  (let* ((title "Loom Client")
         (session (get-cookie "session"))
         (cw (make-loom-cw :db db :title title :session session))
         (cmd (parm "cmd")))
    (unless (blankp session)
      (let* ((passphrase (loom-login-with-sessionid db session)))
        (setf (loom-cw-passphrase cw) passphrase
              (loom-cw-account-hash cw) (loom-account-hash db passphrase))
        (get-cw-servers cw)
        (cond ((loom-cw-wallets cw)
               (let ((fun (or (cdr (assoc cmd *loom-cmd-to-function-alist*
                                          :test #'equal))
                              'loom-do-wallet-command)))
                 (cond (fun (funcall fun cw))
                       (t
                        (make-cw-loom-menu cw)
                        (setf (loom-cw-title cw) "Loom - Truledger Client"
                              (loom-cw-body cw)
                              (format nil "Unknown command: ~a" cmd))))))
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

(defparameter *default-loom-server-url*
  "https://secure.loom.cc/")

(defun loom-request-initial-server (cw cmd)
  (cond ((and (equal cmd "new-loom-server")
              (parm "addwallet"))
         (loom-do-new-loom-server cw))
        (t (let* ((server (loom-cw-server cw))
                  (url (or (and server (loom-server-url server))
                           *default-loom-server-url*)))
             (setf (loom-cw-title cw) "Loom Servers - Truledger Client"
                   (loom-cw-menu cw)
                   (make-loom-menu :servers :servers :truledger :logout)
                   (loom-cw-body cw)
                   (expand-template (list :serverurl url)
                                    "loom-servers.tmpl"))))))

(defun loom-do-new-loom-server (cw)
  (with-parms (serverurl passphrase walletname private
                         passphrase2 invitation sponsorname
                         addwallet cancel)
    (declare (ignore cancel))
    (unless addwallet
      (return-from loom-do-new-loom-server
        ;; To do: go to the balance page, when that makes sense
        (loom-do-servers-command cw)))
    (let* ((errmsg nil)
           (msg nil)
           (db (loom-cw-db cw))
           (account-passphrase (loom-cw-passphrase cw))
           (servers (loom-cw-servers cw))
           (urlhash (and serverurl (loom-urlhash serverurl)))
           (server (find urlhash servers :test #'equal :key #'loom-server-urlhash))
           (wallets (and server (loom-server-wallets server)))
           (wallet nil))
      (cond ((some #'blankp
                   (list serverurl passphrase))
             (setf errmsg "Loom server url and passphrase required"))
            ((setf wallet
                   (and server
                        (or (find walletname wallets :test #'equal
                                  :key #'loom-wallet-name)
                            (find passphrase wallets :test #'equal
                                  :key (lambda (wallet)
                                         (decrypt
                                          (loom-wallet-encrypted-passphrase
                                           wallet)
                                          account-passphrase))))))
             (setf errmsg (format nil "Wallet ~s already exists."
                                  (loom-wallet-name wallet))))
            ((not (and (eq (blankp passphrase2) (blankp invitation))
                       (eq (blankp passphrase2) (blankp walletname))))
             (setf errmsg "Verification, Wallet Name, and Invitation must all be specified."))
            ((and (not (blankp passphrase2))
                  (not (equal passphrase2 passphrase)))
             (setf errmsg "Passphrase doesn't match Verification"))
            (t
             (let* ((server (loom:make-loom-uri-server serverurl))
                    (wallet
                     (loom:with-loom-transaction (:server server)
                       (or (ignore-errors
                             (loom:get-wallet passphrase t))
                           (ignore-errors
                             (prog1 (loom:get-wallet passphrase t nil t)
                               (setf private t)))))))
               (cond (wallet
                      (let ((wallet-loc (find-if #'loom:location-wallet-p
                                                 (loom:wallet-locations wallet))))
                        (cond (wallet-loc
                               (setf walletname (loom:location-name wallet-loc)
                                     msg (format nil "Existing wallet, ~s, successfully added."
                                                 walletname)))
                              (t (setf errmsg "Can't find wallet location.")))))
                     ((blankp passphrase2)
                      (setf errmsg "Verification, Wallet Name, and Invitation must be specified for a new wallet."))
                     (t
                      (when (blankp sponsorname)
                        (setf sponsorname "My Sponsor"))
                      (handler-case
                          (setf wallet
                                (loom:with-loom-transaction (:server server)
                                  (loom:create-wallet passphrase invitation
                                                      :name walletname
                                                      :sponsor-name sponsorname
                                                      :private-p private))
                                msg (format nil "New wallet, ~s, created."
                                            walletname))
                        (error (c)
                          (setf errmsg (format nil "~a" c))))))
               (unless errmsg
                 ;; We can get a duplicate when bringing in an existing
                 ;; wallet. Code above errors for a new wallet with
                 ;; a known name.
                 (loop with original = walletname
                    with idx = 1
                    while (find walletname wallets
                                :test #'equal :key #'loom-wallet-name)
                    do
                      (setf walletname
                            (format nil "~a ~a" original (incf idx))))
                 (add-loom-wallet
                  db account-passphrase
                  serverurl walletname passphrase private)
                 (get-cw-servers cw)))))
      (unless errmsg
        (setf passphrase nil
              passphrase2 nil
              invitation nil
              private nil
              sponsorname nil))
      (loom-do-servers-command
       cw
       :msg msg
       :errmsg errmsg
       :serverurl serverurl
       :passphrase passphrase
       :passphrase2 passphrase2
       :walletname walletname
       :invitation invitation
       :private private
       :sponsorname sponsorname))))

(defun make-cw-loom-menu (cw)
  (setf (loom-cw-menu cw)
        (if (loom-cw-wallet cw)
            (make-loom-menu
             :servers :wallet :contacts :servers :assets :truledger :logout)
            (make-loom-menu
             :servers :servers :truledger :logout))))

(defun loom-do-servers-command (cw &key
                                msg errmsg serverurl passphrase passphrase2
                                walletname invitation private sponsorname)
  (let* ((servers (loom-cw-servers cw))
         (default-server (loom-cw-server cw))
         (default-wallet (loom-cw-wallet cw))
         (servers-plist
          (loop for server in servers
             for urlhash = (loom-server-urlhash server)
             for server-url = (loom-server-url server)
             appending
               (loop for wallet in (loom-server-wallets server)
                  collect
                    (list :urlhash urlhash
                          :namehash (loom-wallet-namehash wallet)
                          :server-url server-url
                          :walletname (loom-wallet-name wallet)
                          :private (loom-wallet-private-p wallet)
                          :default (eq wallet default-wallet))
                  do
                    (setf server-url "")))))
    (when (blankp serverurl)
      (setf serverurl (and default-server (loom-server-url default-server))))
    (make-cw-loom-menu cw)
    (setf (loom-cw-title cw) "Loom Servers - Truledger Client"
          (loom-cw-body cw)
          (expand-template
           `(:current-server-url ,(and default-server
                                       (loom-server-url default-server))
             :current-wallet-name ,(and default-wallet
                                        (loom-wallet-name default-wallet))
             :errmsg ,(or errmsg msg)
             :serverurl ,serverurl
             :servers ,servers-plist
             ,@(when errmsg
                     (list ;; Most web forms forget the password, but why?
                      :passphrase passphrase
                      :walletname walletname
                      :passphrase2 passphrase2
                      :invitation invitation
                      :private private
                      :sponsorname sponsorname)))
           "loom-servers.tmpl"))))

(defun loom-do-choose-loom-wallet (cw)
  (with-parms (urlhash namehash private choose)
    (declare (ignore private))
    (when choose
      (let* ((db (loom-cw-db cw))
             (server (find urlhash (loom-cw-servers cw)
                           :test #'equal :key #'loom-server-urlhash))
             (wallet (and server
                          (find namehash (loom-server-wallets server)
                                :test #'equal :key #'loom-wallet-namehash)))
             (account-hash (loom-cw-account-hash cw)))
        (when wallet
          (setf (loom-urlhash-preference db account-hash) urlhash
                (loom-namehash-preference db account-hash) namehash))))
    (get-cw-servers cw)
    (loom-do-servers-command cw)))

(defun loom-cw-get-loom-wallet (cw &optional server)
  (let* ((server (or server
                     (loom:make-loom-uri-server
                      (loom-server-url (loom-cw-server cw)))))
         (wallet (loom-cw-wallet cw))
         (account-passphrase (loom-cw-passphrase cw))
         (passphrase (loom-wallet-passphrase wallet account-passphrase))
         (private-p (loom-wallet-private-p wallet)))
    (loom:with-loom-server (server)
      (loom:get-wallet passphrase t nil private-p))))

(defun loom-do-wallet-command (cw &key errmsg payamt namehash location)
  (let* ((servers (loom-cw-servers cw))
         (server (loom-cw-server cw))
         (wallets (loom-cw-wallets cw))
         (wallet (loom-cw-wallet cw))
         (other-servers (loop for s in (remove server servers)
                           collect (list :urlhash (loom-server-urlhash s)
                                         :url (loom-server-url s))))
         (other-wallets (loop for w in (remove wallet wallets)
                           collect (list :namehash (loom-wallet-namehash w)
                                         :name (loom-wallet-name w))))
         contacts
         wallet-contact
         table-contacts)
    (let ((loom-server (loom:make-loom-uri-server (loom-server-url server))))
      (loom:with-loom-transaction (:server loom-server)
        (let* ((loom-wallet (loom-cw-get-loom-wallet cw loom-server))
               (wallet-locations (loom:wallet-locations loom-wallet))
               (wallet-assets (loom:wallet-assets loom-wallet))
               (qty-alist (loom:grid-scan-wallet loom-wallet)))
          (loop for (loc . id.qty) in qty-alist
             for location = (find loc wallet-locations
                                  :test #'equal :key #'loom:location-loc)
             for contact-name = (and location (loom:location-name location))
             when location
             do
               (push (list :loc loc :name contact-name) contacts)
               (loop for (id . qty) in id.qty
                  for asset = (find id wallet-assets
                                    :test #'equal :key #'loom:asset-id)
                  when asset
                  collect (list :asset-amount qty
                                :asset-id id
                                :asset-loc loc
                                :asset-name (loom:asset-name asset)) into assets
                  finally
                    (let ((contact (list :contact-loc loc
                                         :contact-name contact-name
                                         :assets (sort assets #'string-lessp
                                                       :key (lambda (plist)
                                                              (getf plist :asset-name))))))
                      (if (loom:location-wallet-p location)
                          (setf wallet-contact contact)
                          (push contact table-contacts))))))))
    (make-cw-loom-menu cw)
    (setf (loom-cw-title cw) "Loom Wallet - Truledger Client"
          (loom-cw-body cw)
          (expand-template
           `(:current-server-url ,(loom-server-url server)
             :current-wallet-name ,(loom-wallet-name wallet)
             :other-servers ,other-servers                          
             :other-wallets ,other-wallets
             :errmsg ,errmsg
             :payamt ,payamt
             :namehash ,namehash
             :location ,location
             :contacts ,(sort contacts #'string-lessp
                              :key (lambda (plist) (getf plist :name)))
             ,@wallet-contact
             :table-contacts ,(sort table-contacts #'string-lessp
                                    :key (lambda (plist)
                                           (getf plist :contact-name))))
           "loom-wallet.tmpl"))))
    

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
