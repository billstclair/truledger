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
  onload
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
           (body (loom-cw-body cw))
           (plist (list :title (loom-cw-title cw)
                        :jskeyboardp (jskeyboardp body)
                        :menu (loom-cw-menu cw)
                        :onload (loom-cw-onload cw)
                        :body body
                        :client-version *last-commit*
                        :client-date (datestr *save-application-time*))))
      (expand-template plist "index.tmpl"))))

(defparameter *loom-cmd-to-function-alist*
  '(("wallet" . loom-do-wallet-command)
    ("contacts" . loom-do-contacts-command)
    ("new-contact" . loom-do-new-contact) ;loom-contacts.tmpl
    ("enabled-contacts" . loom-do-enabled-contacts) ;loom-contacts.tmpl
    ("contact" . loom-do-contact)
    ("delete-contact" . loom-delete-contact) ;loom-contact.tmpl
    ("rename-contact" . loom-rename-contact) ;loom-contact.tmpl
    ("assets" . loom-do-assets-command)
    ("accept-asset" . loom-do-accept-asset) ;loom-assets.tmpl
    ("new-asset" . loom-do-new-asset)       ;loom-assets.tmpl
    ("enabled-assets" . loom-do-enabled-assets) ;loom-assets.tmpl
    ("asset" . loom-do-asset)
    ("delete-asset" . loom-delete-asset) ;loom-asset.tmpl
    ("rename-asset" . loom-rename-asset) ;loom-asset.tmpl
    ("edit-asset" . loom-edit-asset) ;loom-asset.tmpl
    ("servers" . loom-do-servers-command)
    ("new-loom-server" . loom-do-new-loom-server)       ;loom-servers.tmpl
    ("choose-loom-wallet" . loom-do-choose-loom-wallet) ; loom-servers.tmpl
    ("wallet-operation" . loom-do-wallet-operation)     ;loom-servers.tmpl
    ("change-wallet" . loom-do-change-wallet) ;loom-wallet.tmpl
    ("pay-or-claim" . loom-do-pay-or-claim)   ;loom-wallet.tmpl
    ("save-or-restore" . loom-do-save-or-restore) ;loom-wallet.tmpl
    ("register" . loom-do-register-command)
    ("new-registration" . loom-do-new-registration)
    ("logout" . do-logout)
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
                  (namehash (loom-namehash-preference db account-hash urlhash))
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
                (loom-namehash-preference db account-hash urlhash)
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
         (cmd (parm "cmd"))
         passphrase)
    (handler-case
        (cond ((or (blankp session)
                   (null (ignore-errors
                           (setf passphrase
                                 (loom-login-with-sessionid db session)))))
               (if (equal cmd "new-registration")
                   (loom-do-new-registration cw)
                   (loom-do-register-command cw)))
              (t (setf (loom-cw-passphrase cw) passphrase
                       (loom-cw-account-hash cw) (loom-account-hash db passphrase))
                 (get-cw-servers cw)
                 (cond ((loom-cw-wallets cw)
                        (let ((fun (or (cdr (assoc cmd
                                                   *loom-cmd-to-function-alist*
                                                   :test #'equal))
                                       'loom-do-wallet-command)))
                          (cond (fun (funcall fun cw))
                                (t
                                 (make-cw-loom-menu cw nil)
                                 (setf (loom-cw-title cw)
                                       "Loom - Truledger Client"
                                       (loom-cw-body cw)
                                       (format nil "Unknown command: ~a" cmd))))))
                       (t (loom-request-initial-server cw cmd)))))
      (error (c)
        (make-cw-loom-menu cw nil)
        (setf (loom-cw-title cw) "Loom Error - Truledger Client"
              (loom-cw-body cw) (hsc (format nil "~a" c)))))
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
                   (loom-cw-onload cw)
                   "document.getElementById(\"serverurl\").focus()"
                   (loom-cw-body cw)
                   (expand-template (list :serverurl url
                                          :loom-compatible t)
                                    "loom-servers.tmpl"))))))

(defun load-loom-walletname (server passphrase)
  (let* ((private nil)
         (walletname nil)
         (errmsg nil)
         (wallet
          (loom:with-loom-transaction (:server server)
            (or (ignore-errors
                  (loom:get-wallet passphrase t))
                (ignore-errors
                  (prog1 (loom:get-wallet passphrase t nil t)
                    (setf private t)))))))
    (when wallet
      (let ((wallet-loc (find-if #'loom:location-wallet-p
                                 (loom:wallet-locations wallet))))
        (cond (wallet-loc
               (setf walletname (loom:location-name wallet-loc)))
              (t (setf errmsg "Can't find wallet location.")))))
    (values errmsg walletname private)))

(defun create-loom-wallet (server passphrase invitation walletname
                           sponsorname private-p)
  "Create a new loom wallet on server.
Return two values: wallet and errmsg"
  (handler-case
      (loom:with-loom-transaction (:server server)
        (loom:create-wallet passphrase invitation
                            :name walletname
                            :sponsor-name sponsorname
                            :private-p private-p))
    (error (c)
       (values nil (format nil "~a" c)))))

(defun find-unique-walletname (wallets walletname)
  (loop with original = walletname
     with idx = 1
     while (find walletname wallets
                 :test #'equal :key #'loom-wallet-name)
     do
       (setf walletname
             (format nil "~a ~a" original (incf idx))))
  walletname)

(defun loom-do-new-loom-server (cw)
  (with-parms (serverurl passphrase walletname loom-compatible
                         passphrase2 invitation sponsorname
                         addwallet cancel)
    (declare (ignore cancel))
    (unless addwallet
      (return-from loom-do-new-loom-server
        ;; To do: go to the balance page, when that makes sense
        (loom-do-servers-command cw)))
    (let* ((errmsg nil)
           (private (not loom-compatible))
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
             (let* ((server (make-loom-uri-server db serverurl))
                    (loom-walletname nil)
                    (private-p nil))
               (multiple-value-setq (errmsg loom-walletname private-p)
                 (load-loom-walletname server passphrase))
               (cond ((or errmsg loom-walletname)
                      (unless errmsg
                        (setf walletname loom-walletname
                              private private-p
                              msg (format
                                   nil "Existing wallet, ~s, successfully added."
                                   walletname))))
                     ((blankp passphrase2)
                      (setf errmsg "Verification, Wallet Name, and Invitation must be specified for a new wallet."))
                     (t
                      (when (blankp sponsorname)
                        (setf sponsorname "My Sponsor"))
                      (multiple-value-setq (wallet errmsg)
                        (create-loom-wallet server passphrase invitation
                                            walletname sponsorname private))))
               (unless errmsg
                 ;; We can get a duplicate when bringing in an existing
                 ;; wallet. Code above errors for a new wallet with
                 ;; a known name.
                 (setf walletname (find-unique-walletname wallets walletname))
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
       :errmsg (hsc errmsg)
       :serverurl (hsc serverurl)
       :passphrase (hsc passphrase)
       :passphrase2 (hsc passphrase2)
       :walletname (hsc walletname)
       :invitation (hsc invitation)
       :private private
       :sponsorname (hsc sponsorname)))))

(defun make-cw-loom-menu (cw current-page)
  (setf (loom-cw-menu cw)
        (cond ((loom-cw-wallet cw)
               (make-loom-menu
                current-page :wallet :contacts :servers :assets :truledger :logout))
              ((loom-cw-session cw)
               (make-loom-menu current-page :servers :truledger :logout))
              (t (make-loom-menu nil :truledger)))))

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
                          :loom-compatible (not (loom-wallet-private-p wallet))
                          :default (eq wallet default-wallet))
                  do
                    (setf server-url "")))))
    (when (blankp serverurl)
      (setf serverurl (and default-server (loom-server-url default-server))))
    (make-cw-loom-menu cw :servers)
    (setf (loom-cw-title cw) "Loom Servers - Truledger Client"
          (loom-cw-onload cw)
          "document.getElementById(\"serverurl\").focus()"
          (loom-cw-body cw)
          (expand-template
           `(:current-server-url ,(and default-server
                                       (loom-server-url default-server))
             :current-wallet-name ,(and default-wallet
                                        (loom-wallet-name default-wallet))
             :errmsg ,(or errmsg msg)
             :serverurl ,serverurl
             :servers ,servers-plist
             :loom-compatible ,(not private)
             ,@(when errmsg
                     (list ;; Most web forms forget the password, but why?
                      :passphrase passphrase
                      :walletname walletname
                      :passphrase2 passphrase2
                      :invitation invitation
                      :sponsorname sponsorname)))
           "loom-servers.tmpl"))))

(defun loom-do-choose-loom-wallet (cw)
  (with-parms (urlhash namehash loom-compatible choose)
    (declare (ignore loom-compatible))
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
                (loom-namehash-preference db account-hash urlhash) namehash))))
    (get-cw-servers cw)
    (loom-do-wallet-command cw)))

(defun loom-do-wallet-operation (cw)
  (with-parms (urlhash namehash
                       new-wallet-name rename
                       remove confirm-remove
                       merge-wallet delete confirm-delete)
    (let* ((servers (loom-cw-servers cw))
           (current-namehash (and (equal urlhash
                                         (loom-server-urlhash (loom-cw-server cw)))
                                  (loom-wallet-namehash (loom-cw-wallet cw))))
           (server (find urlhash servers :test #'equal :key #'loom-server-urlhash))
           (wallets (and server (loom-server-wallets server)))
           (wallet (find namehash wallets
                         :test #'equal :key #'loom-wallet-namehash))
           (db (loom-cw-db cw))
           (passphrase (loom-cw-passphrase cw))
           errmsg)
      (cond (wallet
             (handler-case
                 (cond (rename
                        (unless (blankp new-wallet-name)
                          (loom-rename-wallet
                           db passphrase urlhash namehash new-wallet-name)
                          (get-cw-servers cw)
                          (return-from loom-do-wallet-operation
                            (loom-do-servers-command
                             cw :errmsg "Wallet renamed.")))
                        (setf errmsg
                              "You must specify the new Wallet Name."))
                       (remove
                        (when confirm-remove
                          (loom-remove-wallet
                           db (loom-cw-account-hash cw) urlhash namehash)
                          (get-cw-servers cw)
                          (return-from loom-do-wallet-operation
                            (loom-do-servers-command
                             cw :errmsg "Wallet removed.")))
                        (error
                         "You must check the Confirm box to remove a wallet."))
                       (delete
                        (when confirm-delete
                          (loom-merge-wallet
                           db passphrase urlhash namehash merge-wallet)
                          (get-cw-servers cw)
                          (return-from loom-do-wallet-operation
                            (loom-do-servers-command
                             cw :errmsg "Wallet merged.")))
                        (setf errmsg
                              "You must check the Confirm box to delete a wallet.")))
               (error (c)
                 (setf errmsg (princ-to-string c))))
             (let ((wallet-name (loom-wallet-name wallet))
                   (wallet-options
                    (loop for wallet in wallets
                       for wallet-namehash = (loom-wallet-namehash wallet)
                       unless (equal namehash (loom-wallet-namehash wallet))
                       collect (list :namehash wallet-namehash
                                     :name (hsc (loom-wallet-name wallet))
                                     :selected-p (equal wallet-namehash
                                                        (or merge-wallet
                                                            current-namehash))))))
               (make-cw-loom-menu cw :servers)
               (setf (loom-cw-title cw) "Loom Wallet Operation - Truledger Client"
                     (loom-cw-onload cw)
                     "document.getElementById(\"new-wallet-name\").focus()"
                     (loom-cw-body cw)
                     (expand-template
                      (list :errmsg errmsg
                            :current-server-url (hsc (loom-server-url server))
                            :current-wallet-name (hsc wallet-name)
                            :urlhash (loom-server-urlhash server)
                            :namehash (loom-wallet-namehash wallet)
                            :new-wallet-name (hsc (or new-wallet-name wallet-name))
                            :wallet-options wallet-options)
                      "loom-wallet-operation.tmpl"))
               cw))
            (t (loom-do-servers-command cw :errmsg "Unknown server or wallet"))))))

(defun loom-cw-get-loom-wallet (cw &key server (force-server-p t))
  (let* ((server (or server
                     (make-loom-uri-server
                      (loom-cw-db cw)
                      (loom-server-url (loom-cw-server cw)))))
         (wallet (loom-cw-wallet cw))
         (account-passphrase (loom-cw-passphrase cw)))
    (or (and (not force-server-p)
             (loom-stored-wallet wallet account-passphrase))
        (let* ((passphrase (loom-wallet-passphrase wallet account-passphrase))
               (private-p (loom-wallet-private-p wallet))
               (loom-wallet (loom:with-loom-server (server)
                              (loom:get-wallet passphrase t nil private-p))))
          (store-loom-wallet (loom-cw-db cw) account-passphrase wallet loom-wallet)
          loom-wallet))))

(defun loom-do-wallet-command (cw &key errmsg payamt (claimall t) contact-name)
  (let* ((servers (loom-cw-servers cw))
         (server (loom-cw-server cw))
         (wallets (loom-cw-wallets cw))
         (wallet (loom-cw-wallet cw))
         (private-p (loom-wallet-private-p wallet))
         (other-servers (loop for s in (remove server servers)
                           collect (list :urlhash (loom-server-urlhash s)
                                         :url (loom-server-url s))))
         (other-wallets (loop for w in (remove wallet wallets)
                           collect (list :namehash (loom-wallet-namehash w)
                                         :name (loom-wallet-name w))))
         contacts
         wallet-contact
         table-contacts
         wallet-loc
         servers-cipher-text
         save-erase-warning-p
         (save-loom-warning-p (not private-p))
         (save-confirm-p save-loom-warning-p)
         save-disabled-p
         show-restore-p
         restore-disabled-p
         show-remove-p
         )
    (unless contact-name
      (setf contact-name (with-parms (contact-name) contact-name)))
    (let ((loom-server (make-loom-uri-server
                        (loom-cw-db cw) (loom-server-url server))))
      (loom:with-loom-transaction (:server loom-server)
        (let* ((loom-wallet (loom-cw-get-loom-wallet
                             cw :server loom-server))
               (wallet-locations (loom:wallet-locations loom-wallet))
               (wallet-assets (loom:wallet-assets loom-wallet))
               (qty-alist (loom:grid-scan-wallet loom-wallet))
               (wallet-location (find-if #'loom:location-wallet-p wallet-locations)))
          (setf wallet-loc
                (and wallet-locations (loom:location-loc wallet-location))
                servers-cipher-text (loom:wallet-get-property
                                     loom-wallet $truledger-saved-servers))
          (loop for location in (loom:wallet-locations loom-wallet)
             for name = (loom:location-name location)
             for loc = (loom:location-loc location)
             do
               (unless (or (equal loc wallet-loc)
                           (loom:location-disabled-p location))
                 (push (list :name (hsc name)
                             :select (equal name contact-name))
                       contacts)))
          (loop for (loc . id.qty) in qty-alist
             for location = (find loc wallet-locations
                                  :test #'equal :key #'loom:location-loc)
             for contact-name = (and location (loom:location-name location))
             for loc-hash = (folded-hash loc)
             when location
             do
               (loop for (id . qty) in id.qty
                  for asset = (loom:find-asset-by-id id wallet-assets)
                  for asset-name = (and asset (loom:asset-name asset))
                  when asset
                  collect (list :asset-amount qty
                                :asset-id id
                                :asset-loc-hash (hsc loc-hash)
                                :asset-name (hsc asset-name))
                  into assets
                  finally
                    (let ((contact (list :contact-url-name (url-rewrite:url-encode
                                                            contact-name)
                                         :contact-name (hsc contact-name)
                                         :assets (sort assets #'string-lessp
                                                       :key (lambda (plist)
                                                              (getf plist :asset-name))))))
                      (if (loom:location-wallet-p location)
                          (setf wallet-contact contact)
                          (push contact table-contacts))))))))

    (when servers-cipher-text
      (setf show-remove-p t)
      (let* ((account-passphrase (loom-cw-passphrase cw))
             (saved-servers (loom-decode-servers-from-cipher-text
                            account-passphrase servers-cipher-text
                            (loom-wallet-passphrase wallet account-passphrase))))
        (multiple-value-bind (saving-changes-p saving-loses-p restoring-changes-p)
            (loom-compare-servers-to-saved
             account-passphrase servers saved-servers)
          (setf save-loom-warning-p nil
                save-confirm-p nil)
          (unless saving-changes-p
            (setf save-disabled-p t))
          (when saving-loses-p
            (setf save-erase-warning-p t
                  save-confirm-p t))
          (setf show-restore-p t)
          (unless restoring-changes-p
            (setf restore-disabled-p t)))))          
          
    (make-cw-loom-menu cw :wallet)
    (setf (loom-cw-title cw) "Loom Wallet - Truledger Client"
          (loom-cw-onload cw) "document.getElementById(\"payamt\").focus()"
          (loom-cw-body cw)
          (expand-template
           `(:current-server-url ,(hsc (loom-server-url server))
             :current-wallet-name ,(hsc (loom-wallet-name wallet))
             :urlhash ,(loom-server-urlhash server)
             :namehash ,(loom-wallet-namehash wallet)
             :other-servers ,other-servers                          
             :other-wallets ,other-wallets
             :errmsg ,errmsg
             :payamt ,payamt
             :claimall ,claimall
             :contacts ,(sort contacts #'string-lessp
                              :key (lambda (plist) (getf plist :name)))
             ,@wallet-contact
             :table-contacts ,(sort table-contacts #'string-lessp
                                    :key (lambda (plist)
                                           (getf plist :contact-name)))
             :save-erase-warning-p ,save-erase-warning-p
             :save-loom-warning-p ,save-loom-warning-p
             :save-confirm-p ,save-confirm-p
             :save-disabled-p ,save-disabled-p
             :show-restore-p ,show-restore-p
             :restore-disabled-p ,restore-disabled-p
             :show-remove-p ,show-remove-p)
           "loom-wallet.tmpl"))))

(defun loom-do-change-wallet (cw)
  (with-parms (urlhash namehash changeserver changewallet)
    (let ((errmsg nil))
      (cond (changeserver
             (cond ((find urlhash (loom-cw-servers cw)
                          :test #'equal :key #'loom-server-urlhash)
                    (setf (loom-urlhash-preference
                           (loom-cw-db cw) (loom-cw-account-hash cw))
                          urlhash))
                   (t (setf errmsg "Can't find that server."))))
            (changewallet
             (let ((wallet (find namehash (loom-cw-wallets cw)
                                 :test #'equal :key #'loom-wallet-namehash)))
               (cond (wallet
                      (setf (loom-namehash-preference
                             (loom-cw-db cw)
                             (loom-cw-account-hash cw)
                             (loom-wallet-urlhash wallet))
                            namehash))
                     (t (setf errmsg "Can't find that wallet.")))))
            (t (setf errmsg "Unknown change-wallet button.")))
      (unless errmsg
        (get-cw-servers cw))
      (loom-do-wallet-command cw :errmsg errmsg))))
    
(defun loom-do-pay-or-claim (cw)
  (with-parms (urlhash namehash payamt claimall contact-name)
    (let ((errmsg nil))
      (handler-case (loom-do-pay-or-claim-internal
                     cw urlhash namehash payamt claimall contact-name)
        (error (c)
          (setf errmsg (hsc (format nil "~a" c)))
          (loom-do-wallet-command
           cw :errmsg errmsg :payamt payamt :contact-name contact-name))))))

(defun loom-do-pay-or-claim-internal (cw urlhash namehash
                                      payamt claimall contact-name)
  (let* ((servers (loom-cw-servers cw))
         (server (find urlhash servers :test #'equal :key #'loom-server-urlhash))
         (wallet (and server
                      (find namehash (loom-server-wallets server)
                            :test #'equal :key #'loom-wallet-namehash)))
         (db (loom-cw-db cw))
         (account-passphrase (loom-cw-passphrase cw))
         (account-hash (loom-cw-account-hash cw))
         (wallet-loc (and wallet (loom-wallet-location wallet account-passphrase)))
         (errmsg nil)
         (pay-id nil)
         (claim-loc-hash nil)
         (claim-id nil))
    (cond (wallet
           (setf (loom-urlhash-preference db account-hash) urlhash
                 (loom-namehash-preference db account-hash urlhash) namehash)
           (get-cw-servers cw))
          (t (setf errmsg "Unknown server or wallet.")))
    (let ((pay-prefix "pay-")
          (claim-prefix "claim-"))
      (unless errmsg
        (dolist (cell (hunchentoot:post-parameters hunchentoot:*request*))
          (let ((name (car cell)))
            (cond ((eql 0 (search pay-prefix name :test #'equal))
                   (cond ((or (blankp payamt) (blankp contact-name))
                          (setf errmsg "Pay/Claim Amount & Contact must be specified."))
                         (t (setf pay-id (subseq name (length pay-prefix)))))
                   (return))
                  ((eql 0 (search claim-prefix name :test #'equal))
                   (let ((loc-and-id (split-sequence:split-sequence
                                      #\- (subseq name (length claim-prefix)))))
                     (cond ((eql 2 (length loc-and-id))
                            (setf claim-loc-hash (first loc-and-id)
                                  claim-id (second loc-and-id)))
                           (t (setf errmsg "Badly formed claim value"))))
                   (return))))))
      (unless (or errmsg pay-id claim-id)
        (setf errmsg "Neither pay nor claim button pressed."))
      (unless errmsg
        (let* ((loom-server (make-loom-uri-server db (loom-server-url server))))
          (loom:with-loom-transaction (:server loom-server)
            (let* ((loom-wallet (loom-cw-get-loom-wallet
                                 cw :server loom-server :force-server-p nil))
                   (wallet-locations
                    (and loom-wallet (loom:wallet-locations loom-wallet)))
                   (id (or pay-id claim-id)))
              (let* ((asset (loom:find-asset-by-id id loom-wallet))
                     (scale (and asset (loom:asset-scale asset)))
                     (precision (and asset (loom:asset-precision asset)))
                     (qty (and asset
                               (not (blankp payamt))
                               (loom:unformat-loom-qty payamt scale))))
                (cond ((null asset)
                       (setf errmsg "Can't find asset."))
                      (pay-id
                       (let ((contact-loc
                              (loom:find-location
                               contact-name wallet-locations t)))
                         (cond (contact-loc
                                (loom:grid-buy pay-id contact-loc wallet-loc t)
                                (loom:grid-move pay-id qty wallet-loc contact-loc))
                               (t (setf errmsg
                                        (format nil "Can't find contact: ~s"
                                                contact-name))))))
                      (claim-id
                       (let* ((claim-location
                               (find claim-loc-hash wallet-locations
                                     :test #'equal
                                     :key (lambda (location)
                                            (folded-hash
                                             (loom:location-loc location)))))
                              (claim-loc (and claim-location
                                              (loom:location-loc claim-location)))
                              (real-qty
                               (or (and (not claimall) qty)
                                   (loom:grid-touch claim-id claim-loc))))
                         (cond ((< real-qty 0)
                                (setf errmsg "Can't claim all from issuer."))
                               (claim-loc
                                (loom:grid-buy claim-id wallet-loc wallet-loc t)
                                (loom:grid-move
                                 claim-id real-qty claim-loc wallet-loc)
                                (ignore-errors
                                  (loom:grid-sell claim-id claim-loc wallet-loc))
                                (setf payamt (loom:format-loom-qty
                                              real-qty scale precision)
                                      contact-name
                                      (loom:location-name claim-location)))
                               (t (setf errmsg
                                        "Can't find claim location.")))))))))))
      (loom-do-wallet-command
       cw
       :errmsg errmsg
       :payamt payamt
       :claimall claimall
       :contact-name contact-name))))

(defun loom-do-save-or-restore (cw)
  (with-parms (save save-confirm restore remove remove-confirm)
    (let* ((errmsg nil)
           (db (loom-cw-db cw))
           (account-passphrase (loom-cw-passphrase cw))
           (server (loom-cw-server cw))
           (wallet (loom-cw-wallet cw))
           (urlhash (loom-server-urlhash server))
           (namehash (loom-wallet-namehash wallet)))
      (handler-case
          (cond (save
                 (cond (save-confirm
                        (loom-save-wallets db account-passphrase urlhash namehash)
                        (setf errmsg "Server information saved to wallet."))
                       (t (setf
                           errmsg
                           "You must check the \"Confirm\" box to save servers."))))
                (restore
                 (loom-restore-saved-wallets
                  db account-passphrase urlhash namehash)
                 (get-cw-servers cw)
                 (setf errmsg "Server information restored from wallet."))
                (remove
                 (cond (remove-confirm
                        (loom-remove-saved-wallets
                         db account-passphrase urlhash namehash)
                        (setf errmsg "Server information removed from wallet."))
                       (t (setf errmsg "You must check the \"Confirm\" box to remove saved servers.")))))
        (error (c)
          (setf errmsg (princ-to-string c))))
      (loom-do-wallet-command cw :errmsg errmsg))))

(defun loom-do-register-command (cw)
  (when (loom-cw-session cw)
    (return-from loom-do-register-command
      (loom-do-servers-command cw)))
  (make-cw-loom-menu cw nil)
  (setf (loom-cw-title cw) "Loom Register - Truledger Client"
        (loom-cw-onload cw) "document.getElementById(\"account-passphrase\").focus()"
        (loom-cw-body cw)
        (expand-template
         (list :serverurl (hsc *default-loom-server-url*)
               :sponsorname "My Sponsor"
               :loom-compatible t)
         "loom-register.tmpl")))  

(defun loom-login-for-client (client passphrase)
  (let* ((db (db client))
         (account-hash (loom-account-hash db passphrase)))
    (when (loom-urlhash-preference db account-hash)
      (loom-make-session db passphrase))))

(defun loom-do-new-registration (cw)
  (with-parms (account-passphrase account-passphrase2 serverurl
               passphrase passphrase2 walletname invitation sponsorname
               loom-compatible newacct login)
    (let* ((db (loom-cw-db cw))
           (account-hash (loom-account-hash db account-passphrase))
           (loom-passphrase (if (blankp passphrase) account-passphrase passphrase))
           (urlhash (and (not (blankp serverurl)) (loom-urlhash serverurl)))
           (private (not loom-compatible))
           wallet unused-invitation-message errmsg)
      (setf (loom-cw-passphrase cw) account-passphrase
            (loom-cw-account-hash cw) account-hash)
      (when (loom-urlhash-preference db account-hash)
        (setf (loom-cw-session cw) (loom-make-session db account-passphrase))
        (set-cookie "session" (loom-cw-session cw))
        (get-cw-servers cw)
        (setf unused-invitation-message
              (unless (blankp invitation)
                (format nil "Unused invitation: ~a" invitation)))
        (when (and urlhash (not login))
          (let ((server (find urlhash (loom-cw-servers cw)
                              :test #'equal :key #'loom-server-urlhash)))
            (when server
              (setf (loom-urlhash-preference db account-hash) urlhash)
              (unless (blankp loom-passphrase)
                (setf wallet (find loom-passphrase (loom-server-wallets server)
                                   :test #'equal
                                   :key (lambda (wallet)
                                          (loom-wallet-passphrase
                                           wallet account-passphrase))))
                (when wallet
                  (setf (loom-namehash-preference db account-hash urlhash)
                        (loom-wallet-namehash wallet))))
              (get-cw-servers cw)))))
      (cond (login
             (when (loom-cw-session cw)
               (return-from loom-do-new-registration
                 (loom-do-wallet-command
                  cw
                  :errmsg unused-invitation-message)))
             ;; Should try logging in to Truledger here
             (setf errmsg "No loom account with that passphrase."))
            (wallet
             (return-from loom-do-new-registration
               (loom-do-wallet-command cw :errmsg unused-invitation-message)))
            ((not newacct)
             (setf errmsg "No button pressed."))
            ((or (blankp account-passphrase) (blankp serverurl))
             (setf errmsg "Passphrase and Loom Server URL must be supplied."))
            ((not (or urlhash (equal account-passphrase account-passphrase2)))
             (setf errmsg "Passphrase doesn't match verification."))
            ((not (or (blankp passphrase)
                      (blankp invitation)
                      (equal passphrase passphrase2)))
             (setf errmsg "Loom Server Passphrase doesn't match Verification.")))
      (unless errmsg
        (let* ((loom-server (make-loom-uri-server db serverurl))
               loom-walletname
               private-p)
          (multiple-value-setq (errmsg loom-walletname private-p)
            (load-loom-walletname loom-server loom-passphrase))
          (cond ((or errmsg loom-walletname)
                 ;; Either the server is invalid or the wallet already exists
                 (unless errmsg
                   (let* ((server (loom-cw-server cw))
                          (wallets (and server
                                        (equal urlhash (loom-server-urlhash server))
                                        (loom-server-wallets server))))
                     (setf walletname
                           (find-unique-walletname wallets loom-walletname)
                           private private-p))))
                (t
                 (when (blankp sponsorname)
                   (setf sponsorname "My Sponsor"))
                 (setf errmsg (nth-value 1 (create-loom-wallet
                                            loom-server loom-passphrase invitation
                                            walletname sponsorname private)))))))
      (unless errmsg
        (setf (loom-urlhash-preference db account-hash) urlhash)
        (let ((namehash (add-loom-wallet
                         db account-passphrase serverurl walletname
                         loom-passphrase private)))
          (setf (loom-namehash-preference db account-hash urlhash) namehash))
        (get-cw-servers cw))
      (make-cw-loom-menu cw nil)
      (cond (errmsg
             (setf (loom-cw-title cw) "Loom Register - Truledger Client"
                   (loom-cw-body cw)
                   (expand-template
                    (list :errmsg errmsg
                          :account-passphrase (hsc account-passphrase)
                          :account-passphrase2 (hsc account-passphrase2)
                          :serverurl (hsc serverurl)
                          :passphrase (hsc passphrase)
                          :passphrase2 (hsc passphrase2)
                          :walletname (hsc walletname)
                          :invitation (hsc invitation)
                          :sponsorname (hsc sponsorname)
                          :loom-compatible (not private))
                    "loom-register.tmpl"))
             cw)
            (t
             (setf (loom-cw-session cw) (loom-make-session db account-passphrase))
             (set-cookie "session" (loom-cw-session cw))
             (loom-do-wallet-command cw))))))

(defmethod do-logout ((cw loom-cw) &optional no-draw)
  (delete-cookie "session")
  (unless no-draw
    (throw 'raw-return
      (let ((*blank-cookies-p* t))
        (web-server)))))

(defun loom-do-contacts-command (cw &key errmsg (id (loom:random-loc)) name)
  (let* ((server (loom-cw-server cw))
         (wallet (loom-cw-wallet cw))
         (loom-wallet (loom-cw-get-loom-wallet cw :force-server-p t))
         (first-p t)
         (contacts (loop for location in (loom:wallet-locations loom-wallet)
                      for name = (loom:location-name location)
                      for loc = (loom:location-loc location)
                      for loc-hash = (folded-hash loc)
                      collect (list :enabled-name (unless first-p
                                                    (strcat "enabled-" loc-hash))
                                    :enabled (not (loom:location-disabled-p
                                                   location))
                                    :url-encoded-name (url-rewrite:url-encode name)
                                    :name (hsc name))
                      do (setf first-p nil))))
    (setf contacts (cons (car contacts)
                         (sort (cdr contacts) #'string-lessp
                               :key (lambda (plist) (getf plist :name)))))
    (make-cw-loom-menu cw :contacts)
    (setf (loom-cw-title cw) "Loom Contacts - Truledger Client"
          (loom-cw-onload cw) "document.getElementById(\"id\").focus()"
          (loom-cw-body cw) 
          (expand-template
           (list :errmsg errmsg
                 :server-url (hsc (loom-server-url server))
                 :wallet-name (hsc (loom-wallet-name wallet))
                 :id id
                 :name name
                 :contacts contacts)
           "loom-contacts.tmpl"))
    cw))

(defun loom-do-new-contact (cw)
  (with-parms (id name fund)
    (let* ((server (loom-cw-server cw))
           (loom-server (make-loom-uri-server
                         (loom-cw-db cw) (loom-server-url server)))
           errmsg)
      (loom:with-loom-transaction (:server loom-server)
        (let* ((loom-wallet (loom-cw-get-loom-wallet cw :server loom-server))
               (locations (loom:wallet-locations loom-wallet))
               (id-location (find id locations
                                  :test #'equal
                                  :key #'loom:location-loc))
               (wallet-location (find-if #'loom:location-wallet-p locations))
               (wallet-loc (and wallet-location
                                (loom:location-loc wallet-location))))
          (cond ((loom:find-location name locations)
                 (setf errmsg (format nil
                                      "You already have a contact named ~s"
                                      name)))
                (id-location
                 (setf errmsg (format nil "Contact ~s is at that location."
                                      (loom:location-name id-location))))
                ((not wallet-location)
                 (setf errmsg "Can't find wallet location."))
                (t
                 (when fund
                   (let ((bal (loom:grid-touch loom:*zero* wallet-loc t t)))
                     (unless (or (< bal 0) (>= bal 101))
                       (setf errmsg "You don't have 101 usage tokens."))))
                 (unless errmsg
                   (handler-case
                       (progn
                         (setf (loom:wallet-locations loom-wallet)
                               `(,@locations
                                 ,(loom:make-location :name name :loc id))
                               (loom:get-wallet wallet-loc)
                               loom-wallet)
                         (when fund
                           (loom:grid-buy loom:*zero* id wallet-loc t)
                           (loom:grid-move loom:*zero* 100 wallet-loc id)))
                     (error (c)
                       (setf errmsg (princ-to-string c)))))))))
      (if errmsg
          (loom-do-contacts-command
           cw
           :errmsg errmsg
           :id id
           :name name)
          (loom-do-contact cw :errmsg  "Added new contact." :name name)))))

(defun loom-do-enabled-contacts (cw)
  (let* ((prefix "enabled-")
         (prefix-len (length prefix))
         (hashes (loop for cell in (hunchentoot:post-parameters
                                    hunchentoot:*request*)
                    for name = (car cell)
                    when (eql 0 (search prefix name :test #'equal))
                    collect (subseq name prefix-len)))
         (server (loom-cw-server cw))
         (loom-server (make-loom-uri-server
                       (loom-cw-db cw) (loom-server-url server)))
         errmsg)
    (loom:with-loom-transaction (:server loom-server)
      (let* ((wallet (loom-cw-get-loom-wallet cw :server loom-server))
             (locations (loom:wallet-locations wallet))
             (wallet-location (find-if #'loom:location-wallet-p locations))
             (wallet-loc (and wallet-location (loom:location-loc wallet-location)))
             (changed-p nil))
        (unless wallet-loc
          (setf errmsg "Can't find wallet location."))
        (unless errmsg
          (dolist (location locations)
            (unless (loom:location-wallet-p location)
              (let ((enabled-p (member (folded-hash (loom:location-loc location))
                                       hashes
                                       :test #'equal)))
                (unless (xor enabled-p (loom:location-disabled-p location))
                  (setf changed-p t
                        (loom:location-disabled-p location) (not enabled-p))))))
          (when changed-p
            (handler-case
                (setf (loom:get-wallet wallet-loc) wallet)
              (error (c)
                (setf errmsg (princ-to-string c))))))))
    (loom-do-contacts-command cw :errmsg errmsg)))

(defun loom-do-contact (cw &key errmsg name deleting-p include-rename-p new-name)
  (unless name
    (multiple-value-setq (name deleting-p include-rename-p)
      (with-parms (name operation)
        (values name (equal operation "delete") (equal operation "rename")))))
  (let* ((server (loom-cw-server cw))
         (wallet (loom-cw-wallet cw))
         (loom-server (make-loom-uri-server
                       (loom-cw-db cw) (loom-server-url server)))
         loom-wallet location loc wallet-assets qty-alist assets)
    (loom:with-loom-transaction (:server loom-server)
      (setf loom-wallet (loom-cw-get-loom-wallet cw :server loom-server)
            location (loom:find-location name (loom:wallet-locations loom-wallet)))
      (when location
        (setf loc (loom:location-loc location)
              wallet-assets (loom:wallet-assets loom-wallet)
              qty-alist (loom:grid-scan-wallet loom-wallet
                                               :locations (list location)))))

    (unless location
      (return-from loom-do-contact
        (loom-do-contacts-command
         cw :errmsg (format nil "No such contact: ~s" name))))

    (loop for (id . qty) in (cdar qty-alist)
       for asset = (loom:find-asset-by-id id wallet-assets)
       for asset-name = (and asset (loom:asset-name asset))
       when asset do
         (push (list :asset-amount qty
                     :asset-id id
                     :asset-name (hsc asset-name))
               assets))
    (setf assets (sort assets #'string-lessp
                       :key #'(lambda (plist) (getf plist :asset-name))))
    (make-cw-loom-menu cw :contacts)
        (setf (loom-cw-title cw) "Loom Contact - Truledger Client"
              (loom-cw-onload cw)
              (and include-rename-p "document.getElementById(\"new-name\").focus()")
              (loom-cw-body cw)
              (expand-template
               (list :errmsg errmsg
                     :server-url (hsc (loom-server-url server))
                     :wallet-name (hsc (loom-wallet-name wallet))
                     :name (hsc name)
                     :loc (if (loom:location-wallet-p location)
                              "&lt;secret>"
                              loc)
                     :non-wallet-p (not (loom:location-wallet-p location))
                     :url-encoded-name (url-rewrite:url-encode name)
                     :include-options-p (not (or deleting-p include-rename-p))
                     :include-assets-p (and assets (not include-rename-p))
                     :include-rename-p include-rename-p
                     :deleting-p deleting-p
                     :assets assets
                     :new-name (if (blankp new-name) name (hsc new-name))
                     )
               "loom-contact.tmpl"))
        cw))

(defun loom-delete-contact (cw)
  (with-parms (name confirm)
    (unless confirm
      (return-from loom-delete-contact
        (loom-do-contact cw
                         :errmsg "You must check the box to delete the contact."
                         :name name
                         :deleting-p t)))
    (let* ((server (loom-cw-server cw))
           (loom-server (make-loom-uri-server
                         (loom-cw-db cw) (loom-server-url server)))
           errmsg)
      (loom:with-loom-transaction (:server loom-server)
        (let* ((loom-wallet (loom-cw-get-loom-wallet cw :server loom-server))
               (locations (loom:wallet-locations loom-wallet))
               (wallet-location (find-if #'loom:location-wallet-p locations))
               (location (loom:find-location name locations)))
          (cond ((not wallet-location)
                 (setf errmsg "Can't find wallet location."))
                ((eq location wallet-location)
                 (setf errmsg "You may not delete your wallet."))
                (location
                 (handler-case
                     (setf (loom:wallet-locations loom-wallet)
                           (delete location locations :test #'eq)
                           (loom:get-wallet (loom:location-loc wallet-location))
                           loom-wallet)
                   (error (c)
                     (setf errmsg (princ-to-string c)))))
                (t (setf errmsg (format nil "There is no contact named ~s" name))))))
      (if errmsg
          (loom-do-contact cw :errmsg errmsg :name name :deleting-p t)
          (loom-do-contacts-command
           cw
           :errmsg (format nil "Contact ~s deleted." name))))))

(defun loom-rename-contact (cw)
  (with-parms (name new-name)
    (let (errmsg)
      (if (blankp new-name)
          (setf errmsg "New name may not be blank.")
          (let* ((server (loom-cw-server cw))
                 (loom-server
                  (make-loom-uri-server (loom-cw-db cw) (loom-server-url server))))
            (loom:with-loom-transaction (:server loom-server)
              (let* ((loom-wallet (loom-cw-get-loom-wallet cw :server loom-server))
                     (locations (loom:wallet-locations loom-wallet))
                     (wallet-location (find-if #'loom:location-wallet-p locations))
                     (location (loom:find-location name locations)))
                (cond ((loom:find-location new-name locations)
                       (setf errmsg
                             (format nil
                                     "There is already a contact named ~s"
                                     new-name)))
                      ((not location)
                       (return-from loom-rename-contact
                         (loom-do-contacts-command
                          cw
                          :errmsg (format
                                   nil "Can't find contact named ~s" name))))
                      ((not wallet-location)
                       (setf errmsg "Can't find wallet location."))
                      (t (handler-case
                             (setf (loom:location-name location) new-name
                                   (loom:get-wallet
                                    (loom:location-loc wallet-location))
                                   loom-wallet)
                           (error (c)
                             (setf errmsg (princ-to-string c))))))))))
        (loom-do-contact cw
                         :errmsg errmsg
                         :name (if errmsg name new-name)
                         :new-name (and errmsg new-name)
                         :include-rename-p (not (null errmsg))))))

(defun loom-do-assets-command (cw &key errmsg description id name scale precision)
  (let* ((server (loom-cw-server cw))
         (wallet (loom-cw-wallet cw))
         (loom-wallet (loom-cw-get-loom-wallet cw :force-server-p t))
         (assets (loop for asset in (loom:wallet-assets loom-wallet)
                    for name = (loom:asset-name asset)
                    for id = (loom:asset-id asset)
                    for enabled-name = (format nil "enabled-~a" id)
                    for enabled = (not (loom:asset-disabled-p asset))
                    collect (list :enabled-name enabled-name
                                  :enabled enabled
                                  :asset-id id
                                  :name name))))
    (make-cw-loom-menu cw :assets)
    (unless id
      (setf id (loom:random-loc)))
    (setf (loom-cw-title cw) "Loom Assets - Truledger Client"
          (loom-cw-onload cw)
          "document.getElementById(\"description\").focus()"
          (loom-cw-body cw)
          (expand-template
           (list :errmsg errmsg
                 :server-url (hsc (loom-server-url server))
                 :wallet-name (hsc (loom-wallet-name wallet))
                 :description (hsc description)
                 :id (hsc id)
                 :name (hsc name)
                 :scale (hsc scale)
                 :precision (hsc precision)
                 :assets assets)
           "loom-assets.tmpl")))
  cw)

(defun loom-do-accept-asset (cw)
  (with-parms (description)
    (let (errmsg)
      (multiple-value-bind (name id scale precision)
          (ignore-errors (loom:parse-asset-description description))
        (unless name
          (return-from loom-do-accept-asset
            (progn
              (setf errmsg "Bad description. Copy and paste, please.")
              (loom-do-assets-command
               cw :errmsg errmsg :description description))))
        (let* ((new-asset (loom:make-asset :name name
                                           :scale scale
                                           :precision precision
                                           :id id))
               (server (loom-cw-server cw))
               (loom-server (make-loom-uri-server
                             (loom-cw-db cw) (loom-server-url server))))
          (loom:with-loom-transaction (:server loom-server)
            (let* ((wallet (loom-cw-get-loom-wallet
                            cw :server loom-server :force-server-p t))
                   (location (or (find-if #'loom:location-wallet-p
                                          (loom:wallet-locations wallet))
                                 (setf errmsg "Can't find wallet location.")))
                   (assets (loom:wallet-assets wallet)))
              (unless errmsg
                (cond ((find id assets :test #'equal :key #'loom:asset-id)
                       (setf errmsg "You already have an asset with that ID."))
                      ((ignore-errors
                         (setf (loom:wallet-assets wallet)
                               (nconc assets (list new-asset))
                               (loom:get-wallet (loom:location-loc location))
                               wallet)))
                      (t (setf errmsg "Failed to save wallet.")))))
            (when errmsg
              ;; Return-from forces the transaction to be rolled back
              (return-from loom-do-accept-asset
                (loom-do-assets-command
                 cw :errmsg errmsg :description description)))))
        (loom-do-assets-command cw :errmsg "Asset accepted.")))))
                 
(defun loom-do-new-asset (cw)
  (with-parms (id name scale precision)
    (let (errmsg)
      (and (or (typep id 'loom:loom-loc)
               (progn (setf errmsg "Malformed ID.") nil))
           (or (not (blankp name))
               (progn (setf errmsg "Name must be included.") nil))
           (or (blankp scale)
               (setf scale (ignore-errors (parse-integer scale)))
               (progn (setf errmsg "Malformed scale.") nil))
           (or (blankp precision)
               (setf precision (ignore-errors (parse-integer precision)))
               (progn (setf errmsg "Malformed precision.") nil)))
      (cond ((integerp scale)
             (cond ((< scale 0) (setf errmsg "Scale must be >= 0."))
                   ((> scale 20) (setf errmsg "Scale must be <= 20."))
                   ((integerp precision)
                    (when (> precision scale)
                      (setf errmsg "Precision must be <= scale.")))))
            ((integerp precision)
             (cond ((< precision 0) (setf errmsg "Precision must be >= 0."))
                   ((> precision 0) (setf errmsg "Precision must be <= scale.")))))
      (unless errmsg
        (let* ((new-asset (loom:make-asset :name name
                                           :scale scale
                                           :precision precision
                                           :id id))
               (server (loom-cw-server cw))
               (loom-server (make-loom-uri-server
                             (loom-cw-db cw) (loom-server-url server))))
          (loom:with-loom-transaction (:server loom-server)
            (let* ((wallet (loom-cw-get-loom-wallet
                            cw :server loom-server :force-server-p t))
                   (location (or (find-if #'loom:location-wallet-p
                                          (loom:wallet-locations wallet))
                                 (setf errmsg "Can't find wallet location.")))
                   (assets (loom:wallet-assets wallet))
                   loc)
              (unless errmsg
                (setf loc (loom:location-loc location))
                (handler-case (loom:create-asset id loc)
                  (error (c)
                    (setf errmsg (princ-to-string c)))))
              (unless errmsg
                (setf (loom:wallet-assets wallet) (nconc assets (list new-asset)))
                (handler-case (setf (loom:get-wallet loc) wallet)
                  (error (c)
                    (setf errmsg (princ-to-string c)))))
              (unless errmsg
                (setf errmsg "Asset created."
                      id nil name nil scale nil precision nil))))))
      (loom-do-assets-command
       cw
       :errmsg errmsg
       :id id
       :name name
       :scale (and scale (princ-to-string scale))
       :precision (and precision (princ-to-string precision))))))

(defun loom-do-asset (cw &key errmsg id deleting-p include-rename-p new-name
                      include-edit-p scale precision)
  (unless id
    (multiple-value-setq (id deleting-p include-rename-p include-edit-p
                             new-name scale precision)
      (with-parms (id operation new-name scale precision)
        (values id
                (equal operation "delete")
                (equal operation "rename")
                (equal operation "edit")
                new-name scale precision))))
  (let* ((server (loom-cw-server cw))
         (wallet (loom-cw-wallet cw))
         (loom-server (make-loom-uri-server
                       (loom-cw-db cw) (loom-server-url server)))
         loom-wallet asset asset-name description
         wallet-locations qty-alist contacts)

    (loom:with-loom-transaction (:server loom-server)
      (setf loom-wallet (loom-cw-get-loom-wallet cw :server loom-server)
            asset (loom:find-asset-by-id id (loom:wallet-assets loom-wallet)))
      (when asset
        (setf asset-name (loom:asset-name asset)
              description (loom:make-asset-description
                           asset-name id
                           (loom:asset-scale asset)
                           (loom:asset-precision asset))
              wallet-locations (loom:wallet-locations loom-wallet)
              qty-alist (loom:grid-scan-wallet loom-wallet :assets (list asset)))
        (unless scale
          (setf scale (loom:asset-scale asset)
                precision (loom:asset-precision asset)))))

    (unless (or (null scale) (stringp scale))
      (setf scale (princ-to-string scale)))
    (unless (or (null precision) (stringp precision))
      (setf precision (princ-to-string precision)))

    (unless asset
      (return-from loom-do-asset
        (loom-do-assets-command
         cw :errmsg (format nil "No asset with id: ~s" id))))

    (loop for (loc (id . qty)) in qty-alist
       for location = (loom:find-location-by-loc loc wallet-locations)
       for location-name = (and location (loom:location-name location))
       when (and id location) do
         (push (list :non-wallet-p (not (loom:location-wallet-p location))
                     :contact-name (hsc location-name)
                     :asset-amount (hsc qty)
                     :asset-name (hsc asset-name)
                     :url-encoded-contact-name
                     (url-rewrite:url-encode location-name))
               contacts))
    (setf contacts
          (and (setf contacts (nreverse contacts))
               (cons (car contacts)
                     (sort (cdr contacts) #'string-lessp
                           :key #'(lambda (plist) (getf plist :contact-name))))))
    (make-cw-loom-menu cw :assets)
    (setf (loom-cw-title cw) "Loom Contact - Truledger Client"
          (loom-cw-onload cw)
          (and (or include-rename-p include-edit-p)
               "document.getElementById(\"new-name\").focus()")
          (loom-cw-body cw)
          (expand-template
           (list :errmsg errmsg
                 :server-url (hsc (loom-server-url server))
                 :wallet-name (hsc (loom-wallet-name wallet))
                 :name (hsc asset-name)
                 :id (hsc id)
                 :description (hsc description)
                 :scale (hsc scale)
                 :precision (hsc precision)
                 :include-options-p (not (or deleting-p include-rename-p
                                             include-edit-p))
                 :include-contacts-p (and contacts
                                          (not include-rename-p)
                                          (not include-edit-p))
                 :include-rename-p include-rename-p
                 :include-edit-p include-edit-p
                 :deleting-p deleting-p
                 :contacts contacts
                 :new-name (if (blankp new-name) (hsc asset-name) (hsc new-name))
                 )
           "loom-asset.tmpl"))
        cw))

(defun loom-delete-asset (cw)
  (with-parms (id confirm)
    (unless confirm
      (return-from loom-delete-asset
        (loom-do-asset cw
                       :errmsg "You must check the box to delete the asset."
                       :id id
                       :deleting-p t)))
    (let* ((server (loom-cw-server cw))
           (loom-server (make-loom-uri-server
                         (loom-cw-db cw) (loom-server-url server)))
           asset-name
           errmsg)
      (loom:with-loom-transaction (:server loom-server)
        (let* ((loom-wallet (loom-cw-get-loom-wallet cw :server loom-server))
               (assets (loom:wallet-assets loom-wallet))
               (wallet-location (find-if #'loom:location-wallet-p
                                         (loom:wallet-locations loom-wallet)))
               (asset (loom:find-asset-by-id id assets)))
          (cond ((not wallet-location)
                 (setf errmsg "Can't find wallet location."))
                (asset
                 (handler-case
                     (setf asset-name (loom:asset-name asset)
                           (loom:wallet-assets loom-wallet)
                           (delete asset assets :test #'eq)
                           (loom:get-wallet (loom:location-loc wallet-location))
                           loom-wallet)
                   (error (c)
                     (setf errmsg (princ-to-string c)))))
                (t (setf errmsg (format nil "There is no asset with id: ~s" id))))))
      (if errmsg
          (loom-do-asset cw :errmsg errmsg :id id :deleting-p t)
          (loom-do-assets-command
           cw
           :errmsg (format nil "Asset ~s deleted." asset-name))))))

(defun loom-edit-asset (cw)
  (with-parms (id new-name scale precision)
    (let (errmsg)
      (cond ((blankp new-name)
             (setf errmsg "You must specify a name."))
            (t (let ((s (if (blankp scale)
                            ""
                            (ignore-errors (parse-integer scale))))
                     (p (if (blankp precision)
                            ""
                            (ignore-errors (parse-integer precision)))))
                 (cond ((and s p)
                        (cond ((integerp s)
                               (cond ((< s 0)
                                      (setf errmsg "Scale must be non-negative."))
                                     ((and (integerp p) (> p s))
                                      (setf errmsg "Precision must be less than scale."))))
                              ((integerp p)
                               (unless (eql p 0)
                                 (setf errmsg "Precision must be less than scale"))))
                        (unless errmsg
                          (setf scale s precision p)))
                       (t
                        (setf errmsg
                              "Scale and precision must be blank or non-negative integers."))))))
      (when errmsg
        (return-from loom-edit-asset
          (loom-do-asset cw
                         :errmsg errmsg
                         :id id
                         :scale scale
                         :precision precision
                         :include-edit-p t)))
      (let* ((server (loom-cw-server cw))
             (loom-server (make-loom-uri-server
                           (loom-cw-db cw) (loom-server-url server))))
        (loom:with-loom-transaction (:server loom-server)
          (let* ((loom-wallet (loom-cw-get-loom-wallet cw :server loom-server))
                 (assets (loom:wallet-assets loom-wallet))
                 (wallet-location (find-if #'loom:location-wallet-p
                                           (loom:wallet-locations loom-wallet)))
                 (asset (loom:find-asset-by-id id assets)))
            (cond ((not wallet-location)
                   (setf errmsg "Can't find wallet location."))
                  (asset
                   (handler-case
                       (setf (loom:asset-name asset) new-name
                             (loom:asset-scale asset)
                             (unless (blankp scale) scale)
                             (loom:asset-precision asset)
                             (unless (blankp precision) precision)
                             (loom:get-wallet (loom:location-loc wallet-location))
                             loom-wallet)
                     (error (c)
                       (setf errmsg (princ-to-string c)))))
                  (t (setf errmsg
                           (format nil "There is no asset with id: ~s" id))))))
        (if errmsg
            (loom-do-asset cw
                           :errmsg errmsg
                           :id id
                           :scale scale
                           :precision precision
                           :include-edit-p t)
            (loom-do-asset cw
                           :errmsg "Asset updated."
                           :id id))))))

(defun loom-rename-asset (cw)
  (with-parms (id new-name)
    (let (errmsg)
      (when (blankp new-name)
        (return-from loom-rename-asset
          (loom-do-asset cw
                         :errmsg "You must specify a name."
                         :id id
                         :new-name new-name
                         :include-rename-p t)))
      (let* ((server (loom-cw-server cw))
             (loom-server (make-loom-uri-server
                           (loom-cw-db cw) (loom-server-url server))))
        (loom:with-loom-transaction (:server loom-server)
          (let* ((loom-wallet (loom-cw-get-loom-wallet cw :server loom-server))
                 (assets (loom:wallet-assets loom-wallet))
                 (wallet-location (find-if #'loom:location-wallet-p
                                           (loom:wallet-locations loom-wallet)))
                 (asset (loom:find-asset-by-id id assets)))
            (cond ((not wallet-location)
                   (setf errmsg "Can't find wallet location."))
                  (asset
                   (handler-case
                       (setf (loom:asset-name asset) new-name
                             (loom:get-wallet (loom:location-loc wallet-location))
                             loom-wallet)
                     (error (c)
                       (setf errmsg (princ-to-string c)))))
                  (t (setf errmsg
                           (format nil "There is no asset with id: ~s" id))))))
        (if errmsg
            (loom-do-asset cw
                           :errmsg errmsg
                           :id id
                           :include-rename-p t)
            (loom-do-asset cw
                           :errmsg "Asset renamed."
                           :id id))))))

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
