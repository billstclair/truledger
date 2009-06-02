; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Trubanc client web server
;;;

(in-package :trubanc-client-web)

;; Causes WHO and WHOTS to indent prettily by default.
;; Remove when debugged, to reduce bandwidth.

(eval-when nil ;; (:compile-toplevel :execute :load-toplevel)
  (setq cl-who::*indent* t))

(defparameter *require-coupon* nil)

(defparameter *default-menuitems*
  '(("balance" . "Balance")
    ("contacts" . "Contacts")
    ("banks" . "Banks")
    ("assets" . "Assets")
    ("admins" . "Admin")
    ("logout" . "Logout")))    

;; The client-web state
(defstruct cw
  client
  iphone
  title
  error
  bankline
  bankname
  debugstr
  session
  html-output
  body
  menu
  onload
  fraction-asset
  )

(defun parm (name &rest args)
  (hunchentoot:parameter
   (if args (apply #'format nil name args) name)))

(defun parms (&key (post t) (get nil))
  (let ((req hunchentoot:*request*))
    (append (and post (hunchentoot:post-parameters req))
            (and get (hunchentoot:get-parameters req)))))

(defun stringify (x &optional format)
  (format nil (or format "~a") x))

(defun append-debug (cw x)
  (let ((str (cw-debugstr cw)))
    (and str
         (setf (cw-debugstr cw)
               (strcat str x)))))

(defun client-db-dir ()
  "dbs/clientdb")

;; Called from do-trubanc-client in server-web.lisp
;; Returns a string with the contents of the client web page.
(defun web-server ()
  (let* ((client (make-client (client-db-dir))))
    (unwind-protect
         (web-server-internal client)
      (finalize client))))

(defun delete-cookie (name)
  (hunchentoot:set-cookie name :value "" :expires 0))

;; Map a (parm "cmd") value to a function to call
(defparameter *command-map*
  (apply #'make-equal-hash
         '(nil draw-login
           "logout" do-logout
           "login" do-login
           "contact" do-contact
           "bank" do-bank
           "asset" do-asset
           "admin" do-admin
           "spend" do-spend
           "sync" do-sync
           "canceloutbox" do-canceloutbox
           "processinbox" do-processinbox
           "storagefees" do-storagefees
           "dohistory" do-history
           "togglehistory" do-togglehistory
           "toggleinstructions" do-toggleinstructions

           "register" draw-register
           "balance" draw-balance
           "rawbalance" draw-raw-balance
           "contacts" draw-contacts
           "banks" draw-banks
           "assets" draw-assets
           "admins" draw-admin
           "coupon" draw-coupon
           "history" draw-history)))

(defun web-server-internal (client)
  (let* ((iphone (search "iPhone" (hunchentoot:user-agent)))
         (title "Trubanc Client")
         (session (hunchentoot:cookie-in "session"))
         (cw (make-cw :client client :iphone iphone :title title :session session))
         (cmd (parm "cmd"))
         (debug-parm (parm "debug"))
         (debug (hunchentoot:cookie-in "debug")))
    
    (when debug-parm
      (setq debug (not (blankp debug-parm)))
      (if debug
          (hunchentoot:set-cookie "debug" :value "debug")
          (delete-cookie "debug")))
    (when debug
      (setf (cw-debugstr cw) ""
            (showprocess client) (lambda (x) (append-debug cw x))))

    (unless (blankp session)
      (handler-case
          (progn
            (login-with-sessionid client session)
            (when (null cmd) (setq cmd "balance")))
        (error ()
          (delete-cookie "session")
          (setf cmd "logout"
                session nil)))
      (setf (cw-session cw) session))

    (cond ((id client)
           (let ((keephistory (user-preference client "keephistory")))
             (unless keephistory (setq keephistory "keep"))
             (setf (keep-history-p client) (equal keephistory "keep")))

           (init-bank cw)

           (unless (bankid client)
             (when (and (not (equal cmd "logout"))
                        (not (equal cmd "login"))
                        (not (equal cmd "bank"))
                        (not (equal cmd "admins"))
                        (not (equal cmd "admin")))
               (setq cmd "banks"))))
          ((and (not (equal cmd "login")) (not (equal cmd "register")))
           (setq cmd nil)))

    (when (and (or (equal cmd "admins")
                   (equal cmd "admin"))
               (not (is-local-server-bank-p cw)))
      (setq cmd nil))

    (setf (cw-body cw)
          (with-output-to-string (s)
            (setf (cw-html-output cw) s)
            (let ((f (gethash cmd *command-map*)))
              (cond (f (funcall f cw))
                    (session (draw-balance cw))
                    (t (draw-login cw))))))

    ;; Use title, body, onload, and debugstr to fill the page template.
    (let ((str (cw-debugstr cw)))
      (when str
        (setf (cw-debugstr cw)
              (whots (s)
                (:b "=== Debug log ===")
                (:br)
                (:pre (str str))))))

    (with-output-to-string (s)
      (setf (cw-html-output cw) s)
      (write-template cw))))

;; Should support a template.lhtml file that users can easily change.
(defun write-template (cw)
  (let ((title (cw-title cw))
        (bankname (cw-bankname cw))
        (menu (cw-menu cw)))
    (unless title (setq title "A Trubanc Web Client"))
    (unless bankname (setq bankname "Trubanc"))

    (who (s (cw-html-output cw))
      (:html
       (:head
        (:title (str title))
        (:meta :name "viewport" :content "width=device-width")
        (:link :rel "apple-touch-icon" :href="../site-icon.ico")
        (:link :rel "shortcut icon" :href "../site-icon.ico"))
       (:body
        :onload (cw-onload cw)
        (:p
         (:a :href "../"
             (:img :style "vertical-align: middle;border: 1px white"
                   :src "../trubanc-logo-50x49.gif"
                   :alt "Trubanc" :width "50" :height "49"))
         (:b " " (str bankname))
         (when menu
           (str "&nbsp;&nbsp;") (str menu)))
        (write-bankline cw)
        (write-idcode cw)
        (str (cw-body cw))
        (str (cw-debugstr cw))
        (:p
         (:a :href "http://common-lisp.net/"
             (:img
              :style "border: 0;" :src "../little-lambda.png"
              :alt "Made with Lisp" :title "Made with Lisp"
              :width "16" :height "16"))))))))

(defun write-bankline (cw)
  (let* ((client (cw-client cw))
         (bankid (bankid client)))
    (when bankid
      (let ((bank (getbank client bankid)))
        (when bank
          (let ((name (bank-name bank))
                (url (bank-url bank)))
            (who (s (cw-html-output cw))
              (:b "Bank: ")
              (:span :title (hsc bankid) (esc name))
              " "
              (:a :href url (str url))
              (:br))))))))

(defun write-idcode (cw)
  (let* ((client (cw-client cw))
         (id (and client (id client))))
    (when id
      (multiple-value-bind (pubkeysig name) (get-id client id)
        (declare (ignore pubkeysig))
        (who (s (cw-html-output cw))
          (unless (blankp name)
            (who (s)
              (:b "Account name: ") (esc name)
              (:br)))
          (:b "Your ID: ") (str id)
          (:br))))))

(defun draw-login (cw &optional key)
  (let ((page (parm "page")))
    (when (equal page "register")
      (return-from draw-login (draw-register cw key))))

  (setf (cw-onload cw) "document.forms[0].passphrase.focus()")
  (setmenu cw nil)

  (who (s (cw-html-output cw))
    (:form
     :method "post" :action "./" :autocomplete "off"
     (:input :type "hidden" :name "cmd" :value "login")
     (:table
      (:tr
       (:td (:b "Passphrase:"))
       (:td (:input
             :type "password" :name "passphrase" :size "50")
            " "
            (:input
             :type "submit" :name "login" :value "Login")))
      (:tr
       (:td)
       (:td :style "color: red"
            (str (or (cw-error cw) "&nbsp;")))))
     (:a :href "./?cmd=register"
         "Register a new account"))))

(defun draw-register (cw &optional key)
  (settitle cw "Register")
  (setmenu cw nil)
  (setf (cw-onload cw) "document.forms[0].passphrase.focus()")

  (let* ((s (cw-html-output cw))
         (keysize (or (ignore-errors
                        (parse-integer (parm "keysize")))
                      3072)))
    (flet ((keysize-option (size)
             (let ((size-str (stringify size "~d")))
               (who (s)
                 (:option :value size-str :selected (equal keysize size)
                          (str size-str))))))
      (who (s)
        (:form
         :method "post" :action "./" :autocomplete "off"
         (:input :type "hidden" :name "cmd" :value "login")
         (:table
          (:tr
           (:td (:b "Passphrase:"))
           (:td (:input :type "password" :name "passphrase" :size "50")
                (:input :type "submit" :name "login" :value "Login")
                (:input :type "hidden" :name "page" :value "register")))
          (:tr
           (:td)
           (:td :style "color: red"
                (str (or (cw-error cw) "&nbsp;"))))
          (:tr
           (:td (:b "Verification:"))
           (:td (:input :type "password" :name "passphrase2" :size "50")))
          (:tr
           (:td (:b "Coupon:"))
           (:td (:input :type "text" :name "coupon" :size "64")))
          (:tr
           (:td (:b "Account Name" (:br) "(Optional):"))
           (:td (:input :type "text" :name "name" :size "40")))
          (:tr
           (:td (:b "Key size:"))
           (:td
            (:select :name "keysize"
                     (mapc #'keysize-option '(512 1024 2048 3072 4096)))
            (:input :type "submit" :name "newacct" :value "Create account")
            (:input :type "submit" :name "showkey" :value "Show key")))
          (:tr
           (:td)
           (:td
            "To generate a new private key, leave the area below blank, enter a
passphrase, the passphrase again to verify, a bank coupon, an optional
account name, a key size, and click the \"Create account\" button. To
use an existing private key, paste the private key below, enter its
passphrase above, a bank coupon, an optional account name, and click
the \"Create account\" button.  To show your encrypted private key,
enter its passphrase, and click the \"Show key\" button. Warning: if you
forget your passphrase, <b>nobody can recover it, ever</b>."))
          (:tr
           (:td)
           (:td (:textarea :name "privkey" :cols "65" :rows "44"
                           :style "font-family: Monospace;"
                           (esc key))))))))))

(defun settitle (cw subtitle)
  (setf (cw-title cw) (stringify subtitle "~a - Trubanc Client")))

(defun menuitem (cmd text highlight)
  (let ((highlightp (equal cmd highlight)))
    (whots (s)
      (:a :href (stringify cmd "./?cmd=~a")
          (if highlightp
              (who (s) (:b :style "font-size: 120%;" (str text)))
              (who (s) (str text)))))))

(defun server-db-dir ()
  "dbs/serverdb")

(defun server-db-exists-p ()
  (or (get-running-server)
      (let ((db (make-fsdb (server-db-dir))))
        (not (null (db-get db $PRIVKEY))))))

(defun is-local-server-bank-p (cw &optional (acceptor hunchentoot:*acceptor*))
  (let* ((port (acceptor-port acceptor))
         (server (port-server port))
         (client (cw-client cw)))
    (or (and server client (id client) (equal (id client) (bankid server)))
        (and (not server) (not (server-db-exists-p))))))

(defun get-running-server (&optional (acceptor hunchentoot:*acceptor*))
  (port-server (acceptor-port acceptor)))

(defun setmenu (cw &optional highlight (menuitems *default-menuitems*))
  (let ((menu nil)
        (client (cw-client cw)))
    (cond ((and highlight client (bankid client))
           (loop
              for (cmd . text) in menuitems
              do
              (when (or (not (equal cmd "admins"))
                        (is-local-server-bank-p cw))
                (if menu
                    (dotcat menu "&nbsp;&nbsp")
                    (setq menu ""))
                (dotcat menu (menuitem cmd text highlight)))))
          ((and client (id client) (not (server-db-exists-p)))
           (setq menu (menuitem "admins" "Admin" highlight))
           (when (and client (id client))
             (dotcat menu "&nbsp;&nbsp;" (menuitem "logout" "Logout" nil))))
          ((and client (id client))
           (setq menu (menuitem "logout" "Logout"  nil))))
    (setf (cw-menu cw) menu)))

(defun do-logout (cw)
  (when (cw-session cw)
    (logout (cw-client cw)))
  (delete-cookie "session")
  (setf (cw-bankline cw) nil
        (cw-error cw) nil)
  (draw-login cw))

;; Here from the login page when the user presses one of the buttons
(defun do-login (cw)
  (bind-parameters (passphrase passphrase2 coupon name keysize login
                               newacct showkey privkey)

    (let ((client (cw-client cw))
          (err nil))
      (when showkey
        (let ((key (ignore-errors (get-privkey client passphrase))))
          (unwind-protect
               (cond (key
                      (setq privkey (encode-rsa-private-key key passphrase)))
                     (t (setf (cw-error cw) "No key for passphrase")))
            (when key
              (rsa-free key))))
        (return-from do-login (draw-login cw privkey)))

      (when newacct
        (setq login nil)
        (cond ((blankp passphrase)
               (setq err "Passphrase may not be blank"))
              ((and (blankp privkey) (not (equal passphrase passphrase2)))
               (setq err "Passphrase didn't match Verification"))
              ((not (blankp privkey))
               ;; Support adding a passphrase to a private key without one
               (ignore-errors
                 (with-rsa-private-key (pk privkey)
                   (cond ((not (equal passphrase passphrase2))
                          (setq err "Passphrase didn't match Verification"))
                         (t (setq privkey (encode-rsa-private-key pk passphrase)))))))
              (t (setq privkey (parse-integer keysize))))

        (when err
          (setf (cw-error cw) err)
          (return-from do-login (draw-login cw)))

        (cond ((not (blankp coupon))
               (handler-case
                   (multiple-value-bind (bankid url)
                       (parse-coupon coupon)
                     (handler-case (verify-coupon client coupon bankid url)
                       (error (c) (setq err (stringify c)))))
                 (error ()
                   ;; See if "coupon" is just a URL, meaning the user
                   ;; already has an account at that bank.
                   (handler-case (verify-bank client coupon)
                     (error (c) (setq err (stringify c "Invalid coupon: ~a")))))))
              ((and *require-coupon* (not (stringp privkey)))
               (setq err "Bank coupon required for registration")))

        (unless err
          (let ((allocated-privkey-p nil))
            (handler-case
                (progn
                  (unless (integerp privkey)
                    (setq privkey (decode-rsa-private-key privkey passphrase)
                          allocated-privkey-p t))
                  (newuser client :passphrase passphrase :privkey privkey)
                  (setq login t))
              (error (c)
                (when allocated-privkey-p
                  (let ((pk privkey))
                    (setq privkey nil)
                    (rsa-free pk)))
                (setq err (stringify c)))))))

      (when login
        (handler-case
            (let ((session (login-new-session client passphrase)))
              ;; Hunchentoot doesn't appear to have a way to tell
              ;; if the client is accepting cookies.
              ;; Need to set a test cookie in the login page,
              ;; and check whether it comes back.
              (hunchentoot:set-cookie "session" :value session)
              (when newacct
                (unless (blankp coupon)
                  (addbank client coupon name t)))
              (init-bank cw)
              (return-from do-login
                (if (bankid client)
                    (draw-balance cw)
                    (draw-banks cw))))
          (error (c)
            (setq err (stringify c "Login error: ~a")))))

      (setf (cw-error cw) err)

      (draw-login cw))))

(defun do-bank (cw)
  "Here to change banks or add a new bank"
  (bind-parameters (newbank selectbank bankurl name bank)
    (let ((client (cw-client cw))
          (err nil))
      (cond (newbank
             (setq bankurl (trim bankurl))
             (handler-case
                 (progn (addbank client bankurl name)
                        (setf (user-preference client "bankid") (bankid client)))
               (error (c) (setq err (stringify c)))))
            (selectbank
             (cond ((or (not bank) (equal bank ""))
                    (setq err "You must choose a bank"))
                   (t (setf (user-preference client "bankid") bank)
                      (init-bank cw t)
                      (setq err (cw-error cw))))))
      (cond (err
             (setf (cw-error cw) err)
             (draw-banks cw bankurl name))
            (t (draw-balance cw))))))

(defun do-contact (cw)
  (let ((client (cw-client cw))
        (err nil))
    (bind-parameters (addcontact deletecontacts chkcnt id nickname notes)
      (setq chkcnt
            (if (blankp chkcnt)
                0
                (or (ignore-errors (parse-integer chkcnt)) 0)))
      (cond (addcontact
             (when (blankp id)
               (dotimes (i chkcnt)
                 (let ((chki (parm "chk~d" i)))
                   (unless (blankp chki)
                     (setq id  (parm "id~d" i))
                     (return)))))
             (cond ((blankp id)
                    (setq err
                          "you must specify an id, either explicitly or by checking an existing contact"))
                   (t (let ((contact (getcontact client id)))
                        (when contact
                          (when (blankp nickname)
                            (setq nickname (contact-nickname contact)))
                          (when (blankp notes)
                            (setq notes (contact-note contact))))
                        (handler-case (addcontact client id nickname notes)
                          (error (c)
                            (setq err (stringify c "Can't add contact: ~a")))))))
             (if (setf (cw-error cw) err)
               (draw-contacts cw id nickname notes)
               (draw-contacts cw)))
            (deletecontacts
             (dotimes (i chkcnt)
               (let ((chki (parm "chk~d" i)))
                 (unless (blankp chki)
                   (let ((id (parm "id~d" i)))
                     (deletecontact client id)))))
             (draw-contacts cw))
            (t (draw-balance cw))))))

(defun do-asset (cw)
  "Add a new asset, or change the storage fee"
  (let ((client (cw-client cw))
        (err nil))
    (bind-parameters (newasset updatepercent scale precision assetname storage)
      (cond (newasset
             (cond ((or (blankp scale) (blankp precision) (blankp assetname))
                    (setq err
                          "Scale, Precision, and Asset name must all be specified"))
                   ((not (and (is-numeric-p scale t)
                              (is-numeric-p precision t)))
                    (setq err "Scale and Precision must be numbers"))
                   ((and (not (blankp storage)) (not (is-numeric-p storage)))
                    (setq err "Storage fee must be a number"))
                   (t
                    (handler-case
                        (addasset client scale precision assetname storage)
                      (error (c)
                        (setq err (stringify c "Error adding asset: ~a"))))))
             (if (setf (cw-error cw) err)
                 (draw-assets cw scale precision assetname storage)
                 (draw-assets cw)))
            (updatepercent
             (bind-parameters (percentcnt)
               (dotimes (i (parse-integer percentcnt))
                 (let ((assetid (parm "assetid~d" i))
                       (opercent (parm "opercent~d" i))
                       (percent (parm "percent~d" i)))
                   (unless (equal percent opercent)
                     (let ((asset nil))
                       (handler-case
                           (setq asset (getasset client assetid))
                         (error ()
                           (setq err (stringify assetid "Can't find assetid: ~a"))))
                       (when asset
                         (let ((scale (asset-scale asset))
                               (precision (asset-precision asset))
                               (assetname (asset-name asset)))
                           (handler-case
                               (addasset client scale precision assetname percent)
                             (error (c) (setq err (stringify c)))))))
                     (when err (return)))))
               (setf (cw-error cw) err)
               (draw-assets cw)))
            (t (draw-balance cw))))))

(defun do-admin (cw)
  (bind-parameters (bankname bankurl passphrase verification adminpass adminverify)
    (let ((client (cw-client cw))
          (server (get-running-server))
          (err nil))
      (cond (server
             (setq err "Server already running"))
            ((blankp bankname)
             (setq err "Bank name must be set"))
            ((or (blankp bankurl)
                 (not (url-p bankurl)))
             (setq err "Bank URL must be a web address"))
            ((or (blankp passphrase)
                 (not (equal passphrase verification)))
             (setq err "Passphrase didn't match verification"))
            ((or (blankp adminpass)
                 (not (equal adminpass adminverify)))
             (setq err "Admin Passphrase didn't match verification"))
            ((let ((cl (make-client (client-db-dir))))
               (handler-case
                   (progn (login cl passphrase) t)
                 (error ()
                   nil)))
             (setq err "Bank already has a client account. Not handled."))
            (t (let ((server (handler-case
                                 (make-server (server-db-dir) passphrase
                                              :bankname bankname
                                              :bankurl bankurl)
                               (error (c)
                                 (setq err (stringify
                                            c "Error initializing bank: ~a"))))))
                 (destroy-password verification)
                 (destroy-password adminverify)
                 (when server
                   ;; Enable server web hosting
                   (setf (port-server (acceptor-port hunchentoot:*acceptor*))
                         server)
                   (let ((bankid (bankid server))
                         (admin-name (stringify bankname "~a Admin"))
                         (admin-id nil)
                         (admin-exists-p nil))
                     ;; Login as admin on client, creating account if necessary
                     (handler-case (login client adminpass)
                       (error ()
                         (handler-case
                             (newuser client :passphrase adminpass)
                           (error ()
                             (setq err "Can't create admin account")))))
                     (unless err
                       (setq admin-id (id client))
                       (ignore-errors
                         ;; If we can set the bank, the admin user
                         ;; already has an account.
                         (setbank client bankid)
                         (setq admin-exists-p t)))
                     (unless err
                       (let ((privkey-str
                              (encode-rsa-private-key
                               (privkey server) passphrase)))
                         (handler-case
                             (newuser client
                                      :passphrase passphrase
                                      :privkey (decode-rsa-private-key
                                                privkey-str passphrase))
                           (error (c)
                             (setq err
                                   (stringify
                                    c "Can't create server account in client")))))
                       (handler-case
                           (addbank client bankurl bankname)
                         (error (c)
                           (setq err (stringify
                                      c "Can't add bank to client: ~a")))))
                     (unless (or err admin-exists-p)
                       (handler-case
                           (spend client admin-id (tokenid server) "200000")
                         (error (c)
                           (setq err (stringify
                                      c "Can't spend to admin account: ~a"))))
                       (unless err
                         (handler-case
                             (progn
                               (login client adminpass)
                               (addbank client bankurl admin-name)
                               (addcontact client bankid bankname "The bank"))
                           (error (c)
                             (setq err (stringify
                                        c "Can't add bank to admin account: ~a"))))))

                     (destroy-password adminpass)

                     (unless err
                       (handler-case
                           (let ((session (login-new-session client passphrase)))
                             (hunchentoot:set-cookie "session" :value session)
                             (setbank client bankid)
                             (addcontact client admin-id admin-name
                                         "The bank administrator"))
                         (error (c)
                           (setq err (stringify
                                      c "Can't login as bank: ~a")))))

                     (destroy-password passphrase)

                     (unless err
                       (setq bankname nil
                             bankurl nil
                             err "Server started!")))))))

      (setf (cw-error cw) err)
      (draw-admin cw bankname bankurl))))

(defun do-sync (cw)
  (let ((client (cw-client cw))
        (err nil))
    (handler-case (reinit-balances client)
      (error (c)
        (setq err (stringify c))))
    (setf (cw-error cw) err)
    (draw-balance cw)))    

(defun do-spend (cw)
  (let* ((client (cw-client cw))
         (id (id client))
         (err nil)
         (acct2 nil))
    (bind-parameters (amount recipient mintcoupon recipientid
                      allowunregistered note nickname toacct tonewacct)
      (when (blankp recipient)
        (setq recipient recipientid)
        (when (and (not (blankp recipient))
                   (not (blankp allowunregistered))
                   (id-p recipient)
                   (not (ignore-errors (get-id client recipient))))
          (setq err "Recipient ID not registered at bank")))

      (cond ((blankp recipient)
             (unless (blankp mintcoupon) (setq recipient $COUPON)))
            ((not (blankp mintcoupon))
             (setq err "To mint a coupon don't specify a recipient")))

      (unless err
        (cond ((blankp amount) (setq err "Spend amount missing"))
              ((or (equal id recipient) (blankp recipient))
               ;; Spend to yourself = transfer
               (setq recipient id
                     acct2 toacct)
               (cond ((blankp acct2) (setq acct2 tonewacct))
                     ((not (blankp tonewacct))
                      (setq err "Choose \"Transfer to\" from the selector or by typing, but not both")))
               (when (blankp acct2) (setq err "Recipient missing")))
              ((and (not (equal recipient $COUPON))
                    (not (ignore-errors (id-p recipient))))
               (setq err "Recipient ID malformed"))))

      (unless err
        ;; Add contact if nickname specified
        (unless (blankp nickname)
          (addcontact client recipient nickname))

        ;; Will be cleared when we do the spend
        (setq err "Bug: can't find acct/asset to spend")

        ;; Find the spent asset
        (loop
           with prefix = "spentasset"
           with prelen = (length prefix)
           for key.value in (parms)
           for key = (car key.value)
           do
             (when (eql 0 (search prefix key))
               (let ((acct-and-asset (explode #\| (subseq key prelen))))
                 (format t "key: ~s, acct-and-asset: ~s~%" key acct-and-asset)
                 (cond ((not (eql (length acct-and-asset) 2))
                        (setq err "Bug: don't understand spentasset"))
                       (t (setq err (do-spend-internal
                                        cw client acct-and-asset
                                        recipient amount acct2 note)))))
               (return)))
        (setf (cw-error cw) err)
        (if (blankp mintcoupon)
            (if err
                (draw-balance cw amount recipient note toacct tonewacct nickname)
                (draw-balance cw))
            (draw-coupon cw (last-spend-time client)))))))

(defun do-spend-internal (cw client acct-and-asset
                          recipient amount acct2 note)
  (let* ((acctidx (first acct-and-asset))
         (assetidx (second acct-and-asset))
         (acct (parm (strcat "acct" acctidx)))
         (assetid (parm (strcat "assetid" acctidx "|" assetidx)))
         (err nil))
    (cond ((or (blankp acct) (blankp assetid))
           (setq err "Bug: blank acct or assetid"))
          (t
           (when acct2 (setq acct (list acct acct2)))
           (handler-case
               (progn
                 (spend client recipient assetid amount acct note)
                 (setf (cw-fraction-asset cw) assetid))
             (error (c)
               (setq err (stringify  c))))))
    err))

(defun do-canceloutbox (cw)
  (let ((client (cw-client cw))
        (cancelcount (parm "cancelcount")))
    (setq cancelcount
          (if (blankp cancelcount)
              "0"
              (parse-integer cancelcount)))
    (dotimes (i cancelcount)
      (when (parm "cancel~d" i)
        (let ((canceltime (parm "canceltime~d" i)))
          (handler-case (spendreject client canceltime "Spend cancelled")
            (error (c)
              (setf (cw-error cw) (stringify c)))))
        (return)))
    (draw-balance cw)))

(defun do-processinbox (cw)
  (bind-parameters (spendcnt nonspendcnt)
    (let ((client (cw-client cw))
          (directions nil))

      (dotimes (i (if (blankp spendcnt) 0 (parse-integer spendcnt)))
        (let ((time (parm "spendtime~d" i))
              (spend (parm "spend~d" i))
              (note (parm "spendnote~d" i))
              (acct (parm "acct~d" i))
              (nickname (parm "spendnick~d" i))
              (spendid (parm "spendid~d" i)))
          (when (or (equal spend "accept") (equal spend "reject"))
            (push (make-process-inbox
                   :time time
                   :request (if (equal spend "accept")
                                $SPENDACCEPT
                                $SPENDREJECT)
                   :note (unless (blankp note) note)
                   :acct (unless (blankp acct) acct))
                  directions))
          (unless (or (blankp nickname) (blankp spendid))
            (ignore-errors (addcontact client spendid nickname)))))
      
      (dotimes (i (if (blankp nonspendcnt) 0 (parse-integer nonspendcnt)))
        (let ((time (parm "nonspendtime~d" i))
              (process (parm "nonspend~d" i))
              (nickname (parm "nonspendnick~d" i))
              (spendid (parm "nonspendid~d" i)))
          (unless (blankp process)
            (push (make-process-inbox :time time) directions))
          (unless (or (blankp nickname) (blankp spendid))
            (addcontact client spendid nickname))))

      (when directions
        (setq directions (nreverse directions))
        (handler-case (processinbox client directions)
          (error (c)
            (setf (cw-error cw) (stringify c "Error from processinbox: ~a")))))

      (draw-balance cw))))

(defun do-storagefees (cw)
  (let ((client (cw-client cw)))
    (handler-case (storagefees client)
      (error (c)
        (setf (cw-error cw) (stringify c))))
    (draw-balance cw)))

(defun do-togglehistory (cw)
  (let* ((client (cw-client cw))
         (keephistory-p
          (not (equal (or (user-preference client "keephistory") "keep")
                      "keep"))))
    (setf (keep-history-p client) keephistory-p)
    (setf (user-preference client "keephistory")
          (if keephistory-p "keep" "forget"))
    (setf (cw-error cw)
          (if keephistory-p "History enabled" "History disabled"))
    (draw-balance cw)))

(defun hideinstructions(cw)
  (user-preference (cw-client cw) "hideinstructions"))

(defun (setf hideinstructions) (value cw)
  (setf (user-preference (cw-client cw) "hideinstructions") value))

(defun do-toggleinstructions (cw)
  (let ((page (parm "page")))
    (setf (hideinstructions cw)
          (and (blankp (hideinstructions cw)) "hide"))
    (if (equal page "history")
        (draw-history cw)
        (draw-balance cw))))

(defun init-bank (cw &optional report-error-p)
  (let* ((client (cw-client cw))
         (bankid (user-preference client "bankid"))
         (err nil))
    (when bankid
      (handler-case (setbank client bankid nil)
        (error (c)
          (setf err (stringify c "Can't set bank: ~a")
                (user-preference client "bankid") nil
                bankid nil))))
    (unless (bankid client)
      (dolist (bank (getbanks client))
        (let ((bankid (bank-id bank)))
          (handler-case
              (progn (setbank client bankid)
                     (setf (user-preference client "bankid") bankid)
                     (return))
            (error (c)
              (setq err (stringify c "Can't set bank: ~a")
                    bankid nil))))))
    (unless (or (bankid client) err)
      (setq err "No known banks. Please add one."))

  (when report-error-p
    (setf (cw-error cw) err))))

(defun namestr (nickname name id)
  (cond (nickname
         (if name
             (if (not (equal name nickname))
                 (format nil "~a (~a)" nickname name)
                 name)
             nickname))
        (name (stringify name "(~a)"))
        (t id)))

(defun contact-namestr (contact)
  (let ((nickname (hsc (contact-nickname contact)))
        (name (hsc (contact-name contact)))
        (recipid (hsc (contact-id contact))))
    (namestr nickname name recipid)))

(defun id-namestr (cw fromid &optional you)
  "Returns two values: namestring and contact"
  (let ((client (cw-client cw)))
    (cond ((equal fromid "coupon") fromid)
          ((and you (equal fromid (id client))) you)
          (t (let ((contact (getcontact client fromid)))
               (cond (contact
                      (let ((namestr (contact-namestr contact)))
                        (when (equal namestr fromid)
                          (setq namestr "[unknown]"))
                        (setq namestr
                              (format nil "<span title='~a'>~a</span>"
                                      fromid namestr))
                        (values namestr contact)))
                     (t (hsc fromid))))))))

(defun datestr (time)
  "Return the ready-for-html-output formatted date for a timestamp"
  (let ((unix-time (if (stringp time)
                       (parse-integer (strip-fract time))
                       time)))
    (hsc (cybertiggyr-time:format-time
          nil "%d-%b-%y %I:%M:%S%p"
          (unix-to-universal-time unix-time)))))

(defmacro storing-error ((err-var format) &body body)
  `(do-storing-error (lambda (x) (setf ,err-var x)) ,format (lambda () ,@body)))

(defun do-storing-error (setter format thunk)
  (handler-case (funcall thunk)
    (error (c)
      (funcall setter (format nil format c))
      nil)))

(defconstant $nl #.(format nil "~%"))
(defconstant $brn #.(format nil "<br/>~%"))

(defun normalize-note (note)
  (if (or (null note) (equal note ""))
      "&nbsp;"
      (str-replace $nl $brn note)))

(defun namestr-html (cw otherid cnt-thunk idname textname &optional you)
  (multiple-value-bind (namestr contact) (id-namestr cw otherid you)
    (when (and contact
               (not (contact-contact-p contact))
               (not (equal otherid (id (cw-client cw))))
               (not (equal otherid $COUPON)))
      (let* ((cnt (funcall cnt-thunk)))
        (setq namestr
              (whots (s)
                (str namestr)
                (:br)
                (:input :type "hidden"
                        :name (format nil "~a~d" idname cnt)
                        :value otherid)
                "Nickname: "
                (:input :type "text"
                        :name (format nil "~a~d" textname cnt)
                        :size "10")))))
    namestr))

(defun draw-balance (cw &optional
                     spend-amount recipient note toacct tonewacct nickname)
  (let* ((client (cw-client cw))
         (bankid (bankid client))
         (banks (getbanks client))
         (err nil)
         (iphone (strstr (or (hunchentoot:header-in* "User-Agent") "") "iPhone"))
         (bankcode "")
         inboxcode
         seloptions
         assetlist
         assetlist-stream
         (assetidx 0)
         (acctidx 0)
         (gotbal nil)
         (contacts (getcontacts client))
         inbox
         outbox
         accts
         balance
         assets
         acctoptions
         acctheader
         (nonspends nil)
         (spendcnt 0)
         (nonspendcnt 0)
         (inbox-msgtimes (make-equal-hash))
         outboxcode
         balcode
         spendcode
         storagefeecode)

    (settitle cw "Balance")
    (setmenu cw "balance")

    (let ((bankopts
           (with-output-to-string (s)
             (dolist (bank banks)
               (let ((bid (bank-id bank)))
                 (unless (equal bid bankid)
                   (unless (equal (userreq client bid) "-1")
                     (let ((bname (bank-name bank))
                           (burl (bank-url bank)))
                       (who (s)
                         (:option :value bid (str bname) " " (str burl)))))))))))
      (unless (blankp bankopts)
        (setq
         bankcode
         (whots (s)
           (:form
            :method "post" :action "./" :autocomplete "off"
            (:input :type "hidden" :name "cmd" :value "bank")
            (:select
             :name "bank"
             (:option :value "" "Choose a bank...")
             (str bankopts))
            (:input :type "submit" :name "selectbank" :value "Change Bank"))))))

    (when bankid
      ;; Print inbox, if there is one

      (setq inbox (storing-error (err "Error getting inbox: ~a")
                    (getinbox client))
            outbox (storing-error (err "Error getting outbox: ~a")
                     (getoutbox client))
            accts (storing-error (err "Error getting accts: ~a")
                    (getaccts client))
            balance (storing-error (err "Error getting balance: ~a")
                      (getbalance client))
            acctoptions
            (and (> (length accts) 0)
                 (with-output-to-string (s)
                   (dolist (acct accts)
                     (let ((acct (hsc acct)))
                       (who (s)
                         (:option :value acct (str acct))))))))

      (cond ((null inbox)
             (setq inboxcode
                   (whots (s)
                     (:b "=== Inbox empty ===") (:br) (:br))))
            (t
             (setq
              inboxcode
              (with-output-to-string (inbox-stream)
                (when acctoptions
                  (setq acctheader (whots (s) (:th "To Acct"))))
                (setq seloptions
                      (whots (s)
                        (:option :value "accept" "Accept")
                        (:option :value "reject" "Reject")
                        (:option :value "ignore" "Ignore")))

                (setq assets (storing-error (err "Error getting assets: ~a")
                               (getassets client)))

                (dolist (item inbox)
                  (let* ((request (inbox-request item))
                         (fromid (inbox-id item))
                         (time (inbox-time item)))
                    (multiple-value-bind (namestr contact) (id-namestr cw fromid)
                      (cond ((not (equal request $SPEND))
                             (let* ((msgtime (inbox-msgtime item))
                                    (outitem (find msgtime outbox
                                                   :test #'equal
                                                   :key #'outbox-time)))
                               (setf (gethash msgtime inbox-msgtimes) item)
                               (when outitem
                                 (setf (inbox-assetname item) (outbox-assetname
                                                               outitem)
                                       (inbox-formattedamount item)
                                       (outbox-formattedamount outitem)
                                       (inbox-reply item) (inbox-note item)
                                       (inbox-note item) (outbox-note outitem)))
                               (push item nonspends)))
                            (t (let* ((assetid (inbox-assetid item))
                                      (assetname (hsc (inbox-assetname item)))
                                      (amount (hsc (inbox-formattedamount item)))
                                      (itemnote (normalize-note
                                                 (hsc (inbox-note item))))
                                      (selname (stringify spendcnt "spend~d"))
                                      (notename (stringify spendcnt "spendnote~d"))
                                      (acctselname (stringify spendcnt "acct~d"))
                                      (timecode
                                       (whots (s)
                                         (:input :type "hidden"
                                                 :name (stringify spendcnt
                                                                  "spendtime~a")
                                                 :value time)))
                                      (selcode
                                       (whots (s)
                                         (:select :name selname (str seloptions))))
                                      (acctcode
                                       (if (not acctoptions)
                                           ""
                                           (whots (s)
                                             (:td
                                              (:select :name acctselname
                                                       (str acctoptions))))))
                                      (date (datestr time)))

                                 (unless (find assetid assets
                                               :test #'equal :key #'asset-assetid)
                                   (setq assetname
                                         (whots (s)
                                           (str assetname) " "
                                           (:span :style "color: red;"
                                                  (:i "(new)")))))
                                 (unless contact
                                   (setq namestr
                                         (whots (s)
                                           (str namestr)
                                           (:br)
                                           (:input :type "hidden"
                                                   :name (stringify spendcnt
                                                                    "spendid~a")
                                                   :value fromid)
                                           "Nickname: "
                                           (:input :type "text"
                                                   :name (stringify spendcnt
                                                                 "spendnick~a")
                                                   :size "10"))))
                                 (incf spendcnt)
                                 (who (inbox-stream)
                                   (str timecode)
                                   (:tr
                                    (:td "Spend")
                                    (:td (str namestr))
                                    (:td :align "right"
                                         :style "border-right-width: 0;"
                                         (str amount))
                                    (:td :style "border-left-width: 0;"
                                         (str assetname))
                                    (:td (str itemnote))
                                    (:td (str selcode))
                                    (:td
                                     (:textarea
                                      :name notename
                                      :cols "20"
                                      :rows "2"
                                      "&nbsp;"))
                                    (str acctcode)
                                    (:td (str date))))))))))

                (dolist (item nonspends)
                  (let* ((request (inbox-request item))
                         (fromid (inbox-id item))
                         (reqstr (if (equal request $SPENDACCEPT) "Accept" "Reject"))
                         (time (inbox-time item))
                         (reply (normalize-note (hsc (inbox-reply item))))
                         (assetname (hsc (inbox-assetname item)))
                         (amount (hsc (inbox-formattedamount item)))
                         (itemnote (normalize-note (hsc (inbox-note item))))
                         (selname (stringify nonspendcnt "nonspend~d"))
                         (timecode
                          (whots (s)
                            (:input :type "hidden"
                                    :name (stringify nonspendcnt "nonspendtime~d")
                                    :value time)))
                         (selcode
                          (whots (s)
                            (:input :type "checkbox"
                                    :name selname
                                    :checked t)
                            "Remove"))
                         (date (datestr time))
                         (acctcode (if acctoptions
                                       (whots (s) (:td "&nbsp;"))
                                       ""))
                         (namestr (namestr-html cw fromid
                                                (lambda () (1- (incf nonspendcnt)))
                                                "nonspendid" "nonspendnick")))
                      (who (inbox-stream)
                        (str timecode)
                        (:tr
                         (:td (str reqstr))
                         (:td (str namestr))
                         (:td :align "right" :style "border-right-width: 0;"
                              (str amount))
                         (:td :style "border-left-width: 0;"
                              (str assetname))
                         (:td (str itemnote))
                         (:td (str selcode))
                         (:td (str reply))
                         (str acctcode)
                         (:td (str date))))))))
      
             (setq
              inboxcode
              (whots (s)
                (:form
                 :method "post" :action "./" :autocomplete "off"
                 (:input :type "hidden" :name "cmd" :value "processinbox")
                 (:input :type "hidden" :name "spendcnt" :value spendcnt)
                 (:input :type "hidden" :name "nonspendcnt" :value nonspendcnt)
                 (:table
                  :border "1"
                  (:caption (:b "=== Inbox ==="))
                  (:tr
                   (:th "Request")
                   (:th "From")
                   (:th :colspan "2" "Amount")
                   (:th "Note")
                   (:th "Action")
                   (:th "Reply")
                   (str acctheader)
                   (:th "Time"))
                  (str inboxcode))
                 (:br)
                 (:input :type "submit" :name "submit"
                         :value "Process Inbox"))))))

      ;; Prepare outbox display
      (let ((cancelcount 0))
        (setq
         outboxcode
         (with-output-to-string (outbox-stream)
           (dolist (item outbox)
             (let* ((time (outbox-time item))
                    (timestr (hsc time))
                    (date (datestr time))
                    (request (outbox-request item)))
               (when (equal request $SPEND)
                 (let ((recip (outbox-id item))
                       (assetname (hsc (outbox-assetname item)))
                       (amount (hsc (outbox-formattedamount item)))
                       (note (hsc (or (outbox-note item) "")))
                       (label "Cancel")
                       namestr
                       (cancelcode "&nbsp;"))
                   (when (blankp note)
                     (setq note  "&nbsp;"))
                   (cond ((equal recip $COUPON)
                          (let ((timearg (hunchentoot:url-encode time)))
                            (setq label "Redeem"
                                  namestr
                                  (whots (s)
                                    (:a :href (stringify
                                               timearg "./?cmd=coupon&time=~a")
                                        (str recip))))))
                         (t (setq namestr (id-namestr cw recip))))
                   (unless (gethash time inbox-msgtimes)
                     (setq cancelcode
                           (whots (s)
                             (:input :type "hidden"
                                     :name (stringify cancelcount "canceltime~d")
                                     :value timestr)
                             (:input :type "submit"
                                     :name (stringify cancelcount "cancel~d")
                                     :value label)))
                     (incf cancelcount))
                   (who (outbox-stream)
                     (:tr
                      (:td (str date))
                      (:td (str namestr))
                      (:td :align "right" :style "border-right-width: 0;"
                           (str amount))
                      (:td :style "border-left-width: 0;"
                           (str assetname))
                      (:td (str note))
                      (:td (str cancelcode))))))))))
        (unless (blankp outboxcode)
          (setq outboxcode
                (whots (s)
                  (:table
                   :border "1"
                   (:caption (:b "=== Outbox ==="))
                   (:tr
                    (:th "Time")
                    (:th "Recipient")
                    (:th :colspan "2" "Amount")
                    (:th "Note")
                    (:th "Action"))
                   (str outboxcode))))
          (when (> cancelcount 0)
            (setq outboxcode
                  (whots (s)
                    (:form
                     :method "post" :action "./" :autocomplete "off"
                     (:input :type "hidden" :name "cmd" :value "canceloutbox")
                     (:input :type "hidden" :name "cancelcount" :value cancelcount)
                     (str outboxcode)))))))

      (when balance
        (loop
           with bal-stream = (make-string-output-stream)
           for (acct . assets) in balance
           for assetcode-stream = (make-string-output-stream)
           for newassetlist-stream = (make-string-output-stream)
           do
           (setq acct (hsc acct))
           (dolist (bal assets)
             (unless (eql 0 (bccomp (balance-amount bal) 0))
               (let ((assetid (hsc (balance-assetid bal)))
                     (assetname (hsc (balance-assetname bal)))
                     (formattedamount (hsc (balance-formatted-amount bal))))
                 (setq gotbal t)
                 (who (newassetlist-stream)
                   (:input :type "hidden"
                           :name (format nil "assetid~d|~d" acctidx assetidx)
                           :value assetid))
                 (who (assetcode-stream)
                   (:tr
                    (:td :align "right"
                         (:span :style "margin-right: 5px"
                                (str formattedamount)))
                    (:td :title assetid (str assetname))
                    (:td
                     (:input :type "submit"
                             :name (format nil "spentasset~d|~d"
                                           acctidx assetidx)
                             :value "Spend"))))
                 (incf assetidx))))

           (let ((assetcode (get-output-stream-string assetcode-stream))
                 (newassetlist (get-output-stream-string newassetlist-stream)))
             (unless (blankp assetcode)
               (who (bal-stream)
                 (:tr
                  (:th :colspan "3"
                       "- " (str acct) " -"))
                 (str assetcode)))
             (unless (blankp newassetlist)
               (unless assetlist-stream
                 (setq assetlist-stream (make-string-output-stream)))
               (who (assetlist-stream)
                 (:input :type "hidden"
                         :name (stringify acctidx "acct~d")
                         :value acct)
                 (str newassetlist))
               (incf acctidx)))

           finally
           (setq balcode (get-output-stream-string bal-stream)))

        (setq
         balcode
         (whots (bal-stream)
           (:table
            :border "1"
            (:caption (:b "=== Balances ==="))
            (:tr
             (:td
              (:table
               (:tr
                (str balcode))))))

           (let ((enabled (if (keep-history-p client)
                              "enabled"
                              "disabled")))
             (when (and (cw-fraction-asset cw) (showprocess client))
               (let* ((fraction (car (getfraction client (cw-fraction-asset cw)))))
                 (when fraction
                   (let ((amt (fraction-amount fraction))
                         (scale (fraction-scale fraction)))
                     (who (bal-stream)
                       "Fractional balance: "
                       (str amt)
                       (unless (eql 0 (bccomp scale 0))
                         (who (bal-stream)
                           " x 10"
                           (:sup "-" (str scale))))
                       (:br))))))
             (who (bal-stream)
               (:br)
               (:a :href "./?cmd=sync" "Resync with bank")
               (:br)
               (:a :href "./?cmd=rawbalance"
                   "Show raw balance")
               (:br)
               (:a :href "./?cmd=history" "Show history")
               " (" (str enabled) ")"
               (:br))))))

      (when assetlist-stream
        (setq assetlist (get-output-stream-string assetlist-stream)))

        (let ((recipopts nil)
            (found nil)
            (selectmint nil)
            (disablemint nil)
            (recipientid nil)
            (acctcode nil)
            (instructions nil))
        (when gotbal
          (setq recipopts
                (whots (s)
                  (:select
                   :name "recipient"
                   (:option :value "" "Choose contact...")
                   (dolist (contact contacts)
                     (let ((namestr (contact-namestr contact))
                           (recipid (contact-id contact))
                           (selected nil))
                       (unless (equal recipid (id client))
                         (when (equal recipid recipient)
                           (setq selected t
                                 found t))
                         (who (s)
                           (:option :value recipid :selected selected
                                    (str namestr)))))))))

          (cond ((equal (id client) (bankid client))
                 (setq disablemint t))
                ((equal recipient $COUPON)
                 (setq selectmint t)))

          (when (and (not found) (not (equal recipient $COUPON)))
            (setq recipientid recipient))

          (when (> (length accts) 1)
            (setq acctcode
                  (whots (s)
                    (:select
                     :name "toacct"
                     (:option :value "" "Select or fill-in below...")
                     (dolist (acct accts)
                       (let ((selected (equal acct toacct)))
                         (who (s)
                           (:option :value acct :selected selected
                                    (esc acct)))))))))

          (let ((storagefees (getstoragefee client)))
            (when storagefees
              (setq storagefeecode
                    (whots (s)
                      (:form
                       :method "post" :action "./" :autocomplete "off"
                       (:input :type "hidden" :name "cmd" :value "storagefees")
                       (:table
                        :border "1"
                        (:caption (:b "=== Storage Fees ==="))
                        (:tr
                         (:td
                          (:table
                           (dolist (storagefee storagefees)
                             (let* ((formattedamount
                                     (balance-formatted-amount storagefee))
                                    (assetname (balance-assetname storagefee))
                                    (time (balance-time storagefee))
                                    (date (datestr time)))
                               (who (s)
                                 (:tr
                                  (:td :align "right"
                                       (:span :style "margin-right: 5px"
                                              (str formattedamount)))
                                  (:td
                                   (:span :style "margin-right: 5px"
                                          (str assetname)))
                                  (:td (str date))))))))))
                       (:input :type "submit" :name "accept"
                               :value "Move to Inbox"))))))

          (flet ((write-amt (s)
                   (who (s)
                     (:td
                      :valign "top"
                      (:table
                       (:tr
                        (:td
                         (:b "Spend amount:"))
                        (:td
                         (:input :type "text" :name "amount" :size "20"
                                                             :value spend-amount :style "text-align: right;"
                                                             :id "spendamount")))
                       (:tr
                        (:td
                         (:b "Recipient:"))
                        (:td
                         (str recipopts)
                         (:input :type "checkbox" :name "mintcoupon"
                                                  :selected selectmint :disabled disablemint)
                         "Mint coupon"))
                       (:tr
                        (:td (:b "Note:"))
                        (:td
                         (:textarea :name "note" :cols "40" :rows "10"
                                    (str note))))
                       (:tr
                        (:td (:b "Recipient ID:"))
                        (:td
                         (:input :type "text" :name "recipientid" :size "40"
                                                                  :value recipientid)
                         (:input :type "checkbox" :name "allowunregistered")
                         "Allow unregistered"))
                       (:tr
                        (:td (:b "Nickname:"))
                        (:td
                         (:input :type "text" :name "nickname" :size "30"
                                                               :value nickname)))
                       (let ((xfer-input
                              (whots (s)
                                (:input :type "text" :name "tonewacct" :size "30"
                                        :value tonewacct))))
                         (who (s)
                           (:tr
                            (:td (:b "Transfer to:"))
                            (:td (str (or acctcode xfer-input))))
                           (when acctcode
                             (who (s)
                               (:tr
                                (:td)
                                (:td (str xfer-input))))))))))))
            (setq
             spendcode
             (whots (s)
               (:form
                :method "post" :action "./" :autocomplete "off"
                (:input :type "hidden" :name "cmd" :value "spend")
                (:table
                 (:tr
                  (:td
                   :valign "top"
                   (str assetlist)
                   (str balcode))
                  (unless iphone (write-amt s)))
                 (when iphone
                   (who (s)
                     (:tr (write-amt s)))))))))

          (let* ((historytext (if (keep-history-p client) "Disable" "Enable"))
                 (hideinstructions (hideinstructions cw)))
            (setq instructions
                  (whots (s)
                    (:p
                     (:a :href "./?cmd=togglehistory"
                         (str historytext) " history")
                     (:br)
                     (:a :href (if (cw-debugstr cw)
                                   "./?debug"
                                   "./?debug=true")
                         (str (if (cw-debugstr cw)
                                  "Disable debugging"
                                  "Enable debugging")))
                     (:br)
                     (:a :href "./?cmd=toggleinstructions"
                         (str (if hideinstructions
                                  "Show Instructions"
                                  "Hide Instructions"))))
                    (unless hideinstructions
                      (who (s)
                        (:p
                         "To make a spend, fill in the \Spend amount\", choose a \"Recipient\" or
enter a \"Recipient ID\", enter (optionally) a \"Note\", and click the
\"Spend\" button next to the asset you wish to spend.")
                        (:p
                         "To transfer balances, enter the \"Spend Amount\", select or fill-in the
\"Transfer to\" name (letters, numbers, and spaces only), and click
the\"Spend\" button next to the asset you want to transfer from. Each
storage location costs one usage token, and there is currently no way
to recover an unused location. 0 balances will show only on the raw
balance screen.")
                        (:p
                         "To mint a coupon, enter the \"Spend Amount\", check the \"Mint coupon\"
box, and click the \"Spend\" button next to the asset you want to
transfer to the coupon. You can redeem a coupon on the \"Banks\" page.")
                        (:p
                         "Entering a \"Nickname\" will add the \"Recipient ID\" to your contacts
list with that nickname, or change the nickname of the selected
\"Recipient\".")))))))

        (when (cw-error cw)
          (if err
              (setq err (strcat (cw-error cw) "<br/>" err))
              (setq err (cw-error cw))))
        (when err
          (setq err
                (whots (s)
                  (:span :style "color: red"
                         (str err)))))
        (setf (cw-onload cw)
              "document.getElementById(\"spendamount\").focus()")
        (who (s (cw-html-output cw))
          (str err)
          (:br)
          (str bankcode)
          (str inboxcode)
          (str spendcode)
          (str outboxcode)
          (str storagefeecode)
          (str instructions))))))

(defun draw-coupon (cw &optional time)
  (let ((client (cw-client cw)))
    (settitle cw "Coupon")
    (setmenu cw "balance")

    (unless time (setq time (parm "time")))

    (let* ((outbox (getoutbox client))
           (item (find time outbox :test 'equal :key #'outbox-time))
           (timestr (hsc time))
           (datestr (datestr time)))
      (cond (item
             (let ((coupons (outbox-coupons item))
                   (assetname (hsc (outbox-assetname item)))
                   (formattedamount (hsc (outbox-formattedamount item)))
                   (note (outbox-note item)))
               (when coupons
                 (unless (blankp note)
                   (setq note
                         (whots (s)
                           (:tr
                            (:td (:b "Note:"))
                            (:td
                             (:span :style "margin: 5px;"
                                    (esc note)))))))
                 (who (s (cw-html-output cw))
                   (:br)
                   (:b "Coupon for outbox entry at " (str datestr))
                   (:table
                    :border "1"
                    (:tr
                     (:td (:b "Amount:"))
                     (:td
                      (:span :style "margin: 5px;"
                             (esc formattedamount) " "
                             (esc assetname))))
                    (str note)
                    (:tr
                     (:td (:b "Coupon:"))
                     (:td (:span :style "margin: 5px;" (str (car coupons))))))))))
            (t (setf (cw-error cw)
                     (stringify timestr "Couldn't find coupon: ~a"))
               (draw-balance cw))))))

(defun draw-raw-balance (cw)
  (let* ((client (cw-client cw))
         (s (cw-html-output cw)))

    (settitle cw "Raw Balance")
    (setmenu cw "balance")

    (multiple-value-bind (inbox msghash) (getinbox client t)
      (cond ((null inbox)
             (who (s)
               (:br)
               :b "=== Inbox empty ==="
               (:br)))
            (t
             (who (s)
               (:br)
               (:b "=== Inbox ===")
               (:br)
               (:table
                :border "1"
                (dolist (item inbox)
                  (let ((msg (gethash item msghash))
                        (time (inbox-time item)))
                    (unless (blankp msg)
                      (who (s)
                        (:tr
                         (:td :valign "top" (esc time))
                         (:td (:pre (str (trimmsg msg))))))))))))))

    (multiple-value-bind (outbox msghash) (getoutbox client t)
      (cond ((null outbox)
             (who (s)
               (:br)
               (:b "=== Outbox empty ===")
               (:br)))
            (t
             (who (s)
               (:br)
               (:b "=== Outbox===")
               (:br)
               (:table
                :border "1"
                (dolist (item outbox)
                  (let ((msg (gethash item msghash))
                        (time (outbox-time item)))
                    (who (s)
                      (:tr
                       (:td :valign "top" (esc time))
                       (:td (:pre (str (trimmsg msg)))))))))))))

    (multiple-value-bind (balance msghash) (getbalance client nil nil t)
      (loop
         for (acct . bals) in balance
         do
           (who (s)
             (:br)
             (:b (esc acct))
             (:br)
             (:table
              :border "1"
              (dolist (bal bals)
                (let ((msg (gethash bal msghash))
                      (assetname (balance-assetname bal)))
                  (who (s)
                    (:tr
                     (:td :valign "top" (esc assetname))
                     (:td (:pre (str (trimmsg msg))))))))))))

    (multiple-value-bind (fractions msghash) (getfraction client nil t)
      (when fractions
        (who (s)
          (:br)
          (:b "=== Fractional Balances ===")
          (:br)
          (:table
           :border "1"
           (dolist (frac fractions)
             (let ((msg (gethash frac msghash))
                   (assetname (fraction-assetname frac)))
               (who (s)
                 (:tr
                  (:td :valign "top" (esc assetname))
                  (:td (:pre (str (trimmsg msg))))))))))))))

(defun draw-banks(cw &optional bankurl name)
  (let* ((client (cw-client cw))
         (banks (getbanks client))
         (err (cw-error cw))
         (stream (cw-html-output cw)))

    (setf (cw-onload cw) "document.forms[0].bankurl.focus()")
    (settitle cw "Banks")
    (setmenu cw "banks")

    (who (stream)
      (:span :style "color: red;" (str err))
      (:br)
      (:form
       :method "post" :action "./" :autocomplete "off"
       (:input :type "hidden" :name "cmd" :value "bank")
       (:table
        (:tr
         (:td (:b "Bank URL"
                  (:br)
                  "or Coupon:"))
         (:td
          (:input :type "text" :name "bankurl" :size "64"
                                               :value bankurl)))
        (:tr
         (:td (:b "Account Name"
                  (:br)
                  "(optional):"))
         (:td
          (:input :type "text" :name "name" :size "40"
                                            :value name)))
        (:tr
         (:td)
         (:td
          (:input :type "submit" :name "newbank" :value "Add Bank")
          (:input :type "submit" :name "cancel" :value "Cancel"))))))

    (when banks
      (who (stream)
          (:table
           :border "1"
           (:tr
            (:th "Bank")
            (:th "URL")
            (:th "ID")
            (:th "Choose"))
           (dolist (bank banks)
             (let ((bid (bank-id bank)))
               (unless (equal (userreq client bid) "-1")
                 (let ((name (bank-name bank))
                       (url (hsc (bank-url bank))))
                   (when (blankp name)
                     (setq name "unnamed"))
                   (who (stream)
                     (:tr
                      (:td (esc name))
                      (:td (:a :href url (str url)))
                      (:td (esc bid))
                      (:td
                       (:form
                        :method "post" :action "./" :autocomplete "off"
                        :style "margin: 0px;" ; prevent whitespace below button
                        (:input :type "hidden" :name "cmd" :value "bank")
                        (:input :type "hidden" :name "bank" :value bid)
                        (:input :type "submit" :name "selectbank"
                                :value "Choose"))))))))))))))

(defun draw-contacts (cw &optional id nickname notes)
  (let* ((client (cw-client cw))
         (stream (cw-html-output cw))
         (contacts (getcontacts client)))

    (setf (cw-onload cw) "document.forms[0].id.focus()")
    (settitle cw "Contacts")
    (setmenu cw "contacts")

    (who (stream)
      (:span :style "color: red;" (str (cw-error cw)))
      (:br)
      (:form
       :method "post" :action "./" :autocomplete "off"
       (:input :type "hidden" :name "cmd" :value "contact")
       (:table
        (:tr
         (:td :align "right" (:b "ID:"))
         (:td
          (:input :type "text" :name "id" :size "40" :value (esc id))))
        (:tr
         (:td (:b "Nickname" (:br) "(Optional):"))
         (:td
          (:input :type "text" :name "nickname" :size "30" :value (esc nickname))))
        (:tr
         (:td (:b "Notes" (:br) "(Optional):"))
         (:td
          (:textarea :name "notes" :cols "30" :rows "10" (esc notes))))
        (:tr
         (:td)
         (:td
          (:input :type "submit" :name "addcontact" :value "Add/Change Contact")
          (:input :type "submit" :name "cancel" :value "Cancel"))))

       (when contacts
         (who (stream)
           (:br)
           (:input :type "hidden" :name "cmd" :value "contact")
           (:input :type "hidden" :name "chkcnt" :value (length contacts))
           (:table
            :border "1"
            (:tr
             (:th "Nickname")
             (:th "Name")
             (:th "Display")
             (:th "ID")
             (:th "Notes")
             (:th "x"))
            (let ((idx 0))
              (dolist (contact contacts)
                (let* ((id (hsc (contact-id contact)))
                       (name (trim (hsc (contact-name contact))))
                       (nickname (hsc (contact-nickname contact)))
                       (display (namestr nickname name id))
                       (note  (hsc (contact-note contact))))
                  (when (blankp name) (setq name "&nbsp;"))
                  (when (blankp nickname) (setq nickname "&nbsp;"))
                  (setq note
                        (if (blankp note)
                            "&nbsp;"
                            (str-replace $nl $brn note)))
                  (who (stream)
                    (:tr
                     (:td (str nickname))
                     (:td (str name))
                     (:td (str display))
                     (:td (str id))
                     (:td (str note))
                     (:td
                      (:input :type "hidden" :name (stringify idx "id~d")
                                             :value id)
                      (:input :type "checkbox" :name (stringify idx "chk~d")))))
                  (incf idx)))))
           (:input :type "submit" :name "deletecontacts" :value "Delete checked")))))))

(defun draw-assets(cw &optional scale precision assetname storage)
  (let* ((client (cw-client cw))
         (assets (getassets client))
         (stream (cw-html-output cw))
         (scale (hsc scale))
         (precision (hsc precision))
         (assetname (hsc assetname))
         (storage (hsc storage))
         (incnt 0))
    (setf (cw-onload cw) "document.forms[0].scale.focus()")
    (settitle cw "Assets")
    (setmenu cw "assets")

    (who (stream)
      (:span :style "color: red;" (str (cw-error cw)))
      (:br)
      (:form
       :method "post" :action "./" :autocomplete "off"
       (:input :type "hidden" :name "cmd" :value "asset")
       (:table
        (:tr
         (:td (:b "Scale:"))
         (:td (:input :type "text" :name "scale" :size "3" :value scale)))
        (:tr
         (:td (:b "Precision:"))
         (:td (:input :type "text" :name "precision" :size "3" :value precision)))
        (:tr
         (:td (:b "Asset name:"))
         (:td (:input :type "text" :name "assetname" :size "30" :value assetname)))
        (:tr
         (:td (:b "Storage fee (%/year):"))
         (:td (:input :type "text" :name "storage" :size "5" :value storage)))
        (:tr
         (:td)
         (:td (:input :type "submit" :name "newasset" :value "Add Asset")
              (:input :type "submit" :name "cancel" :value "Cancel"))))))

  (when assets
    (who (stream)
      (:form
       :method "post" :action "./" :autocomplete "off"
       (:table
        :border "1"
        (:tr
         (:th "Asset name")
         (:th "Scale")
         (:th "Precision")
         (:th "Storage Fee" (:br) "(%/year)")
         (:th "Owner")
         (:th "Asset ID"))
        (dolist (asset assets)
          (let* ((ownerid (asset-id asset))
                 (namestr (id-namestr cw ownerid))
                 (assetid (asset-assetid asset))
                 (scale (asset-scale asset))
                 (precision (asset-precision asset))
                 (assetname (asset-name asset))
                 (percent (asset-percent asset)))
            (setq percent
                  (if (equal ownerid (id client))
                      (whots (s)
                        (:input :type "hidden"
                                :name (stringify incnt "assetid~d")
                                :value (hsc assetid))
                        (:input :type "hidden"
                                :name (stringify incnt "opercent~d")
                                :value (hsc percent))
                        (:input :type "text"
                                :name (stringify incnt "percent~d")
                                :value (hsc percent)
                                :size "10"
                                :style "text-align: right;"))
                      (hsc percent)))
            (incf incnt)
            (when (blankp percent) (setq percent "&nbsp;"))
            (who (stream)
              (:tr
               (:td (esc assetname))
               (:td :align "right" (esc scale))
               (:td :align "right" (esc precision))
               (:td :align "right" (str percent))
               (:td (str namestr))
               (:td (esc assetid)))))))
       (when (> incnt 0)
         (who (stream)
           (:input :type "hidden" :name "percentcnt" :value incnt)
           (:input :type "hidden" :name "cmd" :value "asset")
           (:br)
           (:input :type "submit" :name "updatepercent"
                   :value "Update Storage Fees"))))))))

(defun draw-admin (cw &optional bankname bankurl)
  (let ((s (cw-html-output cw))
        (disable-p (server-db-exists-p))
        (server (get-running-server)))
    (setf (cw-onload cw) "document.forms[0].bankname.focus()")
    (settitle cw "Admin")
    (setmenu cw "admins")

    (when server
      (setq bankname (bankname server)
            bankurl (bankurl server)))

    (who (s)
      (:span :style "color: red;" (str (cw-error cw)))
      (:br)
      (str (cond (server "Server is running")
                 (disable-p "Server database exists but server not running")
                 (t "Server database not yet created. Enter info below.")))
      (:form
       :method "post" :action "./" :autocomplete "off"
       (:input :type "hidden" :name "cmd" :value "admin")
       (:table
        (:tr
         (:td (:b "Bank Name:"))
         (:td (:input :type "text"
                      :name "bankname"
                      :value bankname
                      :disabled disable-p
                      :size 30)))
        (:tr
         (:td (:b "Bank URL:"))
         (:td (:input :type "text"
                      :name "bankurl"
                      :value bankurl
                      :disabled disable-p
                      :size 30)))
        (unless disable-p
          (who (s)
            (:tr
             (:td (:b "Bank Passphrase:"))
             (:td (:input :type "password"
                          :name "passphrase"
                          :value ""
                          :size 50)))
            (:tr
             (:td (:b "Verification:"))
             (:td (:input :type "password"
                          :name "verification"
                          :value ""
                          :size 50)))
            (:tr
             (:td (:b "Admin Passphrase:"))
             (:td (:input :type "password"
                          :name "adminpass"
                          :value ""
                          :size 50)))
            (:tr
             (:td (:b "Verification:"))
             (:td (:input :type "password"
                          :name "adminverify"
                          :value ""
                          :size 50)))
            (:tr
             (:td)
             (:td (:input :type "submit" :name "create" :value "Start Server")
                  (:input :type "submit" :name "cancel" :value "Cancel"))))))))))

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
