; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Truledger client web server
;;;

(in-package :truledger-client-web)

;; Causes WHO and WHOTS to indent prettily by default.
;; Remove when debugged, to reduce bandwidth.

(eval-when nil ;; (:compile-toplevel :execute :load-toplevel)
  (setq cl-who::*indent* t))

(defparameter *default-menuitems*
  '(("balance" . "Balance")
    ("contacts" . "Contacts")
    ("servers" . "Servers")
    ("assets" . "Assets")
    ("fees" . "Fees")
    ("permissions" . "Permissions")
    ("admins" . "Admin")
    ("loom" . "Loom")
    ("logout" . "Logout")))    

;; The client-web state
(defstruct cw
  client
  iphone
  title
  error
  serverline
  servername
  session
  html-output
  body
  menu
  onload
  fraction-asset
  refresh-version
  postcnt
  postsalt
  postmsg
  )

;; Called from do-truledger-client in server-web.lisp
;; Returns a string with the contents of the client web page.
(defun web-server ()
  (catch 'raw-return
    (let* ((client (make-client (client-db-dir))))
      (unwind-protect
           (web-server-internal client)
        (finalize client)))))

(defun cookie-name (base &optional (acceptor hunchentoot:*acceptor*))
  (let ((port (acceptor-port acceptor)))
    (if (eql port 80)
        base
        (format nil "~a-~d" base port))))

(defvar *blank-cookies-p* nil)

(defun get-cookie (name)
  (unless *blank-cookies-p*
    (hunchentoot:cookie-in (cookie-name name))))

(defun set-cookie (name value)
  (hunchentoot:set-cookie (cookie-name name) :value value))

(defun delete-cookie (name)
  (hunchentoot:set-cookie (cookie-name name) :value "" :expires 0))

;; Map a (parm "cmd") value to a function to call
;; Put the function inside a list to force that command to be a POST,
;; not a GET
(defparameter *command-map*
  (apply #'make-equal-hash
         '(nil draw-login
           "logout" do-logout
           "login" do-login
           "contact" (do-contact)
           "server" (do-server)
           "asset" (do-asset)
           "fee" (do-fee)
           "permission" (do-permission)
           "admin" (do-admin)
           "loom" do-loom
           "spend" (do-spend)
           "sync" (do-sync)
           "canceloutbox" (do-canceloutbox)
           "processinbox" (do-processinbox)
           "storagefees" (do-storagefees)
           "dohistory" (do-history)
           "togglehistory" (do-togglehistory)
           "toggleinstructions" (do-toggleinstructions)
           "toggledebug" do-toggledebug

           "register" draw-register
           "balance" draw-balance
           "rawbalance" draw-raw-balance
           "contacts" draw-contacts
           "servers" draw-servers
           "assets" draw-assets
           "fees" draw-fees
           "permissions" draw-permissions
           "admins" draw-admin
           "coupon" draw-coupon
           "history" draw-history)))

(defun web-server-internal (client)
  (with-debug-stream ((not (blankp (get-cookie "debug"))))
    (handler-bind
        ((error
          (lambda (c)
            (return-from web-server-internal
              (whots (s)
                "Error: " (str c)
                (when (debug-stream-p)
                  (who (s)
                    (:br)
                    (:pre (esc (backtrace-string))))))))))
      (web-server-really-internal client))))

(defconstant $POSTCNT "postcnt")

(defun postcnt (cw)
  (or (parse-integer
       (or (ignore-errors (user-preference (cw-client cw) $POSTCNT))
           "0")
       :junk-allowed t)
      "0"))

(defun (setf postcnt) (cnt cw)
  (check-type cnt integer)
  (ignore-errors
    (setf (user-preference (cw-client cw) $POSTCNT)
          (stringify cnt)))
  cnt)

(defun mix-verify-values (postcnt sessionid salt)
  (sha1 (xorcrypt (stringify postcnt) (xorcrypt sessionid salt))))

;; Make it hard to replay a form submission.
(defun form-compute-verify-values (postcnt &optional (sessionid (get-cookie "session")))
  (when sessionid
    (let* ((salt (newsessionid))
           (msg (mix-verify-values postcnt sessionid salt)))
      (values salt msg))))

(defun form-verify-values-p (postcnt salt msg &optional (sessionid (get-cookie "session")))
  (equal msg (mix-verify-values postcnt sessionid salt)))

(defun init-post-salt-and-msg (cw &optional (sessionid (get-cookie "session")))
  (setf (cw-postcnt cw) (postcnt cw))
  (multiple-value-bind (salt msg)
      (form-compute-verify-values (cw-postcnt cw) sessionid)
    (setf (cw-postsalt cw) salt
          (cw-postmsg cw) msg)))

(defun web-server-really-internal (client)
  (let* ((iphone (search "iPhone" (hunchentoot:user-agent)))
         (title "Truledger Client")
         (session (get-cookie "session"))
         (cw (make-cw :client client :iphone iphone :title title :session session))
         (cmd (parm "cmd")))
    
    (unless (blankp session)
      (handler-case
          (progn
            (login-with-sessionid client session)
            (init-post-salt-and-msg cw session)
            (when (null cmd) (setq cmd "balance")))
        (error ()
          (let ((passphrase (loom-login-with-sessionid (db client) session)))
            (cond (passphrase
                   (throw 'raw-return (loom-web-server)))
                  (t (delete-cookie "session")
                     (setf cmd "logout"
                           session nil))))))
      (setf (cw-session cw) session))

    (cond ((id client)
           (initialize-client-history client)

           (init-server cw)

           (unless (serverid client)
             (when (and (not (equal cmd "logout"))
                        (not (equal cmd "login"))
                        (not (equal cmd "server"))
                        (not (equal cmd "admins"))
                        (not (equal cmd "admin")))
               (setq cmd "servers"))))
          ((not (member cmd '("login" "register" "toggledebug") :test #'equal))
           (setq cmd nil)))

    (when (and (or (equal cmd "admins")
                   (equal cmd "admin"))
               (not (is-local-server-server-p cw)))
      (setq cmd nil))

    (setf (cw-body cw)
          (with-output-to-string (s)
            (setf (cw-html-output cw) s)
            (let ((f (gethash cmd *command-map*)))
              (when (consp f)
                (bind-parameters (postcnt postsalt postmsg)
                  (cond ((not (and (form-verify-values-p postcnt postsalt postmsg)
                                   (eql (1- (cw-postcnt cw))
                                        (ignore-errors (parse-integer postcnt)))))
                         (setf f nil
                               (cw-error cw) "Hacking attempt or multiple logins!"))
                        (t (setq f
                                 (and (equal (post-parm "cmd") cmd)
                                      (car f)))))))
              (cond (f (funcall f cw))
                    (session (draw-balance cw))
                    (t (draw-login cw))))))

    ;; Use title, body, onload, and debugstr to fill the page template.
    (prog1
        (with-output-to-string (s)
          (setf (cw-html-output cw) s)
          (write-template cw))
      (when (cw-postcnt cw)
        (setf (postcnt cw) (1+ (cw-postcnt cw)))))))

;; Should support a template.lhtml file that users can easily change.
(defun write-template (cw)
  (let ((title (cw-title cw))
        (servername (cw-servername cw))
        (menu (cw-menu cw)))
    (unless title (setq title "A Truledger Web Client"))
    (unless servername (setq servername "Truledger"))

    (who (s (cw-html-output cw))
      (:html
       (:head
        (:title (str title))
        (:meta :name "viewport" :content "width=device-width")
        (:link :rel "apple-touch-icon" :href="../site-icon.ico")
        (:link :rel "shortcut icon" :href "../site-icon.ico")
        (:link :rel "stylesheet" :type "text/css" :href "../css/tables.css"))
       (:body
        :onload (cw-onload cw)
        (:p
         (:a :href "../"
             (:img :style "vertical-align: middle;border: 1px white"
                   :src "../truledger-logo-50x49.png"
                   :alt "Truledger" :width "50" :height "49"))
         (:b " " (str servername))
         (when menu
           (str "&nbsp;&nbsp;") (str menu)))
        (write-serverline cw)
        (write-idcode cw)
        (str (cw-body cw))
        (multiple-value-bind (version time)
            (ignore-errors
              (getversion (cw-client cw) (cw-refresh-version cw)))
          (let ((debugstr (get-debug-stream-string)))
            (when debugstr
              (who (s)
                (:b "=== Debug log ===")
                (:br)
                (:pre (str debugstr)))))
          (flet ((write-version (s label version time)
                   (unless (blankp version)
                     (who (s)
                       (who (s)
                         (esc label)
                         (:a
                          :class "version"
                          :href
                          (stringify
                           version
                           "http://github.com/billstclair/truledger/commit/~a")
                          (str version)))
                       (when time
                         (let ((datestr (datestr time)))
                           (who (s) " " (str datestr)))))
                     t)))
            (who (s)
              (:p
               :class "version"
               (when (write-version s "Client: " *last-commit* *save-application-time*)
                 (who (s) (:br)))
               (when (serverid (cw-client cw))
                 (unless (equal *last-commit* version)
                   (write-version s "Server: " version time)))))))
        (:p
         (:a :href "http://common-lisp.net/"
             (:img
              :style "border: 0;" :src "../little-lambda.png"
              :alt "Made with Lisp" :title "Made with Lisp"
              :width "16" :height "16"))))))))

(defun write-serverline (cw)
  (let* ((client (cw-client cw))
         (serverid (serverid client)))
    (when serverid
      (let ((server (getserver client serverid)))
        (when server
          (let ((name (server-info-name server))
                (url (server-info-url server)))
            (who (s (cw-html-output cw))
              (:b "Server: ")
              (:span :title (hsc serverid) (esc name))
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

(defmacro form ((s &optional cmd &rest form-params) &body body)
  (let ((stream (gensym "STREAM")))
    `(who (,stream ,s)
       (:form
        :method "post" :action "./" :autocomplete "off"
        ,@form-params
        ,@(and cmd `((:input :type "hidden" :name "cmd" :value ,cmd)))
        (:input :type "hidden" :name "postcnt" :value (cw-postcnt cw))
        (:input :type "hidden" :name "postsalt" :value (cw-postsalt cw))
        (:input :type "hidden" :name "postmsg" :value (cw-postmsg cw))
        ,@body))))

(defmacro storing-error ((err-var format) &body body)
  `(do-storing-error (lambda (x) (unless ,err-var (setf ,err-var x)))
     ,format (lambda () ,@body)))

(defun do-storing-error (setter format thunk)
  (handler-case (funcall thunk)
    (error (c)
      (funcall setter (format nil format c))
      nil)))

(defun draw-error (cw s &optional (err (cw-error cw)))
  (who (s)
    (:span :style "color: red;"
           (if err
               (who (s) (esc err))
               (who (s) (str "&nbsp;"))))))

(defun draw-login (cw &optional key)
  (let ((page (parm "page")))
    (when (equal page "register")
      (return-from draw-login (draw-register cw key))))

  (setf (cw-onload cw) "document.forms[0].passphrase.focus()")
  (setmenu cw nil)

  (set-cookie "test" "test")

  (who (s (cw-html-output cw))
    (form (s "login")
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
        (:td (draw-error cw s))))
      (:a :href "./?cmd=register"
          "Register a new Truledger account")
      (:br)
      (:a :href "./loom?cmd=register"
          "Register a new Loom account"))))

(defun draw-register (cw &optional key)
  (settitle cw "Register")
  (setmenu cw nil)
  (setf (cw-onload cw) "document.forms[0].passphrase.focus()")

  (set-cookie "test" "test")

  (let* ((s (cw-html-output cw))
         (keysize (or (ignore-errors
                        (parse-integer (parm "keysize")))
                      3072))
         (coupon (parm "coupon"))
         (name (parm "name"))
         (cache-privkey-p (if coupon (not (blankp (parm "cacheprivkey"))) t)))
    (flet ((keysize-option (size)
             (let ((size-str (stringify size "~d")))
               (who (s)
                 (:option :value size-str :selected (equal keysize size)
                          (str size-str))))))
      (who (s)
        (form (s "login")
          (:table
           (:tr
            (:td (:b "Passphrase:"))
            (:td (:input :type "password" :name "passphrase" :size "50")
                 (:input :type "hidden" :name "page" :value "register")))
           (:tr
            (:td)
            (:td (draw-error cw s)))
           (:tr
            (:td (:b "Verification:"))
            (:td (:input :type "password" :name "passphrase2" :size "50")))
           (:tr
            (:td (:b "Coupon:"))
            (:td (:input :type "text" :name "coupon" :value coupon :size "64")))
           (:tr
            (:td (:b "Account Name" (:br) "(Optional):"))
            (:td (:input :type "text" :name "name" :value name :size "40")))
           (:tr
            (:td)
            (:td (:input :type "checkbox"
                         :name "cacheprivkey"
                         :checked cache-privkey-p)
                 "Cache encrypted private key on server"))
           (:tr
            (:td (:b "Key size:"))
            (:td
             (:select :name "keysize"
                      (mapc #'keysize-option '(512 1024 2048 3072 4096)))
             (:input :type "submit" :name "newacct" :value "Create account")
             (:input :type "submit" :name "showkey" :value "Show key")
             (:input :type "submit" :name "login" :value "Login")))
           (:tr
            (:td)
            (:td
             "<p>To generate a new private key, leave the area below blank, enter a
passphrase, the passphrase again to verify, a server coupon, an optional
account name, a key size, and click the \"Create account\" button. To
use an existing private key, paste the private key below, enter its
passphrase above, a server coupon, an optional account name, and click
the \"Create account\" button.  To show your encrypted private key,
enter its passphrase, and click the \"Show key\" button. Warning: if you
forget your passphrase, <b>nobody can recover it, ever</b>.</p>

<p>If you lose your private key, which is stored on the computer running this client, nobody can recover that either. To protect against that, you can choose to cache your encrypted private key on the server, with the checkbox above. If you wish to create a new account in this client, using a previously-cached private key, enter your \"Passphrase\", enter the URL of the server (e.g \"http://truledger.com/\") as the \"Coupon\", and press the \"Create account\" button."))
           (:tr
            (:td)
            (:td (:textarea :name "privkey" :cols "65" :rows "44"
                            :style "font-family: Monospace;"
                            (esc key))))))))))

(defun settitle (cw subtitle)
  (setf (cw-title cw) (stringify subtitle "~a - Truledger Client")))

(defun menuitem (cmd text highlight)
  (let ((highlightp (equal cmd highlight)))
    (whots (s)
      (:a :href (stringify cmd "./?cmd=~a")
          (if highlightp
              (who (s) (:b :style "font-size: 120%;" (str text)))
              (who (s) (str text)))))))

(defun server-db-exists-p ()
  (or (ignore-errors (get-running-server))
      (server-privkey-file-exists-p)))

(defun server-privkey-file-exists-p ()
  (let ((db (make-fsdb (server-db-dir))))
    (not (null (db-get db $PRIVKEY)))))

(defun is-local-server-server-p (cw &optional (acceptor hunchentoot:*acceptor*))
  (let* ((port (acceptor-port acceptor))
         (server (port-server port))
         (client (cw-client cw)))
    (or (and client (id client)
             (equal (id client)
                    (cond (server (serverid server))
                          ((server-db-exists-p)
                           (ignore-errors
                             (let* ((db (make-fsdb (server-db-dir)))
                                    (reqs (parse (parser client)
                                                 (db-get db $SERVERID))))
                               (and reqs
                                    (null (cdr reqs))
                                    (getarg 2 (car reqs)))))))))
        (and (not server) (not (server-db-exists-p))))))

(defun get-running-server ()
  (port-server (get-current-port)))

(defun get-current-port (&optional (acceptor hunchentoot:*acceptor*))
  (acceptor-port acceptor))

(defun setmenu (cw &optional highlight (menuitems *default-menuitems*))
  (let ((menu nil)
        (client (cw-client cw)))
    (cond ((and highlight client (serverid client))
           (loop
              for (cmd . text) in menuitems
              do
              (when (or (not (equal cmd "admins"))
                        (is-local-server-server-p cw))
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

(defmethod do-logout ((cw cw) &optional no-draw)
  (when (cw-session cw)
    (logout (cw-client cw)))
  (delete-cookie "session")
  (setf (cw-serverline cw) nil
        (cw-error cw) nil)
  (unless no-draw
    (draw-login cw)))

(defun maybe-start-server-web (client passphrase)
  (when (and (not (get-running-server))
             (server-privkey-file-exists-p))
    (let* ((db (make-fsdb (server-db-dir)))
           (serverid-reqs
            (ignore-errors (parse (parser client) (db-get db $SERVERID))))
           (serverid (and (eql 1 (length serverid-reqs))
                        (gethash 0 (car serverid-reqs)))))
      (when (and serverid (equal serverid (id client)))
        (handler-case
            (let* ((server (make-server (server-db-dir) passphrase))
                   (backup-url (backup-url-preference server))
                   (notification-email (notification-email-preference server))
                   (backup-enabled-p
                    (and backup-url
                         (not (blankp (backup-enabled-preference server)))))
                   (backup-mode-p (not (blankp (backup-mode-preference server))))
                   (errmsg nil))
              (cond (backup-enabled-p
                     (handler-case
                         (truledger-server:start-backup-process
                          server backup-url notification-email)
                       (error (c)
                         (setq errmsg
                               (format nil "Could not start backup process: ~a" c)))))
                    (backup-mode-p
                     (setf (truledger-server:backup-mode-p server) t)))
              (setf (port-server (get-current-port)) server)
              (when errmsg
                (error errmsg)))
          (error (c)
            (error "While starting server: ~a" c)))))))

;; Here from the login page when the user presses one of the buttons
(defun do-login (cw)
  (setf (cw-refresh-version cw) t)
  (bind-parameters (passphrase passphrase2)
    (unwind-protect (do-login-internal cw passphrase passphrase2)
      (destroy-password passphrase)
      (destroy-password passphrase2))))

(defun do-login-internal (cw passphrase passphrase2)
  (bind-parameters (coupon name cacheprivkey keysize login newacct showkey privkey)
    (when coupon
      (setf coupon (trim coupon)))
    (let ((client (cw-client cw))
          (err nil)
          (url-p (url-p coupon))
          fetched-privkey-p)
      (when showkey
        (let ((key (ignore-errors (get-privkey client passphrase))))
          (unwind-protect
               (cond (key
                      (let ((id (pubkey-id (encode-rsa-public-key key))))
                        (do-port-servers (port server)
                          (declare (ignore port))
                          (when (equal id (serverid server))
                            (setf
                             (cw-error cw)
                             "You may not show the server's private key."))))
                      (unless (cw-error cw)
                        (setq privkey (encode-rsa-private-key key passphrase))))
                     (t (setf (cw-error cw) "No key for passphrase")))
            (when key
              (rsa-free key))))
        (return-from do-login-internal (draw-login cw privkey)))

      (when newacct
        (setq login nil)
        (cond ((blankp passphrase)
               (setq err "Passphrase may not be blank"))
              ((and (not url-p)
                    (blankp privkey)
                    (not (equal passphrase passphrase2)))
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
          (return-from do-login-internal (draw-login cw)))

        (unless (blankp coupon)
          (handler-case
              (let ((url (parse-coupon coupon)))
                (handler-case (verify-coupon client coupon nil url)
                  (error (c) (setq err (stringify c)))))
            (error ()
              (handler-case
                  ;; Ensure that coupon is a URL for a proper server
                  (progn (verify-server client coupon)
                         (when (blankp passphrase2)
                           (setq privkey
                                 (fetch-privkey client coupon passphrase)
                                 fetched-privkey-p t)))
                (error (c)
                  (setq err (stringify c "Invalid coupon: ~a")))))))

        (unless err
          (let ((allocated-privkey-p nil))
            (handler-case
                (progn
                  (cond ((integerp privkey)
                         (when (and (blankp coupon) (server-privkey-file-exists-p))
                           (error "Server coupon required for registration")))
                        (t (setq privkey (decode-rsa-private-key privkey passphrase)
                                 allocated-privkey-p t)))
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
              (set-cookie "session" session)
              (init-post-salt-and-msg cw session)
              (handler-case
                  (when (maybe-start-server-web client passphrase)
                    (setf (cw-error cw) "Server started!"))
                (error (c)
                  (setf (cw-error cw) (stringify c))))
              (cond ((equal (get-cookie "test") "test") (delete-cookie "test"))
                    (t (setf
                        (cw-error cw)
                        #.(format nil "Cookies appear to be disabled.~%
                                       This client won't work without them."))))
              (when newacct
                (unless (blankp coupon)
                  (ignore-errors
                    (addserver client coupon name t)
                    (cond (fetched-privkey-p
                           (setf (privkey-cached-p client) t))
                          (cacheprivkey
                           (setf (need-privkey-cache-p client) t))))))
              (init-server cw)
              (return-from do-login-internal
                (if (serverid client)
                    (draw-balance cw)
                    (draw-servers cw))))
          (error (c)
            (let ((session (loom-login-for-client client passphrase)))
              (when session
                (set-cookie "session" session)
                (do-loom cw)))
            (setq err (stringify c "Login error: ~a")))))

      (setf (cw-error cw) err)
      (draw-login cw (parm "privkey")))))

(defun do-server (cw)
  "Here to change servers or add a new server"
  (bind-parameters (newserver selectserver
                              cacheprivkey uncacheprivkey
                              encrypt unencrypt
                              serverurl name server)
    (let* ((client (cw-client cw))
           (err nil))
      (cond (newserver
             (setq serverurl (trim serverurl))
             (handler-case
                 (progn (addserver client serverurl name)
                        (setf (user-preference client "serverid") (serverid client)))
               (error (c) (setq err (stringify c)))))
            (selectserver
             (cond ((or (not server) (equal server ""))
                    (setq err "You must choose a server"))
                   (t (setf (user-preference client "serverid") server)
                      (init-server cw t)
                      (setq err (cw-error cw)))))
            ((or cacheprivkey uncacheprivkey)
             (let ((serverid (serverid client)))
               (unwind-protect
                    (handler-case
                        (progn (unless (equal server serverid)
                                 (setserver client server))
                               (cache-privkey
                                client (get-cookie "session") uncacheprivkey)
                               (setq err
                                     (if cacheprivkey
                                         "Private key cached on server"
                                         "Private key removed from server")))
                      (error (c) (setq err (stringify c))))
                 (unless (equal serverid (serverid client))
                   (setserver client serverid)))))
            ((or unencrypt encrypt)
             (setf (no-server-encryption-p server) (null encrypt))
             (setf err
                   (if encrypt
                       "Communications will be encrypted, if supported."
                       "Communications will no longer be encrypted."))))
      (cond (err
             (setf (cw-error cw) err)
             (draw-servers cw serverurl name))
            (t (draw-balance cw))))))

(defun do-contact (cw)
  (let ((client (cw-client cw))
        (err nil))
    (bind-parameters (addcontact synccontacts deletecontacts
                                 chkcnt id nickname notes)
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
            (synccontacts
             (handler-case (sync-contacts client)
               (error (c)
                 (setq err (stringify c "Can't sync contacts: ~a"))))
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
    (bind-parameters (newasset updatepercent audit
                      scale precision assetname storage)
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
                 (draw-assets cw
                              :scale scale
                              :precision precision
                              :assetname assetname
                              :storage storage)
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
            (audit
             (let ((assets (getassets client))
                   (id (id client))
                   (serverid (serverid client))
                   (tokenid (tokenid client))
                   (audits nil))
               (dolist (asset assets)
                 (let ((assetid (asset-assetid asset)))
                   (when (and (or (equal serverid id)
                                  (equal id (asset-id asset)))
                              (not (equal assetid tokenid)))
                     (push (cons assetid
                                 (multiple-value-list
                                  (audit client assetid)))
                           audits))))
               (draw-assets cw :audits audits)))
            (t (draw-balance cw))))))

(defun do-fee (cw)
  "Add a new fee, or change or delete an existing one"
  (let ((client (cw-client cw))
        (err nil)
        (values nil))
    (bind-parameters (update add tranfee regfee feecnt)
      (setf feecnt (parse-integer feecnt))
      (cond
        (add
         (unless (blankp tranfee)
           (push (cons :tranfee tranfee) values))
         (unless (blankp regfee)
           (push (cons :regfee regfee) values))
         (dotimes (i feecnt)
           (let ((type (hunchentoot:parameter (format nil "type~d" i)))
                 (asset (hunchentoot:parameter (format nil "asset~d" i)))
                 (amt (hunchentoot:parameter (format nil "amt~d" i))))
             (push (cons (cons type asset) amt) values)))
         (setf values (nreverse values))
         (incf feecnt)
         (push (cons :feecnt feecnt) values))
        (update
         (cond
           ((not (equal (id client) (serverid client)))
            (setf err "Only the server may update fees"))
           (t (let* ((fees nil))
                (unless (blankp regfee)
                  (push (make-fee :type $REGFEE :amount regfee) fees))
                (unless (blankp tranfee)
                  (push (make-fee :type $TRANFEE :amount tranfee) fees))
                (dotimes (i feecnt)
                  (let ((type (hunchentoot:parameter (format nil "type~d" i)))
                        (asset (hunchentoot:parameter (format nil "asset~d" i)))
                        (amt (hunchentoot:parameter (format nil "amt~d" i))))
                    (unless (blankp amt)
                      (push (make-fee :type type
                                      :assetid asset
                                      :formatted-amount amt)
                            fees))))
                (handler-case (apply #'setfees client fees)
                  (error (c)
                    (setf err (format nil "~a" c))))))))
        (t (return-from do-fee (draw-balance cw)))))
    (setf (cw-error cw) err)
    (draw-fees cw values)))

(defun do-permission (cw)
  "Add a new fee, or change or delete an existing one"
  (let* ((client (cw-client cw))
         (id (id client))
         (serverid (serverid client))
         (serverp (equal id serverid))
         (err nil))
    (bind-parameters (permission grantor grantee toid-select toid grant-p
                      show hide add remove grant ungrant)
      (when (blankp grantor) (setf grantor id))
      (when (blankp grantee) (setf grantee id))
      (cond (show)
            (hide (setf permission nil grantor nil))
            (add
             (unless (blankp toid-select)
               (setf toid toid-select))
             (storing-error (err "Error granting permission: ~a")
               (cond ((not (blankp toid))
                      (grant client toid permission grant-p))
                     (serverp (grant client serverid permission t)))))
            (remove
             (storing-error (err "Error denying permission: ~a")
               (deny client grantee permission)))
            (grant
             (storing-error (err "Error granting grant permission: ~a")
               (grant client grantee permission t)))
            (ungrant
             (storing-error (err "Error removing grant permission: ~a")
               (grant client grantee permission nil)))
            (t (return-from do-permission
                 (draw-balance cw))))
      (setf (cw-error cw) err)
      (draw-permissions cw permission grantor))))

(defun backup-preference (server name)
  (db-get (db server) $BACKUP $PREFERENCE name))

(defun (setf backup-preference) (value server name)
  (setf (db-get (truledger-server:wrapped-db server) $BACKUP $PREFERENCE name)
        value))

(defun backup-url-preference (server)
  (backup-preference server "backupurl"))

(defun (setf backup-url-preference) (value server)
  (setf (backup-preference server "backupurl") value))

(defun notification-email-preference (server)
  (backup-preference server "notificationemail"))

(defun (setf notification-email-preference) (value server)
  (setf (backup-preference server "notificationemail") value))

(defun backup-enabled-preference (server)
  (backup-preference server "backupenabled"))

(defun (setf backup-enabled-preference) (value server)
  (setf (backup-preference server "backupenabled") value))

(defun backup-mode-preference (server)
  (backup-preference server "backupmode"))

(defun (setf backup-mode-preference) (value server)
  (setf (backup-preference server "backupmode") value))

(defun do-loom (cw)
  (declare (ignore cw))
  (throw 'raw-return
    (redirect "/client/loom")))

(defun do-admin (cw)
  (bind-parameters (passphrase verification adminpass adminverify)
    (unwind-protect
         (do-admin-internal
             cw passphrase verification adminpass adminverify)
      (destroy-password passphrase)
      (destroy-password verification)
      (destroy-password adminpass)
      (destroy-password adminverify))))

(defun do-admin-internal 
    (cw passphrase verification adminpass adminverify)
  (bind-parameters (servername serverurl backup-url notification-email
                             killclient killserver
                             togglebackup togglebackupmode)
    (let ((client (cw-client cw))
          (server (get-running-server))
          (err nil))
      (cond (killclient
             ;; Bye-bye birdy
             (do-logout cw t)
             (stop-web-server (get-current-port)))
            (killserver
             (setf (port-server (get-current-port)) nil)
             (when server
               (truledger-server:stop-backup-process server nil)))
            (togglebackup
             (setq err (toggle-backup cw server backup-url notification-email)))
            (togglebackupmode
             (cond ((null server) (setq err "Server not running."))
                   ((backup-mode-preference server)
                    (setf (backup-mode-preference server) nil
                          (truledger-server:backup-mode-p server) nil))
                   (t (setf (backup-mode-preference server) "backup"
                            (truledger-server:backup-mode-p server) t))))
            (server
             (setq err "Server already running"))
            ((blankp servername)
             (setq err "Server name must be set"))
            ((or (blankp serverurl)
                 (not (url-p serverurl)))
             (setq err "Server URL must be a web address"))
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
             (setq err "Server already has a client account. Not handled."))
            (t (let ((server (handler-case
                                 (make-server (server-db-dir) passphrase
                                              :servername servername
                                              :serverurl serverurl
                                              :privkey-size 4096)
                               (error (c)
                                 (setq err (stringify
                                            c "Error initializing server: ~a"))
                                 nil))))
                 (when server
                   ;; Enable server web hosting
                   (setf (port-server (acceptor-port hunchentoot:*acceptor*))
                         server)
                   (let ((serverid (serverid server))
                         (admin-name (stringify servername "~a Admin"))
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
                         ;; If we can set the server, the admin user
                         ;; already has an account.
                         (setserver client serverid)
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
                           (addserver client serverurl servername)
                         (error (c)
                           (setq err (stringify
                                      c "Can't add server to client: ~a")))))
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
                               (addserver client serverurl admin-name)
                               (addcontact client serverid servername "The server"))
                           (error (c)
                             (setq err (stringify
                                        c "Can't add server to admin account: ~a"))))))

                     (unless err
                       (handler-case
                           (let ((session (login-new-session client passphrase)))
                             (set-cookie "session" session)
                             (setserver client serverid)
                             (addcontact client admin-id admin-name
                                         "The server administrator"))
                         (error (c)
                           (setq err (stringify
                                      c "Can't login as server: ~a")))))

                     (unless err
                       (setq servername nil
                             serverurl nil
                             err "Server started!")))))))

      (setf (cw-error cw) err)
      (draw-admin cw servername serverurl))))

(defun toggle-backup (cw server backup-url &optional notification-email)
  (declare (ignore cw))
  (handler-case
      (progn
        (unless server
          (error "Can't toggle backup when server isn't enabled"))
        (let ((backup-p (truledger-server:backup-process-url server)))
          (cond (backup-p
                 (setf (backup-enabled-preference server) nil)
                 (truledger-server:stop-backup-process server)
                 "Backup process stopped.")
                ((not (url-p backup-url))
                 (error "Not a URL: ~a" backup-url))
                (t
                 (setf (backup-url-preference server) backup-url
                       (notification-email-preference server) notification-email
                       (backup-enabled-preference server) "enabled")
                 (truledger-server:start-backup-process
                  server backup-url notification-email)
                 "Backup process started."))))
    (error (c)
      (setf (backup-enabled-preference server) nil)
      (stringify c))))

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
                   (blankp allowunregistered)
                   (id-p recipient)
                   (not (ignore-errors (get-id client recipient))))
          (setq err "Recipient ID not registered at server")))

      (cond ((blankp recipient)
             (cond ((blankp mintcoupon)
                    (unless (not (and (blankp toacct) (blankp tonewacct)))
                      (setq err "Recipient missing")))
                   (t (setq recipient $COUPON))))
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
                 (cond ((not (eql (length acct-and-asset) 2))
                        (setq err "Bug: don't understand spentasset"))
                       (t (setq err (do-spend-internal
                                        cw client acct-and-asset
                                        recipient amount acct2 note)))))
               (return)))
        (setf (cw-error cw) err)
        (if (or err (blankp mintcoupon))
            (unless err (draw-balance cw))
            (draw-coupon cw (last-spend-time client))))
      (when err
        (setf (cw-error cw) err)
        (draw-balance
         cw amount recipient note toacct tonewacct nickname mintcoupon)))))

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
          (directions nil)
          (ignored-times nil))

      (dotimes (i (if (blankp spendcnt) 0 (parse-integer spendcnt)))
        (let ((time (parm "spendtime~d" i))
              (spend (parm "spend~d" i))
              (note (parm "spendnote~d" i))
              (acct (parm "acct~d" i))
              (nickname (parm "spendnick~d" i))
              (spendid (parm "spendid~d" i)))
          (cond ((or (equal spend "accept") (equal spend "reject"))
                 (push (make-process-inbox
                        :time time
                        :request (if (equal spend "accept")
                                     $SPENDACCEPT
                                     $SPENDREJECT)
                        :note (unless (blankp note) note)
                        :acct (unless (blankp acct) acct))
                       directions))
                (t (push time ignored-times)))
          (unless (or (blankp nickname) (blankp spendid))
            (ignore-errors (addcontact client spendid nickname)))))

      (dotimes (i (if (blankp nonspendcnt) 0 (parse-integer nonspendcnt)))
        (let ((time (parm "nonspendtime~d" i))
              (process (parm "nonspend~d" i))
              (nickname (parm "nonspendnick~d" i))
              (spendid (parm "nonspendid~d" i)))
          (cond ((blankp process) (push time ignored-times))
                (t (push (make-process-inbox :time time) directions)))
          (unless (or (blankp nickname) (blankp spendid))
            (addcontact client spendid nickname))))

      (setf (getinboxignored client) ignored-times)

      (when directions
        (setq directions (nreverse directions))
        (handler-case
            (progn (processinbox client directions)
                   (ignore-errors
                     (when (need-privkey-cache-p client)
                       (unless (privkey-cached-p client)
                         (cache-privkey client (get-cookie "session")))
                       (setf (need-privkey-cache-p client) nil))))
          (error (c)
            (setf (cw-error cw) (stringify c "Error from processinbox: ~a")))))

      (draw-balance cw))))

(defun do-storagefees (cw)
  (let ((client (cw-client cw)))
    (handler-case (storagefees client)
      (error (c)
        (setf (cw-error cw) (stringify c))))
    (draw-balance cw)))

(defun initialize-client-history (client)
  (let* ((keephistory (or (user-preference client "keephistory")
                          "keep")))
    (setf (keep-history-p client) (equal keephistory "keep"))))

(defun do-togglehistory (cw)
  (let* ((client (cw-client cw))
         (keephistory-p (initialize-client-history client)))
    (setf (user-preference client "keephistory")
          (if keephistory-p "forget" "keep"))
    (setf (cw-error cw)
          (if keephistory-p "History disabled" "History enabled"))
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

(defun do-toggledebug (cw)
  (let ((new-debug (blankp (get-cookie "debug")))
        (client (cw-client cw)))
    (if new-debug
        (set-cookie "debug" "debug")
        (delete-cookie "debug"))
    (enable-debug-stream new-debug)
    (cond ((serverid client) (draw-balance cw))
          ((id client) (draw-servers cw))
          (t (draw-login cw)))))

(defun init-server (cw &optional report-error-p)
  (let* ((client (cw-client cw))
         (serverid (user-preference client "serverid"))
         (err nil))
    (when serverid
      (handler-case (setserver client serverid nil)
        (error (c)
          (setf err (stringify c "Can't set server: ~a")
                (user-preference client "serverid") nil
                serverid nil))))
    (unless (serverid client)
      (dolist (server (getservers client))
        (let ((serverid (server-info-id server)))
          (handler-case
              (progn (setserver client serverid)
                     (setf (user-preference client "serverid") serverid)
                     (return))
            (error (c)
              (setq err (stringify c "Can't set server: ~a")
                    serverid nil))))))
    (unless (or (serverid client) err)
      (setq err "No known servers. Please add one."))

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
          ((and you (equal fromid (id client)))
           (format nil "<span title='~a'>~a</span"
                   fromid you))
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

(defconstant $nl #.(format nil "~%"))
(defconstant $brn #.(format nil "<br/>~%"))

(defun normalize-note (note)
  (if (or (null note) (equal note ""))
      ""
      (str-replace $nl $brn note)))

(defun namestr-html (cw otherid cnt-thunk idname textname &optional you)
  (multiple-value-bind (namestr contact) (id-namestr cw otherid you)
    (when (and (or (null contact)
                   (not (contact-contact-p contact)))
               (not (equal otherid (id (cw-client cw))))
               (not (equal otherid $COUPON)))
      (let* ((cnt (funcall cnt-thunk))
             (nickname (contact-nickname contact))
             (client (cw-client cw)))
        (ignore-errors
          (unless (equal otherid (serverid client))
            (let* ((tokenid (fee-assetid (getfees client))))
              (unless (getbalance client nil tokenid)
                (setq nickname "My Sponsor")))))
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
                        :size "10"
                        :value nickname)))))
    namestr))

(defun contact-options-string (client contacts &optional recipient found-setter)
  (whots (s)
    (:option :value "" "Choose contact...")
    (dolist (contact contacts)
      (let ((namestr (contact-namestr contact))
            (recipid (contact-id contact))
            (selected nil))
        (unless (equal recipid (id client))
          (when (equal recipid recipient)
            (setq selected t)
            (when found-setter
              (funcall found-setter t)))
          (htm
           (:option :value recipid :selected selected
                    (str namestr))))))))

(defun draw-balance (cw &optional
                     spend-amount recipient note toacct tonewacct nickname
                     mintcoupon)
  (let* ((client (cw-client cw))
         (serverid (serverid client))
         (servers (getservers client))
         (err nil)
         (iphone (strstr (or (hunchentoot:header-in* "User-Agent") "") "iPhone"))
         (servercode "")
         inboxcode
         seloptions
         selignoreoptions
         assetlist
         assetlist-stream
         (assetidx 0)
         (acctidx 0)
         (gotbal nil)
         (contacts (storing-error (err "Error getting contacts: ~a")
                     (getcontacts client)))
         inbox
         inboxignored
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

    (let ((serveropts
           (with-output-to-string (s)
             (dolist (server servers)
               (let ((bid (server-info-id server)))
                 (unless (equal bid serverid)
                   (unless (equal (userreq client bid) "-1")
                     (let ((bname (server-info-name server))
                           (burl (server-info-url server)))
                       (who (s)
                         (:option :value bid (str bname) " " (str burl)))))))))))
      (unless (blankp serveropts)
        (setq
         servercode
         (whots (s)
           (form (s "server")
             (:select
              :name "server"
              (:option :value "" "Choose a server...")
              (str serveropts))
             (:input :type "submit" :name "selectserver" :value "Change Server"))))))

    (when serverid
      ;; Print inbox, if there is one

      (setq inbox (storing-error (err "Error getting inbox: ~a")
                    (getinbox client))
            inboxignored (getinboxignored client)
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

      (when inbox
        (setq
         inboxcode
         (with-output-to-string (inbox-stream)
           (when acctoptions
             (setq acctheader (whots (s) (:th "To Acct"))))
           (setq seloptions
                 (whots (s)
                   (:option :value "accept" "Accept")
                   (:option :value "reject" "Reject")
                   (:option :value "ignore" "Ignore"))
                 selignoreoptions
                 (whots (s)
                   (:option :value "accept" "Accept")
                   (:option :value "reject" "Reject")
                   (:option :value "ignore" :selected t "Ignore")))

           (setq assets (storing-error (err "Error getting assets: ~a")
                          (getassets client)))

           (dolist (item inbox)
             (let* ((request (inbox-request item))
                    (fromid (inbox-id item))
                    (time (inbox-time item))
                    (msgtime (inbox-msgtime item)))
               (when msgtime
                 (setf (gethash msgtime inbox-msgtimes) item))
               (cond ((not (equal request $SPEND))
                      (let* ((outitem (find msgtime outbox
                                            :test #'equal
                                            :key #'outbox-time)))
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
                               (namestr (namestr-html cw fromid
                                                      (lambda () spendcnt)
                                                      "spendid" "spendnick"))
                               (timecode
                                (whots (s)
                                  (:input :type "hidden"
                                          :name (stringify spendcnt
                                                           "spendtime~a")
                                          :value time)))
                               (selcode
                                (whots (s)
                                  (:select :name selname
                                           (str (if (member time inboxignored
                                                            :test #'equal)
                                                    selignoreoptions
                                                    seloptions)))))
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
                               ""))
                             (str acctcode)
                             (:td (str date))))
                          (incf spendcnt))))))

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
                               :checked (not (member time inboxignored
                                                     :test #'equal)))
                       "Remove"))
                    (date (datestr time))
                    (acctcode (if acctoptions
                                  (whots (s) (:td "&nbsp;"))
                                  ""))
                    (namestr (namestr-html cw fromid
                                           (lambda () (1- (incf nonspendcnt)))
                                           "nonspendid" "nonspendnick")))
               (incf nonspendcnt)
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
                (form (s "processinbox")
                  (:input :type "hidden" :name "spendcnt" :value spendcnt)
                  (:input :type "hidden" :name "nonspendcnt" :value nonspendcnt)
                  (:table
                   :class "prettytable"
                   (:caption (:b "Inbox"))
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
                  (:input :type "submit" :name "submit"
                          :value "Process Inbox")))))

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
                   :class "prettytable"
                   (:caption (:b "Outbox"))
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
                    (form (s "canceloutbox")
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
             (who (assetcode-stream)
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
                             :style "border-right-width: 0;"
                             (str formattedamount))
                        (:td :title assetid
                             :style "border-left-width: 0;"
                             (str assetname))
                        (:td
                         (:input :type "submit"
                                 :name (format nil "spentasset~d|~d"
                                               acctidx assetidx)
                                 :value "Spend"))))
                     (incf assetidx)))))

             (let ((assetcode (get-output-stream-string assetcode-stream))
                   (newassetlist (get-output-stream-string newassetlist-stream)))
               (unless (blankp assetcode)
                 (who (bal-stream)
                   (:tr
                    (:th :colspan "3"
                         (str acct)))
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
            :class "prettytable"
            (:caption (:b "Balances"))
            (:tr
             (str balcode)))

           (let ((enabled (if (initialize-client-history client)
                              "enabled"
                              "disabled")))
             (when (and (cw-fraction-asset cw) (debug-stream-p))
               (let* ((fraction (getfraction client (cw-fraction-asset cw))))
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
               (:a :href "./?cmd=history" "Show history")
               " (" (str enabled) ")"
               (:br)
               (:a :href "./?cmd=rawbalance"
                   "Show raw balance")
               (:br))))))

      (when assetlist-stream
        (setq assetlist (get-output-stream-string assetlist-stream)))

        (let* ((recipopts nil)
               (found nil)
               (disablemint nil)
               (can-mint-token-p
                (ignore-errors (get-permissions client $MINT-TOKENS)))
               (can-mint-p
                (ignore-errors (get-permissions client $MINT-COUPONS)))
               (recipientid nil)
               (acctcode nil)
               (instructions nil))
        (when gotbal
          (setq recipopts
                (whots (s)
                  (:select
                   :name "recipient"
                   (str (contact-options-string
                         client contacts recipient
                         (lambda (x) (setf found x)))))))
          (when (or (equal (id client) (serverid client))
                    (not (or can-mint-token-p can-mint-p)))
            (setq disablemint t))

          (when (and (not found)
                     (not (equal recipient $COUPON))
                     (not (equal (id client) recipient)))
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
                      (form (s "storagefees")
                        (:table
                         :class "prettytable"
                         (:tr
                          (:th :colspan 3 (str "Storage Fees")))
                         (dolist (storagefee storagefees)
                           (let* ((formattedamount
                                   (balance-formatted-amount storagefee))
                                  (assetname (balance-assetname storagefee))
                                  (time (balance-time storagefee))
                                  (date (datestr time)))
                             (who (s)
                               (:tr
                                (:td :align "right"
                                     :style "border-right-width: 0;"
                                     (str formattedamount))
                                (:td
                                 :style
                                 "margin-right: 5px; border-left-width: 0;"
                                 (str assetname))
                                (:td (str date)))))))
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
                                 :checked mintcoupon
                                 :disabled disablemint)
                         "Mint coupon"))
                       (:tr
                        (:td (:b "Note:"))
                        (:td
                         (:textarea :name "note" :cols "40" :rows "5"
                                    (str note))))
                       (:tr
                        (:td (:b "Recipient ID:"))
                        (:td
                         (:input :type "text" :name "recipientid" :size "40"
                                 :value recipientid)
                         (:br)
                         (:input :type "checkbox"
                                 :name "allowunregistered"
                                 :disabled (not can-mint-token-p))
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
               (form (s "spend")
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

          (let* ((historytext (if (initialize-client-history client)
                                  "Disable" "Enable"))
                 (hideinstructions (hideinstructions cw)))
            (setq instructions
                  (whots (s)
                    (:p
                     (form (s "togglehistory"
                           :style "margin: 0px;") ; prevent whitespace below button
                       (:input :type "submit" :name "togglehistory"
                               :value (stringify historytext "~a history")))
                     (form (s "sync"
                            :style "margin: 0px;") ; prevent whitespace below button
                       (:input :type "submit" :name "resync"
                               :value "Resync with server"))
                     (form (s "toggledebug"
                            :style "margin: 0px;") ; prevent whitespace below button
                       (:input :type "submit" :name "toggledebug"
                               :value (if (debug-stream-p)
                                          "Disable debugging"
                                          "Enable debugging")))
                     (form (s "toggleinstructions"
                            :style "margin: 0px;") ; prevent whitespace below button
                       (:input :type "submit" :name "toggleinstructions"
                               :value (if hideinstructions
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
transfer to the coupon. You can redeem a coupon on the \"Servers\" page.")
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
                  (draw-error cw s err))))
        (setf (cw-onload cw)
              "document.getElementById(\"spendamount\").focus()")
        (who (s (cw-html-output cw))
          (str err)
          (:br)
          (str servercode)
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
                   (:table
                    :class "prettytable"
                    (:tr
                     (:th :colspan 2 "Coupon for outbox entry at " (str datestr)))
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

    (who (s)
      (:br))

    (multiple-value-bind (inbox msghash) (getinbox client t)
      (when inbox
        (who (s)
          (:table
           :class "prettytable"
           (:tr (:th :colspan 2 :style "text-align: left;" "Inbox"))
           (dolist (item inbox)
             (let ((msg (gethash item msghash))
                   (time (inbox-time item)))
               (unless (blankp msg)
                 (who (s)
                   (:tr
                    (:td :valign "top" (esc time))
                    (:td (:pre (str (trimmsg msg)))))))))))))

    (multiple-value-bind (outbox msghash) (getoutbox client t)
      (when outbox
        (who (s)
          (:table
           :class "prettytable"
           (:tr (:th :colspan 2 :style "text-align: left;" (:b "Outbox")))
           (dolist (item outbox)
             (let ((msg (gethash item msghash))
                   (time (outbox-time item)))
               (who (s)
                 (:tr
                  (:td :valign "top" (esc time))
                  (:td (:pre (str (trimmsg msg))))))))))))

    (multiple-value-bind (balance msghash) (getbalance client t nil t)
      (loop
         for (acct . bals) in balance
         do
           (who (s)
             (:table
              :class "prettytable"
              (:tr (:th :colspan 2 :style "text-align: left;" (:b (esc acct))))
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
          (:table
           :class "prettytable"
           (:tr (:th :colspan "2" :style "text-align: left;" "Fractional Balances"))
           (dolist (frac fractions)
             (let ((msg (gethash frac msghash))
                   (assetname (fraction-assetname frac)))
               (who (s)
                 (:tr
                  (:td :valign "top" (esc assetname))
                  (:td (:pre (str (trimmsg msg))))))))))))))

(defun draw-servers(cw &optional serverurl name)
  (let* ((client (cw-client cw))
         (servers (getservers client))
         (err (cw-error cw))
         (stream (cw-html-output cw)))

    (setf (cw-onload cw) "document.forms[0].serverurl.focus()")
    (settitle cw "Servers")
    (setmenu cw "servers")

    (who (stream)
      (draw-error cw stream err)
      (:br)
      (form (stream "server")
        (:table
         (:tr
          (:td (:b "Server URL"
                   (:br)
                   "or Coupon:"))
          (:td
           (:input :type "text" :name "serverurl" :size "64"
                                                :value serverurl)))
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
           (:input :type "submit" :name "newserver" :value "Add Server")
           (:input :type "submit" :name "cancel" :value "Cancel"))))))

    (when servers
      (who (stream)
          (:table
           :class "prettytable"
           (:tr
            (:th "Server")
            (:th "URL")
            (:th "ID")
            (:th "Choose")
            (:th "Private Key")
            (:th "Encryption"))
           (dolist (server servers)
             (let ((bid (server-info-id server)))
               (unless (equal (userreq client bid) "-1")
                 (let ((name (server-info-name server))
                       (url (hsc (server-info-url server)))
                       (cached-p (privkey-cached-p client bid))
                       (encrypted-p (not (no-server-encryption-p bid))))                   (when (blankp name)
                     (setq name "unnamed"))
                   (form (stream "server"
                          :style "margin: 0px;") ; prevent whitespace below button
                     (:input :type "hidden" :name "server" :value bid)
                     (:tr
                      (:td (esc name))
                      (:td (:a :href url (str url)))
                      (:td (:span :class "id" (esc bid)))
                      (:td
                       (:input :type "submit" :name "selectserver"
                                              :value "Choose"))
                      (:td :style "text-align: center;"                        
                       (:input :type "submit"
                               :name (if cached-p "uncacheprivkey" "cacheprivkey")
                               :value (if cached-p "Uncache" "Cache")))
                      (:td
                       (:input :type "submit"
                               :name (if encrypted-p "unencrypt" "encrypt")
                               :value (if encrypted-p "Unencrypt" "Encrypt"))))))))))))))

(defun draw-contacts (cw &optional id nickname notes)
  (let* ((client (cw-client cw))
         (stream (cw-html-output cw))
         (contacts (getcontacts client)))

    (setf (cw-onload cw) "document.forms[0].id.focus()")
    (settitle cw "Contacts")
    (setmenu cw "contacts")

    (who (stream)
      (draw-error cw stream)
      (:br)
      (form (stream "contact")
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
           (:textarea :name "notes" :cols "30" :rows "5" (esc notes))))
         (:tr
          (:td)
          (:td
           (:input :type "submit" :name "addcontact" :value "Add/Change Contact")
           (:input :type "submit" :name "synccontacts" :value "Sync with Server")
           (:input :type "submit" :name "cancel" :value "Cancel"))))

        (when contacts
          (who (stream)
            (:br)
            (:input :type "hidden" :name "cmd" :value "contact")
            (:input :type "hidden" :name "chkcnt" :value (length contacts))
            (:table
             :class "prettytable"
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
                      (:td (:span :class "id" (str id)))
                      (:td (str note))
                      (:td
                       (:input :type "hidden" :name (stringify idx "id~d")
                                              :value id)
                       (:input :type "checkbox" :name (stringify idx "chk~d")))))
                   (incf idx)))))
            (:input :type "submit" :name "deletecontacts"
                    :value "Delete checked")))))))

(defun draw-assets(cw &key scale precision assetname storage
                   audits)
  (let* ((client (cw-client cw))
         (tokenid (tokenid client))
         (assets (getassets client))
         (stream (cw-html-output cw))
         (scale (hsc scale))
         (precision (hsc precision))
         (assetname (hsc assetname))
         (storage (hsc storage))
         (incnt 0)
         (owner-p nil))
    (setf (cw-onload cw) "document.forms[0].scale.focus()")
    (settitle cw "Assets")
    (setmenu cw "assets")

    (who (stream)
      (draw-error cw stream)
      (:br)
      (cond ((not (get-permissions client $ADD-ASSET t))
             (who (stream)
               (:p "You do not have permission to create assets.")))
            (t
             (form (stream "asset")
               (:table
                (:tr
                 (:td (:b "Scale:"))
                 (:td (:input
                       :type "text" :name "scale" :size "3" :value scale)))
                (:tr
                 (:td (:b "Precision:"))
                 (:td (:input
                       :type "text"
                       :name "precision"
                       :size "3"
                       :value precision)))
                (:tr
                 (:td (:b "Asset name:"))
                 (:td (:input
                       :type "text"
                       :name "assetname"
                       :size "30"
                       :value assetname)))
                (:tr
                 (:td (:b "Storage fee (%/year):"))
                 (:td (:input
                       :type "text" :name "storage" :size "5" :value storage)))
                (:tr
                 (:td)
                 (:td (:input
                       :type "submit" :name "newasset" :value "Add Asset")
                      (:input
                       :type "submit" :name "cancel" :value "Cancel"))))))))

  (when assets
    (form (stream "asset")
      (:table
       :class "prettytable"
       (:tr
        (:th "Asset name")
        (:th "Scale")
        (:th "Precision")
        (:th "Storage Fee" (:br) "(%/year)")
        (:th "Owner")
        (:th (str (if audits "Asset ID/Audits" "Asset ID"))))
       (dolist (asset assets)
         (let* ((ownerid (asset-id asset))
                (namestr (id-namestr cw ownerid))
                (assetid (asset-assetid asset))
                (scale (asset-scale asset))
                (precision (asset-precision asset))
                (assetname (asset-name asset))
                (percent (asset-percent asset))
                (cell (assoc assetid audits :test #'equal))
                (balance (second cell))
                (fraction (third cell)))
           (setq percent
                 (if (and (equal ownerid (id client))
                          (not (equal assetid tokenid)))
                     (whots (s)
                       (setf owner-p t)
                       (:input :type "hidden"
                               :name (stringify incnt "assetid~d")
                               :value (hsc assetid))
                       (:input :type "hidden"
                               :name (stringify incnt "opercent~d")
                               :value (hsc percent))
                       (:input :type "text"
                               :name (stringify incnt "percent~d")
                               :value (hsc percent)
                               :size "7"
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
              (:td (:span :class "id" (esc assetid))
                   (when balance
                     (htm
                      (:br)
                      (:b "Balance: ")
                      (str balance)))
                   (when fraction
                     (htm
                      (:br)
                      (:b "Fraction: ")
                      (str fraction)))))))))
      (when (> incnt 0)
        (who (stream)
          (:input :type "hidden" :name "percentcnt" :value incnt)
          (:br)
          (:input :type "submit" :name "updatepercent"
                  :value (if owner-p
                             "Update Storage Fees"
                             "Refresh"))
          (when (or owner-p (equal (id client) (serverid client)))
            (htm
             " "
             (:input :type "submit" :name "audit" :value "Audit")))))))))

(defun render-fee-row (cw feeidx type assetid amt &optional assetname)
  (let* ((client (cw-client cw))
         (stream (cw-html-output cw))
         (serverp (equal (id client) (serverid client)))
         (feeidx-p (integerp feeidx)))
    (unless assetname
      (let ((asset (ignore-errors (getasset client assetid))))
        (setf assetname
              (if asset (asset-name asset) "Unknown asset"))))
    (who (stream)
      (:tr
       (:td
        (if (and serverp feeidx-p)
            (htm
             (:select
              :name (format nil "type~d" feeidx)
              (:option :value $SPEND :selected (equal type $SPEND)
                       (str $SPEND))
              (:option :value $TRANSFER :selected (equal type $TRANSFER)
                       (str $TRANSFER))))
            (htm (str type))))
       (:td :align "right"
            (if serverp
                (htm (:input :type "text"
                             :name (if feeidx-p
                                       (format nil "amt~d" feeidx)
                                       feeidx)
                             :value amt
                             :size "10"
                             :style "text-align: right;"))
                (htm (esc amt))))
       (:td (if (and serverp feeidx-p)
                (htm (:select
                      :name (format nil "asset~d" feeidx)
                      (dolist (asset (getassets client))
                        (let ((name (asset-name asset))
                              (id (asset-assetid asset)))
                          (htm
                           (:option :value id :selected (equal assetid id)
                                    (esc name)))))))
                (htm (esc assetname))))
       (:td (:span :class "id" (esc assetid)))))))

(defun draw-fees (cw &optional values)
  (let* ((client (cw-client cw))
         (stream (cw-html-output cw))
         (serverp (equal (id client) (serverid client)))
         (feeidx 0))
    (multiple-value-bind (tranfee regfee fees) (getfees client t)
      (settitle cw "Fees")
      (setmenu cw "fees")
      (who (stream)
        (draw-error cw stream)
        (:br)
        (form (stream "fee")
          (:table
           :class "prettytable"
           (:tr
            (:th "Fee")
            (:th "Amount")
            (:th "Asset")
            (:th "Asset ID"))
           (render-fee-row
            cw "tranfee"
            (whots (s) "Transaction" (:br) "(refundable)")
            (fee-assetid tranfee)
            (or (cdr (assoc :tranfee values))
                (fee-formatted-amount tranfee))
            (fee-assetname tranfee))
           (render-fee-row
            cw "regfee" "Registration"
            (fee-assetid regfee)
            (or (cdr (assoc :regfee values))
                (fee-formatted-amount regfee))
            (fee-assetname regfee))
           (dolist (fee fees)
             (let* ((type (fee-type fee))
                    (assetid (fee-assetid fee))
                    (cell (assocequal (cons type assetid) values)))
               (when cell (setf values (delete cell values :test #'eq)))
               (render-fee-row
                cw feeidx type
                assetid
                (or (cdr cell) (fee-formatted-amount fee))
                (fee-assetname fee)))
             (incf feeidx))
           (let ((added-fees (remove-if-not #'listp values :key #'car)))
             (dolist (fee added-fees)
               (let ((type (caar fee))
                     (assetid (cdar fee))
                     (amt (cdr fee)))
                 (render-fee-row cw feeidx type assetid amt)
                 (incf feeidx)))
             (let* ((cnt (+ (length fees) (length added-fees)))
                    (feecnt (or (cdr (assoc :feecnt values)) 0))
                    (diff (- feecnt cnt))
                    (tokenid (tokenid client)))
               (dotimes (i diff)
                 (render-fee-row cw feeidx $SPEND tokenid "")
                 (incf feeidx))
               (setf feecnt (max feecnt feeidx))
               (htm
                (:input :type "hidden" :name "feecnt" :value feecnt)))))
          (when serverp
            (htm
             "To remove a fee, clear its amount."
             (:br)
             "The Transaction & Registration fees may not be removed,"
             " but clearing their amounts will restore them to default values."
             (:br)
             (:input :type "submit" :name "update" :value "Update")
             " "
             (:input :type "submit" :name "add" :value "Add fee")
                          " "
             (:input :type "submit" :name "cancel" :value "Cancel"))))))))

(defun draw-permissions (cw &optional shown-permission shown-grantor toid)
  (let* ((client (cw-client cw))
         (stream (cw-html-output cw))
         (id (id client))
         (serverid (serverid client))
         (serverp (equal id serverid))
         (err (cw-error cw))
         (last-perm nil)
         (permissions (storing-error (err "Error getting permissions: ~a")
                        (get-permissions client nil t))))
    (setf permissions (stable-sort permissions #'string<
                                   :key #'permission-permission))
    (settitle cw "Permissions")
    (setmenu cw "permissions")
    (who (stream)
      (draw-error cw stream)
      (:br)
      (:table
       :class "prettytable"
       (:tr
        (:th "Permission")
        (:th "Grantor")
        (:th "Grantee")
        (:th "Action"))
       (dolist (permission permissions)
         (let* ((perm (permission-permission permission))
                (grantor (permission-id permission))
                (grantee (permission-toid permission))
                (grant-p (permission-grant-p permission))
                (shown-p (and (equal perm shown-permission)
                              (equal grantor shown-grantor)))
                (show-name (if shown-p "hide" "show"))
                (show-value (if shown-p "Hide" "Show")))
           (form (stream "permission")
             (:input :type "hidden" :name "permission" :value perm)
             (:input :type "hidden" :name "grantor" :value grantor)
             (:tr
              (:td
               (if (equal perm last-perm)
                   (htm "&nbsp;")
                   (htm (esc perm))))
              (:td (cond (grantor
                          (str (id-namestr cw grantor "You")))
                         ((null grantee) (str "&nbsp;"))
                         (t (str "Default"))))
              (:td (str (if grantee
                            (id-namestr cw grantee "You")
                            "&nbsp;")))
              (:td
               (cond (serverp
                      (if grantor
                          (htm
                           (:input
                            :type "submit" :name show-name :value show-value)
                           (:input
                            :type "submit" :name "remove" :value "Remove"))
                          (htm
                           (:input
                            :type "submit" :name "add" :value "Add"))))
                     (grant-p
                      (htm
                       (:input
                        :type "submit" :name show-name :value show-value)))
                     (t
                      (htm (str "&nbsp;")))))))
           (when shown-p
             (let* ((grants (delete-if-not
                             (lambda (p)
                               (equal (permission-permission p) perm))
                             (get-granted-permissions client)))
                    (contacts (getcontacts client)))
               (dolist (grant grants)
                 (unless (and serverp (equal (permission-toid grant) serverid))
                   (form (stream "permission")
                     (:input :type "hidden" :name "permission" :value perm)
                     (:input :type "hidden" :name "grantor" :value grantor)
                     (:input
                      :type "hidden" :name "grantee"
                      :value (permission-toid grant))
                     (:tr
                      (:td "&nbsp;")
                      (:td "&nbsp")
                      (:td (str (id-namestr cw (permission-toid grant) "You")))
                      (:td
                       (:input
                        :type "submit"
                        :name (if (permission-grant-p grant)
                                  "ungrant"
                                  "grant")
                        :value (if (permission-grant-p grant)
                                   "No Grant"
                                   "Grant"))
                       (:input
                        :type "submit" :name "remove" :value "Remove"))))))
               (form (stream "permission")
                 (:input :type "hidden" :name "permission" :value perm)
                 (:input :type "hidden" :name "grantor" :value grantor)
                 (:tr
                  (:td "&nbsp;")
                  (:td "&nbsp;")
                  (:td
                   (when contacts
                     (htm
                      (:select
                       :name "toid-select"
                       (str (contact-options-string client contacts)))
                      (:br)))
                   (:input :type "text" :name "toid" :value toid))
                  (:td
                   (:input
                    :type "checkbox" :name "grant-p")
                   "grant "
                   (:input
                    :type "submit" :name "add" :value "Add"))))))
           (setf last-perm perm))))
      (form (stream "permission")
        (:input :type "submit" :name "cancel" :value "Cancel")))))

(defun draw-admin (cw &optional servername serverurl)
  (let ((s (cw-html-output cw))
        (disable-p (server-db-exists-p))
        (server (get-running-server))
        (port (get-current-port)))
    (setf (cw-onload cw) "document.forms[0].servername.focus()")
    (settitle cw "Admin")
    (setmenu cw "admins")

    (when server
      (setq servername (servername server)
            serverurl (serverurl server)))

    (who (s)
      (draw-error cw s)
      (:br)
      (str (unless port
             "Web server shut down. Say goodnight, Dick."))

      (when port
        (form (s "admin")
          (str (stringify port "Client web server is running on port ~d."))
          (:br)
          (:input :type "submit" :name "killclient"
                  :value "Shut down web server")
          (:br)(:br)
          (str (cond (server "Server is running.")
                     (disable-p "Server database exists but server not running.")
                     (t "Server database not yet created. Enter info below.")))
          (when (and disable-p (not server))
            (who (s)
              (:br)
              (str "To start it, log out, and log back in as the server.")))
          (when server
            (who (s)
              (:br)
              (:input :type "submit" :name "killserver"
                      :value "Stop Server")
              (:br))))
        (when server
          (let* ((backup-p (truledger-server:backup-process-url server))
                 (backup-url (or backup-p
                                 (backup-url-preference server)
                                 ""))
                 (notification-email
                  (or (truledger-server:backup-notification-email server)
                      (notification-email-preference server)))
                 (backup-mode-p (truledger-server:backup-mode-p server)))
            (cond (backup-mode-p
                   (form (s "admin")
                     "Backup mode enabled"
                     (:br)
                     (:input :type "submit" :name "togglebackupmode"
                             :value "Disable Backup Mode")))
                  (t
                   (form (s "admin")
                     "Backup "
                     (str (if backup-p
                              (if (truledger-server:backup-failing-p server)
                                  "is failing to backup."
                                  "is running.")
                              (if (backup-enabled-preference server)
                                  "has crashed."
                                  "is disabled.")))
                     (:br)
                     (:table
                      (:tr
                       (:td (:b "Backup Server URL:"))
                       (:td (:input :type "text"
                                    :size "30"
                                    :name "backup-url"
                                    :value backup-url
                                    :disabled (not (null backup-p)))))
                      (:tr
                       (:td (:b "Notification Email:"))
                       (:td (:input :type "text"
                                    :size "30"
                                    :name "notification-email"
                                    :value notification-email
                                    :disabled (not (null backup-p))))))
                     (:input :type "submit" :name "togglebackup"
                             :value (if backup-p "Stop Backup" "Start Backup"))
                     (unless backup-p
                       (who (s)
                         (:input :type "submit" :name "togglebackupmode"
                                 :value "Enable backup mode")))))))))

      (when (or server (not disable-p))
        (form (s "admin")
          (:br)
          (:table
           (:tr
            (:td (:b "Server Name:"))
            (:td (:input :type "text"
                         :name "servername"
                         :value servername
                         :disabled disable-p
                         :size 30)))
           (:tr
            (:td (:b "Server URL:"))
            (:td (:input :type "text"
                         :name "serverurl"
                         :value serverurl
                         :disabled disable-p
                         :size 30)))
           (unless disable-p
             (who (s)
               (:tr
                (:td (:b "Server Passphrase:"))
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
                (:td (:input :type "submit" :name "create"
                                            :value "Start Server")
                     (:input :type "submit" :name "cancel"
                                            :value "Cancel")))))))))))

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
