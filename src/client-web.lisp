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

;; Called from do-trubanc-client in server-web.lisp
;; Returns a string with the contents of the client web page.
(defun web-server ()
  (let* ((client (make-client (client-db-dir))))
    (unwind-protect
         (web-server-internal client)
      (finalize client))))

(defun cookie-name (base &optional (acceptor hunchentoot:*acceptor*))
  (let ((port (acceptor-port acceptor)))
    (if (eql port 80)
        base
        (format nil "~a-~d" base port))))

(defun get-cookie (name)
  (hunchentoot:cookie-in (cookie-name name)))

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
           "bank" (do-bank)
           "asset" (do-asset)
           "admin" (do-admin)
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
           "banks" draw-banks
           "assets" draw-assets
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

;; Values for forms, so that another site can't
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
         (title "Trubanc Client")
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
          (delete-cookie "session")
          (setf cmd "logout"
                session nil)))
      (setf (cw-session cw) session))

    (cond ((id client)
           (initialize-client-history client)

           (init-bank cw)

           (unless (bankid client)
             (when (and (not (equal cmd "logout"))
                        (not (equal cmd "login"))
                        (not (equal cmd "bank"))
                        (not (equal cmd "admins"))
                        (not (equal cmd "admin")))
               (setq cmd "banks"))))
          ((not (member cmd '("login" "register" "toggledebug") :test #'equal))
           (setq cmd nil)))

    (when (and (or (equal cmd "admins")
                   (equal cmd "admin"))
               (not (is-local-server-bank-p cw)))
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
        (:link :rel "shortcut icon" :href "../site-icon.ico")
        (:link :rel "stylesheet" :type "text/css" :href "../css/tables.css"))
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
                           "http://github.com/billstclair/trubanc-lisp/commit/~a")
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
               (when (bankid (cw-client cw))
                 (unless (equal *last-commit* version)
                   (write-version s "Server: " version time)))))))
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
          "Register a new account"))))

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
passphrase, the passphrase again to verify, a bank coupon, an optional
account name, a key size, and click the \"Create account\" button. To
use an existing private key, paste the private key below, enter its
passphrase above, a bank coupon, an optional account name, and click
the \"Create account\" button.  To show your encrypted private key,
enter its passphrase, and click the \"Show key\" button. Warning: if you
forget your passphrase, <b>nobody can recover it, ever</b>.</p>

<p>If you lose your private key, which is stored on the computer running this client, nobody can recover that either. To protect against that, you can choose to cache your encrypted private key on the server, with the checkbox above. If you wish to create a new account in this client, using a previously-cached private key, enter your \"Passphrase\", enter the URL of the bank (e.g \"http://trubanc.com/\") as the \"Coupon\", and press the \"Create account\" button."))
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

(defun server-db-exists-p ()
  (or (ignore-errors (get-running-server))
      (server-privkey-file-exists-p)))

(defun server-privkey-file-exists-p ()
  (let ((db (make-fsdb (server-db-dir))))
    (not (null (db-get db $PRIVKEY)))))

(defun is-local-server-bank-p (cw &optional (acceptor hunchentoot:*acceptor*))
  (let* ((port (acceptor-port acceptor))
         (server (port-server port))
         (client (cw-client cw)))
    (or (and client (id client)
             (equal (id client)
                    (cond (server (bankid server))
                          ((server-db-exists-p)
                           (ignore-errors
                             (let* ((db (make-fsdb (server-db-dir)))
                                    (reqs (parse (parser client)
                                                 (db-get db $BANKID))))
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

(defun do-logout (cw &optional no-draw)
  (when (cw-session cw)
    (logout (cw-client cw)))
  (delete-cookie "session")
  (setf (cw-bankline cw) nil
        (cw-error cw) nil)
  (unless no-draw
    (draw-login cw)))

(defun maybe-start-server-web (client passphrase)
  (when (and (not (get-running-server))
             (server-privkey-file-exists-p))
    (let* ((db (make-fsdb (server-db-dir)))
           (bankid-reqs
            (ignore-errors (parse (parser client) (db-get db $BANKID))))
           (bankid (and (eql 1 (length bankid-reqs))
                        (gethash 0 (car bankid-reqs)))))
      (when (and bankid (equal bankid (id client)))
        (handler-case
            (let* ((server (make-server (server-db-dir) passphrase))
                   (backup-url (backup-url-preference server))
                   (backup-enabled-p
                    (and backup-url
                         (not (blankp (backup-enabled-preference server)))))
                   (backup-mode-p (not (blankp (backup-mode-preference server))))
                   (errmsg nil))
              (cond (backup-enabled-p
                     (handler-case
                         (trubanc-server:start-backup-process server backup-url)
                       (error (c)
                         (setq errmsg
                               (format nil "Could not start backup process: ~a" c)))))
                    (backup-mode-p
                     (setf (trubanc-server:backup-mode-p server) t)))
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
    (let ((client (cw-client cw))
          (err nil)
          (url-p (url-p coupon))
          fetched-privkey-p)
      (when showkey
        (let ((key (ignore-errors (get-privkey client passphrase))))
          (unwind-protect
               (cond (key
                      (setq privkey (encode-rsa-private-key key passphrase)))
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
                  ;; Ensure that coupon is a URL for a proper bank
                  (progn (verify-bank client coupon)
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
                           (error "Bank coupon required for registration")))
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
                    (addbank client coupon name t)
                    (cond (fetched-privkey-p
                           (setf (privkey-cached-p client) t))
                          (cacheprivkey
                           (setf (need-privkey-cache-p client) t))))))
              (init-bank cw)
              (return-from do-login-internal
                (if (bankid client)
                    (draw-balance cw)
                    (draw-banks cw))))
          (error (c)
            (setq err (stringify c "Login error: ~a")))))

      (setf (cw-error cw) err)
      (draw-login cw (parm "privkey")))))

(defun do-bank (cw)
  "Here to change banks or add a new bank"
  (bind-parameters (newbank selectbank cacheprivkey uncacheprivkey bankurl name bank)
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
                      (setq err (cw-error cw)))))
            ((or cacheprivkey uncacheprivkey)
             (let ((bankid (bankid client)))
               (unwind-protect
                    (handler-case
                        (progn (unless (equal bank bankid)
                                 (setbank client bank))
                               (cache-privkey
                                client (get-cookie "session") uncacheprivkey)
                               (setq err (if cacheprivkey
                                             "Private key cached on server"
                                             "Private key removed from server")))
                      (error (c) (setq err (stringify c))))
                 (unless (equal bankid (bankid client))
                   (setbank client bankid))))))                    
      (cond (err
             (setf (cw-error cw) err)
             (draw-banks cw bankurl name))
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

(defun backup-preference (server name)
  (db-get (db server) $BACKUP $PREFERENCE name))

(defun (setf backup-preference) (value server name)
  (setf (db-get (trubanc-server:wrapped-db server) $BACKUP $PREFERENCE name)
        value))

(defun backup-url-preference (server)
  (backup-preference server "backupurl"))

(defun (setf backup-url-preference) (value server)
  (setf (backup-preference server "backupurl") value))

(defun backup-enabled-preference (server)
  (backup-preference server "backupenabled"))

(defun (setf backup-enabled-preference) (value server)
  (setf (backup-preference server "backupenabled") value))

(defun backup-mode-preference (server)
  (backup-preference server "backupmode"))

(defun (setf backup-mode-preference) (value server)
  (setf (backup-preference server "backupmode") value))

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
  (bind-parameters (bankname bankurl backup-url killclient killserver
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
               (trubanc-server:stop-backup-process server nil)))
            (togglebackup
             (setq err (toggle-backup cw server backup-url)))
            (togglebackupmode
             (cond ((null server) (setq err "Server not running."))
                   ((backup-mode-preference server)
                    (setf (backup-mode-preference server) nil
                          (trubanc-server:backup-mode-p server) nil))
                   (t (setf (backup-mode-preference server) "backup"
                            (trubanc-server:backup-mode-p server) t))))
            (server
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
                                            c "Error initializing bank: ~a"))
                                 nil))))
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

                     (unless err
                       (handler-case
                           (let ((session (login-new-session client passphrase)))
                             (set-cookie "session" session)
                             (setbank client bankid)
                             (addcontact client admin-id admin-name
                                         "The bank administrator"))
                         (error (c)
                           (setq err (stringify
                                      c "Can't login as bank: ~a")))))

                     (unless err
                       (setq bankname nil
                             bankurl nil
                             err "Server started!")))))))

      (setf (cw-error cw) err)
      (draw-admin cw bankname bankurl))))

(defun toggle-backup (cw server backup-url)
  (declare (ignore cw))
  (handler-case
      (progn
        (unless server
          (error "Can't toggle backup when server isn't enabled"))
        (let ((backup-p (trubanc-server:backup-process-url server)))
          (cond (backup-p
                 (setf (backup-enabled-preference server) nil)
                 (trubanc-server:stop-backup-process server)
                 "Backup process stopped.")
                ((not (url-p backup-url))
                 (error "Not a URL: ~a" backup-url))
                (t
                 (setf (backup-url-preference server) backup-url
                       (backup-enabled-preference server) "enabled")
                 (trubanc-server:start-backup-process server backup-url)
                 "Backup proces started."))))
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
                   (not (blankp allowunregistered))
                   (id-p recipient)
                   (not (ignore-errors (get-id client recipient))))
          (setq err "Recipient ID not registered at bank")))

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
    (cond ((bankid client) (draw-balance cw))
          ((id client) (draw-banks cw))
          (t (draw-login cw)))))

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
    (when (and (or (null contact)
                   (not (contact-contact-p contact)))
               (not (equal otherid (id (cw-client cw))))
               (not (equal otherid $COUPON)))
      (let* ((cnt (funcall cnt-thunk))
             (nickname (contact-nickname contact))
             (client (cw-client cw)))
        (ignore-errors
          (unless (equal otherid (bankid client))
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

(defun draw-balance (cw &optional
                     spend-amount recipient note toacct tonewacct nickname
                     mintcoupon)
  (let* ((client (cw-client cw))
         (bankid (bankid client))
         (banks (getbanks client))
         (err nil)
         (iphone (strstr (or (hunchentoot:header-in* "User-Agent") "") "iPhone"))
         (bankcode "")
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
           (form (s "bank")
             (:select
              :name "bank"
              (:option :value "" "Choose a bank...")
              (str bankopts))
             (:input :type "submit" :name "selectbank" :value "Change Bank"))))))

    (when bankid
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
                               "&nbsp;"))
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
               (:a :href "./?cmd=history" "Show history")
               " (" (str enabled) ")"
               (:br)
               (:a :href "./?cmd=rawbalance"
                   "Show raw balance")
               (:br))))))

      (when assetlist-stream
        (setq assetlist (get-output-stream-string assetlist-stream)))

        (let ((recipopts nil)
            (found nil)
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

          (when (equal (id client) (bankid client))
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
                               :value "Resync with bank"))
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
                  (draw-error cw s err))))
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

(defun draw-banks(cw &optional bankurl name)
  (let* ((client (cw-client cw))
         (banks (getbanks client))
         (err (cw-error cw))
         (stream (cw-html-output cw)))

    (setf (cw-onload cw) "document.forms[0].bankurl.focus()")
    (settitle cw "Banks")
    (setmenu cw "banks")

    (who (stream)
      (draw-error cw stream err)
      (:br)
      (form (stream "bank")
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
           :class "prettytable"
           (:tr
            (:th "Bank")
            (:th "URL")
            (:th "ID")
            (:th "Choose")
            (:th "Private Key"))
           (dolist (bank banks)
             (let ((bid (bank-id bank)))
               (unless (equal (userreq client bid) "-1")
                 (let ((name (bank-name bank))
                       (url (hsc (bank-url bank)))
                       (cached-p (privkey-cached-p client bid)))
                   (when (blankp name)
                     (setq name "unnamed"))
                   (form (stream "bank"
                          :style "margin: 0px;") ; prevent whitespace below button
                     (:input :type "hidden" :name "bank" :value bid)
                     (:tr
                      (:td (esc name))
                      (:td (:a :href url (str url)))
                      (:td (esc bid))
                      (:td
                       (:input :type "submit" :name "selectbank"
                                              :value "Choose"))
                      (:td :style "text-align: center;"                        
                       (:input :type "submit"
                               :name (if cached-p "uncacheprivkey" "cacheprivkey")
                               :value (if cached-p "Uncache" "Cache"))))))))))))))

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
                      (:td (str id))
                      (:td (str note))
                      (:td
                       (:input :type "hidden" :name (stringify idx "id~d")
                                              :value id)
                       (:input :type "checkbox" :name (stringify idx "chk~d")))))
                   (incf idx)))))
            (:input :type "submit" :name "deletecontacts"
                    :value "Delete checked")))))))

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
      (draw-error cw stream)
      (:br)
      (form (stream "asset")
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
    (form (stream "asset")
      (:table
       :class "prettytable"
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
          (:br)
          (:input :type "submit" :name "updatepercent"
                  :value "Update Storage Fees")))))))

(defun draw-admin (cw &optional bankname bankurl)
  (let ((s (cw-html-output cw))
        (disable-p (server-db-exists-p))
        (server (get-running-server))
        (port (get-current-port)))
    (setf (cw-onload cw) "document.forms[0].bankname.focus()")
    (settitle cw "Admin")
    (setmenu cw "admins")

    (when server
      (setq bankname (bankname server)
            bankurl (bankurl server)))

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
              (str "To start it, log out, and log back in as the bank.")))
          (when server
            (who (s)
              (:br)
              (:input :type "submit" :name "killserver"
                      :value "Stop Server")
              (:br))))
        (when server
          (let* ((backup-p (trubanc-server:backup-process-url server))
                 (backup-url (or backup-p
                                 (backup-url-preference server)
                                 ""))
                 (backup-mode-p (trubanc-server:backup-mode-p server)))
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
                              "is running."
                              (if (backup-enabled-preference server)
                                  "has crashed."
                                  "is disabled.")))
                     (:br)
                     "Backup server URL: "
                     (:input :type "text"
                             :name "backup-url"
                             :value backup-url
                             :disabled (not (null backup-p)))
                     (:br)
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
                (:td (:input :type "submit" :name "create"
                                            :value "Start Server")
                     (:input :type "submit" :name "cancel"
                                            :value "Cancel")))))))))))

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
