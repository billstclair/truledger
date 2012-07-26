; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Truledger client web server
;;;

(in-package :truledger-client-web)

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
           "balancemisc" (do-balancemisc)
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
              (expand-template
               (list :errmsg (hsc (princ-to-string c))
                     :backtrace (and (debug-stream-p)
                                     (hsc (backtrace-string))))
               "backtrace.tmpl")))))
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
          (let ((passphrase
                 (ignore-errors
                   (loom-login-with-sessionid (db client) session))))
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

(defun jskeyboardp (body)
  (or (search "password" body :test #'string-equal)
      (search "keyboardInput" body :test #'string=)))

(defun write-template (cw)
  (let* ((title (or (cw-title cw) "A Truledger Web Client"))
         (menu (cw-menu cw))
         (header (make-truledger-header cw))
         (page (multiple-value-bind (server-version server-time)
                   (handler-case
                       (getversion (cw-client cw) (cw-refresh-version cw))
                     (error () nil))
                 (when (equal server-version *last-commit*)
                   (setf server-version nil
                         server-time nil))
                 (let* ((body (cw-body cw)))
                   (expand-template
                    (list :title title
                          :jskeyboardp (jskeyboardp body)
                          :onload (cw-onload cw)
                          :menu menu
                          :pre-body header
                          :body body
                          :debugstr (get-debug-stream-string)
                          :client-version *last-commit*
                          :client-date (datestr *save-application-time*)
                          :server-version server-version
                          :server-date (datestr server-time))
                    "index.tmpl")))))
    (write-string page (cw-html-output cw))))

(defun make-truledger-header (cw)
  (let* ((client (cw-client cw))
         (serverid (serverid client))
         (id (and client (id client)))
         server-name server-url
         acct-name)
    (when serverid
      (let ((server (getserver client serverid)))
        (when server
          (setf server-name (server-info-name server)
                server-url (server-info-url server)))))
    (when id
      (multiple-value-bind (pubkeysig name) (get-id client id)
        (declare (ignore pubkeysig))
        (unless (blankp name)
          (setf acct-name name))))
    (expand-template
     (list :server-name (hsc server-name)
           :server-url (hsc server-url)
           :serverid serverid
           :acct-name (hsc acct-name)
           :id id)
     "truledger-header.tmpl")))

(defun postcnt-plist (cw &optional cmd)
  `(,@(and cmd (list :cmd cmd))
      :postcnt ,(cw-postcnt cw)
      :postsalt ,(cw-postsalt cw)
      :postmsg ,(cw-postmsg cw)))

(defmacro storing-error ((err-var format) &body body)
  `(do-storing-error (lambda (x) (unless ,err-var (setf ,err-var x)))
     ,format (lambda () ,@body)))

(defun do-storing-error (setter format thunk)
  (handler-case (funcall thunk)
    (error (c)
      (funcall setter (format nil format c))
      nil)))

(defun expand-cw-template (cw plist template)
  (expand-template
   `(:errmsg ,(hsc (cw-error cw))
             ,@plist)
   template))

(defun draw-login (cw &optional key proxy-host proxy-port)
  (let ((page (parm "page")))
    (when (equal page "register")
      (return-from draw-login (draw-register cw key proxy-host proxy-port))))

  (setf (cw-onload cw) "document.getElementById(\"passphrase\").focus()")
  (setmenu cw nil)
  (set-cookie "test" "test")

  (let ((body (expand-cw-template
               cw
               (postcnt-plist cw)
               "login.tmpl")))
    (princ body (cw-html-output cw))))

(defun draw-register (cw &optional key proxy-host proxy-port)
  (settitle cw "Register")
  (setmenu cw nil)
  (setf (cw-onload cw) "document.getElementById(\"passphrase\").focus()")

  (set-cookie "test" "test")

  (let* ((keysize (or (ignore-errors
                        (parse-integer (parm "keysize")))
                      4096))
         (coupon (parm "coupon"))
         (name (parm "name"))
         (proxy-host (or proxy-host (parm "proxy-host")))
         (proxy-port (or proxy-port (parm "proxy-port")))
         (cache-privkey-p (if coupon (not (blankp (parm "cacheprivkey"))) t))
         (body (expand-cw-template
                cw
                `(,@(postcnt-plist cw)
                    :coupon ,(hsc coupon)
                    :name ,(hsc name)
                    :proxy-host ,(hsc proxy-host)
                    :proxy-port ,(hsc proxy-port)
                    :cache-privkey-p ,cache-privkey-p
                    :keysizes ,(loop for size in '(512 1024 2048 3072 4096)
                                  for selected-p = (eql size keysize)
                                  collect (list :size size :selected-p selected-p))
                    :key ,key)
                "register.tmpl")))
    (princ body (cw-html-output cw))))

(defun settitle (cw subtitle)
  (setf (cw-title cw) (stringify subtitle "~a - Truledger Client")))

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

(defun setmenu (cw &optional highlight (menuitems *default-menuitems*))
  (let ((items nil)
        (client (cw-client cw)))
    (cond ((and highlight client (serverid client))
           (loop
              for (cmd . text) in menuitems
              do
                (when (or (not (equal cmd "admins"))
                          (is-local-server-server-p cw))
                  (push (list :url ""
                              :cmd cmd
                              :highlight (equal highlight cmd)
                              :name text)
                        items))))
          ((and client (id client))
           (push (list :cmd "servers" :name "Servers") items)
           (unless (and (server-db-exists-p)
                        (let ((server (port-server (get-current-port))))
                          (or (null server)
                              (not (equal (id client) (serverid server))))))
             (push (list :cmd "admins" :name "Admin") items))
           (push (list :cmd "logout" :name "Logout") items)))
    (setf (cw-menu cw)
          (expand-template (list :title "Truledger"
                                 :items (nreverse items))
                           "menu.tmpl"))))

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
                (error errmsg))
              server)
          (error (c)
            (error "While starting server: ~a" c)))))))

;; Here from the login page when the user presses one of the buttons
(defun do-login (cw)
  (setf (cw-refresh-version cw) t)
  (bind-parameters (passphrase passphrase2)
    (unwind-protect (do-login-internal cw passphrase passphrase2)
      (destroy-password passphrase)
      (destroy-password passphrase2))))

(defun url-tld (url)
  (let ((uri (and url (ignore-errors (puri:parse-uri url)))))
    (when uri
      (let* ((host (puri:uri-host uri))
             (dotpos (and host (position #\. host :from-end t))))
        (and dotpos (subseq host (1+ dotpos)))))))

;; Default to localhost:4444 for .i2p and localhost:8118 for .onion
(defun get-http-proxy (&optional url)
  (bind-parameters (proxy-host proxy-port)
    (let ((tld (url-tld url)))
      (when (and tld (blankp proxy-host))
        (when (member tld '("i2p" "onion") :test #'string-equal)
          (setf proxy-host "localhost"
                proxy-port
                (if (blankp proxy-port)
                    (if (string-equal tld "i2p") "4444" "8118")
                    proxy-port)))))
    (setf proxy-port
          (if (blankp proxy-port)
              80
              (or (ignore-errors (parse-integer proxy-port))
                  (return-from get-http-proxy :error))))
    (unless (blankp proxy-host)
      (when (equalp proxy-host "localhost")
        (setf proxy-host "127.0.0.1"))
      (list proxy-host proxy-port))))

(defun do-login-internal (cw passphrase passphrase2)
  (bind-parameters (coupon name cacheprivkey keysize login newacct showkey privkey
                           proxy-host proxy-port)
    (when coupon
      (setf coupon (trim coupon)))
    (let ((client (cw-client cw))
          (err nil)
          (url-p (url-p coupon))
          (http-proxy nil)
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
        (return-from do-login-internal
          (draw-login cw privkey proxy-host proxy-port)))

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
          (return-from do-login-internal
            (draw-login cw privkey proxy-host proxy-port)))

        (flet ((get-proxy (url)
                 (setf http-proxy (get-http-proxy url))
                 (when (eq http-proxy :error)
                   (setf (cw-error cw) "Proxy Port must be blank or an integer")
                   (return-from do-login-internal
                     (draw-login cw (parm "privkey") proxy-host proxy-port)))
                 (when http-proxy
                   (setf proxy-host (first http-proxy)
                         proxy-port (princ-to-string (second http-proxy))))))
          (unless (blankp coupon)
            (handler-case
                (let ((url (parse-coupon coupon)))
                  (get-proxy url)
                  (handler-case (verify-coupon client coupon nil url
                                               :http-proxy http-proxy)
                    (error (c) (setq err (stringify c)))))
              (error ()
                (handler-case
                    ;; Ensure that coupon is a URL for a proper server
                    (progn
                      (get-proxy coupon)
                      (verify-server client coupon nil http-proxy)
                      (when (blankp passphrase2)
                        (setq privkey
                              (fetch-privkey client coupon passphrase
                                             :http-proxy http-proxy)
                              fetched-privkey-p t)))
                  (error ()
                    (setq err
                          (if (blankp passphrase2)
                              "Failed to fetch saved private key"
                              "Invalid coupon"))))))))

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
                    (addserver client coupon :name name :couponok t
                               :http-proxy http-proxy)
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
      (draw-login cw (parm "privkey") proxy-host proxy-port))))

(defun do-server (cw)
  "Here to change servers or add a new server"
  (bind-parameters (newserver selectserver
                              cacheprivkey uncacheprivkey
                              encrypt unencrypt
                              serverurl name server)
    (setq serverurl (trim serverurl))
    (let* ((client (cw-client cw))
           (err nil)
           (http-proxy (get-http-proxy
                        (or (ignore-errors (parse-coupon serverurl))
                            serverurl))))
      (cond ((eq http-proxy :error)
             (setf err "Proxy Port must be blank or an integer"))
            (newserver
             (handler-case
                 (progn (addserver client serverurl :name name
                                   :http-proxy http-proxy)
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
             (let ((proxy-host (and (consp http-proxy) (car http-proxy)))
                   (proxy-port (and (consp http-proxy)
                                    (princ-to-string (second http-proxy)))))
               (draw-servers
                cw serverurl name proxy-host proxy-port)))
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
                            (setf nickname (contact-nickname contact)))
                          (when (blankp notes)
                            (setf notes (contact-note contact))))
                        (setf nickname (trim nickname)
                              notes (trim notes))
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
                        (assetstring
                         (hunchentoot:parameter (format nil "assetstring~d" i)))
                        (amt (hunchentoot:parameter (format nil "amt~d" i))))
                    (unless (or (and (blankp assetstring) (blankp asset))
                                (blankp amt))
                      (push (make-fee :type type
                                      :assetid (if (blankp assetstring)
                                                   asset
                                                   assetstring)
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
  (bind-parameters (servername serverurl proxy-host proxy-port
                               backup-url notification-email
                               killclient killserver
                               togglebackup togglebackupmode)
    (let ((client (cw-client cw))
          (server (get-running-server))
          (http-proxy (get-http-proxy serverurl))
          (err nil))
      (when http-proxy
        (setf proxy-host (first http-proxy)
              proxy-port (princ-to-string (second http-proxy))))
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
            ((eq http-proxy :error)
             (setq err "Proxy Port must be blank or an integer"))
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
                                 nil)))
                     (port (acceptor-port hunchentoot:*acceptor*)))
                 (when server
                   ;; Enable server web hosting
                   (setf (port-server port)
                         server)
                   (let ((serverid (serverid server))
                         (admin-name (stringify servername "~a Admin"))
                         (admin-id nil)
                         (admin-exists-p nil))
                     (let ((privkey-str
                            (encode-rsa-private-key
                             (privkey server) passphrase)))
                       (handler-case
                           (newuser client
                                    :passphrase passphrase
                                    :privkey privkey-str)
                         (error (c)
                           (setq err
                                 (stringify
                                  c "Can't create server account in client")))))
                     ;; Make sure we can communicate to server
                     (unless err
                       (handler-case
                           (progn
                             ;;(break "do-admin-internal calling serverid-for-url")
                             (serverid-for-url
                              client serverurl
                              :serverid serverid
                              :http-proxy http-proxy))
                         (error ()
                           (setq err "Can't contact server at Server URL. Try again.")
                           (setf (port-server port) nil)
                           (recursive-delete-directory
                            (fsdb:db-filename (db server) nil))
                           (logout client)
                           (progn ;;ignore-errors
                             (remove-user client serverid passphrase)
                             (let ((session (login-new-session client adminpass)))
                               (set-cookie "session" session)
                               (init-post-salt-and-msg cw session))))))
                     ;; Login as admin on client, creating account if necessary
                     (unless err
                       (handler-case (login client adminpass)
                         (error ()
                           (handler-case
                               (newuser client :passphrase adminpass)
                             (error ()
                               (setq err "Can't create admin account"))))))
                     (unless err
                       (setq admin-id (id client))
                       (ignore-errors
                         ;; If we can set the server, the admin user
                         ;; already has an account.
                         (setserver client serverid)
                         (setq admin-exists-p t)))
                     (unless err
                       (handler-case
                           (progn
                             (login client passphrase)
                             (addserver client serverurl :name servername
                                        :http-proxy http-proxy))
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
                               (addserver client serverurl :name admin-name
                                          :http-proxy http-proxy)
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
      (draw-admin cw servername serverurl proxy-host proxy-port))))

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
         (fee-plist nil)
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
                       (t (multiple-value-setq (fee-plist err)
                            (do-spend-internal
                                cw client acct-and-asset
                                recipient amount acct2 note)))))
               (return)))
        (unless err
          (if (blankp mintcoupon)
            (draw-balance cw :fee-plist fee-plist)
            (draw-coupon cw (last-spend-time client)))))
      (when err
        (setf (cw-error cw) err)
        (draw-balance
         cw
         :spend-amount amount
         :recipient recipient
         :note note
         :toacct toacct
         :tonewacct tonewacct
         :nickname nickname
         :mintcoupon mintcoupon
         :recipientid recipientid
         :allowunregistered allowunregistered)))))

(defun do-spend-internal (cw client acct-and-asset
                          recipient amount acct2 note)
  (let* ((acctidx (first acct-and-asset))
         (assetidx (second acct-and-asset))
         (acct (parm (strcat "acct" acctidx)))
         (assetid (parm (strcat "assetid" acctidx "|" assetidx)))
         (err nil)
         (fees nil))
    (cond ((or (blankp acct) (blankp assetid))
           (setq err "Bug: blank acct or assetid"))
          (t
           (when acct2 (setq acct (list acct acct2)))
           (handler-case
               (progn
                 (setf fees (spend client recipient assetid amount acct note))
                 (setf (cw-fraction-asset cw) assetid))
             (error (c)
               (setq err (stringify c))))))
    (values fees err)))

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

(defun do-balancemisc (cw)
  (with-parms (togglehistory resync toggledebug toggleinstructions)
    (cond (togglehistory (do-togglehistory cw))
          (resync (do-sync cw))
          (toggledebug (do-toggledebug cw))
          (toggleinstructions (do-toggleinstructions cw)))))

(defun do-togglehistory (cw)
  (let* ((client (cw-client cw))
         (keephistory-p (toggle-client-history client)))
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
           (format nil "<span title='~a'>~a</span>"
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
  (and time
       (let ((unix-time (if (stringp time)
                            (parse-integer (strip-fract time))
                            time)))
         (hsc (cybertiggyr-time:format-time
               nil "%d-%b-%y %I:%M:%S%p"
               (unix-to-universal-time unix-time))))))

(defconstant $nl #.(format nil "~%"))
(defconstant $brn #.(format nil "<br/>~%"))

(defun normalize-note (note)
  (if (or (null note) (equal note ""))
      ""
      (str-replace $nl $brn note)))

(defun id-name-info (cw fromid you)
  "Returns five values: contact id namestr you-p unknown-p coupon-p"
  (let ((client (cw-client cw))
        (contact nil)
        (id fromid)
        (you-p nil)
        (namestr nil)
        (unknown-p nil)
        (coupon-p nil))
    (cond ((equal fromid "coupon")
           (setf namestr fromid id nil coupon-p t))
          ((and you (equal fromid (id client)))
           (setf namestr you you-p t))
          (t (setf contact (getcontact client fromid))
             (when contact
               (setf namestr (contact-namestr contact))
               (when (equal namestr fromid)
                 (setf namestr "[unknown]" unknown-p t)))))
    (values contact id namestr you-p unknown-p coupon-p)))

(defun namestr-html (cw otherid cnt-thunk idname textname &optional you)
  (multiple-value-bind (contact id namestr you-p unknown-p coupon-p)
      (id-name-info cw otherid you)
    (let (cnt sponsor-p nickname)
      (when (and (or (null contact)
                     (not (contact-contact-p contact)))
                 (not (equal otherid (id (cw-client cw))))
                 (not (equal otherid $COUPON)))
        (let ((client (cw-client cw)))
          (setf cnt (funcall cnt-thunk)
                nickname (contact-nickname contact))
          (ignore-errors
            (unless (equal otherid (serverid client))
              (let* ((tokenid (fee-assetid (getfees client))))
                (unless (getbalance client nil tokenid)
                  (setf nickname "My Sponsor"
                        sponsor-p t)))))))
      (expand-template
       (list :id id
             :namestr namestr
             :you-p you-p
             :unknown-p unknown-p
             :coupon-p coupon-p
             :regular-p (not (or you-p unknown-p coupon-p sponsor-p))
             :cnt cnt
             :idname idname
             :textname textname
             :sponsor-p sponsor-p
             :otherid otherid
             :nickname (hsc nickname))
       "namestr.tmpl"))))

(defun get-contact-info (client fromid)
  (if (equal fromid (id client))
      (values nil "")
      (let* ((contact (getcontact client fromid))
         (from-name (and contact (contact-name contact)))
         (from-nick (and contact (contact-nickname contact)))
         (default-nickname nil))
    (cond ((or (blankp from-name)
              (and from-nick (equal from-name from-nick)))
           (setf from-name nil))
          ((null from-nick)
           (let ((tokenid (fee-assetid (getfees client))))
             (unless (getbalance client nil tokenid)
               (setf default-nickname "My Sponsor")))))
    (values from-name from-nick default-nickname))))

(defun highlight-assetname (assetid assetname)
  (expand-template
   (list :color (get-highlighted-asset-color assetid)
         :assetname assetname)
   "assetname.tmpl"))

(defun draw-balance (cw &key
                     spend-amount recipient note toacct tonewacct nickname
                     mintcoupon recipientid allowunregistered fee-plist)
  (let* ((client (cw-client cw))
         (serverid (serverid client))
         (servers (getservers client))
         (err (cw-error cw))
         (iphone (strstr (or (hunchentoot:header-in* "User-Agent") "") "iPhone"))
         (serveropts (loop for server in servers
                        for bid = (server-info-id server)
                        unless (equal bid serverid)
                        collect (list :id bid
                                      :name (hsc (server-info-name server))
                                      :url (hsc (server-info-url server)))))
         (postcnt-plist (postcnt-plist cw))
         (debugging-enabled-p (debug-stream-p))
         (history-enabled-p (initialize-client-history client))
         (instructions-enabled-p (not (hideinstructions cw)))
         outbox
         acctoptions
         inbox-p
         (inbox-msgtimes (make-equal-hash))
         (spendcnt 0)
         (nonspendcnt 0)
         spends
         non-spends
         cancelcount
         outbox-items
         accts
         balance
         accts-plists
         acctbals-plists
         (truncated-balance-p nil)
         recipopts
         acct-select-options
         fractionamt fractionscale
         transaction-fee
         storage-fee
         storage-amounts
         storagefees
         body)

    (when serverid
      (setf outbox (storing-error (err "Error getting outbox: ~a")
                     (getoutbox client))
            accts (storing-error (err "Error getting accts: ~a")
                    (getaccts client))
            balance (storing-error (err "Error getting balance: ~a")
                      (getbalance client)))
      (let* ((inbox (storing-error (err "Error getting inbox: ~a")
                      (getinbox client)))
             (inboxignored (getinboxignored client))
             (assets (storing-error (err "Error getting assets: ~a")
                       (getassets client))))
        ;; Have to do this after getinbox. That updates the client db's storage fees.
        (setf storage-amounts (storing-error (err "Error getting storage fees: ~a")
                                (getstoragefee client)))
        (when inbox
          (setf inbox-p t)
          (dolist (item inbox)
            (let* ((request (inbox-request item))
                   (fromid (inbox-id item))
                   (time (inbox-time item))
                   (msgtime (inbox-msgtime item))
                   from-name from-nick default-nickname)
              (multiple-value-setq (from-name from-nick default-nickname)
                (get-contact-info client fromid))
              (when msgtime
                (setf (gethash msgtime inbox-msgtimes) item))
              (cond ((not (equal request $SPEND))
                     (let* ((outitem (find msgtime outbox
                                           :test #'equal
                                           :key #'outbox-time)))
                       (when outitem
                         (setf (inbox-assetname item) (outbox-assetname outitem)
                               (inbox-formattedamount item)
                               (outbox-formattedamount outitem)
                               (inbox-reply item) (inbox-note item)
                               (inbox-note item) (outbox-note outitem)))
                       (let* ((request (inbox-request item))
                              (fromid (inbox-id item))
                              (reqstr (if (equal request $SPENDACCEPT)
                                          "Accept" "Reject"))
                              (time (inbox-time item))
                              (reply (normalize-note (hsc (inbox-reply item))))
                              (assetid (inbox-assetid item))
                              (assetname (hsc (inbox-assetname item)))
                              (amount (hsc (inbox-formattedamount item)))
                              (itemnote (normalize-note (hsc (inbox-note item))))
                              (remove-checked-p (not (member time inboxignored
                                                             :test #'equal)))
                              (date (datestr time)))
                         (push (list :nonspendcnt nonspendcnt
                                     :time time
                                     :reqstr reqstr
                                     :fromid fromid
                                     :from-name (hsc from-name)
                                     :from-nick (hsc from-nick)
                                     :nickname (hsc default-nickname)
                                     :amount amount
                                     :assetname (highlight-assetname
                                                 assetid assetname)
                                     :itemnote itemnote
                                     :remove-checked-p remove-checked-p
                                     :reply reply
                                     :date date)
                               non-spends)
                         (incf nonspendcnt))))
                    (t (let* ((assetid (inbox-assetid item))
                              (assetname (hsc (inbox-assetname item)))
                              (asset-new-p (not (find assetid assets
                                                      :test #'equal
                                                      :key #'asset-assetid)))
                              (amount (hsc (inbox-formattedamount item)))
                              (itemnote (normalize-note
                                         (hsc (inbox-note item))))
                              (ignore-selected-p
                               (member time inboxignored :test #'equal))
                              (date (datestr time)))
                         (push (list :fromid fromid
                                     :from-name from-name
                                     :from-nick from-nick
                                     :amount amount
                                     :assetname (highlight-assetname
                                                 assetid assetname)
                                     :asset-new-p asset-new-p
                                     :itemnote itemnote
                                     :spendcnt spendcnt
                                     :time time
                                     :acctoptions acctoptions
                                     :ignore-selected-p ignore-selected-p
                                     :date date)
                               spends)
                         (incf spendcnt))))))
          (setf spends (nreverse spends)
                non-spends (nreverse non-spends))))

      (when outbox
        (dolist (item outbox)
          (let* ((time (outbox-time item))
                 (timestr (hsc time))
                 (date (datestr time))
                 (request (outbox-request item)))
            (when (equal request $SPEND)
              (let ((nameid (outbox-id item))
                    (assetid (outbox-assetid item))
                    (assetname (hsc (outbox-assetname item)))
                    (amount (hsc (outbox-formattedamount item)))
                    (note (hsc (outbox-note item)))
                    (label "Cancel")
                    name
                    coupontime
                    this-cancelcount)
                (cond ((equal nameid $COUPON)
                       (setf name "Coupon"
                             coupontime (hunchentoot:url-encode time)
                             label "Redeem"))
                      (t (let ((contact (getcontact client nameid)))
                           (cond (contact
                                  (setf name (contact-namestr contact))
                                  (when (equal name nameid)
                                    (setf name "[unknown]"))
                                  (setf nameid nameid))
                                 (t (setf name (hsc nameid)))))))
                (unless (gethash time inbox-msgtimes)
                  (unless cancelcount (setf cancelcount 0))
                  (setf this-cancelcount cancelcount)
                  (incf cancelcount))
                (push (list :date date
                            :coupontime coupontime
                            :nameid nameid
                            :name name
                            :amount amount
                            :assetname (highlight-assetname assetid assetname)
                            :note note
                            :cancelcount this-cancelcount
                            :time timestr
                            :label label)
                      outbox-items)))))
        (setf outbox-items (nreverse outbox-items)))

      (when balance
        (let ((contacts (storing-error (err "Error getting contacts: ~a")
                          (getcontacts client))))
          (when contacts
            (dolist (contact contacts)
              (let* ((namestr (contact-namestr contact))
                     (recipid (contact-id contact))
                     (selected-p (equal recipid recipient)))
                (unless (or (equal recipid (id client))
                            (equal recipid serverid))
                  (push (list :recipid (hsc recipid)
                              :selected-p selected-p
                              :name namestr)
                        recipopts))))
            (setf recipopts (nreverse recipopts))))
        (when accts
          (dolist (acct accts)
            (push (list :acct acct
                        :selected-p (equal acct toacct))
                  acct-select-options))
          (setf acct-select-options (nreverse acct-select-options)))
        (loop
           for (acct . assets) in balance
           for acctidx from 0
           for assets-plists = nil
           for bals-plists = nil
           for assetidx = 0
           do
             (setf acct (hsc acct))
             (dolist (bal assets)
               (unless (eql 0 (bccomp (balance-amount bal) 0))
                 (let ((assetid (hsc (balance-assetid bal)))
                       (assetname (hsc (balance-assetname bal)))
                       (formattedamount (hsc (balance-formatted-amount bal)))
                       (truncated-amount (hsc (balance-truncated-amount bal))))
                   (push (list :acctidx acctidx
                               :assetidx assetidx
                               :assetid assetid)
                         assets-plists)
                   (push (list :amount truncated-amount
                               :full-amount (unless (equal truncated-amount
                                                           formattedamount)
                                              (setf truncated-balance-p t)
                                              formattedamount)
                               :assetname (highlight-assetname assetid assetname)
                               :acctidx acctidx
                               :assetidx assetidx)
                         bals-plists)
                   (incf assetidx))))
             (when bals-plists
               (push (list :acctidx acctidx
                           :acct acct
                           :assets (nreverse assets-plists))
                     accts-plists)
               (push (list :acct acct
                           :bals (nreverse bals-plists))
                     acctbals-plists)))
        (setf accts-plists (nreverse accts-plists)
              acctbals-plists (nreverse acctbals-plists))

        (when (and (cw-fraction-asset cw) (debug-stream-p))
          (let* ((fraction (getfraction client (cw-fraction-asset cw))))
            (when fraction
              (setf fractionamt (hsc (fraction-amount fraction))
                    fractionscale (hsc (fraction-scale fraction))))))
        (when fee-plist
          (setf transaction-fee (getf fee-plist :transaction-fee)
                storage-fee (getf fee-plist :storage-fee))))

      (when storage-amounts
        (dolist (storagefee storage-amounts)
          (let* ((formattedamount
                  (balance-formatted-amount storagefee))
                 (assetname (balance-assetname storagefee))
                 (time (balance-time storagefee))
                 (date (datestr time)))
            (push (list :amount formattedamount
                        :assetname assetname
                        :date date)
                  storagefees)))
        (setf storagefees (nreverse storagefees))))

    (setf (cw-error cw) err)
    (settitle cw "Balance")
    (setmenu cw "balance")

    (setf body (expand-cw-template
                cw
                `(,@postcnt-plist
                  :iphone-p ,(not (null iphone))
                  :serveropts ,serveropts
                  :history-enabled-p ,history-enabled-p
                  :debugging-enabled-p ,debugging-enabled-p
                  :instructions-enabled-p ,instructions-enabled-p
                  :inbox-p ,inbox-p
                  :spendcnt ,spendcnt
                  :nonspendcnt ,nonspendcnt
                  :spends ,spends
                  :non-spends ,non-spends
                  :accts ,accts-plists
                  :acctbals ,acctbals-plists
                  :truncated-balance-p ,truncated-balance-p
                  :spend-amount ,(hsc spend-amount)
                  :recipient ,(hsc recipient)
                  :recipopts ,recipopts
                  :note ,(hsc note)
                  :recipientid ,(hsc recipientid)
                  :allowunregistered ,(not (blankp allowunregistered))
                  :acct-select-options ,acct-select-options
                  :tonewacct ,(hsc tonewacct)
                  :nickname ,(hsc nickname)
                  :mintcoupon-p ,(not (null mintcoupon))
                  :fractionamt ,fractionamt
                  :fractionscale ,fractionscale
                  :transaction-fee ,transaction-fee
                  :storage-fee ,storage-fee
                  :cancelcount ,cancelcount
                  :outbox-items ,outbox-items
                  :storagefees ,storagefees)
                "balance.tmpl"))
    (princ body (cw-html-output cw))))

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
                   (assetid (outbox-assetid item))
                   (assetname (hsc (outbox-assetname item)))
                   (formattedamount (hsc (outbox-formattedamount item)))
                   (note (outbox-note item)))
               (when coupons
                 (let ((body (expand-cw-template
                              cw
                              (list :date datestr
                                    :amount formattedamount
                                    :assetname (highlight-assetname
                                                assetid assetname)
                                    :note (unless (blankp note)
                                            (normalize-note (hsc note)))
                                    :coupon (car coupons))
                              "coupon.tmpl")))
                   (princ body (cw-html-output cw))))))
            (t (setf (cw-error cw)
                     (stringify timestr "Couldn't find coupon: ~a"))
               (draw-balance cw))))))

(defun draw-raw-balance (cw)
  (let* ((client (cw-client cw))
         inbox-items outbox-items accts fraction-items)

    (multiple-value-bind (inbox msghash) (getinbox client t)
      (when inbox
        (setf inbox-items
              (loop for item in inbox
                 for msg = (gethash item msghash)
                 for time = (inbox-time item)
                 unless (blankp msg)
                 collect (list :time (hsc time) :msg (trimmsg msg))))))

    (multiple-value-bind (outbox msghash) (getoutbox client t)
      (when outbox
        (setf outbox-items
              (loop for item in outbox
                 for msg = (gethash item msghash)
                 for time = (outbox-time item)
                 collect (list :time (hsc time)
                               :msg  (trimmsg msg))))))

    (multiple-value-bind (balance msghash) (getbalance client t nil t)
      (setf accts
            (loop
               for (acct . bals) in balance
               for balances =
                 (loop for bal in bals
                    for msg = (gethash bal msghash)
                    for assetid = (balance-assetid bal)
                    for assetname = (hsc (balance-assetname bal))
                    collect (list :assetname (highlight-assetname
                                              assetid assetname)
                                  :msg (trimmsg msg)))
               collect (list :acct (hsc acct)
                             :balances balances))))

    (multiple-value-bind (fractions msghash) (getfraction client nil t)
      (when fractions
        (setf fraction-items
              (loop for frac in fractions
                 for msg = (gethash frac msghash)
                 for assetid = (fraction-assetid frac)
                 for assetname = (hsc (fraction-assetname frac))
                 collect (list :assetname (highlight-assetname
                                           assetid assetname)
                               :msg (trimmsg msg))))))

    (settitle cw "Raw Balance")
    (setmenu cw "balance")

    (let ((body (expand-cw-template
                 cw
                 (list :inbox-items inbox-items
                       :outbox-items outbox-items
                       :accts accts
                       :fraction-items fraction-items)
                 "rawbalance.tmpl")))
      (princ body (cw-html-output cw)))))

(defun draw-servers (cw &optional serverurl name proxy-host proxy-port)
  (let* ((client (cw-client cw))
         (servers (getservers client))
         (postcnt-plist (postcnt-plist cw))
         (server-items
          (loop for server in servers
             for bid = (server-info-id server)
             for name = (server-info-name server)
             for url = (server-info-url server)
             for proxy-host = (server-info-proxy-host server)
             for proxy-port = (server-info-proxy-port server)
             for proxy = (and proxy-host
                              (format nil "~a:~d" proxy-host proxy-port))
             for cached-p = (privkey-cached-p client bid)
             unless (equal (userreq client bid) "-1")
             collect (append postcnt-plist
                             (list :bid bid
                                   :name (hsc (if (blankp name) "unnamed" name))
                                   :url (hsc url)
                                   :proxy (hsc proxy)
                                   :cached-p cached-p)))))

    (setf (cw-onload cw) "document.getElementById(\"serverurl\").focus()")
    (settitle cw "Servers")
    (setmenu cw "servers")

    (let ((body (expand-cw-template
                 cw
                 (append postcnt-plist
                          (list :serverurl (hsc serverurl)
                                :name (hsc name)
                                :proxy-host (hsc proxy-host)
                                :proxy-port (hsc proxy-port)
                                :server-items server-items))
                 "servers.tmpl")))
      (princ body (cw-html-output cw)))))

(defun draw-contacts (cw &optional id nickname notes)
  (let* ((client (cw-client cw))
         (postcnt-plist (postcnt-plist cw))
         (contacts (getcontacts client))
         (contact-items
          (loop for contact in contacts
             for id = (hsc (contact-id contact))
             for idx from 0
             for name = (hsc (contact-name contact))
             for nickname = (hsc (contact-nickname contact))
             for display = (namestr nickname name id)
             for note = (normalize-note (hsc (contact-note contact)))
             collect (list :nickname (unless (blankp nickname) nickname)
                           :name (unless (blankp name) name)
                           :display (unless (blankp display) display)
                           :id id
                           :note (unless (blankp note) note)
                           :idx 0))))

    (setf (cw-onload cw) "document.getElementById(\"id\").focus()")
    (settitle cw "Contacts")
    (setmenu cw "contacts")

    (let ((body (expand-cw-template
                 cw
                 (append postcnt-plist
                         (list :id (hsc id)
                               :nickname (hsc nickname)
                               :notes (hsc notes)
                               :chkcnt (length contact-items)
                               :contact-items contact-items))
                 "contacts.tmpl")))
      (princ body (cw-html-output cw)))))
                               

(defun draw-assets (cw &key scale precision assetname storage
                    audits)
  (let* ((client (cw-client cw))
         (err (cw-error cw))
         (tokenid (storing-error (err "Error getting tokenid: ~a")
                    (tokenid client)))
         (assets (storing-error (err "Error getting assets: ~a")
                   (getassets client)))
         (create-assets-p (storing-error (err "Error getting permissions: ~a")
                            (get-permissions client $ADD-ASSET t)))
         (postcnt-plist (postcnt-plist cw))
         (asset-items nil)
         (scale (hsc scale))
         (precision (hsc precision))
         (assetname (hsc assetname))
         (storage (hsc storage))
         (percentcnt 0)
         (one-owner-p nil))

    (dolist (asset assets)
      (let* ((ownerid (asset-id asset))
             (namestr (id-namestr cw ownerid "You"))
             (assetid (asset-assetid asset))
             (scale (asset-scale asset))
             (precision (asset-precision asset))
             (assetname (hsc (asset-name asset)))
             (percent (asset-percent asset))
             (cell (assoc assetid audits :test #'equal))
             (balance (second cell))
             (fraction (third cell))
             (owner-p (and (equal ownerid (id client))
                           (not (equal assetid tokenid)))))
        (when owner-p (setf one-owner-p t))
        (push (list* :assetname (highlight-assetname assetid assetname)
                     :scale (hsc scale)
                     :precision (hsc precision)
                     :owner-p owner-p
                     :cnt percentcnt
                     :opercent percent
                     :percent percent
                     :owner namestr
                     :assetid assetid
                     :balance (hsc balance)
                     :fraction (hsc fraction)
                     postcnt-plist)
              asset-items)
        (when owner-p (incf percentcnt))))
    (setf asset-items (nreverse asset-items))

    (setf (cw-error cw) err)
    (setf (cw-onload cw) "document.getElementById(\"scale\").focus()")
    (settitle cw "Assets")
    (setmenu cw "assets")

    (let ((body (expand-cw-template
                 cw
                 (list* :create-assets-p create-assets-p
                        :scale scale
                        :precision precision
                        :assetname assetname
                        :storage storage
                        :percentcnt percentcnt
                        :asset-items asset-items
                        :owner-p one-owner-p
                        :audit-p (or one-owner-p
                                     (equal (id client) (serverid client)))
                        postcnt-plist)
                 "assets.tmpl")))
      (princ body (cw-html-output cw)))))

(defun draw-fees (cw &optional values)
  (let* ((client (cw-client cw))
         (serverp (equal (id client) (serverid client)))
         (feeidx 0)
         (fee-items nil)
         (err nil)
         (assets (storing-error (err "Error getting assets: ~a")
                   (getassets client))))
    (multiple-value-bind (tranfee regfee fees)
        (storing-error (err "Error getting fees: ~a") (getfees client t))
      (when tranfee
        (push (list :type "Transaction (refundable)"
                    :serverp serverp
                    :amtname "tranfee"
                    :amt (hsc (or (cdr (assoc :tranfee values))
                                  (fee-formatted-amount tranfee)))
                    :assetname (highlight-assetname
                                (fee-assetid tranfee)
                                (hsc (fee-assetname tranfee)))
                    :assetid (hsc (fee-assetid tranfee)))
              fee-items))
      (when regfee
        (push (list :type "Registration"
                    :serverp serverp
                    :amtname "regfee"
                    :amt (hsc (or (cdr (assoc :regfee values))
                                  (fee-formatted-amount regfee)))
                    :assetname (highlight-assetname
                                (fee-assetid regfee)
                                (hsc (fee-assetname regfee)))
                    :assetid (hsc (fee-assetid regfee)))
              fee-items))
      (dolist (fee fees)
        (let ((assetid (fee-assetid fee)))
          (unless (find assetid assets :test #'equal :key #'asset-assetid)
            (let ((asset (getasset client assetid)))
              (when asset
                (push asset assets))))))
      (setf assets (sort assets #'asset-lessp))
      (flet ((asset-options (assetid)
               (loop for asset in assets
                  for id = (asset-assetid asset)
                  collect (list :id (hsc id)
                                :assetname (highlight-assetname
                                            id (hsc (asset-name asset)))
                                :selected-p (equal id assetid))))
             (type-options (type)
               (list (list :value $SPEND
                                         :selected-p (equal type $SPEND))
                                   (list :value $TRANSFER
                                         :selected-p (EQUAL type $TRANSFER)))))
        (dolist (fee fees)
          (let* ((type (fee-type fee))
                 (assetid (fee-assetid fee))
                 (cell (assocequal (cons type assetid) values))
                 (amt (hsc (or (cdr cell) (fee-formatted-amount fee)))))
            (when cell (setf values (delete cell values :test #'eq)))
            (push (if serverp
                      (list :feeidx feeidx
                            :serverp t
                            :type-options (type-options type)
                            :amt amt
                            :asset-options (asset-options assetid)
                            :assetid (hsc assetid))
                      (list :type (hsc type)
                            :amt amt
                            :assetname (highlight-assetname
                                        assetid (hsc (fee-assetname fee)))
                            :assetid (hsc assetid)))
                  fee-items)
            (incf feeidx)))
        (let ((added-fees (remove-if-not #'listp values :key #'car)))
          (dolist (fee added-fees)
            (let ((type (caar fee))
                  (assetid (cdar fee))
                  (amt (cdr fee)))
              (push (list :feeidx feeidx
                          :type-options (type-options type)
                          :amt amt
                          :asset-options (asset-options assetid)
                          :assetid nil)
                    fee-items)
              (incf feeidx)))
          (let* ((cnt (+ (length fees) (length added-fees)))
                 (feecnt (or (cdr (assoc :feecnt values)) 0))
                 (diff (- feecnt cnt)))
            (dotimes (i diff)
              (push (list :feeidx feeidx
                          :type-options (type-options nil)
                          :amt nil
                          :asset-options (asset-options nil)
                          :assetid nil)
                    fee-items)
              (incf feeidx))
            (setf feecnt (max feecnt feeidx))

            (setf (cw-error cw) err)
            (settitle cw "Fees")
            (setmenu cw "fees")
            (let ((body (expand-cw-template
                         cw
                         (list* :serverp serverp
                                :feecnt feecnt
                                :fee-items (nreverse fee-items)
                                (postcnt-plist cw))
                         "fees.tmpl")))
              (princ body (cw-html-output cw)))))))))

(defun draw-permissions (cw &optional shown-permission shown-grantor toid)
  (let* ((client (cw-client cw))
         (id (id client))
         (serverid (serverid client))
         (serverp (equal id serverid))
         (err (cw-error cw))
         (last-perm nil)
         (permissions (storing-error (err "Error getting permissions: ~a")
                        (get-permissions client nil t)))
         (postcnt-plist (postcnt-plist cw))
         (contacts (getcontacts client))
         permission-items)
    (setf permissions (stable-sort permissions #'string<
                                   :key #'permission-permission))
    (dolist (permission permissions)
      (let* ((perm (permission-permission permission))
             (grantor (permission-id permission))
             (grantee (permission-toid permission))
             (grant-p (permission-grant-p permission))
             (shown-p (and (equal perm shown-permission)
                           (equal grantor shown-grantor)))
             (grant-items nil)
             (new-grant-p nil)
             (new-grant-plist nil))

        (when shown-p
          (let* ((grants (delete-if-not
                          (lambda (p)
                            (equal (permission-permission p) perm))
                          (get-granted-permissions client))))
            (dolist (grant grants)
              (let ((toid (permission-toid grant)))
                (push (list* :permission (hsc perm)
                             :grantor (hsc grantor)
                             :grantee (hsc toid)
                             :grantee-name (id-namestr cw toid)
                             :granted-p (permission-grant-p grant)
                             postcnt-plist)
                      grant-items)))
            (setf grant-items (nreverse grant-items))
            (let ((contact-items (loop for contact in contacts
                                    collect (list* :id (hsc (contact-id contact))
                                                   :name (contact-namestr contact)
                                                   postcnt-plist))))
              (setf new-grant-p t
                    new-grant-plist (list :permission (hsc perm)
                                          :contact-items contact-items
                                          :toid toid)))))

        (push (list* :perm (hsc perm)
                     :permstr (unless (equal last-perm perm) (hsc perm))
                     :grantor (hsc grantor)
                     :grantor-string  (cond (grantor
                                             (id-namestr cw grantor "You"))
                                            (grantee "Default"))
                     :grantee (and grantee (id-namestr cw grantee "You"))
                     :shown-p shown-p
                     :serverp serverp
                     :grant-p grant-p
                     :grant-items grant-items
                     :new-grant-p new-grant-p
                     (append new-grant-plist postcnt-plist))
              permission-items)
        (setf last-perm perm)))

    (setf (cw-error cw) err)
    (settitle cw "Permissions")
    (setmenu cw "permissions")

    (let ((body (expand-cw-template
                 cw
                 (list* :permission-items permission-items
                        postcnt-plist)
                 "permissions.tmpl")))
      (princ body (cw-html-output cw)))))

(defun draw-admin (cw &optional servername serverurl proxy-host proxy-port)
  (let* ((disable-p (server-db-exists-p))
         (server (get-running-server))
         (port (get-current-port))
         (hide-server-info-p (and (not server) disable-p))
         backup-p backup-failing-p backup-enabled-p
         backup-url notification-email backup-mode-p
         )

    (setf (cw-onload cw) "document.getElementById(\"servername\").focus()")
    (settitle cw "Admin")
    (setmenu cw "admins")

    (when server
      (setq servername (servername server)
            serverurl (serverurl server)))

    (when server
      (setf backup-p (truledger-server:backup-process-url server)
            backup-url (or backup-p (backup-url-preference server) "")
            notification-email
            (or (truledger-server:backup-notification-email server)
                (notification-email-preference server))
            backup-mode-p (truledger-server:backup-mode-p server))
      (if backup-p
          (setf backup-failing-p (truledger-server:backup-failing-p server))
          (setf backup-enabled-p (backup-enabled-preference server))))

    (let ((body (expand-cw-template
                 cw
                 (list* :disable-p disable-p
                        :server server
                        :port port
                        :hide-server-info-p hide-server-info-p
                        :servername (hsc servername)
                        :serverurl (hsc serverurl)
                        :proxy-host (hsc proxy-host)
                        :proxy-port (hsc proxy-port)
                        :backup-p backup-p
                        :backup-failing-p backup-failing-p
                        :backup-enabled-p backup-enabled-p
                        :backup-url (hsc backup-url)
                        :notification-email (hsc notification-email)
                        :backup-mode-p backup-mode-p
                        (postcnt-plist cw))
                 "admin.tmpl")))
      (princ body (cw-html-output cw)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009-2012 Bill St. Clair
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
