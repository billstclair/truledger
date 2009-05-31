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
    ;;("admins" . "Admin")
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

(defun parm (name)
  (hunchentoot:parameter name))

(defun parms (&key (post t) (get nil))
  (let ((req hunchentoot:*request*))
    (append (and post (hunchentoot:post-parameters req))
            (and get (hunchentoot:get-parameters req)))))

(defun append-debug (cw x)
  (let ((str (cw-debugstr cw)))
    (and str
         (setf (cw-debugstr cw)
               (strcat str x)))))

(defun hsc (x)
  (and x (hunchentoot:escape-for-html x)))

;; Called from do-trubanc-client in server-web.lisp
;; Returns a string with the contents of the client web page.
(defun web-server ()
  (let* ((client (make-client "dbs/clientdb")))
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
           "rawbalance" draw-raw
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
        (error (c)
          (delete-cookie "session")
          (setf (cw-error cw) (format nil "Session login error: ~a" c)
                cmd "logout"
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
                        (not (equal cmd "bank")))
               (setq cmd "banks"))))
          ((and (not (equal cmd "login")) (not (equal cmd "register")))
           (setq cmd nil)))

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
              (format nil "<b>=== Debug log ===</b><br/><pre>~a</pre>~%"
                      str))))

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
              (:b "Bank: ") (esc name) " "
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

  (setf (cw-menu cw) nil
        (cw-onload cw) "document.forms[0].passphrase.focus()")

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
  (setf (cw-menu cw) nil
        (cw-onload cw) "document.forms[0].passphrase.focus()")

  (let* ((s (cw-html-output cw))
         (keysize (or (ignore-errors
                        (parse-integer (parm "keysize")))
                      3072)))
    (flet ((keysize-option (size)
             (let ((size-str (format nil "~d" size)))
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
  (setf (cw-title cw) (format nil "~a - Trubanc Client" subtitle)))

(defun menuitem (cmd text highlight)
  (let ((highlightp (equal cmd highlight)))
    (whots (s)
      (:a :href (format nil "./?cmd=~a" cmd)
          (if highlightp
              (who (s) (:b :style "font-size: 120%;" (str text)))
              (who (s) (str text)))))))

(defun setmenu (cw &optional highlight (menuitems *default-menuitems*))
  (let ((menu nil)
        (client (cw-client cw)))
    (cond ((and highlight client (bankid client))
           (loop
              for (cmd . text) in menuitems
              do
              (when (or (not (equal cmd "admins"))
                        (and client
                             (bankid client)
                             (equal (id client) (bankid client))))
                (if menu
                    (dotcat menu "&nbsp;&nbsp")
                    (setq menu ""))
                (dotcat menu (menuitem cmd text highlight)))))
          (t (setq menu (menuitem "logout" "Logout"  nil))))
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
                       (error (c) (setq err (format nil "~a" c)))))
                 (error ()
                   ;; See if "coupon" is just a URL, meaning the user
                   ;; already has an account at that bank.
                   (handler-case (verify-bank client coupon)
                     (error (c) (setq err (format nil "Invalid coupon: ~a" c)))))))
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
                (setq err (format nil "~a" c)))))))

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
            (setq err (format nil "Login error: ~a" c)))))

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
               (error (c) (setq err (format nil "~a" c)))))
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
                 (let ((chki (parm (format nil "chk~d" i))))
                   (unless (blankp chki)
                     (setq id  (parm (format nil "id~d" i)))
                     (return)))))
             (cond ((blankp id)
                    (setq err
                          "you must specify an id, either explicitly or by checking an existing contact"))
                   (t (handler-case (addcontact client id nickname notes)
                        (error (c)
                          (setq err (format nil "Can't add contact: ~a" c))))))
             (if (setf (cw-error cw) err)
               (draw-contacts cw id nickname notes)
               (draw-contacts cw)))
            (deletecontacts
             (dotimes (i chkcnt)
               (let ((chki (parm (format nil "chk~d" i))))
                 (unless (blankp chki)
                   (let ((id (parm (format nil "id~d" i))))
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
                        (setq err (format nil "Error adding asset: ~a" c))))))
             (if (setf (cw-error cw) err)
                 (draw-assets cw scale precision assetname storage)
                 (draw-assets cw)))
            (updatepercent
             (bind-parameters (percentcnt)
               (dotimes (i (parse-integer percentcnt))
                 (let ((assetid (parm (format nil "assetid~d" i)))
                       (opercent (parm (format nil "opercent~d" i)))
                       (percent (parm (format nil "percent~d" i))))
                   (unless (equal percent opercent)
                     (let ((asset nil))
                       (handler-case
                           (setq asset (getasset client assetid))
                         (error ()
                           (setq err (format nil "Can't find assetid: ~a" assetid))))
                       (when asset
                         (let ((scale (asset-scale asset))
                               (precision (asset-precision asset))
                               (assetname (asset-name asset)))
                           (handler-case
                               (addasset client scale precision assetname percent)
                             (error (c) (setq err (format nil "~a" c)))))))
                     (when err (return)))))
               (setf (cw-error cw) err)
               (draw-assets cw)))
            (t (draw-balance cw))))))

(defun do-admin (cw)
  cw)

(defun do-sync (cw)
  (let ((client (cw-client cw))
        (err nil))
    (handler-case (reinit-balances client)
      (error (c)
        (setq err (format nil "~a" c))))
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

        (setq err "Bug: can't find acct/asset to spend")

        ;; Find the spent asset
        (loop
           for key.value in (parms)
           for key = (car key.value)
           with prefix = "spentasset"
           with prelen = (length prefix)
           do
             (when (eql 0 (find prefix key))
               (let ((acct-and-asset (explode "|" (subseq key prelen))))
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
               (setq err (format nil "~a" c))))))
    err))

#||
function do_canceloutbox() {
  global $error;
  global $client;

  $cancelcount = mqpost('cancelcount');
  for ($i=0; $i<$cancelcount; $i++) {
    if (mqpost("cancel$i")) {
      $canceltime = mqpost("canceltime$i");
      $error = $client->spendreject($canceltime, "Spend cancelled");
      draw_balance();
      break;
    }
  }
}

function do_processinbox() {
  global $error;
  global $client;

  $t = $client->t;

  $spendcnt = mqpost('spendcnt');
  $nonspendcnt = mqpost('nonspendcnt');

  $directions = array();
  for ($i=0; $i<$spendcnt; $i++) {
    $time = mqpost("spendtime$i");
    $spend = mqpost("spend$i");
    $note = mqpost("spendnote$i");
    $acct = mqpost("acct$i");
    if ($spend == 'accept' || $spend == 'reject') {
      $dir = array($t->TIME => $time);
      if ($note) $dir[$t->NOTE] = $note;
      $dir[$t->REQUEST] = ($spend == 'accept') ? $t->SPENDACCEPT : $t->SPENDREJECT;
      if ($acct) $dir[$t->ACCT] = $acct;
      $directions[] = $dir;
    }
    $nickname = mqpost("spendnick$i");
    $spendid = mqpost("spendid$i");
    if ($nickname && $spendid) {
      $client->addcontact($spendid, $nickname);
    }
  }

  for ($i=0; $i<$nonspendcnt; $i++) {
    $time = mqpost("nonspendtime$i");
    $process = mqpost("nonspend$i");
    if ($process) {
      $dir = array($t->TIME => $time);
      $directions[] = $dir;
    }
    $nickname = mqpost("nonspendnick$i");
    $spendid = mqpost("nonspendid$i");
    if ($nickname && $spendid) {
      $client->addcontact($spendid, $nickname);
    }
  }

  if (count($directions) > 0) {
    $err = $client->processinbox($directions);
    if ($err) $error = "error from processinbox: $err";
  }

  draw_balance();
}

function do_storagefees() {
  global $client;
  global $error;

  $error = $client->storagefees();
  draw_balance();
}

function do_history() {
  $history = gethistory();
  $history->do_history();
}

function do_togglehistory() {
  global $client, $keephistory, $error;

  $keephistory = ($keephistory == 'keep' ? 'forget' : 'keep');
  $client->keephistory($keephistory == 'keep');

  $client->userpreference('keephistory', $keephistory);
  $error = ($keephistory == 'keep' ? "History enabled" : "History disabled");

  draw_balance();
}
||#

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
          (setf err (format nil "Can't set bank: ~a" c)
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
              (setq err (format nil "Can't set bank: ~a" c)
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
        (name (format nil "(~a)" name))
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
  (let ((unixtime (if (stringp time)
                      (parse-integer (strip-fract time))
                      time)))
    (hsc (cybertiggyr-time:format-time nil "%d-%m-%Y %I:%M:%S%p" unixtime))))

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
                                      (selname (format nil "spend~d" spendcnt))
                                      (notename (format nil "spendnote~d" spendcnt))
                                      (acctselname (format nil "acct~d" spendcnt))
                                      (timecode
                                       (whots (s)
                                         (:input :type "hidden"
                                                 :name (format nil "spendtime~a"
                                                               spendcnt)
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
                                               :test #'equal :key #'asset-id)
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
                                                   :name (format nil "spendid~a"
                                                                 spendcnt)
                                                   :value fromid)
                                           "Nickname: "
                                           (:input :type "text"
                                                   :name (format nil "spendnick~a"
                                                                 spendcnt)
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
                                      :rows "2"))
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
                         (selname (format nil "nonspend~d" nonspendcnt))
                         (timecode
                          (whots (s)
                            (:input :type "hidden"
                                    :name (format nil "nonspendtime~d" nonspendcnt)
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
                                       "")))
                    (multiple-value-bind (namestr contact) (id-namestr cw fromid)
                      (unless contact
                        (setq namestr
                              (whots (s)
                                (str namestr)
                                (:br)
                                (:input :type "hidden"
                                        :name (format nil "nonspendid~d" nonspendcnt)
                                        :value fromid)
                                "Nickname: "
                                (:input :type "text"
                                        :name (format nil "nonspendnick~d"
                                                      nonspendcnt)
                                        :size "10"))))
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
                         (:td (str date)))))))))
      
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
                                    (:a :href (format nil
                                                      "./?cmd=coupon&time=~a"
                                                      timearg)
                                        (str recip))))))
                         (t (setq namestr (id-namestr cw recip))))
                   (unless (gethash time inbox-msgtimes)
                     (setq cancelcode
                           (whots (s)
                             (:input :type "hidden" :name "canceltime$cancelcount"
                                     :value timestr)
                             (:input :type "submit"
                                     :name (format nil "cancel~d" cancelcount)
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
                      (:td (str assetname))
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
                         "- " (str acct) " -")
                    (str $nl)
                    (str assetcode))))
               (unless (blankp newassetlist)
                 (unless assetlist-stream
                   (setq assetlist-stream (make-string-output-stream)))
                 (who (assetlist-stream)
                   (:input :type "hidden"
                           :name (format nil "acct~d" acctidx)
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
               (:br)))))

        (when assetlist-stream
          (setq assetlist (get-output-stream-string assetlist-stream)))

        (let ((recipopts nil)
              (found nil)
              (selectmint nil)
              (disablemint nil)
              (recipientid nil)
              (acctcode nil))
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
                   (hideinstructions (hideinstructions cw))
                   (instructions
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
                       (when hideinstructions
                         (who (s)
                           (:br)
                           (:a :href "./?cmd=toggleinstructions"
                               "Show Instructions"))))
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
\"Recipient\".")
                          (:p
                           (:a :href "./?cmd=toggleinstructions"
                               "Hide Instructions")))))))

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
                (str instructions)))))))))

#||

function draw_coupon($time = false) {
  global $client;
  global $error;
  global $onload, $body;
  
  $t = $client->t;

  settitle('Coupon');
  setmenu('balance');

  $outbox = $client->getoutbox();
  if (!$time) $time = mq($_REQUEST['time']);
  $items = $outbox[$time];
  $timestr = hsc($time);
  $datestr = datestr($time);
  if ($items) {
    foreach ($items as $item) {
      $request = $item[$t->REQUEST];
      if ($request == $t->SPEND) {
        $assetname = hsc($item[$t->ASSETNAME]);
        $formattedamount = hsc($item[$t->FORMATTEDAMOUNT]);
        $note = hsc($item[$t->NOTE]);
        if ($note) $note = "<tr><td><b>Note:</b></td><td><span style=\"margin: 5px;\">$note</span></td></tr>\n";
      } elseif ($request == $t->COUPONENVELOPE) {
        $coupon = hsc(trim($item[$t->COUPON]));
        $body = <<<EOT
<br/>
<b>Coupon for outbox entry at $datestr</b>
<table border="1">
<tr><td><b>Amount:</b></td><td><span style="margin: 5px;">$formattedamount $assetname</span></td></tr>
$note<tr><td><b>Coupon:</b></td><td><span style="margin: 5px;">$coupon</span></td></tr>
</table>
EOT;
        return;
      }
    }
  }
  $error = "Couldn't find coupon: $timestr";
  draw_balance();
}

function draw_raw_balance() {
  global $body;
  global $client;

  settitle('Raw Balance');
  setmenu('balance');

  if ($client) {
    $t = $client->t;
    $db = $client->db;
    $id = $client->id;
    $bankid = $client->bankid;
    if (!($id && $bankid)) return;

    $body = '';


    $key = $client->userbankkey($t->INBOX);
    $inbox = $db->contents($key);
    if (count($inbox) == 0) {
      $body .= "<br/><b>=== Inbox empty ===</b><br/>\n";
    } else {
      $body .= '<br/><b>=== Inbox ===</b><br/>
<table border="1">

';
      foreach ($inbox as $file) {
        $msg = $db->get("$key/$file");
        $body .= <<<EOT
<tr>
<td valign="top">$file</td>
<td><pre>$msg</pre></td>
</tr>

EOT;
      }
      $body .= "</table>\n";
    }

    $key = $client->userbankkey($t->OUTBOX);
    $outbox = $db->contents($key);
    if (count($outbox) == 0) {
      $body .= "<br/><b>=== Outbox empty ===</b><br/>\n";
    } else {
      $body .= '<br/><b>=== Outbox===</b><br/>
<table border="1">

';
      foreach ($outbox as $file) {
        $msg = $db->get("$key/$file");
        $body .= <<<EOT
<tr>
<td valign="top">$file</td>
<td><pre>$msg</pre></td>
</tr>

EOT;
      }
      $body .= "</table>\n";
    }

    $key = $client->userbankkey($t->BALANCE);
    $accts = $db->contents($key);
    foreach ($accts as $acct) {
      $body .= '<br/><b>' . $acct . '</b><br/>
<table border="1">

';
      $assets = $db->contents("$key/$acct");
      foreach ($assets as $assetid) {
        $asset = $client->getasset($assetid);
        $assetname = $asset[$t->ASSETNAME];
        $msg = $db->get("$key/$acct/$assetid");
        $body .= <<<EOT
<tr>
<td valign="top">$assetname</td>
<td><pre>$msg</pre></td>
</tr>
EOT;
      }
      $body .= "</table><br/>\n";
    }

    $key = $client->userfractionkey();
    $assetids = $db->contents($key);
    if (count($assetids) > 0) {
      $body .= '<br/><b>=== Fractional Balances ===</b><br/>
<table border="1">
';
      foreach($assetids as $assetid) {
        $asset = $client->getasset($assetid);
        $assetname = $asset[$t->ASSETNAME];
        $msg = $db->get("$key/$assetid");
        $body .= <<<EOT
<tr>
<td valign="top">$assetname</td>
<td><pre>$msg</pre></td>
</tr>
EOT;
      }
      $body .= "</table><br/>\n";
    }  
  }
}

||#

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
                      (:input :type "hidden" :name (format nil "id~d" idx)
                                             :value id)
                      (:input :type "checkbox" :name (format nil "chk~d" idx)))))
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
                                :name (format nil "assetid~d" incnt)
                                :value (hsc assetid))
                        (:input :type "hidden"
                                :name (format nil "opercent~d" incnt)
                                :value (hsc percent))
                        (:input :type "text"
                                :name (format nil "percent~d" incnt)
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

#||
function gethistory() {
  global $history;

  if (!$history) {
    require_once "history.php";
    $history = new history();
  }
  return $history;
}

function draw_history() {
  global $client;

  $history = gethistory();
  return $history->draw_history();
}

function draw_admin($name=false, $tokens=false) {
  global $onload, $body;
  global $error;
  global $client;

  settitle('Admin');
  setmenu('admin');

  $onload = "document.forms[0].name.focus()";

  $body = 'No admin stuff yet';
}

||#

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
