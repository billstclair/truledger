; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Trubanc client web server
;;;

(in-package :trubanc-client-web)

;; Causes WHO and WHOTS to indent prettily by default.
;; Remove when debugged, to reduce bandwidth.
#||
(eval-when (:compile-toplevel :execute :load-toplevel)
  (setq cl-who::*indent* t))
||#

(defparameter *require-coupon* nil)

(defparameter *default-menuitems*
  '(("balance" "Balance")
    ("contacts" "Contacts")
    ("banks" "Banks")
    ("assets" "Assets")
    ;;("admins" "Admin")
    ("logout" "Logout")))    

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

(defun append-debug (cw x)
  (let ((str (cw-debugstr cw)))
    (and str
         (setf (cw-debugstr cw)
               (strcat str x)))))

(defun hsc (x)
  (hunchentoot:escape-for-html x))

;; Called from do-trubanc-client in server-web.lisp
;; Returns a string with the contents of the client web page.
(defun web-server ()
  (let* ((client (make-client "dbs/clientdb")))
    (unwind-protect
         (web-server-internal client)
      (finalize client))))

(defun delete-cookie (name)
  (hunchentoot:set-cookie name :value "" :expires 0))

(defun web-server-internal (client)
  (let* ((iphone (search "iPhone" (hunchentoot:user-agent)))
         (title "Trubanc Client")
         (session (hunchentoot:cookie-in "session"))
         (cw (make-cw :client client :iphone iphone :title title :session session))
         (cmd (hunchentoot:parameter "cmd")))
    
    (when (hunchentoot:cookie-in "debug")
      (setf (cw-debugstr cw) ""
            (showprocess client) (lambda (x) (append-debug cw x))))

    (unless (blankp session)
      (handler-case
          (login-with-sessionid client session)
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

           (cond ((bankid client) (setq cmd "balance"))
                 (t (when (and (not (equal cmd "logout"))
                               (not (equal cmd "login"))
                               (not (equal cmd "bank")))
                      (setq cmd "banks")))))
          ((and (not (equal cmd "login")) (not (equal cmd "register")))
           (setq cmd nil)))

    (setf (cw-body cw)
          (with-output-to-string (s)
            (setf (cw-html-output cw) s)
            (cond ((not cmd) (draw-login cw))
                  ((equal cmd "logout") (do-logout cw))
                  ((equal cmd "login") (do-login cw))
                  ((equal cmd "contact") (do-contact cw))
                  ((equal cmd "bank") (do-bank cw))
                  ((equal cmd "asset") (do-asset cw))
                  ((equal cmd "admin") (do-admin cw))
                  ((equal cmd "spend") (do-spend cw))
                  ((equal cmd "canceloutbox") (do-canceloutbox cw))
                  ((equal cmd "processinbox") (do-processinbox cw))
                  ((equal cmd "storagefees") (do-storagefees cw))
                  ((equal cmd "dohistory") (do-history cw))
                  ((equal cmd "togglehistory") (do-togglehistory cw))
                  ((equal cmd "toggleinstructions") (do-toggleinstructions cw))
            
                  ((equal cmd "register") (draw-register cw))
                  ((equal cmd "balance") (draw-balance cw))
                  ((equal cmd "rawbalance") (draw-raw cw))
                  ((equal cmd "contacts") (draw-contacts cw))
                  ((equal cmd "banks") (draw-banks cw))
                  ((equal cmd "assets") (draw-assets cw))
                  ((equal cmd "admins") (draw-admin cw))
                  ((equal cmd "coupon") (draw-coupon cw))
                  ((equal cmd "history") (draw-history cw))
                  (session (draw-balance cw))
              
                  (t (draw-login cw)))))

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
        (str (cw-debugstr cw)))))))

(defun write-bankline (cw)
  (let* ((client (cw-client cw))
         (bankid (bankid client)))
    (when bankid
      (let ((bank (getbank client bankid)))
        (when bank
          (let ((name (bank-name bank))
                (url (bank-url bank)))
            (who (s (cw-html-output cw))
              (:b "Bank: ") (esc name)
              (:a :href url (str url))
              (:br))))))))

(defun write-idcode (cw)
  (let* ((client (cw-client cw))
         (id (and client (id client))))
    (when id
      (multiple-value-bind (pubkeysig name) (get-id client id)
        (declare (ignore pubkeysig))
        (when name
          (who (s (cw-html-output cw))
            (:b "Account name: ") (esc name)
            (:br)
            (:b "Your ID: ") (str id)
            (:br)))))))

(defun draw-login (cw &optional key)
  (let ((page (hunchentoot:parameter "page")))
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
                        (parse-integer (hunchentoot:parameter "keysize")))
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
           (:td (:textarea :name "privkey" :cols "64" :rows "42"
                           (esc key))))))))))

(defun settitle (cw subtitle)
  (setf (cw-title cw) (format nil "~a - Trubanc Client" subtitle)))

(defun menuitem (cmd text highlight)
  (whots (s)
    (:a :href (format nil "./?cmd=~a" cmd)
        (str (and (equal cmd highlight) "<b>"))
        (str text)
        (str (and (equal cmd highlight) "</b>")))))

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
          (unless key (setf (cw-error cw) "No key for passphrase"))
          (return-from do-login (draw-login cw key))))

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
                     (error (c) (setq err (princ c)))))
               (error ()
                 ;; See if "coupon" is just a URL, meaning the user
                 ;; already has an account at that bank.
                 (handler-case (verify-bank client coupon)
                   (error (c) (setq err (format nil "Invalid coupon: ~a" c)))))))
            ((and *require-coupon* (not (stringp privkey)))
             (setq err "Bank coupon required for registration")))

      (unless err
        (handler-case
            (progn
              (newuser client :passphrase passphrase :privkey privkey)
              (setq login t))
          (error (c)
            (setq err (princ c))))))

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
                (addbank client coupon name t)
                (init-bank cw)))
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
               (error (c) (setq err (princ c)))))
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

#||

function do_contact() {
  global $client;
  global $error;

  $addcontact = mqpost('addcontact');
  $deletecontacts = mqpost('deletecontacts');
  $chkcnt = mqpost('chkcnt');

  if ($addcontact) {
    $id = mqpost('id');
    $nickname = mqpost('nickname');
    $notes = mqpost('notes');
    if (!$id) {
      for ($i=0; $i<$chkcnt; $i++) {
        $chki = mqpost("chk$i");
        if ($chki) {
          $id = mqpost("id$i");
          break;
        }
      }
    }
    $err = '';
    if ($id) $err = $client->addcontact($id, $nickname, $notes);
    else $error = "You must specify an ID, either explicitly or by checking an existing contact";
    if ($err) {
      $error = "Can't add contact: $err";
      draw_contacts($id, $nickname, $notes);
    } else draw_contacts();
  } elseif ($deletecontacts) {
    for ($i=0; $i<$chkcnt; $i++) {
      $chki = mqpost("chk$i");
      if ($chki) {
        $id = mqpost("id$i");
        $client->deletecontact($id);
      }
    }
    draw_contacts();
  } else draw_balance();

  }

// Here to add a new asset
function do_asset() {
  global $client;
  global $error;

  $t = $client->t;
  $error = false;

  $newasset = mqpost('newasset');
  $updatepercent = mqpost('updatepercent');

  if ($newasset) {
    $scale = mqpost('scale');
    $precision = mqpost('precision');
    $assetname = mqpost('assetname');
    $storage = mqpost('storage');
    if (!((strlen($scale) > 0) && (strlen($precision) > 0) &&
          (strlen($assetname) > 0))) {
      $error = "Scale, Precision, and Asset name must all be specified";
    } elseif (!(is_numeric($scale) && is_numeric($precision))) {
      $error = "Scale and Precision must be numbers";
    } elseif ($storage && !is_numeric($storage)) {
      $error = "Storage fee must be a number";
    } else {
      $error = $client->addasset($scale, $precision, $assetname, $storage);
    }
    if ($error) draw_assets($scale, $precision, $assetname, $storage);
    else draw_assets();
  } elseif ($updatepercent) {
    $percentcnt = mqpost('percentcnt');
    for ($i=0; $i<$percentcnt; $i++) {
      $assetid = mqpost("assetid$i");
      $opercent = mqpost("opercent$i");
      $percent = mqpost("percent$i");
      if (!($percent === $opercent)) {   // Detect differences in trailing zeroes
        $asset = $client->getasset($assetid);
        if (is_string($asset)) {
          $error = "Can't find assetid: $assetid";
        } else {
          $scale = $asset[$t->SCALE];
          $precision = $asset[$t->PRECISION];
          $assetname = $asset[$t->ASSETNAME];
          $error = $client->addasset($scale, $precision, $assetname, $percent);
        }
        if ($error) break;
      }
    }
    draw_assets();
  } else draw_balance();
}

function do_admin() {
  global $client;

}

function do_spend() {
  global $error;
  global $client;
  global $fraction_asset;

  $t = $client->t;
  $u = $client->u;
  $id = $client->id;

  $amount = mqpost('amount');
  $recipient = mqpost('recipient');
  $mintcoupon = mqpost('mintcoupon');
  $recipientid = mqpost('recipientid');
  $allowunregistered = mqpost('allowunregistered');
  $note = mqpost('note');
  $nickname = mqpost('nickname');
  $toacct = mqpost('toacct');
  $tonewacct = mqpost('tonewacct');
  $acct2 = '';

  $error = false;
  if (!$recipient) {
    $recipient = $recipientid;
    if ($recipient && !$allowunregistered &&
        $u->is_id($recipient) && !$client->get_id($recipient)) {
      $error = 'Recipient ID not registered at bank';
    }
  }
  if (!$recipient) {
    if ($mintcoupon) $recipient = $t->COUPON;
  } elseif ($mintcoupon) $error = "To mint a coupon don't specify a recipient";
  if (!$error) {
    if (!($amount || ($amount === '0'))) $error = 'Spend amount missing';
    elseif ($id == $recipient || !$recipient) {
      // Spend to yourself = transfer
      $recipient = $id;
      $acct2 = $toacct;
      if (!$acct2) $acct2 = $tonewacct;
      elseif ($tonewacct) $error = 'Choose "Transfer to" from the selector or by typing, but not both';
      if (!$acct2) $error = 'Recipient missing';
    } elseif ($recipient != $t->COUPON && !$u->is_id($recipient)) {
      $error = "Recipient ID malformed";
    }
  }
  if ($error) {
    draw_balance($amount, $recipient, $note, $toacct, $tonewacct, $nickname);
  } else {
    // Add contact if nickname specified
    if ($nickname) {
      $client->addcontact($recipient, $nickname);
    }

    // Find the spent asset
    $found = false;
    foreach ($_POST as $key => $value) {
      $prefix = 'spentasset';
      $prelen = strlen($prefix);
      if (substr($key, 0, $prelen) == $prefix) {
        $acctdotasset = substr($key, $prelen);
        $acctdotasset = explode('|', $acctdotasset);
        if (count($acctdotasset) != 2) {
          $error = "Bug: don't understand spentasset";
          draw_balance($amount, $recipient, $note, $toacct, $tonewacct, $nickname);
        } else {
          $acctidx = $acctdotasset[0];
          $assetidx = $acctdotasset[1];
          $acct = mqpost("acct$acctidx");
          $assetid = mqpost("assetid$acctidx|$assetidx");
          if (!$acct || !$assetid) {
            $error = "Bug: blank acct or assetid";
            draw_balance($amount, $recipient, $note, $toacct, $tonewacct, $nickname);
          } else {
            if ($acct2) $acct = array($acct, $acct2);
            $error = $client->spend($recipient, $assetid, $amount, $acct, $note);
            if ($error) {
              draw_balance($amount, $recipient, $note, $toacct, $tonewacct, $nickname);
            } elseif ($mintcoupon) {
              draw_coupon($client->lastspendtime);
            } else {
              $fraction_asset = $assetid;
              draw_balance();
            }
          }
        }
        $found = true;
        break;
      }
    }
    if (!$found) {
      $error = "Bug: can't find acct/asset to spend";
      draw_balance($amount, $recipient, $note, $toacct, $tonewacct, $nickname);
    }
  }
}

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

function hideinstructions($newvalue=false) {
  global $client;

  $key = 'hideinstructions';
  if ($newvalue === false) $newvalue = $client->userpreference($key);
  else $client->userpreference($key, $newvalue);
  return $newvalue;  
}

function do_toggleinstructions() {
  global $client;

  $page = mqrequest('page');
  hideinstructions(hideinstructions() ? '' : 'hide');
  if ($page == 'history') draw_history();
  else draw_balance();
}

||#

(defun init-bank (cw &optional report-error-p)
  (let* ((client (cw-client cw))
         (banks (getbanks client))
         (bankid (user-preference client "bankid"))
         (err nil))
    (when bankid
      (handler-case (setbank client bankid nil)
        (error (c)
          (setf err (format nil "Can't set bank: ~a" c)
                (user-preference client "bankid") nil
                bankid nil))))
    (unless bankid
      (dolist (bank banks)
        (let ((bankid (bank-id bank)))
          (handler-case
              (progn (setbank client bankid)
                     (setf (user-preference client "bankid") bankid)
                     (return))
            (error (c)
              (setq err (format nil "Can't set bank: ~a" c)
                    bankid nil))))))
    (unless (or bankid err)
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
         (balcode "")
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
         assets
         outboxcode
         balcode
         gotbal-p
         spendcode
         storagefeecode
         instructions)

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
                         (:option :value bid (str bname) (str burl)))))))))))
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
            balance (storing-error (err "Error getting balance: ~a"))
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
                                    :checked t
                                    "Remove")))
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
                       (note (hsc (outbox-note item)))
                       (label "Cancel")
                       namestr
                       (cancelcode "&nbsp;"))
                   (unless (not (blankp note))
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
        (setq
         balcode
         (with-output-to-string (bal-stream)
           (loop
              for (acct . assets) in balance
              for assetcode-stream = (make-string-output-stream)
              for newassetlist-stream = (make-string-output-stream)
              do
                (setq acct (hsc acct))
                (dolist (bal assets)
                  (unless (eql 0 (bccomp (balance-amount bal) 0))
                    (setq gotbal-p t)
                    (let ((assetid (hsc (balance-assetid bal)))
                          (assetname (hsc (balance-assetname bal)))
                          (formattedamount (hsc (balance-formatted-amount bal))))
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
              finally
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
                    (incf acctidx))))))
        (setq
         balcode
         (with-output-to-string (bal-stream)
           (who (bal-stream)
             (:table
              :border "1"
              (:caption (:b "=== Balances ==="))
              (:tr
               (:td
                (:table
                 (:tr
                  (:td :colspan "3" "&nbsp;")
                  (str balcode)))))))
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

            (when (> (length accts) 0)
              (setq acctcode
                    (whots (s)
                      (:select
                       :name "toacct"
                       (:option :value "" "Select or fill-in below...")
                       (dolist (acct accts)
                         (let ((selected (equal acct toacct)))
                           (who (s)
                             (:option :value (esc acct) :selected selected
                                      (str acct)))))))))

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
                                      (timestr (hsc time))
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
                           :input :type "text" :name "amount" :size "20"
                           :value spend-amount :style "text-align: right;"))
                         (:tr
                          (:td
                           (:b "Recipient:"))
                          (:td
                           (str recipopts)
                           (:input :type "checkbox" :name "mintcoupon"
                                   :selected selectmint :disabled disablemint
                                   "Mint coupon")))
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
                           (:input :type "checkbox" :name "allowunregistered"
                                   "Allow unregistered")))
                         (:tr
                          (:td (:b "Nickname:"))
                          (:td
                           (:input :type "text" :name "nickname" :size "30"
                                   :value nickname)))
                         (:tr
                          (:td (:b "Transfer to:"))
                          (:td (str acctcode)))
                         (:tr
                          (:td
                           (:input :type "text" :name "tonewacct" :size "30"
                                   :value tonewacct))))))))
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

))))))

#||

      $onload = "document.forms[0].amount.focus()";
      $closespend = "</form>\n";
      $historytext = ($keephistory == 'keep' ? "Disable" : "Enable") . " history";
      $instructions = '<p><a href="./?cmd=togglehistory">' .
                      $historytext . "</a>\n";
      if (hideinstructions()) {
        $instructions .= '<br>
<a href="./?cmd=toggleinstructions">Show Instructions</a>
</p>
';
      } else {
        $instructions .= <<<EOT
</p>
<p>
To make a spend, fill in the "Spend amount", choose a "Recipient" or
enter a "Recipient ID, enter (optionally) a "Note", and click the
"Spend" button next to the asset you wish to spend.
</p>
<p>
To transfer balances, enter the "Spend Amount", select or fill-in the
"Transfer to" name (letters, numbers, and spaces only), and click
the"Spend" button next to the asset you want to transfer from. Each
storage location costs one usage token, and there is currently no way
to recover an unused location. 0 balances will show only on the raw
balance screen.
</p>
<p>
To mint a coupon, enter the "Spend Amount", check the "Mint coupon"
box, and click the "Spend" button next to the asset you want to
transfer to the coupon. You can redeem a coupon on the "Banks" page.
</p>
<p>
Entering a "Nickname" will add the "Recipient ID" to your contacts
list with that nickname, or change the nickname of the selected
"Recipient".
</p>
<p>
<a href="./?cmd=toggleinstructions">Hide Instructions</a>
</p>
EOT;
      }
    }
  }

  if ($saveerror) {
    if ($error) $error = "$saveerror<br/>$error";
    else $error = $saveerror;
  }
  if ($error) {
    $error = "<span style=\"color: red\";\">$error</span>\n";
  }
  $body = "$error<br/>$bankcode$inboxcode$spendcode$outboxcode$storagefeecode$instructions";
}

# ||

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
                     (:form
                      :method "post" :action "./" :autocomplete "off"
                      (:input :type "hidden" :name "cmd" :value "bank")
                      (:input :type "hidden" :name "bank" :value bid)
                      (:tr
                       (:td (esc name))
                       (:td (:a :href url (str url)))
                       (:td (esc bid))
                       (:td
                        (:input :type "submit" :name "selectbank"
                                               :value "Choose"))))))))))))))

#||
function draw_contacts($id=false, $nickname=false, $notes=false) {
  global $onload, $body;
  global $error;
  global $client;

  $t = $client->t;

  $onload = "document.forms[0].id.focus()";
  settitle('Contacts');
  setmenu('contacts');

  $id = hsc($id);
  $nickname = hsc($nickname);
  $notes = hsc($notes);

  $body = <<<EOT
<span style="color: red;">$error</span><br/>
<form method="post" action="./" autocomplete="off">
<input type="hidden" name="cmd" value="contact">
<table>
<tr>
<td align="right"><b>ID:</b></td>
<td><input type="text" name="id" size="40" value="$id"/></td>
</tr><tr>
<td><b>Nickname<br/>(Optional):</b></td>
<td><input type="text" name="nickname" size="30" value="$nickname"/></td>
</tr><tr>
<td><b>Notes<br/>(Optional):</b></td>
<td><textarea name="notes" cols="30" rows="10">$notes</textarea></td>
</tr><tr>
<td></td>
<td><input type="submit" name="addcontact" value="Add/Change Contact"/>
<input type="submit" name="cancel" value="Cancel"/></td>
</tr>
</table>

EOT;

  $contacts = $client->getcontacts();
  $cnt = count($contacts);
  if ($cnt > 0) {
    $body .= '<br/><form method="post" action="./" autocomplete="off">
<input type="hidden" name="cmd" value="contact"/>
<input type="hidden" name="chkcnt" value="' . $cnt . '"/>
<table border="1">
<tr>
<th>Nickname</th>
<th>Name</th>
<th>Display</th>
<th>ID</th>
<th>Notes</th>
<th>x</th>
</tr>';
    $idx = 0;
    foreach ($contacts as $contact) {
      $id = hsc($contact[$t->ID]);
      $name = trim(hsc($contact[$t->NAME]));
      $nickname = trim(hsc($contact[$t->NICKNAME]));
      $display = namestr($nickname, $name, $id);
      if (!$name) $name = '&nbsp;';
      if (!$nickname) $nickname = '&nbsp;';
      
      $note = hsc($contact[$t->NOTE]);
      if (!$note) $note = "&nbsp;";
      else $note = str_replace("\n", "<br/>\n", $note);
      $body .= <<<EOT
<tr>
<td>$nickname</td>
<td>$name</td>
<td>$display</td>
<td>$id</td>
<td>$note</td>
<td>
<input type="hidden" name="id$idx" value="$id"/>
<input type="checkbox" name="chk$idx"/>
</td>
</tr>

EOT;
      $idx++;
    }
    $body .= <<<EOT
</table>
<br/>
<input type="submit" name="deletecontacts" value="Delete checked"/>
</form>
EOT;
  }
}

function draw_assets($scale=false, $precision=false, $assetname=false, $storage=false) {
  global $onload, $body;
  global $error;
  global $client;

  $t = $client->t;

  $onload = "document.forms[0].scale.focus()";

  settitle('Assets');
  setmenu('assets');

  $scale = hsc($scale);
  $precision = hsc($precision);
  $assetname = hsc($assetname);

  $body .= <<<EOT
<span style="color: red;">$error</span><br/>
<form method="post" action="./" autocomplete="off">
<input type="hidden" name="cmd" value="asset"/>
<table>
<tr>
<td><b>Scale:</b></td>
<td><input type="text" name="scale" size="3" value="$scale"/>
</tr><tr>
<td><b>Precision:</b></td>
<td><input type="text" name="precision" size="3" value="$precision"/></td>
</tr><tr>
<td><b>Asset name:</b></td>
<td><input type="text" name="assetname" size="30" value="$assetname"/></td>
</tr><tr>
<td><b>Storage fee (%/year):</b></td>
<td><input type="text" name="storage" size="5" value="$storage"/></td>
</tr><tr>
<td></td>
<td><input type="submit" name="newasset" value="Add Asset"/>
<input type="submit" name="cancel" value="Cancel"/></td>
</tr>
</table>
</form>

EOT;

  $assets = $client->getassets();
  if (count($assets) > 0) {
    $body .= '<form method="post" action="./" autocomplete="off">
<table border="1">
<tr>
<th>Asset name</th>
<th>Scale</th>
<th>Precision</th>
<th>Storage Fee<br/>(%/year)</th>
<th>Owner</th>
<th>Asset ID</th>
</tr>
';
    $incnt = 0;
    foreach ($assets as $asset) {
      $ownerid = $asset[$t->ID];
      $namestr = id_namestr($ownerid, $contact);
      $assetid = $asset[$t->ASSET];
      $scale = $asset[$t->SCALE];
      $precision = $asset[$t->PRECISION];
      $assetname = $asset[$t->ASSETNAME];
      $percent = $asset[$t->PERCENT];
      if ($ownerid == $client->id) {
        $percent = <<<EOT
<input type="hidden" name="assetid$incnt" value="$assetid"/>
<input type="hidden" name="opercent$incnt" value="$percent"/>
<input type="text" name="percent$incnt" value="$percent" size="10" style="text-align: right;"/>
EOT;
        $incnt++;
      }
      if (!$percent) $percent = "&nbsp;";
      $body .= <<<EOT
<tr>
<td>$assetname</td>
<td align="right">$scale</td>
<td align="right">$precision</td>
<td align="right">$percent</td>
<td>$namestr</td>
<td>$assetid</td>
</tr>

EOT;
    }
    $body .= "</table>\n";
    if ($incnt > 0) {
      $body .= '<input type="hidden" name="percentcnt" value="' . $incnt . '"/>
<input type="hidden" name="cmd" value="asset"/>
<br/><input type="submit" name="updatepercent" value="Update Storage Fees"/>
';
    }
    $body .= "</form>\n";
  }
}

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
