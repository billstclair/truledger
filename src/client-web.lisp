; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Trubanc client web server
;;;

(in-package :trubanc-client-web)


(defvar *debugstr*)
(defvar *client*)
(defvar *iphone*)
(defvar *title*)
(defvar *body*)
(defvar *menu*)
(defvar *onload*)
(defvar *error*)
(defvar *bankline*)
(defvar *bankname*)
(defvar *html-output*)

(defparameter *default-menuitems*
  '(('balance "Balance")
    ('contacts "Contacts")
    ('banks "Banks")
    ('assets "Assets")
    ;;('admins "Admin")
    ('logout "Logout")))    

(defun append-debug (x)
  (and *debugstr* (dotcat *debugstr* x)))

(defun hsc (x)
  (hunchentoot:escape-for-html x))

(defun debugmsg (x)
  "Add a string to the debug output.
   Does NOT add a newline."
  (debugmsg *client*))

;; Called from do-trubanc-client in server-web.lisp
;; Returns a string with the contents of the client web page.
(defun web-server ()
  (let ((*client* (make-client "dbs/clientdb"))
        (*iphone* (search "iPhone" (hunchentoot:user-agent)))
        (*title* "Trubanc Client")
        (*error* nil)
        (*bankline* "")
        (*bankname* nil)
        (*debugstr* nil)
        (cmd (hunchentoot:parameter "cmd"))
        (session (hunchentoot:cookie-in "session")))
    
    (when (hunchentoot:cookie-in "debug")
      (setf *debugstr* ""
            (showprocess *client*) 'append-debug))

    (when session
      (setq session (hunchentoot:cookie-value session))
      (handler-case
          (progn (login-with-sessionid *client* session)
                 (setq cmd "balance"))
        (error (c)
          (hunchentoot:set-cookie "session" :value nil)
          (setq *error* (format nil "Session login error: ~a" c)
                cmd "logout"
                session nil))))

    (cond ((id *client*)
           (let ((keephistory (user-preference *client* "keephistory")))
             (unless keephistory (setq keephistory "keep"))
             (setf (keep-history-p *client*) (equal keephistory "keep")))

           (init-bank)

           (unless (bankid *client*)
             (when (and cmd
                        (not (equal cmd "logout"))
                        (not (equal cmd "login"))
                        (not (equal cmd "bank")))
               (setq cmd "banks"))))
          ((and (not (equal cmd "login")) (not (equal cmd "register")))
           (setq cmd nil)))

    (let ((*body* (with-output-to-string (*html-output*)
                    (cond ((not cmd) (draw-login))
                          ((equal cmd "logout") (do-logout))
                          ((equal cmd "login") (do-login))
                          ((equal cmd "contact") (do-contact))
                          ((equal cmd "bank") (do-bank))
                          ((equal cmd "asset") (do-asset))
                          ((equal cmd "admin") (do-admin))
                          ((equal cmd "spend") (do-spend))
                          ((equal cmd "canceloutbox") (do-canceloutbox))
                          ((equal cmd "processinbox") (do-processinbox))
                          ((equal cmd "storagefees") (do-storagefees))
                          ((equal cmd "dohistory") (do-history))
                          ((equal cmd "togglehistory") (do-togglehistory))
                          ((equal cmd "toggleinstructions") (do-toggleinstructions))
            
                          ((equal cmd "register") (draw-register))
                          ((equal cmd "balance") (draw-balance))
                          ((equal cmd "rawbalance") (draw-raw))
                          ((equal cmd "contacts") (draw-contacts))
                          ((equal cmd "banks") (draw-banks))
                          ((equal cmd "assets") (draw-assets))
                          ((equal cmd "admins") (draw-admin))
                          ((equal cmd "coupon") (draw-coupon))
                          ((equal cmd "history") (draw-history))
                          (session (draw-balance))
              
                          (t (draw-login))))))

      ;; Use *title*, *body*, *onload*, and *debugstr* to fill the page template.
      (when *debugstr*
        (setq *debugstr*
              (format nil "<b>=== Debug log ===</b><br/><pre>~a</pre>~%"
                      *debugstr*)))

      (with-output-to-string (*html-output*)
        (write-template)))))

;; Should support a template.lhtml file that users can easily change.
(defun write-template ()
  (unless *title* (setq *title* "A Trubanc Web Client"))
  (unless *bankname* (setq *bankname* "Trubanc"))
  (unless *menu* (setq *menu* ""))

  (who (*html-output*)
    (:html
     (:head
      (:title (str *title*))
      (:meta :name "viewport" :content "width=device-width")
      (:link :rel "apple-touch-icon" :href="../site-icon.ico")
      (:link :rel "shortcut icon" :href "../site-icon.ico"))
     (:body
      :onload *onload*)
      (:p
       (:a :href="../"
           (:img :style "vertical-align: middle;border: 1px white"
                 :src "../trubanc-logo-50x49.gif"
                 :alt "Trubanc" :width "50" :height="49"))
       " "
       (:b (str *bankname*))
       (when *menu*
         (str "&nbsp;&nbsp;") (str *menu*)))
      (write-bankline)
      (write-idcode)
      (str *body*)
      (str *debugstr*))))

(defun write-bankline ()
  (let ((bankid (bankid *client*)))
    (when bankid
      (let ((bank (getbank *client* bankid)))
        (when bank
          (let ((name (bank-name bank))
                (url (bank-url bank)))
            (who (*html-output*)
              (:b "Bank: ") (esc name)
              (:a :href url (str url))
              (:br))))))))

(defun write-idcode ()
  (let ((id (and *client* (id *client*))))
    (when id
      (multiple-value-bind (pubkeysig name) (get-id *client* id)
        (declare (ignore pubkeysig))
        (when name
          (who (*html-output*)
            (:b "Account name: ") (esc name)
            (:br)
            (:b "Your ID: ") (str id)
            (:br)))))))

(defun draw-login (&optional key)
  (let ((page (hunchentoot:parameter "page")))
    (when (equal page "register")
      (return-from draw-login (draw-register key))))

  (setq *menu* nil
        *onload* "document.forms[0].passphrase.focus()")

  (who (*html-output*)
    (:form :method "post" :action "./" :autocomplete "off"
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
                  (str (or *error* "&nbsp;")))))
           (:a :href "./?cmd=register"
               "Register a new account"))))

(defun draw-register (&key key)

  (settitle "Register")
  (setq *menu* ""
        *onload* "document.forms[0].passphrase.focus()")

  (let* ((keysize (or (ignore-errors
                        (parse-integer (hunchentoot:parameter "page")))
                      3072)))
    (flet ((keysize-option (size)
             (let ((size-str (format nil "~d" size)))
               (who (*html-output*)
                 (:option :value size-str :selected (equal keysize size)
                          (str size-str))))))
      (who (*html-output*)
        (:form :method "post" :action "./" :autocomplete "off"
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
                      (str (or *error* "&nbsp;"))))
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

(defun settitle (subtitle)
  (setq *title* (format nil "~a - Trubanc Client" subtitle)))

#||

function menuitem($cmd, $text, $highlight) {
  $res = "<a href=\"./?cmd=$cmd\">";
  if ($cmd == $highlight) $res .= '<b>';
  $res .= $text;
  if ($cmd == $highlight) $res .= '</b>';
  $res .= '</a>';
  return $res;
}

function setmenu($highlight=false, $menuitems=false) {
  global $menu, $default_menuitems;
  global $client;

  if (!$menuitems) $menuitems = $default_menuitems;

  $menu = '';
  if ($highlight && $client->bankid) {
    foreach ($menuitems as $cmd => $text) {
      if ($cmd != 'admins' ||
          ($client->bankid && $client->id == $client->bankid)) {
        if ($menu) $menu .= '&nbsp;&nbsp';
        $menu .= menuitem($cmd, $text, $highlight);
      }
    }
  } else {
    $menu .= menuitem('logout', 'Logout', false);
  }
}

function do_logout() {
  global $session, $client, $bankline, $error;

  if ($session) $client->logout();
  setcookie('session', false);
  $bankline = '';
  $error = '';
  draw_login();
}

// Here from the login page when the user presses one of the buttons
function do_login() {
  global $title, $body, $onload;
  global $keysize, $require_coupon;
  global $error;
  global $client, $ssl;

  $t = $client->t;

  $passphrase = mqpost('passphrase');
  $passphrase2 = mqpost('passphrase2');
  $coupon = mqpost('coupon');
  $name = mqpost('name');
  $keysize = mqpost('keysize');
  $login = mqpost('login');
  $newacct = mqpost('newacct');
  $showkey = mqpost('showkey');

  if ($showkey) {
    $key = $client->getprivkey($passphrase);
    if (!$key) $error = "No key for passphrase";
    draw_login($key);
    return;
  } elseif ($newacct) {
    $login = false;
    $privkey = mqpost('privkey');
    if (!$passphrase) {
      $error = "Passphrase may not be blank";
    } elseif (!$privkey && $passphrase != $passphrase2) {
      $error = "Passphrase didn't match Verification";
    } else {
      if ($privkey) {
        // Support adding a passphrase to a private key without one
        $pk = $ssl->load_private_key($privkey);
        if ($pk) {
          if ($passphrase != $passphrase2) {
            $error = "Passphrase didn't match Verification";
            draw_login();
            return;
          }
          openssl_pkey_export($pk, $privkey, $passphrase);
          openssl_free_key($pk);
        }
      } else $privkey = $keysize;

      if ($coupon) {
        $err = $client->parsecoupon($coupon, $bankid, $url, $coupon_number);
        if ($err) {
          // See if "coupon" is just a URL, meaning the user
          // already has an account at that bank.
          $err2 = $client->verifybank($coupon, $bankid);
          if ($err2) $error = "Invalid coupon: $err";
        } else {
          $error = $client->verifycoupon($coupon, $bankid, $url);
        }
      } elseif ($require_coupon && !privkey) {
        $error = "Bank coupon required for registration";
      }

      if (!$error) {
        $error = $client->newuser($passphrase, $privkey);
        if (!$error) $login = true;
      }
    }
  }

  if ($login) {
    $session = $client->login_new_session($passphrase);
    if (is_string($session)) {
      $error = "Login error: $session";
    } else {
      $session = $session[0];
      if (!setcookie('session', $session)) {
        $error = "You must enable cookies to use this client";
      } else {
        if ($newacct) {
          $error = $client->addbank($coupon, $name, true);
        }
        if (!$error) {
          setbank();
          if ($client->bankid) draw_balance();
          else draw_banks();
          return;
        }
      }
    }
  }

  draw_login();
}

// Here to change banks or add a new bank
function do_bank() {
  global $client;
  global $error;

  $error = false;

  $newbank = mqpost('newbank');
  $selectbank = mqpost('selectbank');

  $bankurl = '';
  $name = '';
  if ($newbank) {
    $bankurl = trim(mqpost('bankurl'));
    $name = mqpost('name');
    $error = $client->addbank($bankurl, $name);
    if (!$error) $client->userpreference('bankid', $client->bankid);
  } elseif ($selectbank) {
    $bankid = mqpost('bank');
    if (!$bankid) $error = "You must choose a bank";
    else $client->userpreference('bankid', $bankid);
    setbank(true);
  }
  if ($error) draw_banks($bankurl, $name);
  else draw_balance();
}

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

function init-bank($reporterror=false) {
  global $banks, $bank;
  global $error;
  global $client;

  $t = $client->t;

  $banks = $client->getbanks();
  $bank = false;
  $bankid = $client->userpreference('bankid');
  if ($bankid) {
    $err = $client->setbank($bankid, false);
    if ($err) {
      $err = "Can't set bank: $err";
      $client->userpreference('bankid', '');
      $bankid = false;
    }
  }
  if (!$bankid) {
    foreach ($banks as $bank) {
      $bankid = $bank[$t->BANKID];
      $err = $client->setbank($bankid);
      if ($err) {
        $err = "Can't set bank: $err";
        $bankid = false;
      }
      else {
        $client->userpreference('bankid', $bankid);
        break;
      }
    } 
    if (!$bankid) {
      $err = "No known banks. Please add one.";
    }
  }

  if ($reporterror) $error = $err;
}

function namestr($nickname, $name, $id) {
 if ($nickname) {
    if ($name) {
      if ($name != $nickname) $namestr = "$nickname ($name)";
      else $namestr = $name;
    } else $namestr = $nickname;
  } elseif ($name) $namestr = "($name)";
  else $namestr = "$id";
  return "$namestr";
}

function contact_namestr($contact) {
  global $client;

  $t = $client->t;

  $nickname = hsc(@$contact[$t->NICKNAME]);
  $name = hsc($contact[$t->NAME]);
  $recipid = hsc($contact[$t->ID]);
  return namestr($nickname, $name, $recipid);
}

function id_namestr($fromid, &$contact, $you=false) {
  global $client;

  if ($fromid == 'coupon') return $fromid;

  if ($you && $fromid == $client->id) return $you;

  $contact = $client->getcontact($fromid);
  if ($contact) {
    $namestr = contact_namestr($contact);
    if ($namestr == $fromid) $namestr = "[unknown]";
    $namestr = "<span title=\"$fromid\">$namestr</span>";
  } else $namestr = hsc($fromid);
  return $namestr;
}

// Return the ready-for-html-output formatted date for a timestamp
function datestr($time) {
  global $timestamp;

  $unixtime = $timestamp->stripfract($time);
  return hsc(date("j-M-y g:i:sa T", $unixtime));
}

function draw_balance($spend_amount=false, $recipient=false, $note=false,
                      $toacct=false, $tonewacct=false, $nickname=false) {
  global $client;
  global $error;
  global $onload, $body;
  global $iphone, $keephistory;
  global $fraction_asset;
  
  $t = $client->t;

  $bankid = $client->bankid();
  $banks = $client->getbanks();

  settitle('Balance');
  setmenu('balance');

  $saveerror = $error;
  $error = false;

  $bankopts = '';
  foreach ($banks as $bid => $b) {
    if ($bid != $bankid) {
      if ($client->userreq($bid) != -1) {
        $bname = $b[$t->NAME];
        $burl = $b[$t->URL];
        $bankopts .= "<option value=\"$bid\">$bname $burl</option>\n";
      }
    }
  }

  $bankcode = '';
  if ($bankopts) {
    $bankcode .= <<<EOT
<form method="post" action="./" autocomplete="off">
<input type="hidden" name="cmd" value="bank">
<select name="bank">
<option value="">Choose a bank...</option>
$bankopts
</select>
<input type="submit" name="selectbank" value="Change Bank"/>
</form>

EOT;
  }

  $inboxcode = '';
  $balcode = '';
  $assetlist = '';
  $assetidx = 0;
  $acctidx = 0;
  $gotbal = false;
  $contacts = $client->getcontacts();
  $havecontacts = (count($contacts) > 0);

  if (!$error && $client->bankid) {
    // Print inbox, if there is one
    $inbox = $client->getinbox();
    $outbox = $client->getoutbox();
    $accts = $client->getaccts();

    $acctoptions = '';
    if (count($accts) > 1) {
      $first = true;
      foreach ($accts as $acct) {
        $acct = hsc($acct);
        $acctoptions .= <<<EOT
          <option value="$acct">$acct</option>

EOT;
      }
    }

    // Try again, in case we just needed to sync.
    // Maybe this should be hidden by getinbox()
    if (is_string($inbox)) $inbox = $client->getinbox();

    $acctheader = '';
    if (is_string($inbox)) {
      $error = "Error getting inbox: $inbox";
      $inbox = array();
    }
    elseif (count($inbox) == 0) $inboxcode .= "<b>=== Inbox empty ===</b><br/><br/>\n";
    else {
      if ($acctoptions) $acctheader = "\n<th>To Acct</th>";

      $inboxcode .= <<<EOT
<table border="1">
<caption><b>=== Inbox ===</b></caption>
<tr>
<th>Request</th>
<th>From</th>
<th colspan="2">Amount</th>
<th>Note</th>
<th>Action</th>
<th>Reply</th>$acctheader
<th>Time</th>
</tr>

EOT;
      $seloptions = <<<EOT
<option value="accept">Accept</option>
<option value="reject">Reject</option>
<option value="ignore">Ignore</option>

EOT;

      if (is_string($outbox)) {
        $error = "Error getting outbox: $outbox";
        $outbox = array();
      }
      $nonspends = array();
      $spendcnt = 0;
      $assets = $client->getassets();
      foreach ($inbox as $itemkey => $item) {
        $item = $item[0];
        $request = $item[$t->REQUEST];
        $fromid = $item[$t->ID];
        $time = $item[$t->TIME];
        $namestr = id_namestr($fromid, $contact);

        if ($request != $t->SPEND) {
          $msgtime = $item[$t->MSGTIME];
          $outitem = $outbox[$msgtime];
          // outbox entries are array($spend, $tranfee)
          if ($outitem) $outitem = $outitem[0];
          if ($outitem) {
            $item[$t->ASSETNAME] = $outitem[$t->ASSETNAME];
            $item[$t->FORMATTEDAMOUNT] = $outitem[$t->FORMATTEDAMOUNT];
            $item['reply'] = $item[$t->NOTE];
            $item[$t->NOTE] = $outitem[$t->NOTE];
          }
          $nonspends[] = $item;
        }
        else {
          $assetid = $item[$t->ASSET];
          $assetname = hsc($item[$t->ASSETNAME]);
          if (!@$assets[$assetid]) {
            $assetname .= ' <span style="color: red;"><i>(new)</i></span>';
          }
          $amount = hsc($item[$t->FORMATTEDAMOUNT]);
          $itemnote = hsc($item[$t->NOTE]);
          if (!$itemnote) $itemnote = '&nbsp;';
          else $itemnote = str_replace("\n", "<br/>\n", $itemnote);
          $selname = "spend$spendcnt";
          $notename = "spendnote$spendcnt";
          $acctselname = "acct$spendcnt";
          if (!@$contact[$t->CONTACT]) {
            $namestr .= <<<EOT
<br/>
<input type="hidden" name="spendid$spendcnt" value="$fromid"/>
Nickname:
<input type="text" name="spendnick$spendcnt" size="10"/>
EOT;
          }
          $timecode = <<<EOT
<input type="hidden" name="spendtime$spendcnt" value="$time">
EOT;

          $spendcnt++;
          $selcode = <<<EOT
<select name="$selname">
$seloptions
</select>

EOT;
          $acctcode = '';
          if ($acctoptions) {
            $acctcode = <<<EOT
<td><select name="$acctselname">
$acctoptions
</select></td>
EOT;
          }
          $date = datestr($time);
          $inboxcode .= <<<EOT
$timecode
<tr>
<td>Spend</td>
<td>$namestr</td>
<td align="right" style="border-right-width: 0;">$amount</td>
<td style="border-left-width: 0;">$assetname</td>
<td>$itemnote</td>
<td>$selcode</td>
<td><textarea name="$notename" cols="20" rows="2"></textarea></td>
$acctcode
<td>$date</td>
</tr>

EOT;
        }
      }
      $nonspendcnt = 0;
      foreach ($nonspends as $item) {
        $request = $item[$t->REQUEST];
        $fromid = $item[$t->ID];
        $reqstr = ($request == $t->SPENDACCEPT) ? "Accept" : "Reject";
        $time = $item[$t->TIME];
        $namestr = id_namestr($fromid, $contact);
        $assetname = hsc($item[$t->ASSETNAME]);
        $amount = hsc($item[$t->FORMATTEDAMOUNT]);
        $itemnote = hsc($item[$t->NOTE]);
        if (!$itemnote) $itemnote = '&nbsp;';
        else $itemnote = $itemnote = str_replace("\n", "<br/>\n", $itemnote);
        $reply = hsc($item['reply']);
        if (!$reply) $reply = '&nbsp;';
        else $reply = str_replace("\n", "<br/>\n", $reply);
        $selname = "nonspend$nonspendcnt";
        if (!@$contact[$t->CONTACT]) {
          $namestr .= <<<EOT
<br/>
<input type="hidden" name="nonspendid$nonspendcnt" value="$fromid"/>
Nickname:
<input type="text" name="nonspendnick$nonspendcnt" size="10"/>
EOT;
          }
        $timecode = <<<EOT
<input type="hidden" name="nonspendtime$nonspendcnt" value="$time">
EOT;
        $nonspendcnt++;
        $selcode = <<<EOT
<input type="checkbox" name="$selname" checked="checked">Remove</input>

EOT;
        $date = datestr($time);
        $acctcode = '';
        if ($acctoptions) $acctcode = "\n<td>&nbsp;</td>";
        $inboxcode .= <<<EOT
$timecode
<tr>
<td>$reqstr</td>
<td>$namestr</td>
<td align="right" style="border-right-width: 0;">$amount</td>
<td style="border-left-width: 0;">$assetname</td>
<td>$itemnote</td>
<td>$selcode</td>
<td>$reply</td>$acctcode
<td>$date</td>
</tr>

EOT;
      }
      
      $inboxcode = <<<EOT
<form method="post" action="./" autocomplete="off">
<input type="hidden" name="cmd" value="processinbox"/>
<input type="hidden" name="spendcnt" value="$spendcnt"/>
<input type="hidden" name="nonspendcnt" value="$nonspendcnt"/>
$inboxcode
</table>
<br/>
<input type="submit" name="submit" value="Process Inbox"/>
</form>

EOT;
    }

    // Index the spends in the inbox by MSGTIME
    $inboxspends = array();
    foreach ($inbox as $items) {
      $item = $items[0];
      $request = $item[$t->REQUEST];
      $inboxspends[$item[$t->MSGTIME]] = $item;
    }

    // Prepare outbox display
    $cancelcount = 0;
    $outboxcode = '';
    foreach ($outbox as $time => $items) {
      $timestr = hsc($time);
      $date = datestr($time);
      foreach ($items as $item) {
        $request = $item[$t->REQUEST];
        if ($request == $t->SPEND) {
          $recip = $item[$t->ID];
          if (!$outboxcode) $outboxcode = <<<EOT
<table border="1">
<caption><b>=== Outbox ===</b></caption>
<tr>
<th>Time</th>
<th>Recipient</th>
<th colspan="2">Amount</th>
<th>Note</th>
<th>Action</th>
</tr>
EOT;
          $assetname = hsc($item[$t->ASSETNAME]);
          $amount = hsc($item[$t->FORMATTEDAMOUNT]);
          $not = hsc($item[$t->NOTE]);
          if (!$not) $not = '&nbsp;';
          $label = 'Cancel';
          if ($recip == $t->COUPON) {
            $label = 'Redeem';
            $recip = hsc($recip);
            $timearg = urlencode($time);
            $namestr = <<<EOT
<a href="./?cmd=coupon&time=$timearg">$recip</a>
EOT;
          } else {
            $namestr = id_namestr($recip, $contact);
          }
          $cancelcode = '&nbsp;';
          if (!@$inboxspends[$time]) {
            $cancelcode = <<<EOT
<input type="hidden" name="canceltime$cancelcount" value="$timestr"/>
<input type="submit" name="cancel$cancelcount" value="$label"/>

EOT;
            $cancelcount++;
          }
          $outboxcode .= <<<EOT
<tr>
<td>$date</td>
<td>$namestr</td>
<td align="right" style="border-right-width: 0;">$amount</td>
<td style="border-left-width: 0;">$assetname</td>
<td>$not</td>
<td>$cancelcode</td>
</tr>
EOT;
        }
      }
    }
    if ($outboxcode) {
      $outboxcode .= "</table>\n";
      if ($cancelcount > 0) {
        $outboxcode = <<<EOT
<form method="post" action="./" autocomplete="off">
<input type="hidden" name="cmd" value="canceloutbox"/>
<input type="hidden" name="cancelcount" value="$cancelcount"/>
$outboxcode
</form>

EOT;
      }
    }

    $balance = $client->getbalance();
    if (is_string($balance)) $error = $balance;
    elseif (count($balance) > 0) {
      $balcode = "<table border=\"1\">\n<caption><b>=== Balances ===</b></caption>
<tr><td><table>";
      $firstacct = true;
      foreach ($balance as $acct => $assets) {
        $acct = hsc($acct);
        $assetcode = '';
        $newassetlist = '';
        foreach ($assets as $asset => $data) {
          if ($data[$t->AMOUNT] != 0) {
            $gotbal = true;
            $assetid = hsc($data[$t->ASSET]);
            $assetname = hsc($data[$t->ASSETNAME]);
            $formattedamount = hsc($data[$t->FORMATTEDAMOUNT]);
            $submitcode = '';
            $newassetlist .= <<<EOT
<input type="hidden" name="assetid$acctidx|$assetidx" value="$assetid"/>

EOT;
            $submitcode = <<<EOT
<input type="submit" name="spentasset$acctidx|$assetidx" value="Spend"/>

EOT;
            $assetidx++;
            $assetcode .= <<<EOT
<tr>
<td align="right"><span style="margin-right: 5px">$formattedamount</span></td>
<td>$assetname</td>
<td>$submitcode</td>
</tr>

EOT;
          }
        }

        if ($assetcode) {
          if (!$firstacct) {
            $balcode .= "<tr><td colspan=\"3\">&nbsp;</td></tr>\n";
          } else $firstacct = false;
          $balcode .= "<tr><th colspan=\"3\">- $acct -</th></tr>\n$assetcode";
        }

        if ($newassetlist) {
          $assetlist .= <<<EOT
<input type="hidden" name="acct$acctidx" value="$acct"/>
$newassetlist
EOT;
          $acctidx++;
        }
      }
      $balcode .= "</table>\n</td></tr></table>\n";
      $enabled = ($keephistory == 'keep' ? 'enabled' : 'disabled');
      if ($fraction_asset && $_COOKIE['debug']) {
        $fraction = $client->getfraction($fraction_asset);
        $amt = $fraction[$t->AMOUNT];
        $scale = $fraction[$t->SCALE];
        $balcode .= "Fractional balance: $amt";
        if ($scale) $balcode .= " x 10<sup>-$scale</sup>";
        $balcode .= "<br/>\n";
      }
      $balcode .= <<<EOT
<br/>
<a href="./?cmd=rawbalance">Show raw balance</a>
<br/>
<a href="./?cmd=history">Show history</a> ($enabled)
<br/>

EOT;
    }

    $openspend = '';
    $spendcode = '';
    $closespend = '';
    $instructions = '';
    $storagefeecode = '';
    if ($gotbal) {
      $recipopts = '<select name="recipient">
<option value="">Choose contact...</option>
';
      $found = false;
      foreach ($contacts as $contact) {
        $namestr = contact_namestr($contact);
        $recipid = $contact[$t->ID];
        if ($recipid != $client->id) {
          $selected = '';
          if ($recipid == $recipient) {
            $selected = ' selected="selected"';
            $found = true;
          }
          $recipopts .= <<<EOT
<option value="$recipid"$selected>$namestr</option>

EOT;
        }
      }
      $recipopts .= "</select>\n";
      $selectmint = '';
      if ($recipient == $t->COUPON) $selectmint = ' checked="checked"';
      $recipientid = '';
      if (!$found && $recipient != $t->COUPON) $recipientid = $recipient;
      $openspend = '<form method="post" action="./" autocomplete="off">
<input type="hidden" name="cmd" value="spend"/>

';
      $acctoptions = '';
      if (count($accts) > 1) {
        $first = true;
        foreach ($accts as $acct) {
          $selcode = '';
          if ($acct == $toacct) $selcode = ' selected="selected"';
          $acct = hsc($acct);
          $acctoptions .= <<<EOT
<option value="$acct"$selcode>$acct</option>

EOT;
        }
      }
      $acctcode = '';
      if ($acctoptions) {
        $acctcode = <<<EOT

<td><select name="toacct">
<option value="">Select or fill-in below...</option>
$acctoptions
</select></td>
</tr>
<tr>
<td><b>&nbsp;</b></td>

EOT;
      }

      $storagefeecode = '';
      $storagefees = $client->getstoragefee();
      if (is_string($storagefees)) {
        $error = $storagefees;
        $storagefees = array();
      }
      $storagefeecode = '';
      if (count($storagefees) > 0) {
        $storagefeecode = <<<EOT
<form method="post" action="./" autocomplete="off">
<input type="hidden" name="cmd" value="storagefees"/>
<table border="1">
<caption><b>=== Storage Fees ===</b></caption>
<tr><td><table>

EOT;
        foreach ($storagefees as $assetid => $storagefee) {
          $formattedamount = $storagefee[$t->FORMATTEDAMOUNT];
          $assetname = $storagefee[$t->ASSETNAME];
          $time = $storagefee[$t->TIME];
          $timestr = hsc($time);
          $date = datestr($time);
          $storagefeecode .= <<<EOT
<tr>
<td align="right"><span style="margin-right: 5px">$formattedamount</span></td>
<td><span style="margin-right: 5px">$assetname</span></td>
<td>$date</td>
</tr>

EOT;
        }
        $storagefeecode .= <<<EOT
</table></td></tr>
</table>
<input type="submit" name="accept" value="Move to Inbox"/>
</form>

EOT;
      }

      $disablemint = '';
      if ($client->id == $client->bankid) {
        $selectmint = '';
        $disablemint = ' disabled="disabled"';
      }
      $spendcode = <<<EOT

<table>
<tr>
<td><b>Spend amount:</b></td>
<td><input type="text" name="amount" size="20" value="$spend_amount" style="text-align: right;"/>
</tr><tr>
<td><b>Recipient:</b></td>
<td>$recipopts
<input type="checkbox" name="mintcoupon"$selectmint$disablemint>Mint coupon</input></td>
</tr><tr>
<td><b>Note:</b></td>
<td><textarea name="note" cols="40" rows="10">$note</textarea></td>
</tr><tr>
<td><b>Recipient ID:</b></td>
<td><input type="text" name="recipientid" size="40" value="$recipientid"/>
<input type="checkbox" name="allowunregistered">Allow unregistered</input></td>
</tr><tr>
<td><b>Nickname:</b></td>
<td><input type="text" name="nickname" size="30" value="$nickname"/></td>
<tr>
<td><b>Transfer to:</b></td>$acctcode
<td><input type="text" name="tonewacct" size="30" value="$tonewacct"/></td>
</tr>
</table>
EOT;
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
  $fullspend = <<<EOT
$openspend
<table>
<tr>
<td valign="top">
$assetlist$balcode
</td>

EOT;
  if ($iphone) $fullspend .= "</tr>\n<tr>\n";
  $fullspend .= <<<EOT
<td valign="top">
$spendcode
</td>
</table>
$closespend
EOT;
  $body = "$error<br/>$bankcode$inboxcode$fullspend$outboxcode$storagefeecode$instructions";
}

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

function draw_banks($bankurl='', $name='') {
  global $onload, $body;
  global $error;
  global $client;

  $t = $client->t;

  $banks = $client->getbanks();

  $onload = "document.forms[0].bankurl.focus()";
  settitle('Banks');
  setmenu('banks');

  $body .= <<<EOT
<span style="color: red;">$error</span><br/>
<form method="post" action="./" autocomplete="off">
<input type="hidden" name="cmd" value="bank"/>
<table>
<tr>
<td><b>Bank URL<br/>or Coupon:</b></td>
<td><input type="text" name="bankurl" size="64" value="$bankurl"/>
</tr><tr>
<td><b>Account Name<br/>(optional):</b></td>
<td><input type="text" name="name" size="40" value="$name"/></td>
</tr><tr>
<td></td>
<td><input type="submit" name="newbank" value="Add Bank"/>
<input type="submit" name="cancel" value="Cancel"/></td>
</tr>
</table>
</form>

EOT;

  if (count($banks) > 0) {
    $body .= '<table border="1">
<tr>
<th>Bank</th>
<th>URL</th>
<th>ID</th>
<th>Choose</th>
</tr>
';
    foreach ($banks as $bid => $b) {
      if ($client->userreq($bid) != -1) {
        $name = hsc($b[$t->NAME]);
        if (!$name) $name = "unnamed";
        $url = hsc($b[$t->URL]);
        $body .= <<<EOT
<form method="post" action="./" autocomplete="off">
<input type="hidden" name="cmd" value="bank"/>
<input type="hidden" name="bank" value="$bid"/>
<tr>
<td>$name</td>
<td><a href="$url">$url</a></td>
<td>$bid</td>
<td><input type="submit" name="selectbank" value="Choose"/></td>
</tr>
</form>

EOT;
      }
    }
    $body .= "</table>\n";
  }
  
}

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
