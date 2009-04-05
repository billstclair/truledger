; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Trubanc server
;;;

(in-package :trubanc)

(defclass server ()
  ((db :type db
       :initarg :db
       :accessor db)
   (parser :type parser
           :initarg :parser
           :accessor parser)
   (timestamp :type timestamp
              :initform (make-instance 'timestamp)
              :accessor timestamp)
   (pubkeydb :type db
             :initarg :pubkeydb
             :accessor pubkeydb)
   (bankname :type string
             :initarg :bankname
             :initform "A Random Trubanc"
             :accessor bankname)
   (bankurl :type (or string null)
            :initarg :bankurl
            :initform nil
            :accessor bankurl)
   (regfee :type integer
           :initarg :regfee
           :initform 10
           :accessor regfee)
   (tranfee :type integer
            :initarg :tranfee
            :initform 1
            :accessor tranfee)
   (privkey :type string
            :accessor privkey)
   (bankid :type string
           :accessor bankid)
   (always-verify-sigs-p :type boolean
                         :initarg :always-verify-sigs-p
                         :initform t
                         :accessor always-verify-sigs-p)
   (debugmsgs :type list                ;of strings
              :initform nil
              :accessor debugmsgs)))

(defparameter $UNPACK-REQS-KEY "unpack_reqs")

(defmethod initialize-intance ((server server) &key db passphrase)
  (let ((pubkeydb (db-subdir db $PUBKEY)))
    (setf (pubkeydb server) pubkeydb
          (parser server)
          (make-instance 'parser
                         :keydb pubkeydb
                         :bank-getter (lambda () (bankid server))
                         :always-verify-sigs-p (always-verify-sigs-p server)))
    (setup-db server passphrase)))

(defmethod gettime ((server server))
  (let* ((db (db server)))
    (with-db-lock (db $TIME)
      (let ((res (next (timestamp server) (db-get db $TIME))))
        (db-put db $TIME res)
        res))))

(defun account-dir (id)
  (strcat $ACCOUNT "/" id  "/"))

(defun acct-last-key (id)
  (strcat (account-dir id) $LAST))

(defmethod get-acct-last ((server server) id)
  (db-get (db server) (acct-last-key id)))

(defun acct-req-key (id)
  (strcat (account-dir id) $REQ))

(defmethod get-acct-req ((server server) id)
  (db-get (db server) (acct-req-key id)))

(defun acct-time-key (id)
  (strcat (account-dir id) $TIME))

(defun balance-key (id)
  (strcat (account-dir id) $BALANCE))

(defun acct-balance-key (id &optional (acct $MAIN))
  (strcat (balance-key id) "/" acct))

(defun asset-balance-key (id asset &optional (acct $MAIN))
  (strcat (acct-balance-key id acct) "/" asset))

(defun fraction-balance-key (id asset)
  (strcat (account-dir id) $FRACTION "/" asset))

(defmethod asset-balance ((server server) id asset &optional (acct "main"))
  (let* ((key (asset-balance-key id asset acct))
         (msg (db-get (db server) key)))
    (if (not msg)
        "0"
        (unpack-bankmsg server msg $ATBALANCE $BALANCE $AMOUNT))))

(defun outbox-key (id)
  (strcat (account-dir id) $OUTBOX))

(defun outbox-hash-key (id)
  (strcat (account-dir id) $OUTBOXHASH))

(defun inbox-key (id)
  (strcat (account-dir id) $INBOX))


(defmethod storage-info ((server server) id assetid)
  "Get the values necessary to compute the storage fee.
   Inputs:
    ID - the user ID
    ASSETID - the asset ID
   Return values:
    1) storage fee percent
    2) the ID of the asset issuer
    3) the fraction balance for ID/ASSETID
    4) the time of the fraction"
  (let* ((parser (parser server))
         (db (db server))
         (msg (db-get db (strcat $ASSET "/" assetid))))
    (unless msg
      (error "Uknown asset: ~%" assetid))
    (let* ((reqs (parse parser msg))
           (req (elt reqs 1)))
      (unless req (return-from storage-info nil))

      (let ((args (match-pattern parser req)))
        (unless (equal (gethash $kREQUEST args) $ATSTORAGE)
          (return-from storage-info nil))
        (setq req (gethash $kMSG args)
              args (match-pattern parser req))
        (unless (equal (gethash $kREQUEST args) $STORAGE)
          (return-from storage-info nil))
        (let ((issuer (gethash $kCUSTOMER args))
              (percent (gethash $kPERCENT args)))
          (let* ((fraction nil)
                 (fractime nil)
                 (key (fraction-balance-key id assetid))
                 (msg (db-get db key)))
            (when msg
              (setq args (unpack-bankmsg server msg $ATFRACTION $FRACTION)
                    fraction (gethash $kAMOUNT args)
                    fractime (gethash $kTIME args)))
            (values percent issuer fraction fractime)))))))

(defun storage-fee-key (id &optional assetid)
  (let ((res (strcat (account-dir id) $STORAGEFEE)))
    (if assetid
        (strcat res "/" assetid)
        res)))

(defun outbox-dir (id)
  (strcat (account-dir id) $OUTBOX))

(defmethod outbox-hash ((server server) id &optional newitem removed-items)
  (let ((db (db server)))
    (dirhash db (outbox-key id)
             (lambda (msg) (unpack-bankmsg server msg))
             newitem
             removed-items)))

(defmethod outbox-hash-msg ((server server) id)
  (multiple-value-bind (hash count) (outbox-hash server id)
    (bankmsg server
             $kOUTBOXHASH
             (bankid server)
             (get-acct-last server id)
             count
             hash)))

(defun balance-hash-key (id)
  (strcat (account-dir id) $BALANCEHASH))

(defmethod is-asset-p ((server server) assetid)
  (db-get (db server) (strcat $ASSET "/" assetid)))

#||
;; Continue here
  function lookup_asset($assetid) {
    $t = $this->t;
    $u = $this->u;

    $asset = $this->is_asset($assetid);
    if (!$asset) return false;
    $res = $this->unpack_bankmsg($asset, $t->ATASSET, $t->ASSET);
    if (is_string($res)) return $res;
    $req1 = @$res[$this->unpack_reqs_key][1];
    if ($req1) {
      $args = $u->match_pattern($req1);
      if (is_string($args)) return "While matching asset storage fee: $args";
      $res[$t->PERCENT] = @$args[$t->PERCENT];
    }
    return $res;
  }

  function lookup_asset_name($assetid) {
    $assetreq = $this->lookup_asset($assetid);
    return $assetreq[$this->t->ASSETNAME];
  }

  function is_alphanumeric($char) {
    $ord = ord($char);
    return ($ord >= ord('0') && $ord <= ord('9')) ||
      ($ord >= ord('A') && $ord <= ord('Z')) ||
      ($ord >= ord('a') && $ord <= ord('z'));
  }

  function is_acct_name($acct) {
    for ($i=0; $i<strlen($acct); $i++) {
      if (!$this->is_alphanumeric(substr($acct, $i, 1))) return false;
    }
    return true;
  }

  // Initialize the database, if it needs initializing
  function setupDB($passphrase) {
    $db = $this->db;
    $ssl = $this->ssl;
    $t = $this->t;
    $u = $this->u;

    $bankname = $this->bankname;
    if (!$db->get($t->PRIVKEY)) {
      // http://www.rsa.com/rsalabs/node.asp?id=2004 recommends that 3072-bit
      // RSA keys are equivalent to 128-bit symmetric keys, and they should be
      // secure past 2031.
      $privkey = $ssl->make_privkey(3072, $passphrase);
      $db->put($t->PRIVKEY, $privkey);
      $privkey = $ssl->load_private_key($privkey, $passphrase);
      $this->privkey = $privkey;
      $pubkey = $ssl->privkey_to_pubkey($privkey);
      $bankid = $ssl->pubkey_id($pubkey);
      $this->bankid = $bankid;
      $db->put($t->TIME, 0);
      $db->put($t->BANKID, $this->bankmsg($t->BANKID, $bankid));
      $regmsg = $this->bankmsg($t->REGISTER, $bankid, "\n$pubkey", $bankname);
      $regmsg = $this->bankmsg($t->ATREGISTER, $regmsg);
      $db->put($t->PUBKEY . "/$bankid", $pubkey);
      $db->put($t->PUBKEYSIG . "/$bankid", $regmsg);
      $token_name = "Usage Tokens";
      if ($this->bankname) $token_name = "$bankname $token_name";
      $tokenid = $u->assetid($bankid, 0, 0, $token_name);
      $this->tokenid = $tokenid;
      $db->put($t->TOKENID, $this->bankmsg($t->TOKENID, $tokenid));
      $asset = $this->bankmsg($t->ASSET, $bankid, $tokenid, 0, 0, $token_name);
      $db->put($t->ASSET . "/$tokenid", $this->bankmsg($t->ATASSET, $asset));
      $this->regfee = 10;
      $db->put($t->REGFEE, $this->bankmsg($t->REGFEE, $bankid, 0, $tokenid, $this->regfee));
      $this->tranfee = 2;
      $db->put($t->TRANFEE, $this->bankmsg($t->TRANFEE, $bankid, 0, $tokenid, $this->tranfee));
      $accountdir = $t->ACCOUNT . "/$bankid";
      $db->put($this->accttimekey($bankid), 0);
      $db->put($this->acctlastkey($bankid), 0);
      $db->put($this->acctreqkey($bankid), 0);
      $mainkey = $this->acctbalancekey($bankid);
      // $t->BALANCE => array($t->BANKID,$t->TIME, $t->ASSET, $t->AMOUNT, $t->ACCT=>1),
      $msg = $this->bankmsg($t->BALANCE, $bankid, 0, $tokenid, -1);
      $msg = $this->bankmsg($t->ATBALANCE, $msg);
      $db->put("$mainkey/$tokenid", $msg);
    } else {
      $privkey = $ssl->load_private_key($db->get($t->PRIVKEY), $passphrase);
      $this->privkey = $privkey;
      $this->bankid = $this->unpack_bank_param($db, $t->BANKID);
      $this->tokenid = $this->unpack_bank_param($db, $t->TOKENID);
      $this->regfee = $this->unpack_bank_param($db, $t->REGFEE, $t->AMOUNT);
      $this->tranfee = $this->unpack_bank_param($db, $t->TRANFEE, $t->AMOUNT);
    }
  }

  // Unpack wrapped initialization parameter
  function unpack_bank_param($db, $type, $key=false) {
    if (!$key) $key = $type;
    return $this->unpack_bankmsg($db->get($type), $type, false, $key, true);
  }

  // Bank sign a message
  function banksign($msg) {
    $sig = $this->ssl->sign($msg, $this->privkey);
    return "$msg:\n$sig";
  }

  // Make an unsigned message from the args.
  // Takes as many args as you care to pass.
  function makemsg() {
    $t = $this->t;
    $u = $this->u;

    $req = func_get_args();
    $args = $u->match_pattern($req);
    // I don't like this at all, but I don't know what else to do
    if (!$args) return call_user_func_array(array($this, 'failmsg'), $req);
    if (is_string($args)) return $this->failmsg($args);
    $msg = '(';
    $skip = false;
    foreach ($args as $k => $v) {
      if (is_int($k)) {
        if ($skip) $skip = false;
        else {
          if ($msg != '(') $msg .= ',';
          $msg .= $u->escape($v);
        }
      } elseif ($k == $t->MSG) {
        $skip = true;
        $msg .= ",$v";
      }
    }
    $msg .= ')';
    return $msg;
  }

  // Make a bank signed message from the args.
  // Takes as many args as you care to pass
  function bankmsg() {
    $req = func_get_args();
    $req = array_merge(array($this->bankid), $req);
    $msg = call_user_func_array(array($this, 'makemsg'), $req);
    return $this->banksign($msg);
  }

  function shorten_failmsg_msg($msg) {
    if (strlen($msg) > 1024) {
      $msg = substr($msg, 0, 1021) . "...";
    }
    return $msg;
  }

  // Takes as many args as you care to pass
  function failmsg() {
    $args = func_get_args();
    if (count($args) > 0) $args[0] = $this->shorten_failmsg_msg($args[0]);
    $msg = array_merge(array($this->bankid, $this->t->FAILED), $args);
    return $this->banksign($this->u->makemsg($msg));
  }

  function maybedie($msg, $die) {
    if ($die) die("$msg\n");
    return $msg;
  }

  // Reverse the bankmsg() function, optionally picking one field to return
  function unpack_bankmsg($msg, $type=false, $subtype=false, $idx=false, $fatal=false) {
    $bankid = $this->bankid;
    $parser = $this->parser;
    $t = $this->t;
    $u = $this->u;

    $reqs = $parser->parse($msg);
    if (!$reqs) return $this->maybedie($parser->errmsg, $fatal);
    $req = $reqs[0];
    $args = $u->match_pattern($req);
    if (is_string($args)) $this->maybedie("While matching bank-wrapped message: $args", $fatal);
    if ($args[$t->CUSTOMER] != $bankid && $bankid) {
      return $this->maybedie("bankmsg not from bank", $fatal);
    }
    if ($type && $args[$t->REQUEST] != $type) {
      if ($fatal) die("Bankmsg wasn't of type: $type\n");
      return false;
    }
    if (!$subtype) {
      if ($idx) {
        $res = $args[$idx];
        return $this->maybedie($res, $fatal && !$res);
      }
      $args[$this->unpack_reqs_key] = $reqs; // save parse results
      return $args;
    }

    $req = $args[$t->MSG];      // this is already parsed
    if (!$req) return $this->maybedie("No wrapped message", $fatal);
    $args = $u->match_pattern($req);
    if (is_string($args)) return $this->maybedie("While matching wrapped customer message: $args", $fatal);
    if (is_string($subtype) && !$args[$t->REQUEST] == $subtype) {
      if ($fatal) die("Wrapped message wasn't of type: $subtype\n");
      return false;
    }
    if ($idx) {
      $res = $args[$idx];
      return $this->maybedie($res, $fatal && !$res);
    }
    $args[$this->unpack_reqs_key] = $reqs; // save parse results
    return $args;
  }

  function scaninbox($id) {
    $db = $this->db;
    $inboxkey = $this->inboxkey($id);
    $times = $db->contents($inboxkey);
    $res = array();
    foreach ($times as $time) {
      $item = $db->get("$inboxkey/$time");
      if ($item) $res[] = $item;
    }
    return $res;
  }

  function signed_balance($time, $asset, $amount, $acct=false) {
    if ($acct) {
      return $this->bankmsg($this->t->BALANCE, $time, $asset, $amount, $acct);
    } else {
      return $this->bankmsg($this->t->BALANCE, $time, $asset, $amount);
    }
  }

  function signed_spend($time, $id, $assetid, $amount, $note=false, $acct=false) {
    $bankid = $this->bankid;
    if ($note && $acct) {
      return $this->bankmsg($this->t->SPEND, $bankid, $time, $id, $assetid, $amount, $note, $acct);
    } elseif ($note) {
      return $this->bankmsg($this->t->SPEND, $bankid, $time, $id, $assetid, $amount, $note);
    } elseif ($acct) {
      return $this->bankmsg($this->t->SPEND, $bankid, $time, $id, $assetid, $amount, "acct=$acct");
    } else return $this->bankmsg($this->t->SPEND, $bankid, $time, $id, $assetid, $amount);
  }

  function enq_time($id) {
    $db = $this->db;
    $time = $this->gettime();
    $key = $this->accttimekey($id);
    $lock = $db->lock($key);
    $q = $db->get($key);
    if (!$q) $q = $time;
    else $q .= ",$time";
    $db->put($key, $q);
    $db->unlock($lock);
    return $q;
  }

  function deq_time($id, $time) {
    $db = $this->db;
    $key = $this->accttimekey($id);
    $lock = $db->lock($key);
    $q = $db->get($key);
    $res = false;
    if ($q) {
      $times = explode(',', $q);
      foreach ($times as $k => $v) {
        if ($v == $time) {
          $res = $time;
          unset($times[$k]);
          $q = implode(',', $times);
          $db->put($key, $q);
        }
      }
    }
    $db->unlock($lock);
    if (!$res) return "Timestamp not enqueued: $time";
    $unixtime = $this->timestamp->stripfract($time);
    if ($unixtime > ($time + 10*60)) {
      return "Timestamp too old: $time";
    }
    return false;
  }

  function match_bank_signed_message($inmsg) {
    $t = $this->t;
    $u = $this->u;
    $parser = $this->parser;

    $req = $parser->parse($inmsg);
    if (!$req) return $parser->errmsg;
    if ($req) $req = $req[0];
    $args = $u->match_pattern($req);
    if (is_string($args)) return "Failed to match bank-signed message";
    if ($args[$t->CUSTOMER] != $this->bankid) {
      return "Not signed by this bank";
    }
    $msg = $args[$t->MSG];
    $req = $parser->parse($msg);
    if (!$req) return $parser->errmsg;
    if ($req) $req = $req[0];
    return $u->match_pattern($req);
  }

  // Add $amount to the bank balance for $assetid in the main account
  // Any non-false return value is an error string
  function add_to_bank_balance($assetid, $amount) {
    $bankid = $this->bankid;
    $db = $this->db;

    if ($amount == 0) return;
    $key = $this->assetbalancekey($bankid, $assetid);
    $lock = $db->lock($key);
    $res = $this->add_to_bank_balance_internal($key, $assetid, $amount);
    $db->unlock($lock);
    return $res;
  }

  function add_to_bank_balance_internal($key, $assetid, $amount) {
    $bankid = $this->bankid;
    $db = $this->db;
    $t = $this->t;

    $balmsg = $db->get($key);
    $balargs = $this->unpack_bankmsg($balmsg, $t->ATBALANCE, $t->BALANCE);
    if (is_string($balargs) || !$balargs) {
      return "Error unpacking bank balance: '$balargs'";
    } elseif (@$balargs[$t->ACCT] && $balargs[$t->ACCT] != $t->MAIN) {
      return "Bank balance message not for main account";
    } else {
      $bal = $balargs[$t->AMOUNT];
      $newbal = bcadd($bal, $amount);
      $balsign = bccomp($bal, 0);
      $newbalsign = bccomp($newbal, 0);
      if (($balsign >= 0 && $newbalsign < 0) ||
          ($balsign < 0 && $newbalsign >= 0)) {
        return "Transaction would put bank out of balance.";
      } else {
        // $t->BALANCE => array($t->BANKID,$t->TIME, $t->ASSET, $t->AMOUNT, $t->ACCT=>1)
        $msg = $this->bankmsg($t->BALANCE, $bankid, $this->gettime(), $assetid, $newbal);
        $msg = $this->bankmsg($t->ATBALANCE, $msg);
        $db->put($key, $msg);
        $key = $this->acctreqkey($bankid);
        // Make sure clients update the balance
        $db->put($key, bcadd(1, $db->get($key)));
      }
    }
    return false;
  }

  // True return is an error string
  function checkreq($args, $msg) {
    $t = $this->t;
    $db = $this->db;

    $id = $args[$t->CUSTOMER];
    $req = $args[$t->REQ];
    $reqkey = $this->acctreqkey($id);
    $res = false;
    $lock = $db->lock($reqkey);
    $oldreq = $db->get($reqkey);
    if (bccomp($req, $oldreq) <= 0) $res = "New req <= old req";
    else $db->put($reqkey, $req);
    $db->unlock($lock);
    if ($res) $res = $this->failmsg($msg, $res);
    return $res;
  }

  // Deal with an (<id>,balance,...) item from the customer for a
  // spend or processinbox request.
  // $id: the customer id
  // $msg: the signed (<id>,balance,...) message, as a string
  // $args: parser->parse(), then utility->match_pattern() output on $balmsg
  // &$state: an array of input and outputs:
  //   'acctbals' => array(<acct> => array(<asset> => $msg))
  //   'bals => array(<asset> => <amount>)
  //   'tokens' => total new /account/<id>/balance/<acct>/<asset> files
  //   'accts => array(<acct> => true);
  //   'oldneg' => array(<asset> => <acct>), negative balances in current account
  //   'newneg' => array(<asset> => <acct>), negative balances in updated account
  //   'time' => the transaction time
  // Returns an error string on error, or false on no error.
  function handle_balance_msg($id, $msg, $args, &$state, $creating_asset=false) {
    $t = $this->t;
    $u = $this->u;
    $db = $this->db;
    $bankid = $this->bankid;

    $asset = $args[$t->ASSET];
    $amount = $args[$t->AMOUNT];
    $acct = @$args[$t->ACCT];
    if (!$acct) $acct = $t->MAIN;

    $state['accts'][$acct] = true;

    if ((!$creating_asset || $asset != $creating_asset) && !$this->is_asset($asset)) {
      return "Unknown asset id: $asset";
    }
    if (!is_numeric($amount)) return "Not a number: $amount";
    if (!$this->is_acct_name($acct)) {
      return "<acct> may contain only letters and digits: $acct";
    }
    if (@$state['acctbals'][$acct][$asset]) {
      return $this->failmsg($msg, "Duplicate acct/asset balance pair");
    }
    $state['acctbals'][$acct][$asset] = $msg;
    $state['bals'][$asset] = bcsub($state['bals'][$asset], $amount);
    if (bccomp($amount, 0) < 0) {
      if (@$state['newneg'][$asset]) {
        return 'Multiple new negative balances for asset: $asset';
      }
      $state['newneg'][$asset] = $acct;
    }

    $assetbalancekey = $this->assetbalancekey($id, $asset, $acct);
    $acctmsg = $db->get($assetbalancekey);
    if (!$acctmsg) {
      if ($id != $bankid) $state['tokens']++;
      $amount = 0;
      $acctargs = false;
    } else {
      $acctargs = $this->unpack_bankmsg($acctmsg, $t->ATBALANCE, $t->BALANCE);
      if (is_string($acctargs) || !$acctargs ||
          $acctargs[$t->ASSET] != $asset ||
          $acctargs[$t->CUSTOMER] != $id) {
        return "Balance entry corrupted for acct: $acct, asset: " .
          $this->lookup_asset_name($asset) . " - $acctmsg";
      }
      $amount = $acctargs[$t->AMOUNT];
      $state['bals'][$asset] = bcadd($state['bals'][$asset], $amount);
      if (bccomp($amount,  0) < 0) {
        if (@$state['oldneg'][$asset]) {
          return "Account corrupted. Multiple negative balances for asset: $asset";
        }
        $state['oldneg'][$asset] = $acct;
      }
    }

    // Compute storage charges:
    // $state['charges'] =
    //   array($assetid =>
    //         array('percent' => <Storage fee percent>,
    //               'issuer' => <Asset issuer>,
    //               'fraction' => <Fraction balance after fraction storage charge>,
    //               'digits' => <Digits of precision to keep on fraction>
    //               'storagefee' => <Total storage fee for asset>))
    $charges = @$state['charges'];
    if (!$charges) $state['charges'] = $charges = array();
    $assetinfo = @$charges[$asset];
    if (!$assetinfo) {
      $assetinfo = array();
      $tokenid = $this->tokenid;
      if ($asset != $tokenid) {
        $percent = $this->storageinfo($id, $asset, $issuer, $fraction, $fractime);
        if ($percent) {
          $digits = $u->fraction_digits($percent);
          if ($fraction) {
            $time = $state['time'];
            $fracfee = $u->storagefee($fraction, $fractime, $time, $percent, $digits);
          } else $fracfee = 0;
          $assetinfo['percent'] = $percent;
          $assetinfo['issuer'] = $issuer;
          $assetinfo['fraction'] = $fraction;
          $assetinfo['storagefee'] = $fracfee;
          $assetinfo['digits'] = $digits;
        }
      }
    }
    $percent = @$assetinfo['percent'];
    if ($percent && bccomp($amount, 0) > 0) {   // no charges for asset issuer
      $digits = $assetinfo['digits'];
      $accttime = $acctargs[$t->TIME];
      $time = $state['time'];
      $fee = $u->storagefee($amount, $accttime, $time, $percent, $digits);
      $storagefee = bcadd($assetinfo['storagefee'], $fee, $digits);
      $assetinfo['storagefee'] = $storagefee;
    }
    $charges[$asset] = $assetinfo;
    $state['charges'] = $charges;
    return false;
  }

  /*** Debugging ***/
  function setdebugmsgs($debugmsgs) {
    $this->debugmsgs = $debugmsgs;
  }

  function debugmsg($msg) {
    if ($this->debugmsgs) {
      $this->debugstr .= $msg;
    }
  }

  /*** Request processing ***/
 
  // Look up the bank's public key
  function do_bankid($args, $reqs, $inmsg) {
    $t = $this->t;
    $db = $this->db;
    $parser = $this->parser;

    $bankid = $this->bankid;
    $coupon = @$args[$t->COUPON];

    // $t->BANKID => array($t->PUBKEY)
    $msg = $db->get($t->PUBKEYSIG . "/$bankid");
    $args = $this->unpack_bankmsg($msg, $t->ATREGISTER);
    if (is_string($args)) return $this->failmsg($msg, "Bank's pubkey is hosed");
    $req = $args[$t->MSG];
    $res = $parser->get_parsemsg($req);

    if ($coupon) {
      // Validate a coupon number
      $coupon_number_hash = sha1($coupon);
      $key = $t->COUPON . "/$coupon_number_hash";
      $coupon  = $db->get($key);
      if ($coupon) {
        $args = $this->unpack_bankmsg($coupon, $t->ATSPEND, $t->SPEND);
        if (is_string($args)) return $this->failmsg($msg, "Can't parse coupon: $args");
        $assetid = $args[$t->ASSET];
        $amount = $args[$t->AMOUNT];
        $id = $args[$t->CUSTOMER];
        if (!$db->get($this->acctlastkey($id))) {
          // Not already registered, coupon must be for usage tokens,
          // and must be big enough to register with
          if ($assetid != $this->tokenid) {
            return $this->failmsg($msg, "Coupon not for usage tokens");
          } elseif ($amount < ($this->regfee + 10)) {
            return $this->failmsg($msg, "It costs " . $this->regfee . " usage tokens to register a new account.");
          }
        }
        $msg = $this->bankmsg($t->COUPONNUMBERHASH, $coupon_number_hash);
      } else {
        $msg = $this->failmsg($inmsg, "Coupon invalid or already redeemed");
      }
      $res .= ".$msg";
    }

    return $res;
  }

  // Lookup a public key
  function do_id($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;
    // $t->ID => array($t->BANKID,$t->ID)
    $customer = $args[$t->CUSTOMER];
    $id = $args[$t->ID];
    $key = $db->get($t->PUBKEYSIG . "/$id");
    if ($key) return $key;
    else return $this->failmsg($msg, 'No such public key');
  }

  // Register a new account
  function do_register($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;
    $u = $this->u;

    // $t->REGISTER => array($t->BANKID,$t->PUBKEY,$t->NAME=>1)
    $id = $args[$t->CUSTOMER];
    $pubkey = $args[$t->PUBKEY];
    if ($db->get($this->acctlastkey($id))) {
      return $this->failmsg($msg, "Already registered");
    }
    if ($this->ssl->pubkey_id($pubkey) != $id) {
      return $this->failmsg($msg, "Pubkey doesn't match ID");
    }
    if ($this->ssl->pubkey_bits($pubkey) > 4096) {
      return $this->failmsg($msg, "Key sizes larger than 4096 not allowed");
    }

    // Process included coupons
    $reqargslist = array();
    for ($i=1; $i<count($reqs); $i++) {
      $req = $reqs[$i];
      $reqargs = $u->match_pattern($req);
      $reqid = $reqargs[$t->CUSTOMER];
      $request = $reqargs[$t->REQUEST];
      if ($request != $t->COUPONENVELOPE) {
        return $this->failmsg($msg, "Non-coupon request with register: $request");
      }
      $reqargslist[] = $reqargs;
    }
    foreach ($reqargslist as $reqargs) {
      $err = $this->do_couponenvelope_raw($reqargs, $id);
      if ($err) return $this->failmsg($msg, $err);
    }

    $regfee = $this->regfee;
    $tokenid = $this->tokenid;
    $success = false;
    if ($regfee > 0) {
      $inbox = $this->scaninbox($id);
      foreach ($inbox as $inmsg) {
        $inmsg_args = $this->unpack_bankmsg($inmsg, false, true);
        if (is_string($inmsg_args)) {
          return $this->failmsg($msg, "Inbox parsing failed: $inmsg_args");
        }
        if ($inmsg_args && $inmsg_args[$t->REQUEST] == $t->SPEND) {
          // $t->SPEND = array($t->BANKID,$t->TIME,$t->ID,$t->ASSET,$t->AMOUNT,$t->NOTE=>1))
          $asset = $inmsg_args[$t->ASSET];
          $amount = $inmsg_args[$t->AMOUNT];
          if ($asset == $tokenid && $amount >= $regfee) {
            $success = true;
            break;
          }
        }
      }
      if (!$success) {
        return $this->failmsg($msg, "Insufficient usage tokens for registration fee");
      }
    }
    $bankid = $this->bankid;
    $db->put($t->PUBKEY . "/$id", $pubkey);
    $msg = $this->parser->get_parsemsg($reqs[0]);
    $res = $this->bankmsg($t->ATREGISTER, $msg);
    $db->put($t->PUBKEYSIG . "/$id", $res);
    $time = $this->gettime();
    if ($regfee != 0) {
      $spendmsg = $this->signed_spend($time, $id, $tokenid, -$regfee, "Registration fee");
      $spendmsg = $this->bankmsg($t->INBOX, $time, $spendmsg);
      $db->put($this->inboxkey($id) . "/$time", $spendmsg);
    }
    $db->put($this->acctlastkey($id), 1);
    $db->put($this->acctreqkey($id), 0);
    return $res;
  }

  // Process a getreq
  function do_getreq($args, $reqs, $msg) {
    $t = $this->t;
    $id = $args[$t->CUSTOMER];
    return $this->bankmsg($t->REQ,
                          $id,
                          $this->db->get($this->acctreqkey($id)));
  }

  // Process a time request
  function do_gettime($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $err = $this->checkreq($args, $msg);
    if ($err) return $err;

    $id = $args[$t->CUSTOMER];

    $lock = $db->lock($this->accttimekey($id));

    $time = $this->gettime();
    $db->put($this->accttimekey($id), $time);

    $db->unlock($lock);

    return $this->bankmsg($t->TIME, $id, $time);
  }

  function do_getfees($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $err = $this->checkreq($args, $msg);
    if ($err) return $err;

    $regfee = $db->get($t->REGFEE);
    $tranfee = $db->get($t->TRANFEE);
    return "$regfee.$tranfee";
  }

  // Process a spend
  function do_spend($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;
    $parser = $this->parser;

    $parser->verifysigs(false);

    $id = $args[$t->CUSTOMER];
    $lock = $db->lock($this->accttimekey($id));
    $res = $this->do_spend_internal($args, $reqs, $msg,
                                    $ok, $assetid, $issuer, $storagefee, $digits);
    $db->unlock($lock);
    // This is outside the customer lock to avoid deadlock with the issuer account.
    if ($ok && $storagefee) {
      $err = $this->post_storagefee($assetid, $issuer, $storagefee, $digits);
      if ($err) {
        $this->debugmsg("post_storagefee failed: $err\n");
      }
    }

    $parser->verifysigs(true);

    return $res;
  }

  function do_spend_internal($args, $reqs, $msg,
                             &$ok, &$assetid, &$issuer, &$storagefee, &$digits) {
    $t = $this->t;
    $u = $this->u;
    $db = $this->db;
    $bankid = $this->bankid;
    $parser = $this->parser;

    $ok = false;

    // $t->SPEND => array($t->BANKID,$t->TIME,$t->ID,$t->ASSET,$t->AMOUNT,$t->NOTE=>1),
    $id = $args[$t->CUSTOMER];
    $time = $args[$t->TIME];
    $id2 = $args[$t->ID];
    $assetid = $args[$t->ASSET];
    $amount = $args[$t->AMOUNT];
    $note = @$args[$t->NOTE];

    // Burn the transaction, even if balances don't match.
    $err = $this->deq_time($id, $time);
    if ($err) return $this->failmsg($msg, $err);

    if ($id2 == $bankid) {
      return $this->failmsg($msg, "Spends to the bank are not allowed.");
    }

    $asset = $this->lookup_asset($assetid);
    if (!$asset) {
      return $this->failmsg($msg, "Unknown asset id: $assetid");
    }
    if (is_string($asset)) {
      return $this->failmsg($msg, "Bad asset: $asset");
    }
    if (!is_numeric($amount)) {
      return $this->failmsg($msg, "Not a number: $amount");
    }

    // Make sure there are no inbox entries older than the highest
    // timestamp last read from the inbox
    $inbox = $this->scaninbox($id);
    $last = $this->getacctlast($id);
    foreach ($inbox as $inmsg) {
      $inmsg_args = $this->unpack_bankmsg($inmsg);
      if ($last < 0 || bccomp($inmsg_args[$t->TIME], $last) <= 0) {
        return $this->failmsg($msg, "Please process your inbox before doing a spend");
      }
    }

    $tokens = 0;
    $tokenid = $this->tokenid;
    $feemsg = '';
    $storagemsg = '';
    $fracmsg = '';
    if ($id != $id2 && $id != $bankid) {
      // Spends to yourself are free, as are spends from the bank
      $tokens = $this->tranfee;
    }

    $bals = array();
    $bals[$tokenid] = 0;
    if ($id != $id2) {
      // No money changes hands on spends to yourself
      $bals[$assetid] = bcsub(0, $amount);
    }
    $acctbals = array();
    $accts = array();
    $oldneg = array();
    $newneg = array();

    $state = array('acctbals' => $acctbals,
                   'bals' => $bals,
                   'tokens' => $tokens,
                   'accts' => $accts,
                   'oldneg' => $oldneg,
                   'newneg' => $newneg,
                   'time' => $time);
    $outboxhashreq = false;
    $balancehashreq = false;
    for ($i=1; $i<count($reqs); $i++) {
      $req = $reqs[$i];
      $reqargs = $u->match_pattern($req);
      if (is_string($reqargs)) return $this->failmsg($msg, $reqargs); // match error
      $reqid = $reqargs[$t->CUSTOMER];
      $request = $reqargs[$t->REQUEST];
      $reqtime = $reqargs[$t->TIME];
      if ($reqtime != $time) return $this->failmsg($msg, "Timestamp mismatch");
      if ($reqid != $id) return $this->failmsg($msg, "ID mismatch");
      $reqmsg = $parser->get_parsemsg($req);
      if ($request == $t->TRANFEE) { 
       if ($feemsg) {
          return $this->failmsg($msg, $t->TRANFEE . ' appeared multiple times');
        }
        $tranasset = $reqargs[$t->ASSET];
        $tranamt = $reqargs[$t->AMOUNT];
        if ($tranasset != $tokenid || $tranamt != $tokens) {
          return $this->failmsg($msg, "Mismatched tranfee asset or amount ($tranasset <> $tokenid || $tranamt <> $tokens)");
        }
        $feemsg = $this->bankmsg($t->ATTRANFEE, $reqmsg);
      } elseif ($request == $t->STORAGEFEE) {
        if ($storagemsg) {
          return $this->failmsg($msg, $t->STORAGEFEE . ' appeared multiple times');
        }
        $storageasset = $reqargs[$t->ASSET];
        $storageamt = $reqargs[$t->AMOUNT];
        if ($storageasset != $assetid) {
          return $this->failmsg($msg, "Storage fee asset id doesn't match spend");
        }
        $storagemsg = $this->bankmsg($t->ATSTORAGEFEE, $reqmsg);
      } elseif ($request == $t->FRACTION) {
        if ($fracmsg) {
          return $this->failmsg($msg, $t->FRACTION . ' appeared multiple times');
        }
        $fracasset = $reqargs[$t->ASSET];
        $fracamt = $reqargs[$t->AMOUNT];
        if ($fracasset != $assetid) {
          return $this->failmsg($msg, "Fraction asset id doesn't match spend");
        }
        $fracmsg = $this->bankmsg($t->ATFRACTION, $reqmsg);
      } elseif ($request == $t->BALANCE) {
        if ($time != $reqargs[$t->TIME]) {
          return $this->failmsg($msg, "Time mismatch in balance item");
        }
        $errmsg = $this->handle_balance_msg($id, $reqmsg, $reqargs, $state);
        if ($errmsg) return $this->failmsg($msg, $errmsg);
        $newbals[] = $reqmsg;
      } elseif ($request == $t->OUTBOXHASH) {
        if ($outboxhashreq) {
          return $this->failmsg($msg, $t->OUTBOXHASH . " appeared multiple times");
        }
        if ($time != $reqargs[$t->TIME]) {
          return $this->failmsg($msg, "Time mismatch in outboxhash");
        }
        $outboxhashreq = $req;
        $outboxhashmsg = $reqmsg;
        $outboxhash = $reqargs[$t->HASH];
        $outboxhashcnt = $reqargs[$t->COUNT];
      } elseif ($request == $t->BALANCEHASH) {
        if ($balancehashreq) {
          return $this->failmsg($msg, $t->BALANCEHASH . " appeared multiple times");
        }
        if ($time != $reqargs[$t->TIME]) {
          return $this->failmsg($msg, "Time mismatch in balancehash");
        }
        $balancehashreq = $req;
        $balancehash = $reqargs[$t->HASH];
        $balancehashcnt = $reqargs[$t->COUNT];
        $balancehashmsg = $reqmsg;
      } else {
        return $this->failmsg($msg, "$request not valid for spend. Only " .
                              $t->TRANFEE . ', ' . $t->BALANCE . ", and " .
                              $t->OUTBOXHASH);
      }
    }

    $acctbals = $state['acctbals'];
    $bals = $state['bals'];
    $tokens = $state['tokens'];
    $accts = $state['accts'];
    $oldneg = $state['oldneg'];
    $newneg = $state['newneg'];
    $charges = $state['charges'];

    // Work the storage fee into the balances
    $storagefee = false;
    if ($charges) {
      $assetinfo = $charges[$assetid];
      if ($assetinfo) {
        $percent = $assetinfo['storagefee'];
        if ($percent) {
          $issuer = $assetinfo['issuer'];
          $storagefee = $assetinfo['storagefee'];
          $fraction = $assetinfo['fraction'];
          $digits = $assetinfo['digits'];
          $bal = $bals[$assetid];
          $bal = bcsub($bal, $storagefee, $digits);
          $u->normalize_balance($bal, $fraction, $digits);
          $bals[$assetid] = $bal;
          if (bccomp($fraction, $fracamt) != 0) {
            return $this->failmsg($msg, "Fraction amount was: $fractamt, sb: $fraction");
          }
          if (bccomp($storagefee, $storageamt) != 0) {
            return $this->failmsg($msg, "Storage fee was: $storageamt, sb: $storagefee");
          }
        }
      }
    }
    if (!$storagefee && ($storagemsg || $fracmsg)) {
      return $this->failmsg($msg, "Storage or fraction included when no storage fee");
    }

    // tranfee must be included if there's a transaction fee
    if ($tokens != 0 && !$feemsg && $id != $id2) {
      return $this->failmsg($msg, $t->TRANFEE . " missing");
    }

    if (bccomp($amount, 0) < 0) {
      // Negative spend allowed only for switching issuer location
      if (!$oldneg[$assetid]) {
        return $this->failmsg($msg, "Negative spend on asset for which you are not the issuer");
      }
      // Spending out the issuance.
      // Mark the new "acct" for the negative as being the spend itself.
      if (!$newneg[$assetid]) $newneg[$assetid] = $args;
    }

    // Check that we have exactly as many negative balances after the transaction
    // as we had before.
    if (count($oldneg) != count($newneg)) {
      return $this->failmsg($msg, "Negative balance count not conserved");
    }
    foreach ($oldneg as $asset => $acct) {
      if (!$newneg[$asset]) {
        return $this->failmsg($msg, "Negative balance assets not conserved");
      }
    }

    // Charge the transaction and new balance file tokens;
    $bals[$tokenid] = bcsub($bals[$tokenid], $tokens);

    $errmsg = "";
    $first = true;
    // Check that the balances in the spend message, match the current balance,
    // minus amount spent minus fees.
    foreach ($bals as $balasset => $balamount) {
      if (bccomp($balamount, 0) != 0) {
        $name = $this->lookup_asset_name($balasset);
        if (!$first) $errmsg .= ', ';
        $first = false;
        $errmsg .= "$name: $balamount";
      }
    }
    if ($errmsg != '') return $this->failmsg($msg, "Balance discrepancies: $errmsg");

    // Check outboxhash
    // outboxhash must be included, except on self spends
    $spendmsg = $parser->get_parsemsg($reqs[0]);
    if ($id != $id2 && $id != $bankid) {
      if (!$outboxhashreq) {
        return $this->failmsg($msg, $t->OUTBOXHASH . " missing");
      } else {
        $hasharray = $this->outboxhash($id, $spendmsg);
        $hash = $hasharray[$t->HASH];
        $hashcnt = $hasharray[$t->COUNT];
        if ($outboxhash != $hash || $outboxhashcnt != $hashcnt) {
          return $this->failmsg($msg, $t->OUTBOXHASH . ' mismatch');
        }
      }
    }

    // balancehash must be included, except on bank spends
    if ($id != $bankid) {
      if (!$balancehashreq) {
        return $this->failmsg($msg, $t->BALANCEHASH . " missing");
      } else {
        $hasharray = $u->balancehash($db, $id, $this, $acctbals);
        $hash = $hasharray[$t->HASH];
        $hashcnt = $hasharray[$t->COUNT];
        if ($balancehash != $hash || $balancehashcnt != $hashcnt) {
          return $this->failmsg($msg, $t->BALANCEHASH . " mismatch, hash sb: $hash, was: $balancehash, count sb: $hashcnt, was: $balancehashcnt");
        }
      }
    }

    // All's well with the world. Commit this puppy.
    // Eventually, the commit will be done as a second phase.
    $outbox_item = $this->bankmsg($t->ATSPEND, $spendmsg);
    if ($feemsg) {
      $outbox_item .= ".$feemsg";
    }
    $res = $outbox_item;

    $newtime = false;
    if ($id2 != $t->COUPON) {
      if ($id != $id2) {
        $newtime = $this->gettime();
        $inbox_item = $this->bankmsg($t->INBOX, $newtime, $spendmsg);
        if ($feemsg) {
          $inbox_item .= ".$feemsg";
        }
      }
    } else {
      // If it's a coupon request, generate the coupon
      $ssl = $this->ssl;
      $random = $this->random;
      if (!$random) {
        require_once "LoomRandom.php";
        $random = new LoomRandom();
        $this->random = $random;
      }
      $coupon_number = $random->random_id();
      $bankurl = $this->bankurl;
      if ($note) {
        $coupon = $this->bankmsg($t->COUPON, $bankurl, $coupon_number, $assetid, $amount, $note);
      } else {
        $coupon = $this->bankmsg($t->COUPON, $bankurl, $coupon_number, $assetid, $amount);
      }
      $coupon_number_hash = sha1($coupon_number);
      $db->put($t->COUPON . "/$coupon_number_hash", "$outbox_item");
      $pubkey = $this->pubkeydb->get($id);
      $coupon = $ssl->pubkey_encrypt($coupon, $pubkey);
      $coupon = $this->bankmsg($t->COUPONENVELOPE, $id, $coupon);
      $res .= ".$coupon";
      $outbox_item .= ".$coupon";
    }

    // I considered adding the transaction tokens to the bank
    // balances here, but am just leaving them in the outbox,
    // to be credited to this customer, if the spend is accepted,
    // or to the recipient, if he rejects it.
    // This means that auditing has to consider balances, outbox
    // fees, and inbox spend items.

    // Update balances
    $balancekey = $this->balancekey($id);
    foreach ($acctbals as $acct => $balances) {
      $acctdir = "$balancekey/$acct";
      foreach ($balances as $balasset => $balance) {
        $balance = $this->bankmsg($t->ATBALANCE, $balance);
        $res .= ".$balance";
        $db->put("$acctdir/$balasset", $balance);
      }
    }

    if ($fracmsg) {
      $key = $this->fractionbalancekey($id, $assetid);
      $db->put($key, $fracmsg);
      $res .= ".$fracmsg";
    }
    if ($storagemsg) $res .= ".$storagemsg";

    if ($id != $id2 && $id != $bankid) {
      // Update outboxhash
      $outboxhash_item = $this->bankmsg($t->ATOUTBOXHASH, $outboxhashmsg);
      $res .= ".$outboxhash_item";
      $db->put($this->outboxhashkey($id), $outboxhash_item);

      // Append spend to outbox
      $db->put($this->outboxdir($id) . "/$time", $outbox_item);
    }

    if ($id != $bankid) {
      // Update balancehash
      $balancehash_item = $this->bankmsg($t->ATBALANCEHASH, $balancehashmsg);
      $res .= ".$balancehash_item";
      $db->put($this->balancehashkey($id), $balancehash_item);
    }

    // Append spend to recipient's inbox
    if ($newtime) {
      $db->put($this->inboxkey($id2) . "/$newtime", $inbox_item);
    }

    // Force the user to do another getinbox, if anything appears
    // in his inbox since he last processed it.
    $db->put($this->acctlastkey($id), -1);

    // We're done
    $ok = true;
    return $res;
  }

  // Credit storage fee to an asset issuer
  function post_storagefee($assetid, $issuer, $storagefee, $digits) {
    $db = $this->db;

    $lock = $db->lock($this->accttimekey($issuer));
    $res = $this->post_storagefee_internal($assetid, $issuer, $storagefee, $digits);
    $db->unlock($lock);
    return $res;
  }

  function post_storagefee_internal($assetid, $issuer, $storagefee, $digits) {
    $db = $this->db;
    $t = $this->t;
    $u = $this->u;
    $parser = $this->parser;
    $bankid = $this->bankid;

    $key = $this->storagefeekey($issuer, $assetid);
    $storagemsg = $db->get($key);
    if ($storagemsg) {
      $reqs = $parser->parse($storagemsg);
      if (!$reqs) return $parser->errmsg;
      if (count($reqs) != 1) return "Bad storagefee msg: $storagemsg";
      $args = $u->match_pattern($reqs[0]);
      if (is_string($args)) return "While posting storagefee: $args";
      if ($args[$t->CUSTOMER] != $bankid ||
          $args[$t->REQUEST] != $t->STORAGEFEE ||
          $args[$t->BANKID] != $bankid ||
          $args[$t->ASSET] != $assetid) {
        return "Storage fee message malformed";
      }
      $amount = $args[$t->AMOUNT];
      $storagefee = bcadd($storagefee, $amount, $digits);
    }
    $time = $this->gettime();
    $storagemsg = $this->bankmsg($t->STORAGEFEE, $bankid, $time, $assetid, $storagefee);
    $db->put($key, $storagemsg);
  }

  // Process a spend|reject
  function do_spendreject($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;
    $parser = $this->parser;

    $parser->verifysigs(false);

    $id = $args[$t->CUSTOMER];
    $lock = $db->lock($this->accttimekey($id));
    $res = $this->do_spendreject_internal($args, $msg, $id);
    $db->unlock($lock);

    $parser->verifysigs(true);

    return $res;
  }

  function do_spendreject_internal($args, $msg, $id) {
    $t = $this->t;
    $u = $this->u;
    $db = $this->db;
    $bankid = $this->bankid;

    $time = $args[$t->TIME];
    $key = $this->outboxkey($id);
    $item = $db->get("$key/$time");
    if (!$item) return $this->failmsg($msg, "No outbox entry for time: $time");
    $args = $this->unpack_bankmsg($item, $t->ATSPEND, $t->SPEND);
    if (is_string($args)) return $this->failmsg($msg, $args);
    if ($time != $args[$t->TIME]) {
      return $this->failmsg($msg, "Time mismatch in outbox item");
    }
    if ($args[$t->ID] == $t->COUPON) {
      return $this->failmsg($msg, "Coupons must be redeemed, not cancelled");
    }
    $recipient = $args[$t->ID];
    $key = $this->inboxkey($recipient);
    $inbox = $db->contents($key);
    foreach ($inbox as $intime) {
      $item = $db->get("$key/$intime");
      // Unlikely, but possible
      if (!$item) return $this->failmsg($msg, "Spend has already been processed");
      $args = $this->unpack_bankmsg($item, $t->INBOX, $t->SPEND);
      if (is_string($args)) return $this->failmsg($msg, $args);
      if ($args[$t->TIME] == $time) {
        // Calculate the fee, if there is one
        $feeamt = '';
        $reqs = $args[$this->unpack_reqs_key];
        $req = $reqs[1];
        if ($req) {
          $args = $u->match_pattern($req);
          if (is_string($args)) return $this->failmsg($msg, $args);
          if ($args[$t->CUSTOMER] != $bankid) {
            return $this->failmsg($msg, "Fee message not from bank");
          }
          if ($args[$t->REQUEST] == $t->ATTRANFEE) {
            $req = $args[$t->MSG];
            $args = $u->match_pattern($req);
            if (is_string($args)) return $this->failmsg($msg, $args);
            if ($args[$t->REQUEST] != $t->TRANFEE) {
              return $this->failmsg($msg, "Fee wrapper doesn't wrap fee message");
            }
            $feeasset = $args[$t->ASSET];
            $feeamt = $args[$t->AMOUNT];
          }
        }
        // Found the inbox item corresponding to the outbox item
        // Make sure it's still there
        $lock = $db->lock("$key/$intime");
        $item2 = $db->get("$key/$intime");
        $db->put("$key/$intime", '');
        $db->unlock($lock);
        if ($item2 == '') {
          return $this->failmsg($msg, "Spend has already been processed");
        }
        if ($feeamt) {
          $this->add_to_bank_balance($feeasset, $feeamt);
        }
        $newtime = $this->gettime();
        $item = $this->bankmsg($t->INBOX, $newtime, $msg);
        $key = $this->inboxkey($id);
        $db->put("$key/$newtime", $item);
        return $item;
      }
    }
    return $this->failmsg($msg, "Spend has already been processed");
  }


  // Redeem coupon by moving it from coupon/<coupon> to the customer inbox.
  // This isn't the right way to do this.
  // It really wants to be like processinbox, with new balances.
  // You have to do it this way for a new registration, though.
  function do_couponenvelope($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;
    
    $id = $args[$t->CUSTOMER];
    $lock = $db->lock($this->accttimekey($id));
    $res = $this->do_couponenvelope_internal($args, $msg, $id);
    $db->unlock($lock);
    return $res;
  }

  function do_couponenvelope_internal($args, $msg, $id) {
    $t = $this->t;

    $res = $this->do_couponenvelope_raw($args, $id);
    if (is_string($res)) return $this->failmsg($msg, $res);
    return $this->bankmsg($t->ATCOUPONENVELOPE, $msg);
  }

  // Called by do_register to process coupons there
  // Returns an error string or false
  function do_couponenvelope_raw($args, $id) {
    $t = $this->t;
    $db = $this->db;
    $bankid = $this->bankid;
    $parser = $this->parser;
    $u = $this->u;

    $encryptedto = $args[$t->ID];
    if ($encryptedto != $bankid) {
      return "Coupon not encrypted to bank";
    }
    $coupon = $args[$t->ENCRYPTEDCOUPON];
    $coupon = $this->ssl->privkey_decrypt($coupon, $this->privkey);
    if ($u->is_coupon_number($coupon)) {
      $coupon_number = $coupon;
    } else {
      $args = $this->unpack_bankmsg($coupon, $t->COUPON);
      if (is_string($args)) return "Error parsing coupon: $args";
      if ($bankid != $args[$t->CUSTOMER]) {
        return "Coupon not signed by bank";
      }
      $coupon_number = $args[$t->COUPON];
    }

    $coupon_number_hash = sha1($coupon_number);

    $key = $t->COUPON . "/$coupon_number_hash";
    $lock = $db->lock($key);
    $outbox_item = $db->get($key);
    if ($outbox_item) $db->put($key, '');
    $db->unlock($lock);

    if (!$outbox_item) {
      $key = $this->inboxkey($id);
      $inbox = $db->contents($key);
      foreach ($inbox as $time) {
        $item = $db->get("$key/$time");
        if (strstr($item, ',' . $t->COUPON . ',')) {
          $reqs = $parser->parse($item);
          if ($reqs && count($reqs) > 1) {
            $req = $reqs[1];
            $msg = $u->match_pattern($req);
            if (!is_string($msg)) {
              if ($coupon_number_hash == $msg[$t->COUPON]) {
                // Customer already redeemded this coupon.
                // Success if he tries to do it again.
                return false;
              }
            }
          }
        }
      }
      return "Coupon already redeemed";
    }

    $args = $this->unpack_bankmsg($outbox_item, $t->ATSPEND);
    if (is_string($args)) {
      // Make sure the spender can cancel the coupon
      $db->put($key, $outbox_item);
      return "While unpacking coupon spend: $args";
    }
    $reqs = $args[$this->unpack_reqs_key];
    $spendreq = $args[$t->MSG];
    $spendmsg = $parser->get_parsemsg($spendreq);
    $feemsg = '';
    if (count($reqs) > 1) {
      $feereq = $reqs[1];
      $feemsg = $parser->get_parsemsg($feereq);
    }
    $newtime = $this->gettime();
    $inbox_item = $this->bankmsg($t->INBOX, $newtime, $spendmsg);
    $cnhmsg = $this->bankmsg($t->COUPONNUMBERHASH, $coupon_number_hash);
    $inbox_item .= $cnhmsg;
    if ($feemsg) $inbox_item .= ".$feemsg";

    $key = $this->inboxkey($id) . "/$newtime";
    $db->put($key, $inbox_item);
    return false;
  }

  // Query inbox
  function do_getinbox($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $id = $args[$t->CUSTOMER];
    $lock = $db->lock($this->accttimekey($id));
    $res = $this->do_getinbox_internal($args, $msg, $id);
    $db->unlock($lock);
    return $res;
  }

  function do_getinbox_internal($args, $msg, $id) {
    $t = $this->t;
    $u = $this->u;
    $db = $this->db;

    $err = $this->checkreq($args, $msg);
    if ($err) return $err;

    $inbox = $this->scaninbox($id);
    $res = $this->bankmsg($t->ATGETINBOX, $msg);
    $last = 1;
    foreach ($inbox as $inmsg) {
      $res .= '.' . $inmsg;
      $args = $u->match_message($inmsg);
      if ($args && !is_string($args)) {
        if ($args[$t->REQUEST] == $t->INBOX) {
          $time = $args[$t->TIME];
          if (bccomp($time, $last) > 0) $last = $time;
        }
        $args = $u->match_pattern($args[$t->MSG]);
      }
      $err = false;
      if (!$args) $err = 'Inbox parse error';
      elseif (is_string($args)) $err = "Inbox match error: $args";
      elseif ($args[$t->ID] != $id && $args[$t->ID] != $t->COUPON) {
        $err = "Inbox entry for wrong ID: " . $args[$t->ID];
      }
      if ($err) return $this->failmsg($msg, $err);
    }

    $key = $this->storagefeekey($id);
    $assetids = $db->contents($key);
    foreach ($assetids as $assetid) {
      $storagefee = $db->get("$key/$assetid");
      $res .= ".$storagefee";
    }

    // Update last time
    $db->put($this->acctlastkey($id), $last);

    /* Not pre-allocating timestamps any more
     * // Append the timestamps, if there are any inbox entries
     * if (count($inbox) > 0) {
     *   // Avoid bumping the global timestamp if the customer already
     *   // has two timestamps > the highest inbox timestamp.
     *   $time = $args[$t->TIME];
     *   $key = $this->accttimekey($id);
     *   $times = explode(',', $db->get($key));
     *   if (!(count($times) >= 2 &&
     *         bccomp($times[0], $time) > 0 &&
     *         bccomp($times[1], $time) > 0)) {
     *     $times = array($this->gettime(), $this->gettime());
     *     $db->put($key, implode(',', $times));
     *   }
     *   $res .= '.' . $this->bankmsg($t->TIME, $id, $times[0]);
     *   $res .= '.' . $this->bankmsg($t->TIME, $id, $times[1]);
     * }
     */
    return $res;
  }

  function do_processinbox($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;
    $parser = $this->parser;

    $parser->verifysigs(false);

    $id = $args[$t->CUSTOMER];
    $lock = $db->lock($this->accttimekey($id));
    $res = $this->do_processinbox_internal($args, $reqs, $msg, $ok, $charges);
    $db->unlock($lock);
    // This is outside the customer lock to avoid deadlock with the issuer account.
    if ($ok && $charges) {
      foreach ($charges as $assetid => $assetinfo) {
        $issuer = @$assetinfo['issuer'];
        $storagefee = @$assetinfo['storagefee'];
        $digits = @$assetinfo['digits'];
        if ($assetid) {
          $err = $this->post_storagefee($assetid, $issuer, $storagefee, $digits);
          if ($err) {
            $this->debugmsg("post_storagefee failed: $err\n");
          }
        }
      }
    }

    $parser->verifysigs(true);

    return $res;
  }

  function do_processinbox_internal($args, $reqs, $msg, &$ok, &$charges) {
    $t = $this->t;
    $u = $this->u;
    $db = $this->db;
    $bankid = $this->bankid;
    $parser = $this->parser;

    $ok = false;

    // $t->PROCESSINBOX => array($t->BANKID,$t->TIME,$t->TIMELIST),
    $id = $args[$t->CUSTOMER];
    $time = $args[$t->TIME];
    $timelist = $args[$t->TIMELIST];
    $inboxtimes = explode('|', $timelist);

    // Burn the transaction, even if balances don't match.
    $err = $this->deq_time($id, $time);
    if ($err) return $this->failmsg($msg, $err);

    $spends = array();
    $fees = array();
    $accepts = array();
    $rejects = array();
    $storagemsgs = array();
    $fracmsgs = array();

    $inboxkey = $this->inboxkey($id);
    foreach ($inboxtimes as $inboxtime) {
      $item = $db->get("$inboxkey/$inboxtime");
      if (!$item) return $this->failmsg($msg, "Inbox entry not found: $inboxtime");
      $itemargs = $this->unpack_bankmsg($item, $t->INBOX, true);
      if ($itemargs[$t->ID] != $id && $itemargs[$t->ID] != $t->COUPON) {
        return $this->failmsg($msg, "Inbox corrupt. Item found for other customer");
      }
      $request = $itemargs[$t->REQUEST];
      if ($request == $t->SPEND) {
        $itemtime = $itemargs[$t->TIME];
        $spends[$itemtime] = array($inboxtime, $itemargs);
        $itemreqs = $itemargs[$this->unpack_reqs_key];
        $itemcnt = count($itemreqs);
        $feereq = ($itemcnt > 1) ? $itemreqs[$itemcnt-1] : false;
        if ($feereq) {
          $feeargs = $u->match_pattern($feereq);
          if ($feeargs && $feeargs[$t->REQUEST] == $t->ATTRANFEE) {
            $feeargs = $u->match_pattern($feeargs[$t->MSG]);
          }
          if (!$feeargs || $feeargs[$t->REQUEST] != $t->TRANFEE) {
            return $this->failmsg($msg, "Inbox corrupt. Fee not properly encoded");
          }
          $fees[$itemtime] = $feeargs;
        }
      }
      elseif ($request == $t->SPENDACCEPT) $accepts[$inboxtime] = $itemargs;
      elseif ($request == $t->SPENDREJECT) $rejects[$inboxtime] = $itemargs;
      else return $this->failmsg($msg, "Inbox corrupted. Found '$request' item");
    }

    $bals = array();
    $outboxtimes = array();

    // Refund the transaction fees for accepted spends
    foreach ($accepts as $itemargs) {
      $outboxtime = $itemargs[$t->TIME];
      $outboxtimes[] = $outboxtime;
      $spendfeeargs = $this->get_outbox_args($id, $outboxtime);
      if (is_string($spendfeeargs)) {
        return $this->failmsg($msg, $spendfeeargs);
      }
      $feeargs = $spendfeeargs[1];
      if ($feeargs) {
        $asset = $feeargs[$t->ASSET];
        $amt = $feeargs[$t->AMOUNT];
        $bals[$asset] = bcadd(@$bals[$asset], $amt);
      }
    }

    $oldneg = array();
    $newneg = array();
    $tobecharged = array();     // amount/time pairs for accepted spends

    // Credit the spend amounts for rejected spends, but do NOT
    // refund the transaction fees
    foreach ($rejects as $itemargs) {
      $outboxtime = $itemargs[$t->TIME];
      $outboxtimes[] = $outboxtime;
      $spendfeeargs = $this->get_outbox_args($id, $outboxtime);
      if (is_string($spendfeeargs)) {
        return $this->failmsg($msg, $spendfeeargs);
      }
      $spendargs = $spendfeeargs[0];
      $asset = $spendargs[$t->ASSET];
      $amt = $spendargs[$t->AMOUNT];
      $spendtime = $spendargs[$t->TIME];
      if (bccomp($amt, 0) < 0) {
        $oldneg[$asset] = $spendargs;
      }
      $bals[$asset] = bcadd(@$bals[$asset], $amt);
      $tobecharged[] = array($t->AMOUNT => $amt,
                             $t->TIME => $spendtime,
                             $t->ASSET => $asset);
    }

    $inboxmsgs = array();
    $acctbals = array();
    $accts = array();
    $res = $this->bankmsg($t->ATPROCESSINBOX, $parser->get_parsemsg($reqs[0]));
    $tokens = 0;

    $state = array('acctbals' => $acctbals,
                   'bals' => $bals,
                   'tokens' => $tokens,
                   'accts' => $accts,
                   'oldneg' => $oldneg,
                   'newneg' => $newneg,
                   'time' => $time);

    $outboxhashreq = false;
    $balancehashreq = false;
    $fracamts = array();
    $storageamts = array();

    // Go through the rest of the processinbox items, collecting
    // accept and reject instructions and balances.
    for ($i=1; $i<count($reqs); $i++) {
      $req = $reqs[$i];
      $reqmsg = $parser->get_parsemsg($req);
      $args = $u->match_pattern($req);
      if ($args[$t->CUSTOMER] != $id) {
        return $this->failmsg
          ($msg, "Item not from same customer as " . $t->PROCESSINBOX);
      }
      $request = $args[$t->REQUEST];
      if ($request == $t->SPENDACCEPT ||
          $request == $t->SPENDREJECT) {
        // $t->SPENDACCEPT => array($t->BANKID,$t->TIME,$t->id,$t->NOTE=>1),
        // $t->SPENDREJECT => array($t->BANKID,$t->TIME,$t->id,$t->NOTE=>1),
        $itemtime = $args[$t->TIME];
        $otherid = $args[$t->ID];
        $inboxpair = $spends[$itemtime];
        if (!$inboxpair || count($inboxpair) != 2) {
          return $this->failmsg($msg, "'$request' not matched in '" .
                                $t->PROCESSINBOX . "' item, itemtime: $itemtime");
        }
        $itemargs = $inboxpair[1];
        if ($request == $t->SPENDACCEPT) {
          // Accepting the payment. Credit it.
          $itemasset = $itemargs[$t->ASSET];
          $itemamt = $itemargs[$t->AMOUNT];
          $itemtime = $itemargs[$t->TIME];
          if (bccomp($itemamt, 0) < 0 && $itemargs[$t->CUSTOMER] != $bankid) {
            $state['oldneg'][$itemasset] = $itemargs;
          }
          $state['bals'][$itemasset] = bcadd(@$state['bals'][$itemasset], $itemamt);
          $tobecharged[] = array($t->AMOUNT => $itemamt,
                                 $t->TIME => $itemtime,
                                 $t->ASSET => $itemasset);
          $res .= '.' . $this->bankmsg($t->ATSPENDACCEPT, $reqmsg);
        } else {
          // Rejecting the payment. Credit the fee.
          $feeargs = $fees[$itemtime];
          if ($feeargs) {
            $feeasset = $feeargs[$t->ASSET];
            $feeamt = $feeargs[$t->AMOUNT];
            $state['bals'][$feeasset] = bcadd(@$state['bals'][$feeasset], $feeamt);
          }
          $res .= '.' . $this->bankmsg($t->ATSPENDREJECT, $reqmsg);
        }
        if ($otherid == $bankid) {
          if ($request == $t->SPENDREJECT &&
              $itemargs[$t->AMOUNT] < 0) {
            return $this->failmsg($msg, "You may not reject a bank charge");
          }
          $inboxtime = $request;
          $inboxmsg = $itemargs;
        } else {
          $inboxtime = $this->gettime();
          $inboxmsg = $this->bankmsg($t->INBOX, $inboxtime, $reqmsg);
        }
        if ($inboxtime) $inboxmsgs[] = array($otherid, $inboxtime, $inboxmsg);
      } elseif ($request == $t->STORAGEFEE) {
        if ($time != $args[$t->TIME]) {
          $argstime = $args[$t->TIME];
          return $this->failmsg($msg, "Time mismatch in storagefee item, was: $argstime, sb: $time");
        }
        $storageasset = $args[$t->ASSET];
        $storageamt = $args[$t->AMOUNT];
        if (@$storagemsgs[$storageasset]) {
          return $this->failmsg($msg, "Duplicate storage fee for asset: $storageasset");
        }
        $storageamts[$storageasset] = $storageamt;
        $storagemsg = $this->bankmsg($t->ATSTORAGEFEE, $reqmsg);
        $storagemsgs[$storageasset] = $storagemsg;
      } elseif ($request == $t->FRACTION) {
        if ($time != $args[$t->TIME]) {
          $argstime = $args[$t->TIME];
          return $this->failmsg($msg, "Time mismatch in fraction item, was: $argstime, sb: $time");
        }
        $fracasset = $args[$t->ASSET];
        $fracamt = $args[$t->AMOUNT];
        if (@$fracmsgs[$fracasset]) {
          return $this->failmsg($msg, "Duplicate fraction balance for asset: $fracasset");
        }
        $fracamts[$fracasset] = $fracamt;
        $fracmsg = $this->bankmsg($t->ATFRACTION, $reqmsg);
        $fracmsgs[$fracasset] = $fracmsg;
      } elseif ($request == $t->OUTBOXHASH) {
        if ($outboxhashreq) {
          return $this->failmsg($msg, $t->OUTBOXHASH . " appeared multiple times");
        }
        if ($time != $args[$t->TIME]) {
          return $this->failmsg($msg, "Time mismatch in outboxhash");
        }
        $outboxhashreq = $req;
        $outboxhashmsg = $parser->get_parsemsg($req);
        $outboxhash = $args[$t->HASH];
        $outboxcnt = $args[$t->COUNT];
      } elseif ($request == $t->BALANCE) {
        if ($time != $args[$t->TIME]) {
          $argstime = $args[$t->TIME];
          return $this->failmsg($msg, "Time mismatch in balance item, was: $argstime, sb: $time");
        }
        $errmsg = $this->handle_balance_msg($id, $reqmsg, $args, $state);
        if ($errmsg) return $this->failmsg($msg, $errmsg);
        $newbals[] = $reqmsg;
      } elseif ($request == $t->BALANCEHASH) {
        if ($balancehashreq) {
          return $this->failmsg($msg, $t->BALANCEHASH . " appeared multiple times");
        }
        if ($time != $args[$t->TIME]) {
          return $this->failmsg($msg, "Time mismatch in balancehash");
        }
        $balancehashreq = $req;
        $balancehash = $args[$t->HASH];
        $balancehashcnt = $args[$t->COUNT];
        $balancehashmsg = $parser->get_parsemsg($req);
      } else {
        return $this->failmsg($msg, "$request not valid for " . $t->PROCESSINBOX .
                              ". Only " . $t->SPENDACCEPT . ", " . $t->SPENDREJECT .
                              ", " . $t->OUTBOXHASH . ", " .
                              $t->BALANCE . ", &" . $t->BALANCEHASH);
      }
    }

    $acctbals = $state['acctbals'];
    $bals = $state['bals'];
    $tokens = $state['tokens'];
    $accts = $state['accts'];
    $oldneg = $state['oldneg'];
    $newneg = $state['newneg'];
    $charges = $state['charges'];

    // Check that we have exactly as many negative balances after the transaction
    // as we had before.
    if (count($oldneg) != count($newneg)) {
      return $this->failmsg($msg, "Negative balance count not conserved");
    }
    foreach ($oldneg as $asset => $acct) {
      if (!$newneg[$asset]) {
        return $this->failmsg($msg, "Negative balance assets not conserved");
      }
    }

    // Charge the new balance file tokens
    $tokenid = $this->tokenid;
    $bals[$tokenid] = bcsub(@$bals[$tokenid], $tokens);

    // Work the storage fees into the balances
    $storagefees = array();
    $fractions = array();
    if ($charges) {
      // Add storage fees for accepted spends and affirmed rejects
      foreach ($tobecharged as $item) {
        $itemamt = $item[$t->AMOUNT];
        $itemtime = $item[$t->TIME];
        $itemasset = $item[$t->ASSET];
        $assetinfo = $charges[$itemasset];
        if ($assetinfo) {
          $percent = $assetinfo['percent'];
          if ($percent) {
            $digits = $assetinfo['digits'];
            $itemfee = $u->storagefee($itemamt, $itemtime, $time, $percent, $digits);
            $storagefee = bcadd($assetinfo['storagefee'], $itemfee, $digits);
            $assetinfo['storagefee'] = $storagefee;
            $charges[$itemasset] = $assetinfo;            
          }
        }
      }
      foreach ($charges as $itemasset => $assetinfo) {
        $percent = @$assetinfo['percent'];
        if ($percent) {
          $digits = $assetinfo['digits'];
          $storagefee = @$assetinfo['storagefee'];
          $bal = bcsub(@$bals[$itemasset], $storagefee, $digits);
          $fraction = bcadd(@$fractions[$itemasset], @$assetinfo['fraction'], $digits);
          $u->normalize_balance($bal, $fraction, $digits);
          $bals[$itemasset] = $bal;
          $assetinfo['fraction'] = $fraction;
          $charges[$itemasset] = $assetinfo;
          $storagefees[$itemasset] = $storagefee;
          $fractions[$itemasset] = $fraction;
        }
      }
    }

    foreach ($storageamts as $storageasset => $storageamt) {
      $storagefee = $storagefees[$storageasset];
      if (bccomp($storageamt, $storagefee) != 0) {
        return $this->failmsg($msg, "Storage fee mismatch, sb: $storageamt, was: $storagefee");
      }
      unset($storagefees[$storageasset]);
    }
    if (count($storagefees) > 0) {
      return $this->failmsg($msg, "Storage fees missing for some assets");      
    }

    foreach ($fracamts as $fracasset => $fracamt) {
      $fraction = $fractions[$fracasset];
      if (bccomp($fracamt, $fraction) != 0) {
        return $this->failmsg($msg, "Fraction mismatch, sb: $fracamt, was: $fraction");
      }
      unset($fractions[$fracasset]);
    }
    if (count($fractions) > 0) {
      return $this->failmsg($msg, "Fraction balances missing for some assets");
    }

    $errmsg = "";
    $first = true;
    // Check that the balances in the spend message, match the current balance,
    // minus amount spent minus fees.
    foreach ($bals as $balasset => $balamount) {
      if ($balamount != 0) {
        $name = $this->lookup_asset_name($balasset);
        if (!$first) $errmsg .= ', ';
        $first = false;
        $errmsg .= "$name: $balamount";
      }
    }
    if ($errmsg != '') return $this->failmsg($msg, "Balance discrepancies: $errmsg");

    // No outbox hash maintained for the bank
    if ($id != $bankid) {
      // Make sure the outbox hash was included iff needed
      if ((count($outboxtimes) > 0 && !$outboxhashreq) ||
          (count($outboxtimes) == 0 && $outboxhashreq)) {
        return $this->failmsg($msg, $t->OUTBOXHASH .
                              ($outboxhashreq ? " included when not needed" :
                               " missing"));
      }

      if ($outboxhashreq) {
        $hasharray = $this->outboxhash($id, false, $outboxtimes);
        $hash = $hasharray[$t->HASH];
        $hashcnt = $hasharray[$t->COUNT];
        if ($outboxhash != $hash || $outboxcnt != $hashcnt) {
          return $this->failmsg
            ($msg, $t->OUTBOXHASH . " mismatch");
        }
      }

      // Check balancehash
      if (!$balancehashreq) {
        return $this->failmsg($msg, $t->BALANCEHASH . " missing");
      } else {
        $hasharray = $u->balancehash($db, $id, $this, $acctbals);
        $hash = $hasharray[$t->HASH];
        $hashcnt = $hasharray[$t->COUNT];
        if ($balancehash != $hash || $balancehashcnt != $hashcnt) {
          return $this->failmsg($msg, $t->BALANCEHASH . ' mismatch');
        }
      }
    }

    // All's well with the world. Commit this puppy.
    // Update balances
    $balancekey = $this->balancekey($id);
    foreach ($acctbals as $acct => $balances) {
      $acctkey = "$balancekey/$acct";
      foreach ($balances as $balasset => $balance) {
        $balance = $this->bankmsg($t->ATBALANCE, $balance);
        $res .= ".$balance";
        $db->put("$acctkey/$balasset", $balance);
      }
    }

    // Update accepted and rejected spenders' inboxes
    foreach ($inboxmsgs as $inboxmsg) {
      $otherid = $inboxmsg[0];
      if ($otherid == $bankid) {
        $request = $inboxmsg[1];
        $itemargs = $inboxmsg[2];
        if ($request == $t->SPENDREJECT) {
          // Return the funds to the bank's account
          $this->add_to_bank_balance($itemargs[$t->ASSET], $itemargs[$t->AMOUNT]);
        }
      } else {
        $inboxtime = $inboxmsg[1];
        $inboxmsg = $inboxmsg[2];
        $inboxkey = $this->inboxkey($otherid);
        $db->put("$inboxkey/$inboxtime", $inboxmsg);
      }
    }

    // Remove no longer needed inbox and outbox entries
    // Probably should have a bank config parameter to archive these somewhere.
    $inboxkey = $this->inboxkey($id);
    foreach ($inboxtimes as $inboxtime) {
      $db->put("$inboxkey/$inboxtime", '');
    }

    // Clear processed outbox entries
    $outboxkey = $this->outboxkey($id);
    foreach ($outboxtimes as $outboxtime) {
      $db->put("$outboxkey/$outboxtime", '');
    }

    if ($id != $bankid) {
      // Update outboxhash
      if ($outboxhashreq) {
        $outboxhash_item = $this->bankmsg($t->ATOUTBOXHASH, $outboxhashmsg);
        $res .= ".$outboxhash_item";
        $db->put($this->outboxhashkey($id), $outboxhash_item);
      }

      // Update balancehash
      $balancehash_item = $this->bankmsg($t->ATBALANCEHASH, $balancehashmsg);
      $res .= ".$balancehash_item";
      $db->put($this->balancehashkey($id), $balancehash_item);
    }

    // Update fractions, and add fraction and storagefee messages to result
    foreach ($storagemsgs as $storagemsg) $res .= ".$storagemsg";
    foreach ($fracmsgs as $fracasset => $fracmsg) {
      $res .= ".$fracmsg";
      $key = $this->fractionbalancekey($id, $fracasset);
      $db->put($key, $fracmsg);
    }
    
    $ok = true;
    return $res;
  }

  // Process a storagefees request
  function do_storagefees($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $id = $args[$t->CUSTOMER];
    $lock = $db->lock($this->accttimekey($id));
    $res = $this->do_storagefees_internal($msg, $args);
    $db->unlock($lock);
    return $res;
  }

  function do_storagefees_internal($msg, $args) {
    $t = $this->t;
    $db = $this->db;
    $u = $this->u;
    $bankid = $this->bankid;

    $err = $this->checkreq($args, $msg);
    if ($err) return $err;

    $id = $args[$t->CUSTOMER];
    $inboxkey = $this->inboxkey($id);
    $key = $this->storagefeekey($id);
    $assetids = $db->contents($key);
    foreach ($assetids as $assetid) {
      $storagefee = $db->get("$key/$assetid");
      $args = $this->unpack_bankmsg($storagefee, $t->STORAGEFEE);
      if (is_string($args)) {
        return $this->failmsg($msg, "storagefee parse error: $args");
      }
      if ($assetid != $args[$t->ASSET]) {
        $feeasset = $args[$t->ASSET];
        return $this->failmsg($msg, "Asset mismatch, sb: $assetid, was: $feeasset");
      }
      $amount = $args[$t->AMOUNT];
      $percent = $this->storageinfo($id, $assetid, $issuer, $fraction, $fractime);
      $digits = $u->fraction_digits($percent);
      $u->normalize_balance($amount, $fraction, $digits);
      if (bccomp($amount, 0, 0) > 0) {
        $time = $this->gettime();
        $storagefee = $this->bankmsg($t->STORAGEFEE, $bankid, $time, $assetid, $fraction);
        $spend = $this->bankmsg($t->SPEND, $bankid, $time, $id, $assetid, $amount, "Storage fees");
        $inbox = $this->bankmsg($t->INBOX, $time, $spend);
        $db->put("$key/$assetid", $storagefee);
        $db->put("$inboxkey/$time", $inbox);
      }
    }
    
    return $this->bankmsg($t->ATSTORAGEFEES, $msg);
  }

  function get_outbox_args($id, $spendtime) {
    $t = $this->t;
    $u = $this->u;
    $db = $this->db;
    $parser = $this->parser;
    $bankid = $this->bankid;

    $outboxkey = $this->outboxkey($id);
    $spendmsg = $db->get("$outboxkey/$spendtime");
    if (!$spendmsg) return "Can't find outbox item: $spendtime";
    $reqs = $parser->parse($spendmsg);
    if (!$reqs) return $parser->errmsg;
    $spendargs = $u->match_pattern($reqs[0]);
    $feeargs = false;
    if (count($reqs) > 1) $feeargs = $u->match_pattern($reqs[1]);
    if ($spendargs[$t->CUSTOMER] != $bankid ||
        $spendargs[$t->REQUEST] != $t->ATSPEND ||
        ($feeargs &&
         ($feeargs[$t->CUSTOMER] != $bankid ||
          $feeargs[$t->REQUEST] != $t->ATTRANFEE))) {
      return "Outbox corrupted";
    }
    $spendargs = $u->match_pattern($spendargs[$t->MSG]);
    if ($feeargs) $feeargs = $u->match_pattern($feeargs[$t->MSG]);
    if ($spendargs[$t->CUSTOMER] != $id ||
        $spendargs[$t->REQUEST] != $t->SPEND ||
        ($feeargs &&
         ($feeargs[$t->CUSTOMER] != $id ||
          $feeargs[$t->REQUEST] != $t->TRANFEE))) {
      return "Outbox inner messages corrupted";
    }
    return array($spendargs, $feeargs); 
  }

  function do_getasset($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $err = $this->checkreq($args, $msg);
    if ($err) return $err;

    $assetid = $args[$t->ASSET];
    $asset = $db->get($t->ASSET . "/$assetid");
    if (!$asset) return $this->failmsg($msg, "Unknown asset: $assetid");
    return $asset;
  }

  function do_asset($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $id = $args[$t->CUSTOMER];
    $lock = $db->lock($this->accttimekey($id));
    $res = $this->do_asset_internal($args, $reqs, $msg);
    $db->unlock($lock);
    return $res;
  }

  function do_asset_internal($args, $reqs, $msg) {
    $t = $this->t;
    $u = $this->u;
    $db = $this->db;
    $bankid = $this->bankid;
    $parser = $this->parser;

    if (count($reqs) < 2) {
      return $this->failmsg($msg, "No balance items");
    }

    // $t->ASSET => array($t->BANKID,$t->ASSET,$t->SCALE,$t->PRECISION,$t->ASSETNAME),
    $id = $args[$t->CUSTOMER];
    $assetid = $args[$t->ASSET];
    $scale = $args[$t->SCALE];
    $precision = $args[$t->PRECISION];
    $assetname = $args[$t->ASSETNAME];
    $storage_msg = false;

    if (!(is_numeric($scale) && is_numeric($precision) &&
          $scale >= 0 && $precision >= 0)) {
      return $this->failmsg($msg, "Scale & precision must be integers >= 0");
    }

    if (bccomp($scale, 10) > 0) {
      return $this->failmsg($msg, 'Maximum scale is 10');
    }

    // Don't really need this restriction. Maybe widen it a bit?
    if (!$this->is_alphanumeric($assetname)) {
      return $this->failmsg($msg, "Asset name must contain only letters and digits");
    }

    if ($assetid != $u->assetid($id, $scale, $precision, $assetname)) {
      return $this->failmsg
        ($msg, "Asset id is not sha1 hash of 'id,scale,precision,name'");
    }

    $exists = ($this->is_asset($assetid));

    $tokens = 1;       // costs 1 token for the /asset/<assetid> file
    if ($id == $bankid) $tokens = 0;

    $bals = array();
    if (!$exists) $bals[$assetid] = -1;
    $acctbals = array();
    $accts = array();
    $oldneg = array();
    $newneg = array();

    $tokenid = $this->tokenid;
    $bals[$tokenid] = 0;

    $state = array('acctbals' => $acctbals,
                   'bals' => $bals,
                   'tokens' => $tokens,
                   'accts' => $accts,
                   'oldneg' => $oldneg,
                   'newneg' => $newneg
                   // 'time' initialized below
                   );

    $balancehashreq = false;

    for ($i=1; $i<count($reqs); $i++) {
      $req = $reqs[$i];
      $args = $u->match_pattern($req);
      if (is_string($req_args)) return $this->failmsg($msg, $args); // match error
      $reqid = $args[$t->CUSTOMER];
      $request = $args[$t->REQUEST];
      $reqtime = $args[$t->TIME];
      if ($i == 1) {
        // Burn the transaction
        $time = $reqtime;
        $err = $this->deq_time($id, $time);
        if ($err) return $this->failmsg($msg, $err);
        $state['time'] = $time;
      }
      if ($reqid != $id) return $this->failmsg($msg, "ID mismatch");
      elseif ($request == $t->STORAGE) {
        if ($storage_msg) return $this->failmsg($msg, "Duplicate storage fee");
        $storage_msg = $parser->get_parsemsg($req);
      }
      elseif ($request == $t->BALANCE) {
        $reqmsg = $parser->get_parsemsg($req);
        $errmsg = $this->handle_balance_msg($id, $reqmsg, $args, $state, $assetid);
        if ($errmsg) return $this->failmsg($msg, $errmsg);
        $newbals[] = $reqmsg;
      } elseif ($request == $t->BALANCEHASH) {
        if ($balancehashreq) {
          return $this->failmsg($msg, $t->BALANCEHASH . " appeared multiple times");
        }
        $balancehashreq = $req;
        $balancehash = $args[$t->HASH];
        $balancehashcnt = $args[$t->COUNT];
        $balancehashmsg = $parser->get_parsemsg($req);
      } else {
        return $this->failmsg($msg, "$request not valid for asset creation. Only " .
                              $t->BALANCE . ' & ' . $t->BALANCEHASH);
      }
    }

    $acctbals = $state['acctbals'];
    $bals = $state['bals'];
    $accts = $state['accts'];
    $tokens = $state['tokens'];
    $oldneg = $state['oldneg'];
    $newneg = $state['newneg'];

    // Check that we have exactly as many negative balances after the transaction
    // as we had before, plus one for the new asset
    if (!$exists) $oldneg[$assetid] = $t->MAIN;
    if (count($oldneg) != count($newneg)) {
      return $this->failmsg($msg, "Negative balance count not conserved");
    }
    foreach ($oldneg as $asset => $acct) {
      if (!$newneg[$asset]) {
        return $this->failmsg($msg, "Negative balance assets not conserved");
      }
    }

    // Charge the new file tokens
    $bals[$tokenid] = bcsub($bals[$tokenid], $tokens);

    $errmsg = "";
    // Check that the balances in the spend message, match the current balance,
    // minus amount spent minus fees.
    foreach ($bals as $balasset => $balamount) {
      if ($balamount != 0) {
        if ($balasset == $assetid) $name = $assetname;
        else $name = $this->lookup_asset_name($balasset);
        if ($errmsg) $errmsg .= ', ';
        $errmsg .= "$name: $balamount";
      }
    }
    if ($errmsg != '') return $this->failmsg($msg, "Balance discrepancies: $errmsg");

    // balancehash must be included
    if (!$balancehashreq) {
      return $this->failmsg($msg, $t->BALANCEHASH . " missing");
    } else {
      $hasharray = $u->balancehash($db, $id, $this, $acctbals);
      $hash = $hasharray[$t->HASH];
      $hashcnt = $hasharray[$t->COUNT];
      if ($balancehash != $hash || $balancehashcnt != $hashcnt) {
        return $this->failmsg($msg, $t->BALANCEHASH .
                              " mismatch, hash: $balancehash, sb: $hash, count: $balancehashcnt, sb: $hashcnt");
      }
    }
  
    // All's well with the world. Commit this puppy.
    // Add asset
    $res = $this->bankmsg($t->ATASSET, $parser->get_parsemsg($reqs[0]));
    if ($storage_msg) {
      $res .= "." . $this->bankmsg($t->ATSTORAGE, $storage_msg);
    }
    $db->put($t->ASSET . "/$assetid", $res);

    // Credit bank with tokens
    $this->add_to_bank_balance($tokenid, $tokens);

    // Update balances
    $balancekey = $this->balancekey($id);
    foreach ($acctbals as $acct => $balances) {
      $acctkey = "$balancekey/$acct";
      foreach ($balances as $balasset => $balance) {
        $balance = $this->bankmsg($t->ATBALANCE, $balance);
        $res .= ".$balance";
        $db->put("$acctkey/$balasset", $balance);
      }
    }

    // Update balancehash
    $balancehash_item = $this->bankmsg($t->ATBALANCEHASH, $balancehashmsg);
    $res .= ".$balancehash_item";
    $db->put($this->balancehashkey($id), $balancehash_item);

    return $res;
  }

  function do_getbalance($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $id = $args[$t->CUSTOMER];
    $lock = $db->lock($this->accttimekey($id));
    $res = $this->do_getbalance_internal($args, $reqs, $msg);
    $db->unlock($lock);
    return $res;
  }

  function do_getbalance_internal($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $err = $this->checkreq($args, $msg);
    if ($err) return $err;

    // $t->GETBALANCE => array($t->BANKID,$t->REQ,$t->ACCT=>1,$t->ASSET=>1));
    $id = $args[$t->CUSTOMER];
    $acct = @$args[$t->ACCT];
    $assetid = @$args[$t->ASSET];

    if ($acct) $acctkeys = array($this->acctbalancekey($id, $acct));
    else {
      $balancekey = $this->balancekey($id);
      $acctnames = $db->contents($balancekey);
      $acctkeys = array();
      foreach ($acctnames as $name) $acctkeys[] = "$balancekey/$name";
    }

    $res = '';
    $assetnames = array();
    foreach ($acctkeys as $acctkey) {
      if ($assetid) $assetnames[] = $assetid;
      else $assetnames = $db->contents($acctkey);
      $assetkeys = array();
      foreach ($assetnames as $name) $assetkeys[] = "$acctkey/$name";

      foreach ($assetkeys as $assetkey) {
        $bal = $db->get($assetkey);
        if ($bal) {
          if ($res) $res .= '.';
          $res .= $bal;
        }
      }
    }

    // Get the fractions
    $assetids = array();
    if ($assetid) $assetids[] = $assetid;
    else {
      foreach ($assetnames as $name) $assetids[] = $name;
    }
      
    foreach($assetids as $assetid) {
      $fractionkey = $this->fractionbalancekey($id, $assetid);
      $fraction = $db->get($fractionkey);
      if ($fraction) $res .= ".$fraction";
    }

    $balancehash = $db->get($this->balancehashkey($id));
    if ($balancehash) {
      if ($res) $res .= '.';
      $res .= $balancehash;
    }

    return $res;
  }

  function do_getoutbox($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $id = $args[$t->CUSTOMER];
    $lock = $db->lock($this->accttimekey($id));
    $res = $this->do_getoutbox_internal($args, $reqs, $msg);
    $db->unlock($lock);
    return $res;
  }

  function do_getoutbox_internal($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $err = $this->checkreq($args, $msg);
    if ($err) return $err;

    // $t->GETOUTBOX => array($t->BANKID,$t->REQ),
    $id = $args[$t->CUSTOMER];

    $msg = $this->bankmsg($t->ATGETOUTBOX, $msg);
    $outboxkey = $this->outboxkey($id);
    $contents = $db->contents($outboxkey);
    foreach ($contents as $time) {
      $msg .= '.' . $db->get("$outboxkey/$time");
    }
    $outboxhash = $db->get($this->outboxhashkey($id));
    if ($outboxhash) $msg .= '.' . $outboxhash;

    return $msg;
  }


  /*** End request processing ***/

  function commands() {
    $t = $this->t;
    $u = $this->u;

    if (!$this->commands) {
      $patterns = $u->patterns();
      $names = array($t->BANKID => array($t->PUBKEY, $t->COUPON=>1),
                     $t->ID => array($t->BANKID,$t->ID),
                     $t->REGISTER => $patterns[$t->REGISTER],
                     $t->GETREQ => array($t->BANKID),
                     $t->GETTIME => array($t->BANKID,$t->REQ),
                     $t->GETFEES => array($t->BANKID,$t->REQ,$t->OPERATION=>1),
                     $t->SPEND => $patterns[$t->SPEND],
                     $t->SPENDREJECT => $patterns[$t->SPENDREJECT],
                     $t->COUPONENVELOPE => $patterns[$t->COUPONENVELOPE],
                     $t->GETINBOX => $patterns[$t->GETINBOX],
                     $t->PROCESSINBOX => $patterns[$t->PROCESSINBOX],
                     $t->STORAGEFEES => $patterns[$t->STORAGEFEES],
                     $t->GETASSET => array($t->BANKID,$t->REQ,$t->ASSET),
                     $t->ASSET => array($t->BANKID,$t->ASSET,$t->SCALE,$t->PRECISION,$t->ASSETNAME),
                     $t->GETOUTBOX => $patterns[$t->GETOUTBOX],
                     $t->GETBALANCE => array($t->BANKID,$t->REQ,$t->ACCT=>1,$t->ASSET=>1));
      $commands = array();
      foreach($names as $name => $pattern) {
        $fun = str_replace('|', '', $name);
        $commands[$name] = array("do_$fun", $pattern);
      }
      $this->commands = $commands;
    }
    return $this->commands;
  }

  // Process a message and return the response
  // This is usually all you'll call from outside
  function process($msg) {
    $parser = $this->parser;
    $t = $this->t;
    $parses = $parser->parse($msg);
    if (!$parses) {
      return $this->failmsg($msg, $parser->errmsg);
    }
    $req = $parses[0][1];
    $commands = $this->commands();
    $method_pattern = $commands[$req];
    if (!$method_pattern) {
      return $this->failmsg($msg, "Unknown request: $req");
    }
    $method = $method_pattern[0];
    $pattern = array_merge(array($t->CUSTOMER,$t->REQUEST), $method_pattern[1]);
    $args = $this->parser->matchargs($parses[0], $pattern);
    if (!$args) {
      return $this->failmsg($msg,
                            "Request doesn't match pattern: " .
                            $parser->formatpattern($pattern));
    }
    if (array_key_exists($t->BANKID, $args)) {
      $argsbankid = $args[$t->BANKID];
      if ($argsbankid != $this->bankid) {
        return $this->failmsg($msg, "bankid mismatch");
      }
    }
    if (strlen(@$args[$t->NOTE]) > 4096) {
      return $this->failmsg($msg, "Note too long. Max: 4096 chars");
    }
    return $this->$method($args, $parses, $msg);
  }

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
