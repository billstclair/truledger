(in-package :trubanc)

(defun file-get-contents (file)
  (with-open-file (stream file :if-does-not-exist nil)
    (when stream
      (let* ((len (file-length stream))
             (s (make-string len)))
        (read-sequence s stream)
        s))))

(defun file-put-contents (file contents)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create)
    (write-sequence contents stream)
    contents))

(defun hex (integer)
  "Return a string encoding integer as hex"
  (format nil "~x" integer))

(defparameter *whitespace* '(#\newline #\return #\tab #\space))

(defun trim (string)
  (string-left-trim *whitespace* (string-right-trim *whitespace* string)))

(defun as-hex (byte)
  (when (or (< byte 0) (> byte 15))
    (error "Not between 0 and 15: ~s" byte))
  (code-char
   (if (< byte 10)
       (+ byte #.(char-code #\0))
       (+ (- byte 10) #.(char-code #\a)))))

(defun as-bin (hex-char)
  (let ((code (char-code hex-char)))
    (cond ((< code #.(char-code #\0))
           (error "Not a hex character: ~s" hex-char))
          ((<= code #.(char-code #\9)) (- code #.(char-code #\0)))
          ((and (>= code #.(char-code #\a))
                (<= code #.(char-code #\f)))
           (+ 10 (- code #.(char-code #\a))))
          ((and (>= code #.(char-code #\A))
                (<= code #.(char-code #\F)))
           (+ 10 (- code #.(char-code #\A))))
          (t (error "Not a hex character: ~s" hex-char)))))

(defun bin2hex (thing)
  "Convert an integer or byte array or string to a hex string"
  (if (integerp thing)
      (format nil "~x" thing)
      (let ((stringp (stringp thing)))
        (with-output-to-string (s)
          (dotimes (i (length thing))
            (let* ((elt (aref thing i))
                   (byte (if stringp (char-code elt) elt))
                   (hi (ash byte -4))
                   (lo (logand byte #xf)))
              (write-char (as-hex hi) s)
              (write-char (as-hex lo) s)))))))

(defun hex2bin (hex &optional res-type)
  "Convert a hex string to binary.
   Result is a byte-string if res-type is :bytes,
   a string if res-type is :string,
   or an integer otherwise (the default)."
  (let* ((len (length hex))
         (bytes (ash (1+ len) -1))
         (res (cond ((eq res-type :string) (make-string bytes))
                    ((eq res-type :bytes)
                     (make-array bytes :element-type '(unsigned-byte 8)))
                    (t nil)))
         (accum 0)
         (cnt (if (evenp len) 2 1))
         (idx -1))
    (dotimes (i len)
      (setq accum (+ (ash accum 4) (as-bin (aref hex i))))
      (when (and res (eql 0 (decf cnt)))
        (setf (aref res (incf idx))
              (if (eq res-type :bytes)
                  accum
                  (code-char accum)))
        (setq accum 0
              cnt 2)))
    (or res accum)))

(defun copy-memory-to-lisp (pointer len byte-array-p)
  (let ((res (if byte-array-p
                 (make-array len :element-type '(unsigned-byte 8))
                 (make-string len))))
    (dotimes (i len)
      (let ((byte (mem-ref pointer :unsigned-char i)))
        (setf (aref res i)
              (if byte-array-p byte (code-char byte)))))
    res))

(defun base64-encode (string)
  (string-to-base64-string string :columns 64))

(defun base64-decode (string)
  (base64-string-to-string string))

(defun strcat (&rest strings)
  "Concatenate a bunch of strings"
  (apply #'concatenate 'string (mapcar 'string strings)))

(defun strstr (haystack needle)
  "Find NEEDLE in HAYSTACK. Return the tail of HAYSTACK including NEEDLE."
  (let ((pos (search needle haystack)))
    (and pos (subseq haystack pos))))

(defun str-replace (old new string)
  "Change all instance of OLD to NEW in STRING"
  (loop with pos = 0
        with old-len = (length old)
        with new-len = (length new)
        with res = string
        for idx = (search old res :start2 pos)
        do
     (when (null idx) (return res))
     (setq res (strcat (subseq res 0 idx) new (subseq res (+ idx old-len)))
           pos (+ idx new-len))))

(defun integer-string-sort (array)
  "Sort a sequence of integers represented as strings.
   Doesn't use bignum math, just prepends leading zeroes.
   Does NOT clobber the array. Returns a new one."
  (let ((maxlen 0)
        (res (copy-seq array)))
    (map nil
         (lambda (item)
           (let ((len (length item)))
             (when (> len maxlen)
               (setq maxlen len))))
         res)
    (let ((i 0))
      (map nil
           (lambda (item)
             (let ((len (length item)))
               (when (< len maxlen)
                 (setf (elt res i)
                       (strcat (make-string (- maxlen len)
                                            :initial-element #\0)
                               item)))
               (incf i)))
           res))
    (sort res 'string-lessp)))

(defun escape (str)
  "Escape a string for inclusion in a message"
  (let ((res "")
        (ptr 0))
    (dotimes (i (length str))
      (let ((char (aref str i)))
        (when (position char "(),:.\\")
          (setq res (strcat res (subseq str ptr i) "\\"))
          (setq ptr i))))
    (strcat res (subseq str ptr))))

(defun makemsg (alist)
  "Make an unsigned message from alist"
  (loop
     with msg = "("
     for i from 0
     for (key . value) in alist
     do
     (when (> i 0) (setq msg (strcat msg ",")))
     (when (not (eql key i)) (setq msg (strcat msg key ":")))
     (setq msg (strcat msg (escape (string value))))
     finally
     (return (strcat msg ")"))))

(defun assetid (id scale precision name)
  "Return the id for an asset"
  (sha1 (format nil "~a,~a,~a,~a" id scale precision name)))

(defclass utility ()
  ((parser :type (or parser null)
           :initarg :parser
           :initform nil
           :accessor utility-parser)
   (bankgetter :initarg :bankgetter
               :initform nil
               :accessor utility-bankgetter)))

(defvar *patterns* nil)

;; Patterns for non-request data
(defun patterns ()
  (or *patterns*
      (let ((patterns '(;; Customer messages
                        (:|balance| .
                         (:|bankid| :|time| :|asset| :|amount| (:|acct|)))
                        (:|outboxhash| . (:|bankid| :|time| :|count| :|hash|))
                        (:|balancehash| . (:|bankid| :|time| :|count| :|hash|))
                        (:|spend| .
                         (:|bankid| :|time| :|id| :|asset| :|amount| (:|note|)))
                        (:|asset| .
                         (:|bankid| :|asset| :|scale| :|precision| :|assetname|))
                        (:|storage| . (:|bankid| :|time| :|asset| :|percent|))
                        (:|storagefee| . (:|bankid| :|time| :|asset| :|amount|))
                        (:|fraction| . (:|bankid| :|time| :|asset| :|amount|))
                        (:|register| . (:|bankid| :|pubkey| (:|name|)))
                        (:|spendaccept| . (:|bankid| :|time| :|id| (:|note|)))
                        (:|spendreject| . (:|bankid| :|time| :|id| (:|note|)))
                        (:|getoutbox| .(:|bankid| :|req|))
                        (:|getinbox| . (:|bankid| :|req|))
                        (:|processinbox| . (:|bankid| :|time| :|timelist|))
                        (:|storagefees| . (:|bankid| :|req|))
                        (:|gettime| . (:|bankid| :|time|))
                        (:|couponenvelope| . (:|id| :|encryptedcoupon|))

                        ;; Bank signed messages
                        (:|failed| . (:|msg| :|errmsg|))
                        (:|tokenid| . (:|tokenid|))
                        (:|bankid| . (:|bankid|))
                        (:|regfee| . (:|bankid| :|time| :|asset| :|amount|))
                        (:|tranfee| . (:|bankid| :|time| :|asset| :|amount|))
                        (:|time| . (:|id| :|time|))
                        (:|inbox| . (:|time| :|msg|))
                        (:|req| . (:|id| :|req|))
                        (:|coupon| .
                         (:|bankurl| :|coupon| :|asset| :|amount| (:|note|)))
                        (:|couponnumberhash| . (:|coupon|))
                        (:|atregister| . (:|msg|))
                        (:|atoutboxhash| . (:|msg|))
                        (:|atbalancehash| . (:|msg|))
                        (:|atgetinbox| . (:|msg|))
                        (:|atbalance| . (:|msg|))
                        (:|atspend| . (:|msg|))
                        (:|attranfee| . (:|msg|))
                        (:|atasset| . (:|msg|))
                        (:|atstorage| . (:|msg|))
                        (:|atstoragefee| . (:|msg|))
                        (:|atfraction| . (:|msg|))
                        (:|atprocessinbox| . (:|msg|))
                        (:|atstoragefees| . (:|msg|))
                        (:|atspendaccept| . (:|msg|))
                        (:|atspendreject| . (:|msg|))
                        (:|atgetoutbox| . (:|msg|))
                        (:|atcoupon| . (:|coupon| :|spend|))
                        (:|atcouponenvelope| . (:|msg|))
                        ))
            (hash (make-hash-table :test 'eq)))
        (loop
           for (key . value) in patterns
           do
             (setf (gethash key hash) value))
        (setq *patterns* hash))))

#||
;; Continue here
  // Parse and match a message.
  // Returns an array mapping parameter names to values.
  // Returns a string if parsing or matching fails.
  function match_message($msg) {
    $parser = $this->parser;

    $reqs = $parser->parse($msg);
    if (!$reqs) return $parser->errmsg || "Parse failed";
    return $this->match_pattern($reqs[0]);
  }

  // Return the hash of a directory, $key, of bank-signed messages.
  // The hash is of the user messages wrapped by the bank signing.
  // $newitems is a new item or an array of new items, not bank-signed.
  // $removed_names is an array of names in the $key dir to remove.
  // $unpacker is an object on which to call the unpack_bankmsg()
  // method with a single-arg, a bank-signed message. It returns
  // a parsed and matched $args array whose :|msg| element is
  // the parsed user message wrapped by the bank signing.
  // Returns array('hash'=>$hash, 'count'=>$count)
  // Where $hash is the sha1 hash, and $count is the number of items
  // hashed.
  // Returns false if there's a problem.
  function dirhash($db, $key, $unpacker, $newitem=false, $removed_names=false) {
    $parser = $this->parser;
    $t = $this->t;

    $contents = $db->contents($key);
    $items = array();
    foreach ($contents as $name) {
      if (!$removed_names || !in_array($name, $removed_names)) {
        $msg = $db->get("$key/$name");
        $args = $unpacker->unpack_bankmsg($msg);
        if (!$args || is_string($args)) return false;
        $req = $args[:|msg|];
        if (!$req) return false;
        $msg = $parser->get_parsemsg($req);
        if (!$msg) return false;
        $items[] = $msg;
      }
    }
    if ($newitem) {
      if (is_string($newitem)) $items[] = $newitem;
      else $items = array_merge($items, $newitem);
    }
    sort($items);
    $str = implode('.', array_map('trim', $items));
    $hash = sha1($str);
    return array(:|hash| => $hash, :|count| => count($items));
  }

  // Compute the balance hash as array(:|hash| => $hash, :|count| => $hashcnt)
  // $id is the ID of the account.
  // $unpacker must have balancekey() and unpack_bankmsg() methods.
  // $acctbals is array($acct => array($assetid => $msg))
  function balancehash($db, $id, $unpacker, $acctbals) {
    $t = $this->t;

    $hash = '';
    $hashcnt = 0;
    $balancekey = $unpacker->balancekey($id);
    $accts = $db->contents($balancekey);
    $needsort = false;
    foreach ($acctbals as $acct => $bals) {
      if (!in_array($acct, $accts)) {
        $accts[] = $acct;
        $needsort = true;
      }
    }
    if ($needsort) sort($accts);
    foreach ($accts as $acct) {
      $newitems = array();
      $removed_names = array();
      $newacct = @$acctbals[$acct];
      if ($newacct) {
        foreach ($newacct as $assetid => $msg) {
          $newitems[] = $msg;
          $removed_names[] = $assetid;
        }
        $hasharray = $this->dirhash($db, "$balancekey/$acct", $unpacker,
                                    $newitems, $removed_names);
        if ($hash != '') $hash .= '.';
        $hash .= $hasharray[:|hash|];
        $hashcnt += $hasharray[:|count|];
      }
    }
    if ($hashcnt > 1) $hash = sha1($hash);
    return array(:|hash| => $hash, :|count| => $hashcnt);
  }

  // Take the values in the passed array and return an array with
  // those values as its keys mapping to $value.
  function array_to_keys($arr, $value=true) {
    $res = array();
    foreach($arr as $v) {
      $res[$v] = $value;
    }
    return $res;
  }

  // Predicate. True if arg looks like a coupon number
  function is_coupon_number($x) {
    return is_string($x) && strlen($x) == 32 && @pack("H*", $x);
  }

  // Predicate. True if arg looks like an ID
  function is_id($x) {
    return is_string($x) && strlen($x) == 40 && @pack("H*", $x);
  }

  // Return the number of digits after the decimal point in a number
  function number_precision($number) {
    $i = strpos($number, '.');
    if ($i === false) return 0;
    return strlen($number) - $i - 1;
  }

  // Calculate the number of digits to keep for the fractional balance
  function fraction_digits($percent) {
    // Add 3 for divide by 365, 2 for percent, 3 more for 1/1000 precision
    return $this->number_precision($percent) + 8;
  }

  // Calculate the storage fee.
  // $balance is the balance
  // $baltime is the time of the $balance
  // $now is the current time
  // $percent is the storage fee rate, in percent/year
  // $digits is the precision for the arithmetic
  // Returns the storage fee, and subtracts it from $balance
  function storagefee($balance, $baltime, $now, $percent, $digits) {
    if (!$percent) return 0;

    $SECSPERYEARPCT = bcmul(60 * 60 * 24 * 365, 100, 0);

    $baltime = bcadd($baltime, 0, 0); // truncate
    $time = bcadd($now, 0, 0);         // truncate
    $fee = bcmul($balance, $percent, $digits);
    $fee = bcmul($fee, bcsub($time, $baltime), $digits);
    $fee = bcdiv($fee, $SECSPERYEARPCT, $digits);
    if (bccomp($fee, 0) < 0) $fee = 0;
    elseif (bccomp($fee, $balance) > 0) $fee = $balance;
    return $fee;
  }

  // Add together $balance & $fraction, to $digits precision.
  // Put the integer part of the total into $balance and the
  // fractional part into $fraction.
  function normalize_balance(&$balance, &$fraction, $digits) {
    $total = bcadd($balance, $fraction, $digits);
    $i = strpos($total, '.');
    if ($i === false) {
      $balance = $total;
      $fraction = 0;
    } else {
      $balance = substr($total, 0, $i);
      $fraction = '0' . substr($total, $i);
      if (bccomp($fraction, 0, $digits) == 0) $fraction = 0;
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
