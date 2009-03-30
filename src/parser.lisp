; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse "(id,[key:]value,...):signature" into an array, verifying signatures
;;; Can separate multiple top-level forms with periods.
;;; Values can be (id,...):signature forms.
;;;

(defconstant $PARSER-MSGKEY "%msg%")

(defclass parser ()
  ((keydb :initarg :keydb
          :type (or db null)
          :accessor parser-keydb
          :initform nil)
   (keydict :initform (make-hash-table :test 'equal)
            :accessor parser-keydict)
   (always-verify-sigs-p :initform nil
                         :initarg :always-verify-sigs-p
                         :accessor parser-always-verify-sigs-p)
   (verify-sigs-p :initform t
                  :initarg :verify-sigs-p
                  :accessor parser-verify-sigs-p)))

(defmethod parse ((parser parser) string &optional
                  (verify-sigs-p (parser-verify-sigs-p parser)))
  "Return a hash table, or signal en error, if the parse could not be done,
   or an ID couldn't be found, or a signature was bad.
   left-paren, right-paren, comma, colon, and period are special chars.
   They, and back-slash, are escaped by backslashes
   If VERIFY-SIGS-P is false, don't verify PGP signatures.
   By default, use (PARSER-VERIFY-SIGS-P PARSER)"
  (let* ((tokens (tokenize string))
         (state nil)
         (res nil)
         (dict nil)
         (dictidx nil)
         (id nil)
         (start nil)
         (key nil)
         (value nil)
         (needsig nil)
         (msg nil)
         (stack (make-array 10 :fill-pointer 0 :adjustable t)))
    (flet ((init-dict ()
             (setq dict (make-hash-table :test 'equal)
                   dictidx -1))
           (append-dict (value)
             (setf (gethash (incf dictidx) dict) value)))
      (loop for first = t then nil
         for (pos . tok) in tokens
         do
         (when (and first (not (eql tok #\()))
           (error "Message does not begin with left paren"))
         (cond ((eql tok #\()
                (setq needsig t)
                (when (and dict state (not (member state '(#\: #\,))))
                  (error "Open paren not after colon or comma at ~d" pos))
                (when (and key (not (eql state #\:)))
                  (error "Missing key at ~d" pos))
                (when dict
                  (vector-push-extend `(,state ,dict ,dictidx ,start ,key) stack)
                  (setq state nil
                        dict nil
                        key nil))
                (setq start pos
                      state #\())
               ((eql tok #\))
                (cond ((eql state #\,)
                       (when key
                         (error "Missing key at ~s" pos))
                       (unless dict (init-dict))
                       (append-dict (or value ""))
                       (setq value nil))
                      ((eql state #\:)
                       (unless dict (init-dict))
                       (setf (gethash key dict) (or value "")
                             value nil))
                      (state
                       (error "Close paren not after value at ~d" pos)))
                (setq msg (subseq string start (1+ pos))
                      state #\)))
               ((eql tok #\:)
                (cond ((eql state #\))
                       (setq state :sig))
                      ((null value)
                       (error "Missing key before colon at ~d" pos))
                      (t
                       (setq key value
                             value nil
                             state #\:))))
               ((eql tok #\,)
                (when (and state
                           (not (member state '(#\, #\())))
                  (error "Misplaced comma at ~d, state: ~s" pos state))
                (unless dict (init-dict))
                (append-dict (or value ""))
                (setq value nil
                      state #\,))
               ((eql tok #\.)
                (when (or state (> (length stack) 0))
                  (error "Misplaced period at ~d" pos))
                (when dict (push dict res))
                (setq dict nil
                      key nil))
               (t 
                (cond ((member state '(#\( #\,))
                       (setq value tok))
                      ((eql state #\:)
                       (when (null dict) (init-dict))
                       (setf (gethash key dict) tok
                             value nil))
                      ((eq state :sig)
                       (setq id (and dict (gethash 0 dict)))
                       (when (not (and (equal id "0")
                                       (equal (gethash dict 1) "bankid")))
                         (unless id
                           (error "Signature without ID at ~d" pos))
                         (when (or verify-sigs-p
                                   (parser-always-verify-sigs-p parser))
                           (let ((pubkey (parser-get-pubkey parser id dict)))
                             (unless pubkey
                               (error "No key for id: ~s at ~d" id pos))
                             (unless (verify msg tok pubkey)
                               (error "Signature verification failed at ~d" pos)))))
                       (setf (gethash $PARSER-MSGKEY dict)
                             (subseq string start (+ pos (length tok))))
                       (cond ((> (length stack) 0)
                              (let ((pop  (vector-pop stack)))
                                (setq value dict
                                      state (pop pop)
                                      dict (pop pop)
                                      dictidx (pop pop)
                                      start (pop pop)
                                      key (pop pop)))
                              (setq needsig t))
                             (t
                              (push dict res)
                              (setq dict nil
                                    needsig nil)))
                       (setq state nil))
                      (t (error "Misplaced value at ~d" pos))))))
      (when needsig
        (error "Premature end of message"))
      (nreverse res))))

(defmethod parser-get-pubkey ((parser parser) id dict)
  (let* ((keydict (parser-keydict parser))
         (keydb (parser-keydb parser))
         (pubkey (gethash id keydict))
         (dict-1 (gethash 1 dict)))
    (when (and (not pubkey)
               (or (equal dict-1 "register")
                   (equal dict-1 "bankid")))
      ;; May be the first time we've seen this ID.
      ;; If it's a key definition message, we're set
      (setq pubkey
            (if (equal dict-1 "register")
                (gethash 3 dict)
                (gethash 2 dict)))
      (let ((pubkeyid (public-key-id pubkey)))
        (if (not (equal id pubkeyid))
            (setq pubkey nil)
            (setf (gethash id keydict) pubkey))))
    (unless pubkey
      (setq pubkey (and keydb (db-get keydb id)))
      (when pubkey
        (unless (equal id (public-key-id pubkey))
          (error "Pubkey doesn't match id: ~s" id))
        (setf (gethash id keydict) pubkey)))
    pubkey))

(defun tokenize (string)
  (let ((res nil)
        (realstart nil)
        (start nil)
        (delims '(#\( #\: #\, #\) #\.))
        (escaped nil)
        (substr ""))
    (loop
       for i from 0
       for chr across string
       do
         (cond ((and (not escaped) (member chr delims))
                (when start
                  (push (cons realstart (strcat substr (subseq string start i)))
                        res)
                  (setq start nil
                        realstart nil
                        substr ""))
                (push (cons i chr) res))
               ((and (not escaped) (eql chr #\\))
                (setq escaped t)
                (when start
                  (setq substr (strcat substr (subseq string start i))
                        start (1+ i))))
               (t
                (unless start
                  (setq start i
                        realstart i))
                (setq escaped nil)))
         finally
         (when start
           (push (cons realstart (strcat substr (subseq string start i)))
                 res)))
    (nreverse res)))

(defmethod get-parsemsg ((parser parser) parse)
  "Return the message string that parsed into the hash table in PARSE."
  (gethash $PARSER-MSGKEY parse))

(defmethod unsigned-message ((parser parser) msg)
  "Return just the message part of a signed message, not including the signature.
   Assumes that the message will parse."
  (let ((pos (search "):" msg :from-end t)))
    (if pos
        (subseq msg 0 (1+ pos))
        msg)))

(defmethod first-message ((parser parser) msg)
  "Return the first message in a list of them.
   Assumes that message parses correctly."
  (let ((pos 0))
    (loop
       (setq pos (position #\. msg :start (1+ pos)))
       (unless pos (return msg))
       (unless (eql (aref msg (1- pos)) #\\)
         (return (subseq msg 0 pos))))))

#||
;; Continue here
  // $parse is an array with numeric and string keys
  // $pattern is an array with numeric keys with string value, and
  // non-numeric keys with non-false values.
  // A numeric key in $pattern must match a numeric key in $parse.
  // A non-numeric key in $pattern must correspond to a numeric key in $parse
  // at the element number in $pattern or the same non-numeric key in $parse.
  // The result maps the non-numeric keys and values in $pattern and
  // their positions, to the matching values in $parse.
  // See the test code below for examples.
  function matchargs($parse, $pattern) {
    $i = 0;
    $res = array();
    foreach ($pattern as $key => $value) {
      if (is_numeric($key)) {
        $name = $value;
        $optional = false;
      } else {
        $name = $key;
        $optional = true;
      }
      $val = @$parse[$i];
      if ($val === NULL) $val = @$parse[$name];
      if (!$optional && $val === NULL) return false;
      if (!($val === NULL)) {
        $res[$name] = $val;
        $res[$i] = $val;
      }
      $i++;
    }
    $msgkey = $this->msgkey;
    foreach ($parse as $key => $value) {
      if ($key != $msgkey && $res[$key] === NULL) return false;
    }
    $res[$msgkey] = @$parse[$msgkey];
    return $res;
  }

  function formatpattern($pattern) {
    $res = "(";
    $comma = false;
    foreach($pattern as $key => $value) {
      if ($comma) $res .= ",";
      $comma = true;
      if (is_numeric($key)) $res .= "<$value>";
      else $res .= "$key=<$key>";
    }
    $res .= ")";
    return $res;
  }

  // Remove the signatures out of a message
  function remove_signatures($msg) {
    $res = "";
    while ($msg) {
      $tail = strstr($msg, "):\n");
      $extralen = 1;
      $matchlen = 3;
      $tail2 = strstr($msg, "\\)\\:\n");
      if (strlen($tail2) > strlen($tail)) {
        $tail = $tail2;
        $extralen = 2;
        $matchlen = 5;
      }
      $i = strlen($msg) - strlen($tail);
      $res .= substr($msg, 0, $i + $extralen);
      $msg = substr($tail, $matchlen);
      $dotpos = strpos($msg, ".");
      $leftpos = strpos($msg, "(");
      $commapos = strpos($msg, ",");
      if ($dotpos === FALSE) $dotpos = $leftpos;
      elseif (!($leftpos === FALSE)) $dotpos = min($dotpos, $leftpos);
      if ($dotpos === FALSE) $dotpos = $commapos;
      elseif (!($commapos === FALSE)) $dotpos = min($dotpos, $commapos);
      $parenpos = strpos($msg, ")");
      if (!($parenpos === false) &&
          ($dotpos === FALSE || $parenpos < $dotpos)) $msg = substr($msg, $parenpos);
      elseif ($dotpos) {
        $res .= "\n";
        $msg = substr($msg, $dotpos);
      } else break;
    }
    return str_replace(",(", ",\n(", $res);
  }

}

||#

;; Test code
#||
$p = new parser(false);
echo "RES: " . $p->remove_signatures("(foo,bar,bletch,(1,2,3):
fjdkf
fjdkf
jfkd
):
jdfksal
jfdkla;
.(a,b,c):
fjkdsal
fjkdsla");
echo "\n";
||#

#||
$parser = new parser(false);
$pattern = array("x", "y", "bletch"=>1);
echo $parser->formatpattern($pattern) . "\n";
$args = $parser->matchargs(array("foo","bar","3"), $pattern);
echo "1: ";
if ($args) print_r($args);
else echo "Didn't match, and I expected it to\n";
$args = $parser->matchargs(array("foo","bar","bletch"=>2), $pattern);
echo "2: ";
if ($args) print_r($args);
else echo "Didn't match, and I expected it to\n";
$args = $parser->matchargs(array("foo","bar","nomatch"=>2), $pattern);
echo "3: ";
if ($args) {
  echo "Didn"t expect this match:\n";
  print_r($args);
}
else echo "Didn't match, as expected\n";
$args = $parser->matchargs(array("foo",""), $pattern);
echo "4: ";
if ($args) print_r($args);
else echo "Didn't match, and I expected it to\n";
$args = $parser->matchargs(array("foo","y"=>""), $pattern);
echo "5: ";
if ($args) print_r($args);
else echo "Didn't match, and I expected it to\n";
$args = $parser->matchargs(array("foo"), $pattern);
echo "6: ";
if ($args) {
  echo "Didn"t expect this match:\n";
  print_r($args);
}
else echo "Didn't match, as expected\n";
||#
#||
require_once "dictdb.php";
$keydb = new dictdb();
$ssl = new ssl();

$privkey = $ssl->make_privkey(512);
$pubkey = $ssl->privkey_to_pubkey($privkey);
$id = $ssl->pubkey_id($pubkey);
$privkey2 = $ssl->make_privkey(512);
$pubkey2 = $ssl->privkey_to_pubkey($privkey2);
$id2 = $ssl->pubkey_id($pubkey2);
$keydb->put($id, $pubkey);
$keydb->put($id2, $pubkey2);
$msg = "($id,\(1\,2\:3\\\\4\.5\),2,x:foo)";
$sig = $ssl->sign($msg, $privkey);
if (!$sig) {
  echo "No signature generated for $msg\n";
  return;
}
$msg = "$msg:$sig";

$msg2 = "($id,$msg,y:$msg)";
$sig2 = $ssl->sign($msg2, $privkey);
$msg .= ".$msg2:$sig2";

$msg3 = "($id2,id,$pubkey2)";
$sig3 = $ssl->sign($msg3, $privkey2);
$msg4 = "($id2,foo,bar)";
$sig4 = $ssl->sign($msg4, $privkey2);
$msg .= ".$msg3:$sig3.$msg4:$sig4.";

echo "$msg\n";
$parser = new parser($keydb, $ssl);
$res = $parser->parse($msg);
if ($res) {
  print_r($res);
} else {
  echo $parser->errmsg;
}
echo "\n";
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
