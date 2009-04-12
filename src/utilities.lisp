; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Various utility functions
;;;

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
                          :if-exists :supersede
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

(defun assocequal (item alist)
  (assoc item alist :test 'equal))

(defun strcat (&rest strings)
  "Concatenate a bunch of strings"
  (apply #'concatenate 'string (mapcar 'string strings)))

(defmacro dotcat (place &rest strings)
  `(setf ,place (strcat ,place ,@strings)))

(defun implode (separator &rest strings)
  (declare (dynamic-extent strings))
  (let ((res (car strings)))
    (dolist (item (cdr strings))
      (setq res (strcat res separator item)))
    res))

(defun explode (separator string)
  (let* ((len (length string))
         (res
          (loop
             with len = (length string)
             for start = 0 then (1+ end)
             while (< start len)
             for end = (or (position separator string :start start) len)
             while end
             collect (subseq string start end))))
    (if (and (> len 0) (eql separator (aref string (1- len))))
        (nconc res (list ""))
        res)))t

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

(defun zero-string (len)
  (make-string len :initial-element #\0))

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
                       (strcat (zero-string (- maxlen len))
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

(defun simple-makemsg (alist)
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

(defvar *patterns* nil)

;; Patterns for non-request data
(defun patterns ()
  (or *patterns*
      (let ((patterns `(;; Customer messages
                        (,$BALANCE .
                         (,$BANKID ,$TIME ,$ASSET ,$AMOUNT (,$ACCT)))
                        (,$OUTBOXHASH . (,$BANKID ,$TIME ,$COUNT ,$HASH))
                        (,$BALANCEHASH . (,$BANKID ,$TIME ,$COUNT ,$HASH))
                        (,$SPEND .
                         (,$BANKID ,$TIME ,$ID ,$ASSET ,$AMOUNT (,$NOTE)))
                        (,$ASSET .
                         (,$BANKID ,$ASSET ,$SCALE ,$PRECISION ,$ASSETNAME))
                        (,$STORAGE . (,$BANKID ,$TIME ,$ASSET ,$PERCENT))
                        (,$STORAGEFEE . (,$BANKID ,$TIME ,$ASSET ,$AMOUNT))
                        (,$FRACTION . (,$BANKID ,$TIME ,$ASSET ,$AMOUNT))
                        (,$REGISTER . (,$BANKID ,$PUBKEY (,$NAME)))
                        (,$SPENDACCEPT . (,$BANKID ,$TIME ,$ID (,$NOTE)))
                        (,$SPENDREJECT . (,$BANKID ,$TIME ,$ID (,$NOTE)))
                        (,$GETOUTBOX .(,$BANKID ,$REQ))
                        (,$GETINBOX . (,$BANKID ,$REQ))
                        (,$PROCESSINBOX . (,$BANKID ,$TIME ,$TIMELIST))
                        (,$STORAGEFEES . (,$BANKID ,$REQ))
                        (,$GETTIME . (,$BANKID ,$TIME))
                        (,$COUPONENVELOPE . (,$ID ,$ENCRYPTEDCOUPON))

                        ;; Bank signed messages
                        (,$FAILED . (,$MSG ,$ERRMSG))
                        (,$TOKENID . (,$TOKENID))
                        (,$BANKID . (,$BANKID))
                        (,$REGFEE . (,$BANKID ,$TIME ,$ASSET ,$AMOUNT))
                        (,$TRANFEE . (,$BANKID ,$TIME ,$ASSET ,$AMOUNT))
                        (,$TIME . (,$ID ,$TIME))
                        (,$INBOX . (,$TIME ,$MSG))
                        (,$REQ . (,$ID ,$REQ))
                        (,$COUPON .
                         (,$BANKURL ,$COUPON ,$ASSET ,$AMOUNT (,$NOTE)))
                        (,$COUPONNUMBERHASH . (,$COUPON))
                        (,$ATREGISTER . (,$MSG))
                        (,$ATOUTBOXHASH . (,$MSG))
                        (,$ATBALANCEHASH . (,$MSG))
                        (,$ATGETINBOX . (,$MSG))
                        (,$ATBALANCE . (,$MSG))
                        (,$ATSPEND . (,$MSG))
                        (,$ATTRANFEE . (,$MSG))
                        (,$ATASSET . (,$MSG))
                        (,$ATSTORAGE . (,$MSG))
                        (,$ATSTORAGEFEE . (,$MSG))
                        (,$ATFRACTION . (,$MSG))
                        (,$ATPROCESSINBOX . (,$MSG))
                        (,$ATSTORAGEFEES . (,$MSG))
                        (,$ATSPENDACCEPT . (,$MSG))
                        (,$ATSPENDREJECT . (,$MSG))
                        (,$ATGETOUTBOX . (,$MSG))
                        (,$ATCOUPON . (,$COUPON ,$SPEND))
                        (,$ATCOUPONENVELOPE . (,$MSG))
                        ))
            (hash (make-hash-table :test 'equal)))
        (loop
           for (key . value) in patterns
           do
             (setf (gethash key hash) value))
        (setq *patterns* hash))))

(defmethod dirhash ((db db) key unpacker &optional newitem removed-names)
  "Return the hash of a directory, KEY, of bank-signed messages.
    The hash is of the user messages wrapped by the bank signing.
    NEWITEM is a new item or an array of new items, not bank-signed.
    REMOVED-NAMES is a list of names in the KEY dir to remove.
    UNPACKER is a function to call with a single-arg, a bank-signed
    message. It returns a parsed and matched ARGS hash table whose $MSG
    element is the parsed user message wrapped by the bank signing.
    Returns two values, the sha1 hash of the items and the number of items."
  (let ((contents (db-contents db key))
        (items nil))
    (dolist (name contents)
      (unless (member name removed-names :test 'equal)
        (let* ((msg (db-get db (strcat key "/" name)))
               (args (funcall unpacker msg))
               (req (gethash $MSG args)))
          (unless req
            (error "Directory msg is not a bank-wrapped message"))
          (unless (setq msg (get-parsemsg req))
            (error "get-parsemsg didn't find anything"))
          (push msg items))))
    (if newitem
        (if (stringp newitem) (push newitem items))
        (setq items (append items newitem)))
    (setq items (sort items 'string-lessp))
    (let* ((str (apply 'implode "." (mapcar 'trim items)))
           (hash (sha1 str)))
      (values hash (length items)))))

(defmethod balancehash ((db db) unpacker balancekey acctbals)
  "Compute the balance hash as two values: hash & count.
   UNPACKER is a function of one argument, a string, representing
   a bank-signed message. It returns the unpackaged bank message
   BALANCEKEY is the key to the user balance directory.
   $acctbals is an alist of alists: ((acct . (assetid . msg)) ...)"
  (let* ((hash nil)
         (hashcnt 0)
         (accts (db-contents db balancekey))
         (needsort nil))
    (loop
       for  acct.bals in acctbals
       for acct = (car acct.bals)
       do
         (unless (member acct acct :test 'equal)
           (push acct accts)
           (setq needsort t)))
    (when needsort (setq accts (sort accts 'string-lessp)))
    (loop
       for acct in accts
       for newitems = nil
       for removed-names = nil
       for newacct = (cdr (assoc acct acctbals :test 'equal))
       do
         (when newacct
           (loop
              for (assetid . msg) in newacct
              do
                (push msg newitems)
                (push assetid removed-names)))
         (multiple-value-bind (hash1 cnt)
             (dirhash db (strcat balancekey "/" acct) unpacker
                      newitems removed-names)
           (setq hash (if hash (strcat hash "." hash1) hash1))
           (incf hashcnt cnt)))
    (when (> hashcnt 1) (setq hash (sha1 hash)))
    (values hash hashcnt)))

(defun hex-char-p (x)
  "Predicate. True if x is 0-9, a-f, or A-F"
  (check-type x character)
  (let ((code (char-code x)))
    (or (and (>= code #.(char-code #\0))
             (<= code #.(char-code #\9)))
        (and (>= code #.(char-code #\a))
             (<= code #.(char-code #\f)))
        (and (>= code #.(char-code #\A))
             (<= code #.(char-code #\F))))))

(defun coupon-p (x)
  "Predicate. True if arg looks like a coupon number."
  (and (stringp x)
       (eql 32 (length x))
       (every 'hex-char-p x)))

(defun id-p (x)
  "Predicate. True if arg looks like an ID."
  (and (stringp x)
       (eql 40 (length x))
       (every 'hex-char-p x)))

(defun fraction-digits (percent)
  "Calculate the number of digits to keep for the fractional balance.
   Add 3 for divide by 365, 2 for percent, 3 more for 1/1000 precision."
  (+ (number-precision percent) 8))

(defun storage-fee (balance baltime now percent digits)
  "Calculate the storage fee.
   BALANCE is the balance.
   BALTIME is the time of the BALANCE, as an integer string.
   NOW is the current time, as an integer string.
   PERCENT is the storage fee rate, in percent/year.
   DIGITS is the precision for the arithmetic.
   Returns two values:
    1) the storage fee
    2) balance - storage-fee"
  (wbp (digits)
    (cond ((eql 0 (bccomp percent 0))
           (values "0" balance))
          (t (let* ((secs-per-year-pct (* 60 60 24 365 100))
                    (fee (bcdiv (bcmul balance
                                       percent
                                       (wbp (0) (bcsub now baltime)))
                                secs-per-year-pct)))
               (cond ((< (bccomp fee 0) 0)
                      (setq fee  "0"))
                     ((> (bccomp fee balance) 0)
                      (setq fee balance)))
               (values fee (bcsub balance fee)))))))

(defun normalize-balance (balance fraction digits)
  "Add together BALANCE & FRACTION, to DIGITS precision.
   Return two values, the integer part and the fractional part."
  (wbp (digits)
    (split-decimal (bcadd balance fraction))))

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
