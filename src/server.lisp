; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Trubanc server
;;;

(in-package :trubanc)

(defun make-server (dir passphrase bankname bankurl)
  "Create a Trubanc server instance."
  (let ((db (make-fsdb dir)))
    (make-instance 'server
                   :db db
                   :passphrase passphrase
                   :bankname bankname
                   :bankurl bankurl)))

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
             :accessor pubkeydb)
   (bankname :type string
             :initarg :bankname
             :initform "A Random Trubanc"
             :accessor bankname)
   (bankurl :type (or string null)
            :initarg :bankurl
            :initform nil
            :accessor bankurl)
   (tokenid :type string
            :accessor tokenid)
   (regfee :type string
           :initarg :regfee
           :initform "10"
           :accessor regfee)
   (tranfee :type string
            :initarg :tranfee
            :initform "2"
            :accessor tranfee)
   (privkey :accessor privkey)
   (bankid :type (or string null)
           :initform nil
           :accessor bankid)
   (always-verify-sigs-p :type boolean
                         :initarg :always-verify-sigs-p
                         :initform t
                         :accessor always-verify-sigs-p)
   (debugmsgs-p :type boolean           ;of strings
                :initform nil
                :accessor debugmsgs-p)
   (debugstr :type string
             :initform ""
             :accessor debugstr)))

(defconstant $UNPACK-REQS-KEY "unpack-reqs")

(defmethod initialize-instance :after ((server server) &key db passphrase)
  (let ((pubkeydb (db-subdir db $PUBKEY)))
    (setf (pubkeydb server) pubkeydb
          (parser server) (make-instance
                           'parser
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
        (unless (equal (getarg $REQUEST args) $ATSTORAGE)
          (return-from storage-info nil))
        (setq req (getarg $MSG args)
              args (match-pattern parser req))
        (unless (equal (getarg $REQUEST args) $STORAGE)
          (return-from storage-info nil))
        (let ((issuer (getarg $CUSTOMER args))
              (percent (getarg $PERCENT args)))
          (let* ((fraction nil)
                 (fractime nil)
                 (key (fraction-balance-key id assetid))
                 (msg (db-get db key)))
            (when msg
              (setq args (unpack-bankmsg server msg $ATFRACTION $FRACTION)
                    fraction (getarg $AMOUNT args)
                    fractime (getarg $TIME args)))
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
             $OUTBOXHASH
             (bankid server)
             (get-acct-last server id)
             count
             hash)))

(defun balance-hash-key (id)
  (strcat (account-dir id) $BALANCEHASH))

(defmethod is-asset-p ((server server) assetid)
  "Returns the message defining ASSETID, or NIL, if there isn't one."
  (db-get (db server) (strcat $ASSET "/" assetid)))

(defmethod lookup-asset ((server server) assetid)
  (let ((asset (is-asset-p server assetid)))
    (when asset
      (let ((res (unpack-bankmsg server asset $ATASSET $ASSET)))
        (let ((req1 (elt 1 (getarg $UNPACK-REQS-KEY res))))
          (when req1
            (let ((args (match-pattern (parser server) req1)))
              (setf (getarg $PERCENT res) (getarg $PERCENT args)))))
        res))))

(defmethod lookup-asset-name ((server server) assetid)
  (let ((assetreq (lookup-asset server assetid)))
    (and assetreq
         (getarg $ASSETNAME assetreq))))

;; More restrictive than Lisp's alphanumericp
(defun is-alphanumeric-p (char)
  (let ((code (char-code char)))
    (or (and (>= code #.(char-code #\0)) (<= code #.(char-code #\9)))
        (and (>= code #.(char-code #\a)) (<= code #.(char-code #\z)))
        (and (>= code #.(char-code #\A)) (<= code #.(char-code #\Z))))))

(defun is-numeric-p (string)
  (loop
     with firstp = t
     with sawdotp = nil
     for chr across string
     for code = (char-code chr)
     do
     (cond ((eql chr #\-)
            (unless firstp (return nil)))
           ((eql chr #\.)
            (when sawdotp (return nil))
            (setq sawdotp t))
           (t (unless (and (>= code #.(char-code #\0))
                           (<= code #.(char-code #\9)))
                (return nil))))
     (setq firstp nil)
     finally (return t)))

(defun is-acct-name-p (acct)
  (and (stringp acct)
       (every 'is-alphanumeric-p acct)))

(defun pubkey-id (pubkey)
  (sha1 (trim pubkey)))

(defmethod unpack-bank-param ((server server) type &optional (key type))
  "Unpack wrapped initialization parameter."
  (unpack-bankmsg server (db-get (db server) type) type nil key))

(defmethod banksign ((server server) msg)
  "Bank sign a message."
  (let ((sig (sign msg (privkey server))))
    (strcat msg #.(format nil ":~%") sig)))

(defmethod makemsg ((server server) &rest req)
  "Make an unsigned message from the args."
  (let ((hash (make-hash-table :test 'equal))
        (i -1))
    (dolist (arg req) (setf (gethash (incf i) hash) arg))
    (loop
       with args = (match-pattern (parser server) hash)
       with msg = "("
       with msgval = (getarg $MSG args)
       for k from 0
       for v = (getarg k args)
       do
         (unless v (return (strcat msg ")")))
         (unless (equal msg "(")
           (setq msg (strcat msg ",")))
         (setq msg (strcat msg (if (eq v msgval) v (escape v)))))))

(defmethod bankmsg ((server server) &rest req)
  "Make a bank signed message from the args."
  (banksign server (apply 'makemsg server (bankid server) req)))

(defun shorten-failmsg-msg (msg)
  (if (> (length msg) 1024)
      (strcat (subseq msg 0 1021) "...")
      msg))

(defmethod failmsg ((server server) &rest req)
  (when (stringp (car req))
    (setf (car req) (shorten-failmsg-msg (car req))))
  (apply 'bankmsg server (bankid server) $FAILED req))


(defmethod unpack-bankmsg ((server server) msg &optional type subtype idx)
  "Reverse the bankmsg() function, optionally picking one field to return."
  (let* ((bankid (bankid server))
         (parser (parser server))
         (reqs (parse parser msg))
         (req (elt reqs 0))
         (args (match-pattern parser req)))
    (when (and bankid (not (equal (getarg $CUSTOMER args) bankid)))
      (error "Bankmsg not from bank"))
    (when (and type (not (equal (getarg $REQUEST args) type)))
      (error "Bankmsg wasn't of type: ~s" type))
    (cond ((not subtype)
           (cond (idx
                  (or (getarg idx args)
                      (error "No arg in bankmsg: ~s" idx)))
                 (t (setf (getarg $UNPACK-REQS-KEY args) reqs) ; save parse results
                    args)))
          (t (setq req (getarg $MSG args)) ;this is already parsed
             (unless req
               (error "No wrapped message"))
             (setq args (match-pattern parser req))
             (when (and subtype (not (equal (getarg $REQUEST args) subtype)))
               (error "Wrapped message wasn't of type: ~%" subtype))
             (cond (idx
                    (or (getarg idx args)
                        (error "No arg with idx: ~s" idx)))
                   (t (setf (getarg $UNPACK-REQS-KEY args) reqs) ; save parse results
                      args))))))

(defmethod setup-db ((server server) passphrase)
  "Initialize the database, if it needs initializing."
  (let ((db (db server)))
    (if (db-get db $PRIVKEY)
        (setf (privkey server) (decode-rsa-private-key
                                (db-get db $PRIVKEY) passphrase)
              (bankid server) (unpack-bank-param server $BANKID)
              (tokenid server) (unpack-bank-param server $TOKENID)
              (regfee server) (unpack-bank-param server $REGFEE $AMOUNT)
              (tranfee server) (unpack-bank-param server $TRANFEE $AMOUNT))
        ;; http://www.rsa.com/rsalabs/node.asp?id=2004 recommends that 3072-bit
        ;; RSA keys are equivalent to 128-bit symmetric keys, and they should be
        ;; secure past 2031.
        (let* ((privkey (rsa-generate-key 3072))
               (privkey-text (encode-rsa-private-key privkey passphrase))
               (pubkey (encode-rsa-public-key privkey))
               (bankid (pubkey-id pubkey))
               (bankname (bankname server))
               (ut "Usage Tokens")
               (token-name (if bankname (strcat bankname " " ut) ut))
               (zero "0")
               (tokenid (assetid bankid zero zero token-name)))
          (db-put db $PRIVKEY privkey-text)
          (setf (privkey server) privkey
                (bankid server) bankid)
          (db-put db $TIME zero)
          (db-put db $BANKID (bankmsg server $BANKID bankid))
          (db-put db (strcat $PUBKEY "/" bankid) pubkey)
          (db-put db (strcat $PUBKEYSIG "/" bankid)
                  (bankmsg server $ATREGISTER
                           (bankmsg server $REGISTER
                                    bankid
                                    (strcat #.(format nil "~%") pubkey)
                                    bankname)))
          (setf (tokenid server) tokenid)
          (db-put db $TOKENID (bankmsg server $TOKENID tokenid))
          (db-put db (strcat $ASSET "/" tokenid)
                  (bankmsg server $ATASSET
                           (bankmsg server $ASSET
                                    bankid tokenid zero zero token-name)))
          (db-put db $REGFEE
                  (bankmsg server $REGFEE bankid zero tokenid (regfee server)))
          (db-put db $TRANFEE
                  (bankmsg server $TRANFEE bankid zero tokenid (tranfee server)))
          (db-put db (acct-time-key bankid) zero)
          (db-put db (acct-last-key bankid) zero)
          (db-put db (acct-req-key bankid) zero)
          (db-put db (asset-balance-key bankid tokenid)
                  (bankmsg server $ATBALANCE
                           (bankmsg server $BALANCE
                                    bankid zero tokenid "-1")))))))

(defmethod scaninbox ((server server) id)
  (loop
     with db = (db server)
     with inbox-key = (inbox-key id)
     with times = (db-contents db inbox-key)
     for time in times
     for item = (db-get db (strcat inbox-key "/" time))
     when item
     collect item))

(defmethod signed-balance (server time asset amount &optional acct)
  (if acct
      (bankmsg server $BALANCE (bankid server) time asset amount acct)
      (bankmsg server $BALANCE (bankid server) time asset amount)))

(defmethod signed-spend ((server server) time id assetid amount &optional note)
  (let ((bankid (bankid server)))
    (if note
        (bankmsg server $SPEND bankid time id assetid amount note)
        (bankmsg server $SPEND bankid time id assetid amount))))

(defmethod enq-time ((server server) id)
  (let* ((db (db server))
         (time (gettime server))
         (key (acct-time-key id)))
    (with-db-lock (db key)
      (let ((q (db-get db key)))
        (if (not q)
            (setq q time)
            (setq q (strcat q "," time)))
        (db-put db key q)
        q))))

(defmethod deq-time (server id time)
  (let ((db (db server))
        (key (acct-time-key id)))
    (with-db-lock (db key)
      (let ((q (db-get db key))
            (res nil))
        (when q
          (let ((times (explode #\, q)))
            (dolist (v times)
              (when (equal v time)
                (setq res time
                      times (delete v times :test 'equal)
                      q (apply 'implode #\, times))
                (db-put db key q)))))
        (unless res (error "Timestamp not enqueued: ~s" time))
        (when (> (bccomp (get-universal-time) (bcadd time (* 10 60)))
                 0)
          (error "Timestamp too old: ~s" time))))
    time))

(defmethod add-to-bank-balance ((server server) assetid amount)
  "Add AMOUNT to the bank balance for ASSETID in the main account.
   Returns new balance, unless you add 0, then it returns nil."
  (let ((bankid (bankid server))
        (db (db server)))
    (unless (eql 0 (bccomp amount 0))
      (let ((key (asset-balance-key bankid assetid)))
        (with-db-lock (db key)
          (let* ((balmsg (db-get db key))
                 (balargs (unpack-bankmsg server balmsg $ATBALANCE $BALANCE)))
            (unless (or (null (getarg $ACCT balargs))
                        (equal $MAIN (getarg $ACCT balargs)))
              (error "Bank balance message not for main account"))
            (let* ((bal (getarg $AMOUNT balargs))
                   (newbal (bcadd bal amount))
                   (balsign (bccomp bal 0))
                   (newbalsign (bccomp newbal 0)))
              (when (or (and (>= balsign 0) (< newbalsign 0))
                        (and (< balsign 0) (>= newbalsign 0)))
                (error "Transaction would put bank out of balance."))
              ;; $BALANCE => `(,$BANKID ,$TIME ,$ASSET ,$AMOUNT (,$ACCT))
              (let ((msg (bankmsg server $ATBALANCE
                                  (bankmsg server $BALANCE
                                           bankid (gettime server) assetid newbal)))
                    (reqkey (acct-req-key bankid)))
                (db-put db key msg)
                ;; Make sure clients update the balance
                (db-put db reqkey (bcadd 1 (db-get db reqkey)))
                newbal))))))))

(defmethod checkreq (server args)
  (let* ((db (db server))
         (id (getarg $CUSTOMER args))
         (req (getarg $REQ args))
         (reqkey (acct-req-key id)))
    (with-db-lock (db reqkey)
      (let ((oldreq (db-get db reqkey)))
        (when (<= (bccomp req oldreq) 0)
          (error "New req <= old req"))
        (db-put db reqkey req)))))

(defstruct balance-state
  acctbals
  bals
  tokens
  accts
  oldneg
  newneg
  time
  charges)

(defmethod handle-balance-msg ((server server) id msg args state &optional
                               creating-asset)
  "Deal with an (<id>,balance,...) item from the customer for a
   spend or processinbox request.
   ID: the customer id
   MSG: the signed (<id>,balance,...) message, as a string
   ARGS: parser->parse(), then utility->match_pattern() output on balmsg
   STATE: a BALANCE-STATE instance.
     acctbals => alist: ((<acct> (<asset> . msg) ...) ...)
     bals => alist: ((<asset> . <amount>) ...)
     tokens => total new /account/<id>/balance/<acct>/<asset> files
     accts => list: (<acct> ...)
     oldneg => alist: ((<asset> . <acct>) ...), negative balances in current account
     newneg => alist: ((<asset> . <acct>) ...), negative balances in updated account
     time => the transaction time"
  (let ((db (db server))
        (bankid (bankid server))
        (asset (getarg $ASSET args))
        (amount (getarg $AMOUNT args))
        (acct (or (getarg $ACCT args) $MAIN)))

    (pushnew acct (balance-state-accts state))

    (when (and (or (not creating-asset) (not (equal asset creating-asset)))
               (not (is-asset-p server asset)))
      (error "Unknown asset id: ~s" asset))

    (unless (is-numeric-p amount)
      (error "Not a number: ~s" amount))

     (unless (is-acct-name-p acct)
       (error "<acct> may contain only letters and digits: ~s" acct))

     (let ((acct-cell (assocequal (balance-state-acctbals state) acct)))
       (unless acct-cell
         (setq acct-cell
               (car (push (cons acct nil) (balance-state-acctbals state)))))
       (when (assocequal asset (cdr acct-cell))
         (error "Duplicate acct/asset balance pair: ~s/~s" acct asset))

       (push (cons asset msg) (cdr acct-cell)))

     (let ((bal-cell (assocequal asset (balance-state-bals state))))
       (unless bal-cell
         (setq bal-cell (car (push (cons asset 0) (balance-state-bals state)))))
       (setf (cdr bal-cell) (bcsub (cdr bal-cell) amount))

       (when (< (bccomp amount 0) 0)
         (when (assocequal asset (balance-state-newneg state))
           (error "Multiple new negative balances for asset: ~s" asset))
         (push (cons asset acct) (balance-state-newneg state)))

       (let* ((asset-balance-key (asset-balance-key id asset acct))
              (acctmsg (db-get db asset-balance-key)))
         (if (not acctmsg)
             (unless (equal id bankid) (incf (balance-state-tokens state)))
             (let* ((acctargs (unpack-bankmsg server acctmsg $ATBALANCE $BALANCE))
                    (amount (getarg $AMOUNT acctargs)))
               (setf (cdr bal-cell) (bcadd (cdr bal-cell) amount))
               (when (< (bccomp amount 0) 0)
                 (when (assocequal asset (balance-state-oldneg state))
                   (error "Account corrupted. ~
                           Multiple negative balances for asset: ~s"
                          asset))
                 (push (cons asset acct) (balance-state-oldneg state)))
               (compute-storage-charges server id state asset amount acctargs)))))))

(defstruct storage-info
  percent
  issuer
  fraction
  digits
  fee)

;; This is separate from handle-balance-msg mostly to get back some
;; horizontal text space. Nobody else calls it.
(defmethod compute-storage-charges ((server server) id state asset amount acctargs)
  "Compute storage charges as a list of storage-info instances"
  (let ((asset-info (cdr (assocequal asset (balance-state-charges state)))))
    (unless asset-info
      (unless (equal asset (tokenid server))
        (multiple-value-bind (percent issuer fraction fractime)
            (storage-info server id asset)
          (setq asset-info (make-storage-info :percent percent
                                              :issuer issuer
                                              :fraction fraction
                                              :fee 0))
          (push (cons asset asset-info) (balance-state-charges state))
          (when percent
            (let ((digits (fraction-digits percent))
                  (time (balance-state-time state)))
              (when fraction
                (setf (storage-info-fee asset-info)
                      (storage-fee fraction fractime time percent digits)))
              (setf (storage-info-digits asset-info) digits))))))
    (when asset-info
      (let ((percent (storage-info-percent asset-info)))
        (when (and percent (> (bccomp amount 0) 0))
          (let* ((digits (storage-info-digits asset-info))
                 (accttime (getarg $TIME acctargs))
                 (time (balance-state-time state))
                 (fee (storage-fee amount accttime time percent digits)))
            (setf (storage-info-fee asset-info)
                  (bcadd (storage-info-fee asset-info) fee digits))))))))

(defmethod debugmsg ((server server) msg)
  (when (debugmsgs-p server)
    (setf (debugstr server) (strcat (debugstr server) msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Request processing
;;;
 
(defvar *message-handlers* (make-hash-table :test 'equal))

(defun get-message-handler (message)
  (gethash message *message-handlers*))

(defun (setf get-message-handler) (value message)
  (setf (gethash message *message-handlers*) value))

(defmacro define-message-handler (name message (server args reqs) &body body)
  `(progn
     (setf (get-message-handler ,message) ',name)
     (defmethod ,name ((,server server) ,args ,reqs) ,@body)))

(define-message-handler do-bankid $BANKID (server inargs reqs)
  "Look up the bank's public key"
  (declare (ignore reqs))
  (let* ((db (db server))
         (bankid (bankid server))
         (coupon (getarg $COUPON inargs))
         (msg (db-get db (strcat $PUBKEYSIG "/" bankid)))
         (args (unpack-bankmsg server msg $ATREGISTER))
         (req (getarg $MSG args))
         (res (get-parsemsg req)))

    (when coupon
      ;; Validate a coupon number
      (let* ((coupon-number-hash (sha1 coupon))
             (key (strcat $COUPON "/" coupon-number-hash))
             (coupon (db-get db key)))
        (unless coupon
          (error "Coupon invalid or already redeemed"))
        (setq args (unpack-bankmsg server coupon $ATSPEND $SPEND))
        (let ((assetid (getarg $ASSET args))
              (amount (getarg $AMOUNT args))
              (id (getarg $CUSTOMER inargs)))
          (unless (db-get db (acct-last-key id))
            ;; Not already registered, coupon must be for usage tokens,
            ;; and must be big enough to register with
            (unless (equal assetid (tokenid server))
              (error "Coupon not for usage tokens"))
            (when (< (bccomp amount (bcadd (regfee server) 10)) 0)
              (error "It costs ~a + 10 usage tokens to register a new account."
                     (regfee server)))))
        (let ((msg (bankmsg server $COUPONNUMBERHASH coupon-number-hash)))
          (setq res (strcat res "." msg)))))

    res))

(define-message-handler do-id $ID (server args reqs)
  "Lookup a public key."
  (declare (ignore reqs))
  (or (db-get (db server)
              (strcat $PUBKEYSIG "/" (getarg $ID args)))
      (error "No such public key")))

(define-message-handler do-register $REGISTER (server args reqs)
  (let ((db (db server))
        (parser (parser server))
        (id (getarg $CUSTOMER args))
        (pubkey (getarg $PUBKEY args)))
    (when (db-get db (acct-last-key id))
      (error "Already registered"))
    (unless (equal (pubkey-id pubkey) id)
      (error "Pubkey doesn't match ID"))
    (when (> (pubkey-bits pubkey) 4096)
      (error "Key sizes larger than 4096 not allowed"))

    ;; Process included coupons
    (let ((reqargs-list nil))
      (dolist (req (cdr reqs))
        (let* ((reqargs (match-pattern parser req))
               (request (getarg $REQUEST reqargs)))
          (unless (equal request $COUPONENVELOPE)
            (error "Non-coupon request with register: ~s" request))
          (push reqargs reqargs-list)))

      (dolist (reqargs (nreverse reqargs-list))
        (do-couponenvelope-raw server reqargs id)))

    ;; Ensure we have enough tokens to register
    (let ((regfee (regfee server))
          (tokenid (tokenid server)))
      (when (> (bccomp regfee 0) 0)
        (let ((inbox (scaninbox server id)))
          (dolist (inmsg inbox
                   (error "Insufficient asset tokens for registration fee"))
            (let ((inmsg-args (unpack-bankmsg server inmsg nil t)))
              (when (equal $SPEND (getarg $REQUEST inmsg-args))
                (let ((asset (getarg $ASSET inmsg-args))
                      (amount (getarg $AMOUNT inmsg-args)))
                  (when (and (equal asset tokenid) (> (bccomp amount regfee) 0))
                    (return))))))))

      ;; Create the account
      (let* ((msg (get-parsemsg (car reqs)))
             (res (bankmsg server $ATREGISTER msg))
             (time (gettime server)))
        (db-put db (strcat $PUBKEY "/" id) pubkey)
        (db-put db (strcat $PUBKEYSIG "/" id) res)
        ;; Post the debit for the registration fee
        (when (> (bccomp regfee 0)  0)
          (let* ((spendmsg (bankmsg server
                                    $INBOX
                                    time
                                    (signed-spend
                                     server time id tokenid (bcsub 0 regfee)
                                     "Registration fee"))))
            (db-put db (strcat (inbox-key id) "/" time) spendmsg)))
        ;; Mark the account as created
        (db-put db (acct-last-key id) "1")
        (db-put db (acct-req-key id) "0")
        res))))

(define-message-handler do-getreq $GETREQ (server args reqs)
  "Process a getreq message"
  (declare (ignore reqs))
  (let ((id (getarg $CUSTOMER args)))
    (bankmsg server $REQ id (db-get (db server) (acct-req-key id)))))

(define-message-handler do-gettime $GETTIME (server args reqs)
  "Process a time request."
  (declare (ignore reqs))
  (checkreq server args)
  (let ((db (db server))
        (id (getarg $CUSTOMER args)))
    (with-db-lock (db (acct-time-key id))
      (let ((time (gettime server)))
        (db-put db (acct-time-key id) time)
        (bankmsg server $TIME id time)))))

(define-message-handler do-getfees $GETFEES (server args reqs)
  (declare (ignore reqs))
  (checkreq server args)
  (let* ((db (db server))
         (regfee (db-get db $REGFEE))
         (tranfee (db-get db $TRANFEE)))
    (strcat regfee "." tranfee)))

(define-message-handler do-spend $SPEND (server args reqs)
  "Process a spend message."
  (with-verify-sigs-p ((parser server) nil)
    (let ((db (db server))
          (id (getarg $CUSTOMER args))
          res assetid issuer storagefee digits)
      (with-db-lock (db (acct-time-key id))
        (multiple-value-setq (res assetid issuer storagefee digits)
          (do-spend-internal server args reqs)))

      ;; This is outside the customer lock to avoid deadlock with the issuer account.
      (when storagefee
        (post-storage-fee server assetid issuer storagefee digits))

      res)))

#||
;; Continue here
  function do_spend_internal($args, $reqs, $msg,
                             &$ok, &$assetid, &$issuer, &$storagefee, &$digits) {
    $t = $this->t;
    $u = $this->u;
    $db = $this->db;
    $bankid = $this->bankid;
    $parser = $this->parser;

    $ok = false;

    // $SPEND => array($BANKID,$TIME,$ID,$ASSET,$AMOUNT,$NOTE=>1),
    $id = $args[$CUSTOMER];
    $time = $args[$TIME];
    $id2 = $args[$ID];
    $assetid = $args[$ASSET];
    $amount = $args[$AMOUNT];
    $note = @$args[$NOTE];

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
      if ($last < 0 || bccomp($inmsg_args[$TIME], $last) <= 0) {
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
      $reqid = $reqargs[$CUSTOMER];
      $request = $reqargs[$REQUEST];
      $reqtime = $reqargs[$TIME];
      if ($reqtime != $time) return $this->failmsg($msg, "Timestamp mismatch");
      if ($reqid != $id) return $this->failmsg($msg, "ID mismatch");
      $reqmsg = $parser->get_parsemsg($req);
      if ($request == $TRANFEE) { 
       if ($feemsg) {
          return $this->failmsg($msg, $TRANFEE . ' appeared multiple times');
        }
        $tranasset = $reqargs[$ASSET];
        $tranamt = $reqargs[$AMOUNT];
        if ($tranasset != $tokenid || $tranamt != $tokens) {
          return $this->failmsg($msg, "Mismatched tranfee asset or amount ($tranasset <> $tokenid || $tranamt <> $tokens)");
        }
        $feemsg = $this->bankmsg($ATTRANFEE, $reqmsg);
      } elseif ($request == $STORAGEFEE) {
        if ($storagemsg) {
          return $this->failmsg($msg, $STORAGEFEE . ' appeared multiple times');
        }
        $storageasset = $reqargs[$ASSET];
        $storageamt = $reqargs[$AMOUNT];
        if ($storageasset != $assetid) {
          return $this->failmsg($msg, "Storage fee asset id doesn't match spend");
        }
        $storagemsg = $this->bankmsg($ATSTORAGEFEE, $reqmsg);
      } elseif ($request == $FRACTION) {
        if ($fracmsg) {
          return $this->failmsg($msg, $FRACTION . ' appeared multiple times');
        }
        $fracasset = $reqargs[$ASSET];
        $fracamt = $reqargs[$AMOUNT];
        if ($fracasset != $assetid) {
          return $this->failmsg($msg, "Fraction asset id doesn't match spend");
        }
        $fracmsg = $this->bankmsg($ATFRACTION, $reqmsg);
      } elseif ($request == $BALANCE) {
        if ($time != $reqargs[$TIME]) {
          return $this->failmsg($msg, "Time mismatch in balance item");
        }
        $errmsg = $this->handle_balance_msg($id, $reqmsg, $reqargs, $state);
        if ($errmsg) return $this->failmsg($msg, $errmsg);
        $newbals[] = $reqmsg;
      } elseif ($request == $OUTBOXHASH) {
        if ($outboxhashreq) {
          return $this->failmsg($msg, $OUTBOXHASH . " appeared multiple times");
        }
        if ($time != $reqargs[$TIME]) {
          return $this->failmsg($msg, "Time mismatch in outboxhash");
        }
        $outboxhashreq = $req;
        $outboxhashmsg = $reqmsg;
        $outboxhash = $reqargs[$HASH];
        $outboxhashcnt = $reqargs[$COUNT];
      } elseif ($request == $BALANCEHASH) {
        if ($balancehashreq) {
          return $this->failmsg($msg, $BALANCEHASH . " appeared multiple times");
        }
        if ($time != $reqargs[$TIME]) {
          return $this->failmsg($msg, "Time mismatch in balancehash");
        }
        $balancehashreq = $req;
        $balancehash = $reqargs[$HASH];
        $balancehashcnt = $reqargs[$COUNT];
        $balancehashmsg = $reqmsg;
      } else {
        return $this->failmsg($msg, "$request not valid for spend. Only " .
                              $TRANFEE . ', ' . $BALANCE . ", and " .
                              $OUTBOXHASH);
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
      return $this->failmsg($msg, $TRANFEE . " missing");
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
        return $this->failmsg($msg, $OUTBOXHASH . " missing");
      } else {
        $hasharray = $this->outboxhash($id, $spendmsg);
        $hash = $hasharray[$HASH];
        $hashcnt = $hasharray[$COUNT];
        if ($outboxhash != $hash || $outboxhashcnt != $hashcnt) {
          return $this->failmsg($msg, $OUTBOXHASH . ' mismatch');
        }
      }
    }

    // balancehash must be included, except on bank spends
    if ($id != $bankid) {
      if (!$balancehashreq) {
        return $this->failmsg($msg, $BALANCEHASH . " missing");
      } else {
        $hasharray = $u->balancehash($db, $id, $this, $acctbals);
        $hash = $hasharray[$HASH];
        $hashcnt = $hasharray[$COUNT];
        if ($balancehash != $hash || $balancehashcnt != $hashcnt) {
          return $this->failmsg($msg, $BALANCEHASH . " mismatch, hash sb: $hash, was: $balancehash, count sb: $hashcnt, was: $balancehashcnt");
        }
      }
    }

    // All's well with the world. Commit this puppy.
    // Eventually, the commit will be done as a second phase.
    $outbox_item = $this->bankmsg($ATSPEND, $spendmsg);
    if ($feemsg) {
      $outbox_item .= ".$feemsg";
    }
    $res = $outbox_item;

    $newtime = false;
    if ($id2 != $COUPON) {
      if ($id != $id2) {
        $newtime = $this->gettime();
        $inbox_item = $this->bankmsg($INBOX, $newtime, $spendmsg);
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
        $coupon = $this->bankmsg($COUPON, $bankurl, $coupon_number, $assetid, $amount, $note);
      } else {
        $coupon = $this->bankmsg($COUPON, $bankurl, $coupon_number, $assetid, $amount);
      }
      $coupon_number_hash = sha1($coupon_number);
      $db->put($COUPON . "/$coupon_number_hash", "$outbox_item");
      $pubkey = $this->pubkeydb->get($id);
      $coupon = $ssl->pubkey_encrypt($coupon, $pubkey);
      $coupon = $this->bankmsg($COUPONENVELOPE, $id, $coupon);
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
        $balance = $this->bankmsg($ATBALANCE, $balance);
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
      $outboxhash_item = $this->bankmsg($ATOUTBOXHASH, $outboxhashmsg);
      $res .= ".$outboxhash_item";
      $db->put($this->outboxhashkey($id), $outboxhash_item);

      // Append spend to outbox
      $db->put($this->outboxdir($id) . "/$time", $outbox_item);
    }

    if ($id != $bankid) {
      // Update balancehash
      $balancehash_item = $this->bankmsg($ATBALANCEHASH, $balancehashmsg);
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
      if ($args[$CUSTOMER] != $bankid ||
          $args[$REQUEST] != $STORAGEFEE ||
          $args[$BANKID] != $bankid ||
          $args[$ASSET] != $assetid) {
        return "Storage fee message malformed";
      }
      $amount = $args[$AMOUNT];
      $storagefee = bcadd($storagefee, $amount, $digits);
    }
    $time = $this->gettime();
    $storagemsg = $this->bankmsg($STORAGEFEE, $bankid, $time, $assetid, $storagefee);
    $db->put($key, $storagemsg);
  }

  // Process a spend|reject
  function do_spendreject($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;
    $parser = $this->parser;

    $parser->verifysigs(false);

    $id = $args[$CUSTOMER];
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

    $time = $args[$TIME];
    $key = $this->outboxkey($id);
    $item = $db->get("$key/$time");
    if (!$item) return $this->failmsg($msg, "No outbox entry for time: $time");
    $args = $this->unpack_bankmsg($item, $ATSPEND, $SPEND);
    if (is_string($args)) return $this->failmsg($msg, $args);
    if ($time != $args[$TIME]) {
      return $this->failmsg($msg, "Time mismatch in outbox item");
    }
    if ($args[$ID] == $COUPON) {
      return $this->failmsg($msg, "Coupons must be redeemed, not cancelled");
    }
    $recipient = $args[$ID];
    $key = $this->inboxkey($recipient);
    $inbox = $db->contents($key);
    foreach ($inbox as $intime) {
      $item = $db->get("$key/$intime");
      // Unlikely, but possible
      if (!$item) return $this->failmsg($msg, "Spend has already been processed");
      $args = $this->unpack_bankmsg($item, $INBOX, $SPEND);
      if (is_string($args)) return $this->failmsg($msg, $args);
      if ($args[$TIME] == $time) {
        // Calculate the fee, if there is one
        $feeamt = '';
        $reqs = $args[$this->unpack_reqs_key];
        $req = $reqs[1];
        if ($req) {
          $args = $u->match_pattern($req);
          if (is_string($args)) return $this->failmsg($msg, $args);
          if ($args[$CUSTOMER] != $bankid) {
            return $this->failmsg($msg, "Fee message not from bank");
          }
          if ($args[$REQUEST] == $ATTRANFEE) {
            $req = $args[$MSG];
            $args = $u->match_pattern($req);
            if (is_string($args)) return $this->failmsg($msg, $args);
            if ($args[$REQUEST] != $TRANFEE) {
              return $this->failmsg($msg, "Fee wrapper doesn't wrap fee message");
            }
            $feeasset = $args[$ASSET];
            $feeamt = $args[$AMOUNT];
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
        $item = $this->bankmsg($INBOX, $newtime, $msg);
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
    
    $id = $args[$CUSTOMER];
    $lock = $db->lock($this->accttimekey($id));
    $res = $this->do_couponenvelope_internal($args, $msg, $id);
    $db->unlock($lock);
    return $res;
  }

  function do_couponenvelope_internal($args, $msg, $id) {
    $t = $this->t;

    $res = $this->do_couponenvelope_raw($args, $id);
    if (is_string($res)) return $this->failmsg($msg, $res);
    return $this->bankmsg($ATCOUPONENVELOPE, $msg);
  }

  // Called by do_register to process coupons there
  // Returns an error string or false
  function do_couponenvelope_raw($args, $id) {
    $t = $this->t;
    $db = $this->db;
    $bankid = $this->bankid;
    $parser = $this->parser;
    $u = $this->u;

    $encryptedto = $args[$ID];
    if ($encryptedto != $bankid) {
      return "Coupon not encrypted to bank";
    }
    $coupon = $args[$ENCRYPTEDCOUPON];
    $coupon = $this->ssl->privkey_decrypt($coupon, $this->privkey);
    if ($u->is_coupon_number($coupon)) {
      $coupon_number = $coupon;
    } else {
      $args = $this->unpack_bankmsg($coupon, $COUPON);
      if (is_string($args)) return "Error parsing coupon: $args";
      if ($bankid != $args[$CUSTOMER]) {
        return "Coupon not signed by bank";
      }
      $coupon_number = $args[$COUPON];
    }

    $coupon_number_hash = sha1($coupon_number);

    $key = $COUPON . "/$coupon_number_hash";
    $lock = $db->lock($key);
    $outbox_item = $db->get($key);
    if ($outbox_item) $db->put($key, '');
    $db->unlock($lock);

    if (!$outbox_item) {
      $key = $this->inboxkey($id);
      $inbox = $db->contents($key);
      foreach ($inbox as $time) {
        $item = $db->get("$key/$time");
        if (strstr($item, ',' . $COUPON . ',')) {
          $reqs = $parser->parse($item);
          if ($reqs && count($reqs) > 1) {
            $req = $reqs[1];
            $msg = $u->match_pattern($req);
            if (!is_string($msg)) {
              if ($coupon_number_hash == $msg[$COUPON]) {
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

    $args = $this->unpack_bankmsg($outbox_item, $ATSPEND);
    if (is_string($args)) {
      // Make sure the spender can cancel the coupon
      $db->put($key, $outbox_item);
      return "While unpacking coupon spend: $args";
    }
    $reqs = $args[$this->unpack_reqs_key];
    $spendreq = $args[$MSG];
    $spendmsg = $parser->get_parsemsg($spendreq);
    $feemsg = '';
    if (count($reqs) > 1) {
      $feereq = $reqs[1];
      $feemsg = $parser->get_parsemsg($feereq);
    }
    $newtime = $this->gettime();
    $inbox_item = $this->bankmsg($INBOX, $newtime, $spendmsg);
    $cnhmsg = $this->bankmsg($COUPONNUMBERHASH, $coupon_number_hash);
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

    $id = $args[$CUSTOMER];
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
    $res = $this->bankmsg($ATGETINBOX, $msg);
    $last = 1;
    foreach ($inbox as $inmsg) {
      $res .= '.' . $inmsg;
      $args = $u->match_message($inmsg);
      if ($args && !is_string($args)) {
        if ($args[$REQUEST] == $INBOX) {
          $time = $args[$TIME];
          if (bccomp($time, $last) > 0) $last = $time;
        }
        $args = $u->match_pattern($args[$MSG]);
      }
      $err = false;
      if (!$args) $err = 'Inbox parse error';
      elseif (is_string($args)) $err = "Inbox match error: $args";
      elseif ($args[$ID] != $id && $args[$ID] != $COUPON) {
        $err = "Inbox entry for wrong ID: " . $args[$ID];
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
     *   $time = $args[$TIME];
     *   $key = $this->accttimekey($id);
     *   $times = explode(',', $db->get($key));
     *   if (!(count($times) >= 2 &&
     *         bccomp($times[0], $time) > 0 &&
     *         bccomp($times[1], $time) > 0)) {
     *     $times = array($this->gettime(), $this->gettime());
     *     $db->put($key, implode(',', $times));
     *   }
     *   $res .= '.' . $this->bankmsg($TIME, $id, $times[0]);
     *   $res .= '.' . $this->bankmsg($TIME, $id, $times[1]);
     * }
     */
    return $res;
  }

  function do_processinbox($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;
    $parser = $this->parser;

    $parser->verifysigs(false);

    $id = $args[$CUSTOMER];
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

    // $PROCESSINBOX => array($BANKID,$TIME,$TIMELIST),
    $id = $args[$CUSTOMER];
    $time = $args[$TIME];
    $timelist = $args[$TIMELIST];
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
      $itemargs = $this->unpack_bankmsg($item, $INBOX, true);
      if ($itemargs[$ID] != $id && $itemargs[$ID] != $COUPON) {
        return $this->failmsg($msg, "Inbox corrupt. Item found for other customer");
      }
      $request = $itemargs[$REQUEST];
      if ($request == $SPEND) {
        $itemtime = $itemargs[$TIME];
        $spends[$itemtime] = array($inboxtime, $itemargs);
        $itemreqs = $itemargs[$this->unpack_reqs_key];
        $itemcnt = count($itemreqs);
        $feereq = ($itemcnt > 1) ? $itemreqs[$itemcnt-1] : false;
        if ($feereq) {
          $feeargs = $u->match_pattern($feereq);
          if ($feeargs && $feeargs[$REQUEST] == $ATTRANFEE) {
            $feeargs = $u->match_pattern($feeargs[$MSG]);
          }
          if (!$feeargs || $feeargs[$REQUEST] != $TRANFEE) {
            return $this->failmsg($msg, "Inbox corrupt. Fee not properly encoded");
          }
          $fees[$itemtime] = $feeargs;
        }
      }
      elseif ($request == $SPENDACCEPT) $accepts[$inboxtime] = $itemargs;
      elseif ($request == $SPENDREJECT) $rejects[$inboxtime] = $itemargs;
      else return $this->failmsg($msg, "Inbox corrupted. Found '$request' item");
    }

    $bals = array();
    $outboxtimes = array();

    // Refund the transaction fees for accepted spends
    foreach ($accepts as $itemargs) {
      $outboxtime = $itemargs[$TIME];
      $outboxtimes[] = $outboxtime;
      $spendfeeargs = $this->get_outbox_args($id, $outboxtime);
      if (is_string($spendfeeargs)) {
        return $this->failmsg($msg, $spendfeeargs);
      }
      $feeargs = $spendfeeargs[1];
      if ($feeargs) {
        $asset = $feeargs[$ASSET];
        $amt = $feeargs[$AMOUNT];
        $bals[$asset] = bcadd(@$bals[$asset], $amt);
      }
    }

    $oldneg = array();
    $newneg = array();
    $tobecharged = array();     // amount/time pairs for accepted spends

    // Credit the spend amounts for rejected spends, but do NOT
    // refund the transaction fees
    foreach ($rejects as $itemargs) {
      $outboxtime = $itemargs[$TIME];
      $outboxtimes[] = $outboxtime;
      $spendfeeargs = $this->get_outbox_args($id, $outboxtime);
      if (is_string($spendfeeargs)) {
        return $this->failmsg($msg, $spendfeeargs);
      }
      $spendargs = $spendfeeargs[0];
      $asset = $spendargs[$ASSET];
      $amt = $spendargs[$AMOUNT];
      $spendtime = $spendargs[$TIME];
      if (bccomp($amt, 0) < 0) {
        $oldneg[$asset] = $spendargs;
      }
      $bals[$asset] = bcadd(@$bals[$asset], $amt);
      $tobecharged[] = array($AMOUNT => $amt,
                             $TIME => $spendtime,
                             $ASSET => $asset);
    }

    $inboxmsgs = array();
    $acctbals = array();
    $accts = array();
    $res = $this->bankmsg($ATPROCESSINBOX, $parser->get_parsemsg($reqs[0]));
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
      if ($args[$CUSTOMER] != $id) {
        return $this->failmsg
          ($msg, "Item not from same customer as " . $PROCESSINBOX);
      }
      $request = $args[$REQUEST];
      if ($request == $SPENDACCEPT ||
          $request == $SPENDREJECT) {
        // $SPENDACCEPT => array($BANKID,$TIME,$id,$NOTE=>1),
        // $SPENDREJECT => array($BANKID,$TIME,$id,$NOTE=>1),
        $itemtime = $args[$TIME];
        $otherid = $args[$ID];
        $inboxpair = $spends[$itemtime];
        if (!$inboxpair || count($inboxpair) != 2) {
          return $this->failmsg($msg, "'$request' not matched in '" .
                                $PROCESSINBOX . "' item, itemtime: $itemtime");
        }
        $itemargs = $inboxpair[1];
        if ($request == $SPENDACCEPT) {
          // Accepting the payment. Credit it.
          $itemasset = $itemargs[$ASSET];
          $itemamt = $itemargs[$AMOUNT];
          $itemtime = $itemargs[$TIME];
          if (bccomp($itemamt, 0) < 0 && $itemargs[$CUSTOMER] != $bankid) {
            $state['oldneg'][$itemasset] = $itemargs;
          }
          $state['bals'][$itemasset] = bcadd(@$state['bals'][$itemasset], $itemamt);
          $tobecharged[] = array($AMOUNT => $itemamt,
                                 $TIME => $itemtime,
                                 $ASSET => $itemasset);
          $res .= '.' . $this->bankmsg($ATSPENDACCEPT, $reqmsg);
        } else {
          // Rejecting the payment. Credit the fee.
          $feeargs = $fees[$itemtime];
          if ($feeargs) {
            $feeasset = $feeargs[$ASSET];
            $feeamt = $feeargs[$AMOUNT];
            $state['bals'][$feeasset] = bcadd(@$state['bals'][$feeasset], $feeamt);
          }
          $res .= '.' . $this->bankmsg($ATSPENDREJECT, $reqmsg);
        }
        if ($otherid == $bankid) {
          if ($request == $SPENDREJECT &&
              $itemargs[$AMOUNT] < 0) {
            return $this->failmsg($msg, "You may not reject a bank charge");
          }
          $inboxtime = $request;
          $inboxmsg = $itemargs;
        } else {
          $inboxtime = $this->gettime();
          $inboxmsg = $this->bankmsg($INBOX, $inboxtime, $reqmsg);
        }
        if ($inboxtime) $inboxmsgs[] = array($otherid, $inboxtime, $inboxmsg);
      } elseif ($request == $STORAGEFEE) {
        if ($time != $args[$TIME]) {
          $argstime = $args[$TIME];
          return $this->failmsg($msg, "Time mismatch in storagefee item, was: $argstime, sb: $time");
        }
        $storageasset = $args[$ASSET];
        $storageamt = $args[$AMOUNT];
        if (@$storagemsgs[$storageasset]) {
          return $this->failmsg($msg, "Duplicate storage fee for asset: $storageasset");
        }
        $storageamts[$storageasset] = $storageamt;
        $storagemsg = $this->bankmsg($ATSTORAGEFEE, $reqmsg);
        $storagemsgs[$storageasset] = $storagemsg;
      } elseif ($request == $FRACTION) {
        if ($time != $args[$TIME]) {
          $argstime = $args[$TIME];
          return $this->failmsg($msg, "Time mismatch in fraction item, was: $argstime, sb: $time");
        }
        $fracasset = $args[$ASSET];
        $fracamt = $args[$AMOUNT];
        if (@$fracmsgs[$fracasset]) {
          return $this->failmsg($msg, "Duplicate fraction balance for asset: $fracasset");
        }
        $fracamts[$fracasset] = $fracamt;
        $fracmsg = $this->bankmsg($ATFRACTION, $reqmsg);
        $fracmsgs[$fracasset] = $fracmsg;
      } elseif ($request == $OUTBOXHASH) {
        if ($outboxhashreq) {
          return $this->failmsg($msg, $OUTBOXHASH . " appeared multiple times");
        }
        if ($time != $args[$TIME]) {
          return $this->failmsg($msg, "Time mismatch in outboxhash");
        }
        $outboxhashreq = $req;
        $outboxhashmsg = $parser->get_parsemsg($req);
        $outboxhash = $args[$HASH];
        $outboxcnt = $args[$COUNT];
      } elseif ($request == $BALANCE) {
        if ($time != $args[$TIME]) {
          $argstime = $args[$TIME];
          return $this->failmsg($msg, "Time mismatch in balance item, was: $argstime, sb: $time");
        }
        $errmsg = $this->handle_balance_msg($id, $reqmsg, $args, $state);
        if ($errmsg) return $this->failmsg($msg, $errmsg);
        $newbals[] = $reqmsg;
      } elseif ($request == $BALANCEHASH) {
        if ($balancehashreq) {
          return $this->failmsg($msg, $BALANCEHASH . " appeared multiple times");
        }
        if ($time != $args[$TIME]) {
          return $this->failmsg($msg, "Time mismatch in balancehash");
        }
        $balancehashreq = $req;
        $balancehash = $args[$HASH];
        $balancehashcnt = $args[$COUNT];
        $balancehashmsg = $parser->get_parsemsg($req);
      } else {
        return $this->failmsg($msg, "$request not valid for " . $PROCESSINBOX .
                              ". Only " . $SPENDACCEPT . ", " . $SPENDREJECT .
                              ", " . $OUTBOXHASH . ", " .
                              $BALANCE . ", &" . $BALANCEHASH);
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
        $itemamt = $item[$AMOUNT];
        $itemtime = $item[$TIME];
        $itemasset = $item[$ASSET];
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
        return $this->failmsg($msg, $OUTBOXHASH .
                              ($outboxhashreq ? " included when not needed" :
                               " missing"));
      }

      if ($outboxhashreq) {
        $hasharray = $this->outboxhash($id, false, $outboxtimes);
        $hash = $hasharray[$HASH];
        $hashcnt = $hasharray[$COUNT];
        if ($outboxhash != $hash || $outboxcnt != $hashcnt) {
          return $this->failmsg
            ($msg, $OUTBOXHASH . " mismatch");
        }
      }

      // Check balancehash
      if (!$balancehashreq) {
        return $this->failmsg($msg, $BALANCEHASH . " missing");
      } else {
        $hasharray = $u->balancehash($db, $id, $this, $acctbals);
        $hash = $hasharray[$HASH];
        $hashcnt = $hasharray[$COUNT];
        if ($balancehash != $hash || $balancehashcnt != $hashcnt) {
          return $this->failmsg($msg, $BALANCEHASH . ' mismatch');
        }
      }
    }

    // All's well with the world. Commit this puppy.
    // Update balances
    $balancekey = $this->balancekey($id);
    foreach ($acctbals as $acct => $balances) {
      $acctkey = "$balancekey/$acct";
      foreach ($balances as $balasset => $balance) {
        $balance = $this->bankmsg($ATBALANCE, $balance);
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
        if ($request == $SPENDREJECT) {
          // Return the funds to the bank's account
          $this->add_to_bank_balance($itemargs[$ASSET], $itemargs[$AMOUNT]);
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
        $outboxhash_item = $this->bankmsg($ATOUTBOXHASH, $outboxhashmsg);
        $res .= ".$outboxhash_item";
        $db->put($this->outboxhashkey($id), $outboxhash_item);
      }

      // Update balancehash
      $balancehash_item = $this->bankmsg($ATBALANCEHASH, $balancehashmsg);
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

    $id = $args[$CUSTOMER];
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

    $id = $args[$CUSTOMER];
    $inboxkey = $this->inboxkey($id);
    $key = $this->storagefeekey($id);
    $assetids = $db->contents($key);
    foreach ($assetids as $assetid) {
      $storagefee = $db->get("$key/$assetid");
      $args = $this->unpack_bankmsg($storagefee, $STORAGEFEE);
      if (is_string($args)) {
        return $this->failmsg($msg, "storagefee parse error: $args");
      }
      if ($assetid != $args[$ASSET]) {
        $feeasset = $args[$ASSET];
        return $this->failmsg($msg, "Asset mismatch, sb: $assetid, was: $feeasset");
      }
      $amount = $args[$AMOUNT];
      $percent = $this->storageinfo($id, $assetid, $issuer, $fraction, $fractime);
      $digits = $u->fraction_digits($percent);
      $u->normalize_balance($amount, $fraction, $digits);
      if (bccomp($amount, 0, 0) > 0) {
        $time = $this->gettime();
        $storagefee = $this->bankmsg($STORAGEFEE, $bankid, $time, $assetid, $fraction);
        $spend = $this->bankmsg($SPEND, $bankid, $time, $id, $assetid, $amount, "Storage fees");
        $inbox = $this->bankmsg($INBOX, $time, $spend);
        $db->put("$key/$assetid", $storagefee);
        $db->put("$inboxkey/$time", $inbox);
      }
    }
    
    return $this->bankmsg($ATSTORAGEFEES, $msg);
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
    if ($spendargs[$CUSTOMER] != $bankid ||
        $spendargs[$REQUEST] != $ATSPEND ||
        ($feeargs &&
         ($feeargs[$CUSTOMER] != $bankid ||
          $feeargs[$REQUEST] != $ATTRANFEE))) {
      return "Outbox corrupted";
    }
    $spendargs = $u->match_pattern($spendargs[$MSG]);
    if ($feeargs) $feeargs = $u->match_pattern($feeargs[$MSG]);
    if ($spendargs[$CUSTOMER] != $id ||
        $spendargs[$REQUEST] != $SPEND ||
        ($feeargs &&
         ($feeargs[$CUSTOMER] != $id ||
          $feeargs[$REQUEST] != $TRANFEE))) {
      return "Outbox inner messages corrupted";
    }
    return array($spendargs, $feeargs); 
  }

  function do_getasset($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $err = $this->checkreq($args, $msg);
    if ($err) return $err;

    $assetid = $args[$ASSET];
    $asset = $db->get($ASSET . "/$assetid");
    if (!$asset) return $this->failmsg($msg, "Unknown asset: $assetid");
    return $asset;
  }

  function do_asset($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $id = $args[$CUSTOMER];
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

    // $ASSET => array($BANKID,$ASSET,$SCALE,$PRECISION,$ASSETNAME),
    $id = $args[$CUSTOMER];
    $assetid = $args[$ASSET];
    $scale = $args[$SCALE];
    $precision = $args[$PRECISION];
    $assetname = $args[$ASSETNAME];
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
      $reqid = $args[$CUSTOMER];
      $request = $args[$REQUEST];
      $reqtime = $args[$TIME];
      if ($i == 1) {
        // Burn the transaction
        $time = $reqtime;
        $err = $this->deq_time($id, $time);
        if ($err) return $this->failmsg($msg, $err);
        $state['time'] = $time;
      }
      if ($reqid != $id) return $this->failmsg($msg, "ID mismatch");
      elseif ($request == $STORAGE) {
        if ($storage_msg) return $this->failmsg($msg, "Duplicate storage fee");
        $storage_msg = $parser->get_parsemsg($req);
      }
      elseif ($request == $BALANCE) {
        $reqmsg = $parser->get_parsemsg($req);
        $errmsg = $this->handle_balance_msg($id, $reqmsg, $args, $state, $assetid);
        if ($errmsg) return $this->failmsg($msg, $errmsg);
        $newbals[] = $reqmsg;
      } elseif ($request == $BALANCEHASH) {
        if ($balancehashreq) {
          return $this->failmsg($msg, $BALANCEHASH . " appeared multiple times");
        }
        $balancehashreq = $req;
        $balancehash = $args[$HASH];
        $balancehashcnt = $args[$COUNT];
        $balancehashmsg = $parser->get_parsemsg($req);
      } else {
        return $this->failmsg($msg, "$request not valid for asset creation. Only " .
                              $BALANCE . ' & ' . $BALANCEHASH);
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
    if (!$exists) $oldneg[$assetid] = $MAIN;
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
      return $this->failmsg($msg, $BALANCEHASH . " missing");
    } else {
      $hasharray = $u->balancehash($db, $id, $this, $acctbals);
      $hash = $hasharray[$HASH];
      $hashcnt = $hasharray[$COUNT];
      if ($balancehash != $hash || $balancehashcnt != $hashcnt) {
        return $this->failmsg($msg, $BALANCEHASH .
                              " mismatch, hash: $balancehash, sb: $hash, count: $balancehashcnt, sb: $hashcnt");
      }
    }
  
    // All's well with the world. Commit this puppy.
    // Add asset
    $res = $this->bankmsg($ATASSET, $parser->get_parsemsg($reqs[0]));
    if ($storage_msg) {
      $res .= "." . $this->bankmsg($ATSTORAGE, $storage_msg);
    }
    $db->put($ASSET . "/$assetid", $res);

    // Credit bank with tokens
    $this->add_to_bank_balance($tokenid, $tokens);

    // Update balances
    $balancekey = $this->balancekey($id);
    foreach ($acctbals as $acct => $balances) {
      $acctkey = "$balancekey/$acct";
      foreach ($balances as $balasset => $balance) {
        $balance = $this->bankmsg($ATBALANCE, $balance);
        $res .= ".$balance";
        $db->put("$acctkey/$balasset", $balance);
      }
    }

    // Update balancehash
    $balancehash_item = $this->bankmsg($ATBALANCEHASH, $balancehashmsg);
    $res .= ".$balancehash_item";
    $db->put($this->balancehashkey($id), $balancehash_item);

    return $res;
  }

  function do_getbalance($args, $reqs, $msg) {
    $t = $this->t;
    $db = $this->db;

    $id = $args[$CUSTOMER];
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

    // $GETBALANCE => array($BANKID,$REQ,$ACCT=>1,$ASSET=>1));
    $id = $args[$CUSTOMER];
    $acct = @$args[$ACCT];
    $assetid = @$args[$ASSET];

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

    $id = $args[$CUSTOMER];
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

    // $GETOUTBOX => array($BANKID,$REQ),
    $id = $args[$CUSTOMER];

    $msg = $this->bankmsg($ATGETOUTBOX, $msg);
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
      $names = array($BANKID => array($PUBKEY, $COUPON=>1),
                     $ID => array($BANKID,$ID),
                     $REGISTER => $patterns[$REGISTER],
                     $GETREQ => array($BANKID),
                     $GETTIME => array($BANKID,$REQ),
                     $GETFEES => array($BANKID,$REQ,$OPERATION=>1),
                     $SPEND => $patterns[$SPEND],
                     $SPENDREJECT => $patterns[$SPENDREJECT],
                     $COUPONENVELOPE => $patterns[$COUPONENVELOPE],
                     $GETINBOX => $patterns[$GETINBOX],
                     $PROCESSINBOX => $patterns[$PROCESSINBOX],
                     $STORAGEFEES => $patterns[$STORAGEFEES],
                     $GETASSET => array($BANKID,$REQ,$ASSET),
                     $ASSET => array($BANKID,$ASSET,$SCALE,$PRECISION,$ASSETNAME),
                     $GETOUTBOX => $patterns[$GETOUTBOX],
                     $GETBALANCE => array($BANKID,$REQ,$ACCT=>1,$ASSET=>1));
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
    $pattern = array_merge(array($CUSTOMER,$REQUEST), $method_pattern[1]);
    $args = $this->parser->matchargs($parses[0], $pattern);
    if (!$args) {
      return $this->failmsg($msg,
                            "Request doesn't match pattern: " .
                            $parser->formatpattern($pattern));
    }
    if (array_key_exists($BANKID, $args)) {
      $argsbankid = $args[$BANKID];
      if ($argsbankid != $this->bankid) {
        return $this->failmsg($msg, "bankid mismatch");
      }
    }
    if (strlen(@$args[$NOTE]) > 4096) {
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
