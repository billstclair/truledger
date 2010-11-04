; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Trubanc server
;;;

(in-package :trubanc-server)

(defun make-server (dir passphrase &key
                    (bankname "")
                    (bankurl "")
                    (privkey-size 3072))
  "Create a Trubanc server instance."
  (let ((db (make-fsdb dir)))
    (make-instance 'server
                   :db db
                   :passphrase passphrase
                   :bankname bankname
                   :bankurl bankurl
                   :privkey-size privkey-size)))

(defparameter *tranfee-default* "2")
(defparameter *regfee-default* "10")

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
   ;; It's important for backup that nobody writes to pubkeydb
   (pubkeydb :type db
             :accessor pubkeydb)
   (bankname :type (or string null)
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
           :initform *regfee-default*
           :accessor regfee)
   (tranfee :type string
            :initarg :tranfee
            :initform *tranfee-default*
            :accessor tranfee)
   (fees :type list                     ;((operation assetid amount ...) ...)
         :initarg :fees
         :initform nil
         :accessor fees)
   (privkey-size :accessor privkey-size
                 :initarg :privkey-size
                 :initform 3072)
   (privkey :accessor privkey)
   (bankid :type (or string null)
           :initform nil
           :accessor bankid)
   (always-verify-sigs-p :type boolean
                         :initarg :always-verify-sigs-p
                         :initform t
                         :accessor always-verify-sigs-p)
   (backup-mode-p :type boolean
                  :initarg :backup-mode-p
                  :initform nil
                  :accessor backup-mode-p)))

(defmethod initialize-instance :after ((server server) &key db passphrase)
  (let ((pubkeydb (db-subdir db $PUBKEY)))
    (setf (pubkeydb server) pubkeydb
          (parser server) (make-instance
                           'parser
                           :keydb pubkeydb
                           :bank-getter (lambda () (bankid server))
                           :always-verify-sigs-p (always-verify-sigs-p server)))
    (setup-db server passphrase)
    (set-shutdown-msg server)))

(defmethod gettime ((server server))
  (let* ((db (db server)))
    (with-db-lock (db $TIME)
      (let ((res (next (timestamp server) (db-get db $TIME))))
        (db-put db $TIME res)
        res))))

(defun account-dir (id)
  (append-db-keys $ACCOUNT id))

(defun acct-last-key (id)
  (append-db-keys (account-dir id) $LAST))

(defmethod get-acct-last ((server server) id)
  (db-get (db server) (acct-last-key id)))

(defun acct-req-key (id &optional server)
  (if (and server (backup-mode-p server))
      (append-db-keys (account-dir id) $BACKUP $REQ)
      (append-db-keys (account-dir id) $REQ)))

(defmethod get-acct-req ((server server) id)
  (db-get (db server) (acct-req-key id server)))

(defun acct-time-key (id)
  (append-db-keys (account-dir id) $TIME))

(defun balance-key (id)
  (append-db-keys (account-dir id) $BALANCE))

(defun acct-balance-key (id &optional (acct $MAIN))
  (append-db-keys (balance-key id) acct))

(defun asset-balance-key (id asset &optional (acct $MAIN))
  (append-db-keys (acct-balance-key id acct) asset))

(defun fraction-balance-key (id asset)
  (append-db-keys (account-dir id) $FRACTION asset))

(defmethod asset-balance ((server server) id asset &optional (acct "main"))
  (let* ((key (asset-balance-key id asset acct))
         (msg (db-get (db server) key)))
    (if (not msg)
        "0"
        (unpack-bankmsg server msg $ATBALANCE $BALANCE $AMOUNT))))

(defun outbox-key (id)
  (append-db-keys (account-dir id) $OUTBOX))

(defun outbox-hash-key (id)
  (append-db-keys (account-dir id) $OUTBOXHASH))

(defun inbox-key (id)
  (append-db-keys (account-dir id) $INBOX))

(defmethod storage-info ((server server) id assetid &optional for-issuer-p)
  "Get the values necessary to compute the storage fee.
   Inputs:
    ID - the user ID
    ASSETID - the asset ID
   Return values:
    1) storage fee percent
    2) the fraction balance for ID/ASSETID
    3) the time of the fraction
    4) the ID of the asset issuer"
  (let* ((parser (parser server))
         (db (db server))
         (msg (db-get db $ASSET assetid)))
    (when msg
      (let* ((reqs (parse parser msg))
             (req (cadr reqs)))
        (unless req (return-from storage-info nil))

        (let ((args (match-pattern parser req)))
          (unless (equal (getarg $REQUEST args) $ATSTORAGE)
            (return-from storage-info nil))
          (setq req (getarg $MSG args)
                args (match-pattern parser req))
          (unless (equal (getarg $REQUEST args) $STORAGE)
            (return-from storage-info nil))
          (let ((issuer (getarg $CUSTOMER args)))
            (unless (and (not for-issuer-p) (equal issuer id))
              (let* ((percent (getarg $PERCENT args))
                     (fraction "0")
                     (fractime "0")
                     (key (fraction-balance-key id assetid))
                     (msg (db-get db key)))
                (when msg
                  (setq args (unpack-bankmsg server msg $ATFRACTION $FRACTION)
                        fraction (getarg $AMOUNT args)
                        fractime (getarg $TIME args)))
                (values percent fraction fractime issuer)))))))))

(defun storage-fee-key (id &optional assetid)
  (let ((res (append-db-keys (account-dir id) $STORAGEFEE)))
    (if assetid
        (append-db-keys res assetid)
        res)))

(defmethod unpacker ((server server))
  #'(lambda (msg) (unpack-bankmsg server msg)))

(defmethod outbox-hash ((server server) id &optional newitem removed-items)
  (let ((db (db server)))
    (multiple-value-bind (hash cnt)
        (dirhash db (outbox-key id) (unpacker server) newitem removed-items)
      (values (or hash "") (or cnt 0)))))

(defmethod outbox-hash-msg ((server server) id)
  (multiple-value-bind (hash count) (outbox-hash server id)
    (bankmsg server
             $OUTBOXHASH
             (bankid server)
             (get-acct-last server id)
             count
             hash)))

(defun balance-hash-key (id)
  (append-db-keys (account-dir id) $BALANCEHASH))

(defun permission-key (id &optional permission)
  (apply #'append-db-keys (account-dir id) $PERMISSION
         (and permission (list permission))))

(defmethod is-asset-p ((server server) assetid)
  "Returns the message defining ASSETID, or NIL, if there isn't one."
  (db-get (db server) $ASSET assetid))

(defmethod lookup-asset ((server server) assetid)
  (let ((asset (is-asset-p server assetid)))
    (when asset
      (let ((res (unpack-bankmsg server asset $ATASSET $ASSET)))
        (let* ((reqs (getarg $UNPACK-REQS-KEY res))
               (req1 (and (> (length reqs) 1) (elt reqs 1))))
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

(defun is-alphanumeric-or-space-p (char)
  (or (eql char #\space)
      (is-alphanumeric-p char)))

(defun is-numeric-p (string &optional integer-p)
  (loop
     with firstp = t
     with sawdotp = integer-p
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
       (every #'is-alphanumeric-p acct)))

(defun pubkey-id (pubkey)
  (sha1 (trim pubkey)))

(defun privkey-id (privkey)
  (with-rsa-private-key (key privkey)
    (pubkey-id (encode-rsa-public-key key))))

(defmethod unpack-bank-param ((server server) type &optional (key type))
  "Unpack wrapped initialization parameter."
  (unpack-bankmsg server (db-get (db server) type) type nil key))

(defun signmsg (privkey msg)
  (let ((sig (sign msg privkey)))
    (strcat msg #.(format nil ":~%") sig)))

(defmethod banksign ((server server) msg)
  "Bank sign a message."
  (signmsg (privkey server) msg))

(defmethod bankmsg ((server server) &rest req)
  "Make a bank signed message from the args."
  (banksign server (apply #'makemsg (parser server) (bankid server) req)))

(defun shorten-failmsg-msg (msg)
  (if (> (length msg) 1024)
      (strcat (subseq msg 0 1021) "...")
      msg))

(defmethod failmsg ((server server) &rest req)
  (when (stringp (car req))
    (setf (car req) (shorten-failmsg-msg (car req))))
  (banksign server (apply #'simple-makemsg (bankid server) $FAILED req)))


(defmethod unpack-bankmsg ((server server) msg &optional type subtype idx)
  "Reverse the bankmsg() function, optionally picking one field to return.
   IDX can be that field, or :no-error, to not error if SUBTYPE is wrong."
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
             (when (and subtype
                        (not (eq subtype t))
                        (not (equal (getarg $REQUEST args) subtype)))
               (when (eq idx :no-error)
                 (return-from unpack-bankmsg nil))
               (error "Wrapped message wasn't of type: ~s" subtype))
             (cond ((and idx (not (eq idx :no-error)))
                    (or (getarg idx args)
                        (error "No arg with idx: ~s" idx)))
                   (t (setf (getarg $UNPACK-REQS-KEY args) reqs) ; save parse results
                      args))))))

(defmethod unpack-bank-name ((server server))
  (let ((db (db server))
        (bankid (bankid server)))
    (unpack-bankmsg
     server (db-get db $PUBKEYSIG bankid) $ATREGISTER $REGISTER $NAME)))

(defmethod set-shutdown-msg ((server server))
  (let ((db (db server)))
    (unless (db-get db $SHUTDOWNMSG)
      (setf (db-get db $SHUTDOWNMSG)
            (failmsg server "" "Server is shut down")))))

(defmethod setup-db ((server server) passphrase)
  "Initialize the database, if it needs initializing."
  (let ((db (db server)))
    (if (db-get db $PRIVKEY)
        (let* ((privkey (decode-rsa-private-key (db-get db $PRIVKEY) passphrase))
               (pubkey (encode-rsa-public-key privkey))
               (bankid (pubkey-id pubkey)))
          (setf (privkey server) privkey
                (bankid server) bankid
                (bankurl server) (db-get db $BANKURL)
                (bankname server) (unpack-bank-name server)
                (tokenid server) (unpack-bank-param server $TOKENID)
                (regfee server) (unpack-bank-param server $REGFEE $AMOUNT)
                (tranfee server) (unpack-bank-param server $TRANFEE $AMOUNT))
          (load-fees server))
        ;; http://www.rsa.com/rsalabs/node.asp?id=2004 recommends that 3072-bit
        ;; RSA keys are equivalent to 128-bit symmetric keys, and they should be
        ;; secure past 2031.
        (let* ((privkey (rsa-generate-key (or (privkey-size server) 3072)))
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
          (db-put db $BANKURL (bankurl server))
          (setf (db-get db $PUBKEY bankid) pubkey)
          (setf (db-get db $PUBKEYSIG bankid)
                (bankmsg server $ATREGISTER
                         (bankmsg server $REGISTER
                                  bankid
                                  (strcat #.(format nil "~%") pubkey)
                                  bankname)))
          (setf (tokenid server) tokenid)
          (db-put db $TOKENID (bankmsg server $TOKENID tokenid))
          (setf (db-get db $ASSET tokenid)
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

(defun load-fees (server)
  (let ((db (db server))
        (parser (parser server))
        (bankid (bankid server))
        (fee-alist nil))
    (dolist (operation (db-contents db $FEE))
      (let* ((msg (db-get db $FEE operation))
             (reqs (parse parser msg))
             (cell (list operation)))
        (push cell fee-alist)
        (dolist (req reqs)
          (let* ((args (match-pattern parser req))
                 (asset (getarg $ASSET args))
                 (amount (getarg $AMOUNT args)))
            (unless (and (EQUAL (getarg $CUSTOMER args) bankid)
                         (equal (getarg $REQUEST args) $FEE)
                         (equal (getarg $BANKID args) bankid)
                         (equal (getarg $OPERATION args) operation))
              (error "Malformed fee message for operation: ~s" operation))
            (setf (cdr cell)
                  (cons amount (cons asset (cdr cell))))))))
    (setf fee-alist (nreverse fee-alist))
    (dolist (cell fee-alist)
      (setf (cdr cell) (nreverse (cdr cell))))
    (setf (fees server) fee-alist)))

(defmethod scan-inbox ((server server) id)
  (loop
     with db = (db server)
     with inbox-key = (inbox-key id)
     with times = (db-contents db inbox-key)
     for time in times
     for item = (db-get db inbox-key time)
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
            (dotcat q "," time))
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
                      times (delete v times :test #'equal)
                      q (apply #'implode #\, times))
                (db-put db key q)))))
        (unless res (error "Timestamp not enqueued: ~s" time))
        (when (> (bccomp (get-unix-time) (bcadd time (* 10 60)))
                 0)
          (error "Timestamp too old: ~s" time))))
    time))

(defmethod add-to-bank-balance ((server server) assetid amount
                                &optional (acct $MAIN))
  "Add AMOUNT to the bank balance for ASSETID in ACCT, default \"main\".
   Returns new balance, unless you add 0, then it returns nil."
  (let ((bankid (bankid server))
        (db (db server)))
    (unless (eql 0 (bccomp amount 0))
      (let ((key (asset-balance-key bankid assetid acct)))
        (with-db-lock (db key)
          (let* ((balmsg (db-get db key))
                 (balargs (and balmsg
                               (unpack-bankmsg
                                server balmsg $ATBALANCE $BALANCE))))
            (unless (or (null balargs)
                        (equal acct (or (getarg $ACCT balargs) $MAIN)))
              (error "Bank balance message not for ~s account" acct))
            (let* ((bal (if balargs (getarg $AMOUNT balargs) 0))
                   (newbal (bcadd bal amount))
                   (balsign (bccomp bal 0))
                   (newbalsign (bccomp newbal 0)))
              (when (or (and (>= balsign 0) (< newbalsign 0))
                        (and (< balsign 0) (>= newbalsign 0)))
                (error "Transaction would put bank out of balance."))
              ;; $BALANCE => `(,$BANKID ,$TIME ,$ASSET ,$AMOUNT (,$ACCT))
              (let ((msg (bankmsg
                          server $ATBALANCE
                          (apply #'bankmsg
                           server $BALANCE
                           bankid (gettime server) assetid newbal
                           (unless (equal acct $MAIN) (list acct)))))
                    (reqkey (acct-req-key bankid))
                    )
                (db-put db key msg)
                ;; Make sure clients update the balance
                ;; Need to figure out another way to do this.
                ;; It can make it impossible for the bank
                ;; to spend anything, e.g. transaction fees.
                (db-put db reqkey (bcadd 1 (db-get db reqkey)))
                newbal))))))))

(defmethod checkreq (server args)
  (let* ((db (db server))
         (id (getarg $CUSTOMER args))
         (req (getarg $REQ args))
         (reqkey (acct-req-key id server)))
    (with-db-lock (db reqkey)
      (let ((oldreq (or (db-get db reqkey) "0")))
        (when (<= (bccomp req oldreq) 0)
          (error "New req (~s) <= old req (~s)" req oldreq))
        (db-put db reqkey req)))))

(defstruct balance-state
  (acctbals (make-equal-hash))
  (bals (make-equal-hash))
  (tokens "0")
  (oldneg (make-equal-hash))
  (newneg (make-equal-hash))
  time
  (charges (make-equal-hash)))

(defmethod handle-balance-msg ((server server) id msg args state &optional
                               creating-asset)
  "Deal with an (<id>,balance,...) item from the customer for a
   spend or processinbox request.
   ID: the customer id
   MSG: the signed (<id>,balance,...) message, as a string
   ARGS: parser->parse(), then utility->match_pattern() output on balmsg
   STATE: a BALANCE-STATE instance.
     acctbals => hash: <acct> => (hash: <asset> => <msg>)
     bals => hash: <asset> => <amount>
     tokens => total new /account/<id>/balance/<acct>/<asset> files
     oldneg => hash: <asset> => <acct>, negative balances in current account
     newneg => hash: <asset> => <acct>, negative balances in updated account
     time => the transaction time"
  (unless (equal $BALANCE (getarg $REQUEST args))
    (error "Non-balance message passed to handle-balance-msg"))
  (let ((db (db server))
        (bankid (bankid server))
        (asset (getarg $ASSET args))
        (amount (getarg $AMOUNT args))
        (acct (or (getarg $ACCT args) $MAIN)))

    (when (and (or (not creating-asset) (not (equal asset creating-asset)))
               (not (is-asset-p server asset)))
      (error "Unknown asset id: ~s" asset))

    (unless (is-numeric-p amount)
      (error "Not a number: ~s" amount))

     (unless (is-acct-name-p acct)
       (error "<acct> may contain only letters and digits: ~s" acct))

     (let ((acct-hash (get-inited-hash acct (balance-state-acctbals state))))
       (when (gethash asset acct-hash)
         (error "Duplicate acct/asset balance pair: ~s/~s" acct asset))

       (setf (gethash asset acct-hash) msg))

     (let ((bals (balance-state-bals state)))
       (setf (gethash asset bals) (bcsub (gethash asset bals 0) amount))

       (when (< (bccomp amount 0) 0)
         (when (gethash asset (balance-state-newneg state))
           (error "Multiple new negative balances for asset: ~s" asset))
         (setf (gethash asset (balance-state-newneg state)) acct))

       (let* ((asset-balance-key (asset-balance-key id asset acct))
              (acctmsg (db-get db asset-balance-key)))
         (cond ((not acctmsg)
                (unless (equal id bankid)
                  (setf (balance-state-tokens state)
                        (bcadd (balance-state-tokens state) 1)))
                (compute-storage-charges server id state asset nil nil))
               (t (let* ((acctargs
                          (unpack-bankmsg server acctmsg $ATBALANCE $BALANCE))
                         (amount (getarg $AMOUNT acctargs)))
                    (setf (gethash asset bals) (bcadd (gethash asset bals 0) amount))
                    (when (< (bccomp amount 0) 0)
                      (when (gethash asset (balance-state-oldneg state))
                        (error "Account corrupted. ~
                           Multiple negative balances for asset: ~s"
                               asset))
                      (setf (gethash asset (balance-state-oldneg state)) acct))
                    (compute-storage-charges
                     server id state asset amount acctargs))))))))

(defmethod handle-wrapper-balance-msg
    ((server server) db id msg args &optional creating-asset)
  "Deal with an (<id>,balance,...) item from the customer for a
   spend or processinbox request.
   DB: a wrapped-db instance for (db server)
   ID: the customer id
   TIME: the transaction time
   MSG: the signed (<id>,balance,...) message, as a string
   ARGS: parser->parse(), then utility->match_pattern() output on balmsg"
  (unless (equal $BALANCE (getarg $REQUEST args))
    (error "Non-balance message passed to handle-balance-msg"))
  (let ((asset (getarg $ASSET args))
        (amount (getarg $AMOUNT args))
        (acct (or (getarg $ACCT args) $MAIN)))

    (when (and (or (not creating-asset) (not (equal asset creating-asset)))
               (not (is-asset-p server asset)))
      (error "Unknown asset id: ~s" asset))

    (unless (is-numeric-p amount)
      (error "Not a number: ~s" amount))

     (unless (is-acct-name-p acct)
       (error "<acct> may contain only letters and digits: ~s" acct))

     (let* ((asset-balance-key (asset-balance-key id asset acct)))
       (when (db-wrapper-get db asset-balance-key)
         (error "Duplicate acct/asset balance pair: ~s/~s" acct asset))
       (db-put db asset-balance-key (bankmsg server $ATBALANCE msg)))))

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
  (let ((asset-info (gethash asset (balance-state-charges state))))
    (unless asset-info
      (unless (equal asset (tokenid server))
        (multiple-value-bind (percent fraction fractime issuer)
            (storage-info server id asset)
          (setq asset-info (make-storage-info :percent percent
                                              :issuer issuer
                                              :fraction fraction
                                              :fee 0))
          (setf (gethash asset (balance-state-charges state)) asset-info)
          (when percent
            (let ((digits (fraction-digits percent))
                  (time (balance-state-time state)))
              (when fraction
                (setf (storage-info-fee asset-info)
                      (storage-fee fraction fractime time percent digits)))
              (setf (storage-info-digits asset-info) digits))))))
    (when (and asset-info amount)
      (let ((percent (storage-info-percent asset-info)))
        (when (and percent (> (bccomp amount 0) 0))
          (let* ((digits (storage-info-digits asset-info))
                 (accttime (getarg $TIME acctargs))
                 (time (balance-state-time state))
                 (fee (storage-fee amount accttime time percent digits)))
            (setf (storage-info-fee asset-info)
                  (wbp (digits)
                    (bcadd (storage-info-fee asset-info) fee)))))))))

(defmethod validate-db-update ((server server) db id time diffs
                               &optional tobecharged)
  "SERVER is the current server instance.
   DB is a db-wrapper around (DB SERVER).
   ID is the ID of the current user.
   TIME is the current transaction timestamp.
   DIFFS is an alist mapping assetids to numeric strings, encoding
     the differences between the committed balances and those in
     the wrapper: committed - wrapper = spend amounts
   TOBECHARGED is a list of (asset amount time) lists for which to
     compute storage charges.
   Calculate the difference between the balances before and after
   application of the db-wrapper values, including storage charges.
   Signal an error if the differences are different than those
   in DIFFS.
   Returns an alist mapping assetids to storage-info instances
   for the storage charges."
  (check-type db db-wrapper)
  (check-type diffs list)
  (let ((res nil)                       ; alist: assetid => storage-info
        (accum nil)                     ; alist: assetid => diff
        (wrapped-db (db-wrapper-db db))
        (outbox-contents (db-wrapper-contents db (outbox-key id)))
        (accts (db-wrapper-contents db (balance-key id)))
        (bankid (bankid server))
        (tokenid (tokenid server))
        (parser (parser server))
        (negative-assets nil)
        (balance-changed-p nil))
    (flet ((unwrap-balance (msg acct asset)
             (let* ((reqs (parse parser msg))
                    (req (elt reqs 0))
                    (args (match-pattern parser req)))
               (when (equal (getarg $REQUEST args) $ATBALANCE)
                 (unless (equal (getarg $CUSTOMER args) bankid)
                   (error "Bankmsg not from bank"))
                 (setf req (getarg $MSG args) ;already parsed
                       args (match-pattern parser req)))
               (unless (equal (getarg $REQUEST args) $BALANCE)
                 (error "Balance request not ~s" $BALANCE))
               (unless (equal (getarg $CUSTOMER args) id)
                 (error "Balance for wrong user"))
               (let ((balasset (getarg $ASSET args))
                     (balacct (or (getarg $ACCT args) $MAIN)))
                 (unless (equal asset balasset)
                   (error "Balance asset SB: ~s, was: ~s" asset balasset))
                 (unless (equal acct balacct)
                   (error "Balace acct SB: ~s, was: ~s" acct balacct)))
               (values (getarg $AMOUNT args)
                       (getarg $TIME args))))
           (unwrap-fraction (msg asset)
             (let* ((reqs (parse parser msg))
                    (req (elt reqs 0))
                    (args (match-pattern parser req)))
               (when (equal (getarg $REQUEST args) $ATFRACTION)
                 (unless (equal (getarg $CUSTOMER args) bankid)
                   (error "Bankmsg not from bank"))
                 (setf req (getarg $MSG args) ;already parsed
                       args (match-pattern parser req)))
               (unless (equal (getarg $REQUEST args) $FRACTION)
                 (error "Fraction request not ~s" $FRACTION))
               (unless (equal (getarg $CUSTOMER args) id)
                 (error "Fraction for wrong user"))
               (let ((balasset (getarg $ASSET args)))
                 (unless (equal asset balasset)
                   (error "Fraction asset SB: ~s, was: ~s" asset balasset)))
               (getarg $AMOUNT args)))
           (unwrap-hash (msg atname name)
             (let* ((reqs (parse parser msg))
                    (req (elt reqs 0))
                    (args (match-pattern parser req)))
               (when (equal (getarg $REQUEST args) atname)
                 (unless (equal (getarg $CUSTOMER args) bankid)
                   (error "Bankmsg not from bank"))
                 (setf req (getarg $MSG args) ;already parsed
                       args (match-pattern parser req)))
               (unless (equal (getarg $REQUEST args) name)
                 (error "Hash request not ~s" name))
               (unless (equal (getarg $CUSTOMER args) id)
                 (error "Fraction for wrong user"))
               (values (getarg $HASH args)
                       (getarg $COUNT args))))
           (do-storage-charges (asset amount amount-time)
             (unless (equal asset tokenid)
               (let ((info (cdr (assoc asset res :test #'equal))))
                 (unless info
                   (multiple-value-bind (percent fraction fractime issuer)
                       (storage-info server id asset)
                     (setf info (make-storage-info :percent percent
                                                   :issuer issuer
                                                   :fraction fraction
                                                   :fee 0))
                     (push (cons asset info) res)
                     (when percent
                       (let ((digits (fraction-digits percent)))
                         (when fraction
                           (setf (storage-info-fee info)
                                 (storage-fee
                                  fraction fractime time percent digits)))
                         (setf (storage-info-digits info) digits)))))
                 (let ((percent (storage-info-percent info)))
                   (when (and percent amount (> (bccomp amount 0) 0))
                     (let* ((digits (storage-info-digits info))
                            (fee (storage-fee amount amount-time time
                                              percent digits)))
                       (setf (storage-info-fee info)
                             (wbp (digits)
                               (bcadd (storage-info-fee info) fee))))))))))
      (declare (dynamic-extent #'unwrap-balance #'unwrap-fraction
                               #'unwrap-hash #'do-storage-charges))
      ;; Put balance diffs in accum, while collecting storage charges in res.
      (dolist (acct accts)
        (dolist (asset (db-wrapper-contents db (acct-balance-key id acct)))
          (let* ((key (asset-balance-key id asset acct))
                 (oldmsg (db-get wrapped-db key))
                 (newmsg (db-get db key))
                 (new-amount (unwrap-balance newmsg acct asset)))
            (when (and newmsg (not oldmsg))
              ;; New balance file. Must charge a token
              (let ((cell (assoc tokenid diffs :test #'equal)))
                (unless cell
                  (setf cell (cons tokenid "0"))
                  (push cell diffs))
                (setf (cdr cell) (bcadd "1" (cdr cell)))))
            (unless (equal oldmsg newmsg)
              (setf balance-changed-p t))
            (multiple-value-bind (old-amount old-time)
                (and oldmsg (unwrap-balance oldmsg acct asset))
              (when (and old-amount (< (bccomp old-amount 0) 0))
                (unless (<= (bccomp new-amount 0) 0)
                  (error "Old balance negative, but new positive, for acct: ~s, asset: ~s"
                         acct asset)))
              (when (< (bccomp new-amount 0) 0)
                (when (member asset negative-assets :test #'equal)
                  (error "Multiple negative balances for asset: ~s" asset))
                (push asset negative-assets))
              (when old-amount
                (do-storage-charges asset old-amount old-time))
              (let ((diff (bcsub (or old-amount 0) new-amount))
                    (cell (assoc asset accum :test #'equal)))
                (if cell
                    (setf (cdr cell) (bcadd diff (cdr cell)))
                    (push (cons asset diff) accum)))))))
      ;; Add in caller's storage charges
      (dolist (asset-amt-time tobecharged)
        (apply #'do-storage-charges asset-amt-time))
      ;; Subtract diffs from accum
      (loop for (asset . amount) in diffs
           for cell = (assoc asset accum :test #'equal)
         do
           (unless cell
             (setf cell (cons asset 0))
             (push cell accum))
           (setf (cdr cell) (bcsub (cdr cell) amount)))
      ;; Subtract the storage charges from the accum amounts
      (loop for (asset . info) in res
         for cell = (assoc asset accum :test #'equal)
         for digits = (storage-info-digits info)
         when digits
         do
         (let ((amount (wbp (digits)
                         (bcsub (cdr cell) (storage-info-fee info)))))
           (multiple-value-bind (amount fraction)
               (normalize-balance amount (storage-info-fraction info) digits)
             (setf (cdr cell) amount
                   (storage-info-fraction info) fraction))))
      ;; Check for balance mismatch
      (loop for (asset . balance) in accum
         do
           (unless (bc= balance 0)
             (error "Balance mismatch, asset: ~s, diff: ~s"
                    asset balance)))
      ;; Check for fraction mismatch
      (setf res (delete-if-not (lambda (info)
                                 (storage-info-percent (cdr info)))
                               res))
      (loop for (asset . info) in res
         for key = (fraction-balance-key id asset)
         for msg = (db-get db key)
         for was = (and msg (unwrap-fraction msg asset))
         for sb = (storage-info-fraction info)
         do
           (unless (equal sb was)
             (break "Fraction mismatch, asset: ~s, SB: ~s, Was: ~s"
                    asset sb was)
             (error "Fraction mismatch, asset: ~s, SB: ~s, Was: ~s"
                    asset sb was)))
      ;; Check outbox hash
      (when outbox-contents
        (let ((msg (db-get db (outbox-hash-key id))))
          (multiple-value-bind (newhash newcnt)
              (unwrap-hash msg $ATOUTBOXHASH $OUTBOXHASH)
            (multiple-value-bind (hash cnt)
                (dirhash db (outbox-key id) (unpacker server))
              (when (null hash) (setf hash ""))
              (when (null cnt) (setf cnt 0))
              (unless (and (equal hash newhash) (bc= cnt newcnt))
                (error "Outbox hash mismatch"))))))
      ;; Check balance hash
      (when (and balance-changed-p (not (equal id bankid)))
        (let ((msg (db-get db (balance-hash-key id))))
          (unless msg
            (error "Missing balance hash update"))
          (multiple-value-bind (newhash newcnt)
              (unwrap-hash msg $ATBALANCEHASH $BALANCEHASH)
            (multiple-value-bind (hash cnt)
                (balancehash db (unpacker server) (balance-key id))
              (unless (and (equal hash newhash)
                           (bc= cnt newcnt))
                (error "Balance hash mismatch"))))))
      res)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Request processing
;;;
 
(defvar *message-handlers* (make-equal-hash))

(defun get-message-handler (message)
  (or (gethash message *message-handlers*)
      (error "No message handler for ~s" message)))

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
         (msg (db-get db $PUBKEYSIG bankid))
         (args (unpack-bankmsg server msg $ATREGISTER))
         (req (getarg $MSG args))
         (res (get-parsemsg req)))

    (when coupon
      ;; Validate a coupon number
      (let* ((coupon-number-hash (sha1 coupon))
             (key (append-db-keys $COUPON coupon-number-hash))
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
          (dotcat res "." msg))))

    res))

(define-message-handler do-id $ID (server args reqs)
  "Lookup a public key."
  (declare (ignore reqs))
  (or (db-get (db server) $PUBKEYSIG (getarg $ID args))
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
        (let ((inbox (scan-inbox server id)))
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
        (setf (db-get db $PUBKEY id) pubkey
              (db-get db $PUBKEYSIG id) res)
        ;; Post the debit for the registration fee
        (when (> (bccomp regfee 0)  0)
          (let* ((spendmsg (bankmsg server
                                    $INBOX
                                    time
                                    (signed-spend
                                     server time id tokenid (bcsub 0 regfee)
                                     "Registration fee"))))
            (setf (db-get db (inbox-key id) time) spendmsg)))
        ;; Mark the account as created
        (db-put db (acct-last-key id) "1")
        (db-put db (acct-req-key id) "0")
        res))))

(define-message-handler do-getreq $GETREQ (server args reqs)
  "Process a getreq message"
  (declare (ignore reqs))
  (let ((id (getarg $CUSTOMER args)))
    (bankmsg server $REQ id (or (db-get (db server) (acct-req-key id server)) "0"))))

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
  "Process a getfees message."
  (declare (ignore reqs))
  (checkreq server args)
  (let* ((db (db server))
         (regfee (db-get db $REGFEE))
         (tranfee (db-get db $TRANFEE))
         (res (strcat regfee "." tranfee)))
    (dolist (type (db-contents db $FEE))
      (dotcat res "." (db-get db $FEE type)))
    res))

(define-message-handler do-setfees $SETFEES (server args reqs)
  "Process a setfees message."
  (let ((db (db server))
        (id (getarg $CUSTOMER args)))
    (unless (equal id (bankid server))
      (error "Only the bank can set fees"))
    (with-db-lock (db (acct-time-key id))
      (do-setfees-internal server args reqs))
    (bankmsg server $ATSETFEES (get-parsemsg (car reqs)))))

(defun do-setfees-internal (server args reqs)
  (let ((db (db server))
        (parser (parser server))
        (time (getarg $TIME args))
        (count (getarg $COUNT args))
        (cnt 0)
        (bankid (bankid server))
        (tokenid (tokenid server))
        (msg-alist nil)
        (fee-alist nil)
        (tranmsg nil)
        (tranfee nil)
        (regmsg nil)
        (regfee "0"))

    ;; Burn the transaction
    (deq-time server bankid time)

    (loop
       for req in (cdr reqs)
       for reqargs = (match-pattern parser req)
       for reqid = (getarg $CUSTOMER reqargs)
       for request = (getarg $REQUEST reqargs)
       for reqbank = (getarg $BANKID reqargs)
       for reqtime = (getarg $TIME reqargs)
       for reqop = (getarg $OPERATION reqargs)
       for reqasset = (getarg $ASSET reqargs)
       for reqamount = (getarg $AMOUNT reqargs)
       for reqmsg = (get-parsemsg req)
       do
         (incf cnt)
         (unless (and (equal reqid bankid) (equal reqbank bankid))
           (error "Fee record not from and for bank"))
         (unless (equal reqtime time) (error "Timestamp mismatch"))
           (unless (equal reqid bankid) (error "ID mismatch"))
         (let ((amt (ignore-errors (parse-integer reqamount))))
           (unless (and amt
                        (>= amt (if (equal request $FEE) 0 1)))
             (error "Fee amount not a positive integer")))
         (cond ((or (equal request $TRANFEE)
                    (equal request $REGFEE))
                (unless (equal reqasset tokenid)
                  (error "~a asset must be token id" request))
                (if (equal request $TRANFEE)
                    (setf tranmsg reqmsg
                          tranfee reqamount)
                    (setf regmsg reqmsg
                          regfee reqamount)))
               ((equal request $FEE)
                (when (blankp reqop)
                  (error "Fee operation may not be blank"))
                (unless (db-get db $ASSET reqasset)
                  (error "Asset unknown: ~s" reqasset))
                (let ((cell (assocequal reqop msg-alist)))
                  (if cell
                      (dotcat (cdr cell) "." reqmsg)
                      (push (cons reqop reqmsg) msg-alist)))
                (let ((cell (assocequal reqop fee-alist)))
                  (if cell
                      (setf (cdr cell)
                            (cons reqamount (cons reqasset (cdr cell))))
                      (push (list reqop reqamount reqasset) fee-alist))))
               (t (error "Unknown fee request: ~s" request))))
    (unless tranmsg
      (setf tranfee *tranfee-default*
            tranmsg (bankmsg server $TRANFEE bankid time tokenid tranfee)))
    (unless regmsg
      (setf regfee *regfee-default*
            regmsg (bankmsg server $REGFEE bankid time tokenid regfee)))
    (unless (bc= cnt count)
      (error "Wrong number of fee messages, SB: ~s, was: ~s" count cnt))
    
    (dolist (cell fee-alist)
      (setf (cdr cell) (nreverse (cdr cell))))

    (setf (tranfee server) tranfee
          (regfee server) regfee
          (fees server) fee-alist)

    (setf (db-get db $TRANFEE) tranmsg
          (db-get db $REGFEE) regmsg)

    (let ((operations (db-contents db $FEE)))
      (dolist (cell msg-alist)
        (let ((operation (car cell)))
          (setf (db-get db $FEE operation) (cdr cell))
          (setf operations (delete operation operations :test #'equal))))
      (dolist (operation operations)
        (setf (db-get db $FEE operation) nil)))))

(defun getfees (server operation)
  (let ((res nil))
    (dolist (fee (fees server))
      (when (equal (car fee) operation)
        (push (cdr fee) res)))
    (nreverse res)))

(define-message-handler do-spend $SPEND (server args reqs)
  "Process a spend message."
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        res feesdiffs storage-infos)
    (with-db-lock (db (acct-time-key id))
      (multiple-value-setq (res feesdiffs storage-infos)
        (with-db-wrapper (db (db server))
          (do-spend-internal server db args reqs))))

    ;; Credit the non-refundable fees to the bank as storage fees
    (loop
       for (assetid . amount) in feesdiffs
       do
         (post-storage-fee server assetid (bankid server) amount nil))

    ;; This is outside the customer lock to avoid deadlock with the issuer account.
    (loop for (assetid . info) in storage-infos
       do
         (post-storage-fee
          server
          assetid
          (storage-info-issuer info)
          (storage-info-fee info)
          (storage-info-digits info)))
    res))

;; New implementation, using db-wrapper
;; Need to deal with negative spend, moving issuer to different acct or
;; different ID.
(defmethod do-spend-internal ((server server) db args reqs)
  "Do the work for a spend.
   Returns three values:
    res: The result message to be returned to the client
    feediffs: list of (assetid . amount) pairs for bank fees
    storage-infos: list of storage-info instances for storage fees"
  (let* ((bankid (bankid server))
         (parser (parser server))
         (spendmsg (get-parsemsg (car reqs)))
         (id (getarg $CUSTOMER args))
         (time (getarg $TIME args))
         (id2 (getarg $ID args))
         (assetid (getarg $ASSET args))
         (amount (getarg $AMOUNT args))
         (note (getarg $NOTE args))
         (asset (lookup-asset server assetid))
         (operation (if (equal id id2) $TRANSFER $SPEND))
         (fees (and (not (equal id bankid)) (getfees server operation))))

    ;; Remove fees for assets for which the spender is the issuer
    (setf fees
          (delete-if
           #'(lambda (fee)
               (let ((asset (lookup-asset server (car fee))))
                 (equal id (getarg $CUSTOMER asset))))
           fees))

    ;; Burn the transaction, even if balances don't match.
    (deq-time server id time)

    (when (equal id2 bankid)
      (error "Spends to the bank are not allowed."))

    (unless asset
      (error "Unknown asset id: ~s" assetid))

    (unless (is-numeric-p amount)
      (error "Not a number: ~s" amount))

    ;; Make sure there are no bank debit inbox entries older
    ;; than the highest timestamp last read from the inbox
    (let ((inbox (scan-inbox server id))
          (last (get-acct-last server id)))
      (dolist (inmsg inbox)
        (let* ((inmsg-args (unpack-bankmsg server inmsg))
               (spendreq (getarg $MSG inmsg-args)))
          (when (and (or (< (bccomp last 0) 0)
                         (<= (bccomp (getarg $TIME inmsg-args) last) 0))
                     (let ((spendargs (match-pattern (parser server) spendreq)))
                       (and (equal bankid (getarg $CUSTOMER spendargs))
                            (equal $SPEND (getarg $REQUEST spendargs))
                            (< (bccomp (getarg $AMOUNT spendargs) 0) 0))))
            (error
             "Please process bank debits in your account before doing a spend.")))))

    (let* ((tokens (if (and (not (equal id id2))
                            (not (equal id bankid)))
                       (tranfee server)
                       "0"))
           (tokenid (tokenid server))
           (feesdiffs nil)
           (outbox-item (bankmsg server $ATSPEND spendmsg))
           (res outbox-item)
           (feemsg nil)
           (storageamts nil)  ;list of (assetid . amt) for storage fees
           (fracids nil)      ;list of assetid for fractions
           (outboxhash-msg nil)
           (balancehash-msg nil))

      (when (equal id2 $COUPON)
        ;; If it's a coupon request, generate the coupon
        (let* ((coupon-number (random-id))
               (bankurl (bankurl server))
               (coupon
                (if note
                    (bankmsg server $COUPON bankurl coupon-number
                             assetid amount note)
                    (bankmsg server $COUPON bankurl coupon-number
                             assetid amount)))
               (coupon-number-hash (sha1 coupon-number)))
          (setf (db-get db $COUPON coupon-number-hash) outbox-item)
          (setq coupon
                (bankmsg server $COUPONENVELOPE id
                         (pubkey-encrypt
                          coupon (db-get (pubkeydb server) id))))
          (dotcat res "." coupon)
          (dotcat outbox-item "." coupon)))

      (loop
         for req in (cdr reqs)
         for reqargs = (match-pattern parser req)
         for reqid = (getarg $CUSTOMER reqargs)
         for request = (getarg $REQUEST reqargs)
         for reqtime = (getarg $TIME reqargs)
         for reqmsg = (get-parsemsg req)
         do
           (unless (equal reqtime time) (error "Timestamp mismatch"))
           (unless (equal reqid id) (error "ID mismatch"))
           
           (cond ((equal request $TRANFEE)
                  (when feemsg (error "~s appeared multiple times" $TRANFEE))
                  (let ((tranasset (getarg $ASSET reqargs))
                        (tranamt (getarg $AMOUNT reqargs)))
                    (unless (and (equal tranasset tokenid)
                                 (equal tranamt tokens))
                      (error "Mismatched tranfee asset or amount")))
                  (setq feemsg (bankmsg server $ATTRANFEE reqmsg))
                  (dotcat outbox-item "." feemsg)
                  (dotcat res "." feemsg))                   
                 ((equal request $STORAGEFEE)
                  (let ((assetid (getarg $ASSET reqargs))
                        (storagemsg (bankmsg server $ATSTORAGEFEE reqmsg)))
                    (when (assocequal assetid storageamts)
                      (error "Duplicate asset in storage fees"))
                    (push (cons assetid (getarg $AMOUNT reqargs)) storageamts)
                    (dotcat res "." storagemsg)))
                 ((equal request $FRACTION)
                  (let* ((fracasset (getarg $ASSET reqargs))
                         (fracmsg (bankmsg server $ATFRACTION reqmsg))
                         (key (fraction-balance-key id fracasset)))
                    (when (member fracasset fracids :test #'equal)
                      (error "Duplicate fraction assetid"))
                    (push fracasset fracids)
                    (db-put db key fracmsg)
                    (dotcat res "." fracmsg)))
                 ((equal request $BALANCE)
                  (let ((balmsg (handle-wrapper-balance-msg
                                 server db id reqmsg reqargs)))
                    (dotcat res "." balmsg)))
                 ((equal request $OUTBOXHASH)
                  (when outboxhash-msg
                    (error "~s appeared multiple times" $OUTBOXHASH))
                  (setf outboxhash-msg (bankmsg server $ATOUTBOXHASH reqmsg))
                  (db-put db (outbox-hash-key id) outboxhash-msg)
                  (dotcat res "." outboxhash-msg))
                 ((equal request $BALANCEHASH)
                  (when balancehash-msg
                    (error "~s appeared multiple times" $BALANCEHASH))
                  (setq balancehash-msg (bankmsg server $ATBALANCEHASH reqmsg))
                  (db-put db (balance-hash-key id) balancehash-msg)
                  (dotcat res "." balancehash-msg))
                 ((equal request $FEE)
                  (let ((feeop (getarg $OPERATION reqargs))
                        (feeasset (getarg $ASSET reqargs))
                        (feeamount (getarg $AMOUNT reqargs)))
                    (unless (equal feeop operation)
                      (error "Fee operation SB: ~s, was: ~s" operation feeop))
                    (let ((cell (assocequal feeasset fees)))
                      (unless (and cell (bc= (cadr cell) feeamount))
                        (error "No fee known for assetid: ~s, amount: ~s"
                               feeasset feeamount))
                      (setf fees (delete cell fees :test #'eq)))
                    (push (cons feeasset feeamount) feesdiffs)
                    (let ((msg (bankmsg server $ATFEE reqmsg)))
                      (dotcat res "." msg))))
                 (t
                  (error "~s not valid for spend. Only ~s, ~s, ~s, ~s, ~s, & ~s"
                         request
                         $TRANFEE $STORAGEFEE $FRACTION
                         $BALANCE $OUTBOXHASH $BALANCEHASH))))

      ;; tranfee must be included if there's a transaction fee
      (when (and (not (eql 0 (bccomp tokens 0)))
                 (not feemsg)
                 (not (equal id id2)))
        (error "~s  missing" $TRANFEE))

      ;; Ensure all non-refundable fees have been included
      (when fees
        (error "Non-refundable fees missing"))

      ;; Write outbox and check outboxhash
      ;; outboxhash must be included, except on self spends
      (unless (or (equal id id2) (equal id bankid))
        (unless outboxhash-msg
          (error "~s missing" $OUTBOXHASH))
        (setf (db-get db (outbox-key id) time) outbox-item))

      ;; balancehash must be included, except on bank spends
      (unless (equal id bankid)
        (unless balancehash-msg
          (error "~s missing" $BALANCEHASH)))

      ;; Generate inbox item for recipient, if there is one
      (unless (or (equal id2 $COUPON) (equal id id2))
        (let* ((newtime (gettime server))
               (inbox-item (bankmsg server $INBOX newtime spendmsg)))
          (when feemsg
            (dotcat inbox-item "." feemsg))
          (setf (db-get db (inbox-key id2) newtime) inbox-item)))

      ;; Force the user to do another getinbox, if anything appears
      ;; in his inbox since he last processed it.
      (db-put db (acct-last-key id) "-1")

      (let* ((diffs `((,assetid . ,(if (equal id id2) "0" amount))
                      (,tokenid . ,tokens)
                      ,@feesdiffs))
             ;; Here's where the storage fees are computed and
             ;; the balances validated.
             (storage-infos (validate-db-update server db id time diffs)))
        (setf storage-infos
              (delete-if
               (lambda (info)
                 (or (null (storage-info-percent info))
                     (wbp ((storage-info-digits info))
                       (bc= (storage-info-fee info) 0))))
               storage-infos
               :key #'cdr))
        (setf storageamts
              (delete-if
               (lambda (amt) (wbp ((number-precision amt)) (bc= amt 0)))
               storageamts
               :key #'cdr))
        (unless (and (eql (length storage-infos) (length storageamts))
                     (loop for (assetid . info) in storage-infos
                        for amt = (cdr (assocequal assetid storageamts))
                        unless (equal amt (storage-info-fee info))
                        return nil
                        finally (return t)))
          (error "Storage info mismatch"))
        (values res feesdiffs storage-infos)))))

(defmethod post-storage-fee ((server server) assetid issuer storage-fee digits)
  "Credit storage fee to an asset issuer.
   Digits can be null when storage-fee has no fraction and you just want
   to preserve the digits in the existing sum."
  (let ((db (db server))
        (parser (parser server))
        (bankid (bankid server)))
    (with-db-lock (db (acct-time-key issuer))
      (let* ((key (storage-fee-key issuer assetid))
             (storage-msg (db-get db key)))
        (when storage-msg
          (let ((reqs (parse parser storage-msg)))
            (unless (eql 1 (length reqs))
              (error "Bad storagefee msg: ~s" storage-msg))
            (let* ((args (match-pattern parser (car reqs)))
                   (amount (getarg $AMOUNT args)))
              (unless (and (equal (getarg $CUSTOMER args) bankid)
                           (equal (getarg $REQUEST args) $STORAGEFEE)
                           (equal (getarg $BANKID args) bankid)
                           (equal (getarg $ASSET args) assetid))
                (error "Storage fee message malformed"))
              (when (null digits)
                (setf digits (number-precision amount)))
              (setq storage-fee (wbp (digits) (bcadd storage-fee amount))))))
        (let* ((time (gettime server))
               (storage-msg (bankmsg server $STORAGEFEE
                                     bankid time assetid storage-fee)))
          (db-put db key storage-msg))))))

(define-message-handler do-spendreject $SPENDREJECT (server args reqs)
  "Process a spend|reject"
  (let ((db (db server))
        (parser (parser server))
        (id (getarg $CUSTOMER args))
        (msg (get-parsemsg (car reqs))))
    (with-db-lock (db (acct-time-key id))
      (let* ((bankid (bankid server))
             (time (getarg $TIME args))
             (key (outbox-key id))
             (item (db-get db key time)))
        (unless item
          (error "No outbox entry for time: ~s" time))
        (let ((args (unpack-bankmsg server item $ATSPEND $SPEND)))
          (unless (equal time (getarg $TIME args))
            (error "Time mismatch in outbox item"))
          (when (equal (getarg $ID args) $COUPON)
            (error "Coupons must be redeemed, not cancelled"))
          (let* ((recipient (getarg $ID args))
                 (key (inbox-key recipient))
                 (inbox (db-contents db key))
                 (feeamt nil)
                 (feeasset nil))
            (dolist (intime inbox
                     (error "Time not found in inbox: ~s" time))
              (let* ((item (db-get db key intime))
                     (item2 nil)
                     (args (and item (unpack-bankmsg
                                      server item $INBOX $SPEND :no-error))))
                (when (and args (equal (getarg $TIME args) time))
                  ;; Calculate the fee, if there is one
                  (let* ((reqs (getarg $UNPACK-REQS-KEY args))
                         (req (second reqs)))
                    (when req
                      (let ((args (match-pattern parser req)))
                        (unless (equal (getarg $CUSTOMER args) bankid)
                          (error "Fee message not from bank"))
                        (when (equal (getarg $REQUEST args) $ATTRANFEE)
                          (setq req (getarg $MSG args)
                                args (match-pattern parser req))
                          (unless (equal (getarg $REQUEST args) $TRANFEE)
                            (error "Fee wrapper doesn't wrap fee message"))
                          (setq feeasset (getarg $ASSET args)
                                feeamt (getarg $AMOUNT args))))))
                  ;; Found the inbox item corresponding to the outbox item.
                  ;; Make sure it's still there.
                  (with-db-lock (db (append-db-keys key intime))
                    (setf item2 (db-get db key intime)
                          (db-get db key intime) nil))
                  (unless item2
                    (error "Recipient inbox item removed during spend-reject processing"))
                  (when feeamt
                    (add-to-bank-balance server feeasset feeamt))
                  (let* ((newtime (gettime server))
                         (item (bankmsg server $INBOX newtime msg))
                         (key (inbox-key id)))
                    (setf (db-get db key newtime) item)
                    (return-from do-spendreject item)))))))))))

(define-message-handler do-couponenvelope $COUPONENVELOPE (server args reqs)
  "Redeem coupon by moving it from coupon/<coupon> to the customer inbox.
   This isn't the right way to do this.
   It really wants to be like processinbox, with new balances.
   You have to do it this way for a new registration, though."
  (let ((db (db server))
        (id (getarg $CUSTOMER args)))
    (with-db-lock (db (acct-time-key id))
      (do-couponenvelope-raw server args id)
      (bankmsg server $ATCOUPONENVELOPE (get-parsemsg (car reqs))))))

(defun do-couponenvelope-raw (server args id)
  "Called by do_register to process coupons there.
   Returns an error string or false."
  (check-type server server)

  (let ((db (db server))
        (bankid (bankid server))
        (parser (parser server))
        (encrypted-to (getarg $ID args)))
    (unless (equal encrypted-to bankid)
      (error "Coupon not encrypted to bank"))

    (let* ((coupon (privkey-decrypt (getarg $ENCRYPTEDCOUPON args) (privkey server)))
           (coupon-number (and (coupon-number-p coupon) coupon)))

      (unless coupon-number
        (let ((args (unpack-bankmsg server coupon $COUPON)))
          (unless (equal bankid (getarg $CUSTOMER args))
            (error "Coupon not signed by bank"))
          (setq coupon-number (getarg $COUPON args))))

      (let* ((coupon-number-hash (sha1 coupon-number))
             (key (append-db-keys $COUPON coupon-number-hash))
             (outbox-item nil))
        (with-db-lock (db key)
          (setq outbox-item (db-get db key))
          (when outbox-item (db-put db key nil)))

    (cond ((not outbox-item)
           (let* ((key (inbox-key id))
                  (inbox (db-contents db key)))
             (or (dolist (time inbox nil)
                   (let ((item (db-get db key time)))
                     (when (search #.(strcat "," $COUPON ",") item)
                       (let ((reqs (parse parser item)))
                         (when (> (length reqs) 1)
                           (let* ((req (second reqs))
                                  (msg (match-pattern parser req)))
                             (when (equal coupon-number-hash (getarg COUPON msg))
                               ;; Customer already redeemded this coupon.
                               ;; Success if he tries to do it again.
                               (return t))))))))
                 (error "Coupon already redeemed"))))
          (t
           (let* ((ok nil)
                  (args (unwind-protect
                             (prog1 (unpack-bankmsg server outbox-item $ATSPEND)
                                    (setq ok t))
                          (unless ok
                            ;; Make sure the spender can cancel the coupon
                            (db-put db key outbox-item))))
                  (reqs (getarg $UNPACK-REQS-KEY args))
                  (spendreq (getarg $MSG args))
                  (spendmsg (get-parsemsg spendreq))
                  (feemsg nil)
                  (feereq nil)
                  (newtime (gettime server))
                  (inbox-item (bankmsg server $INBOX newtime spendmsg))
                  (cnhmsg (bankmsg server $COUPONNUMBERHASH coupon-number-hash)))
             (dotcat inbox-item "." cnhmsg)
             (when (> (length reqs) 1)
               (setq feereq (second reqs)
                     feemsg (get-parsemsg feereq))
               (dotcat inbox-item "." feemsg))
             (let ((key (append-db-keys (inbox-key id) newtime)))
               (db-put db key inbox-item)))))))))

(define-message-handler do-getinbox $GETINBOX (server args reqs)
  "Query inbox"
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (parser (parser server)))
    (with-db-lock (db (acct-time-key id))
      (checkreq server args)
      (let* ((inbox (scan-inbox server id))
             (res (bankmsg server $ATGETINBOX (get-parsemsg (car reqs))))
             (last "1"))

        (dolist (inmsg inbox)
          (dotcat res "." inmsg)
          (let ((args (match-message parser inmsg)))
            (when (equal (getarg $REQUEST args) $INBOX)
              (let ((time (getarg $TIME args)))
                (when (> (bccomp time last) 0)
                  (setq last time))))
            (setq args (match-pattern parser (getarg $MSG args)))
            (let ((argsid (getarg $ID args)))
              (unless (or (equal argsid id) (equal argsid $COUPON))
                (error "Inbox entry for wrong ID: ~s" id)))))

        (let* ((key (storage-fee-key id))
               (asset-ids (db-contents db key)))
          (dolist (assetid asset-ids)
            (dotcat res "." (db-get db key assetid))))

    ;; Update last time
    (db-put db (acct-last-key id) last)

    res))))

(define-message-handler do-processinbox $PROCESSINBOX (server args reqs)
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        res storage-infos)

    (with-db-lock (db (acct-time-key id))
      (multiple-value-setq (res storage-infos)
        (with-db-wrapper (db (db server))
          (do-processinbox-internal server db args reqs))))
      
    (loop
       for (assetid . assetinfo) in storage-infos
       for issuer = (storage-info-issuer assetinfo)
       for storage-fee = (storage-info-fee assetinfo)
       for digits = (storage-info-digits assetinfo)
       when (and issuer storage-fee digits)
       do
         (post-storage-fee server assetid issuer storage-fee digits))

    res))

(defun checktime (was sb place)
  (unless (equal was sb)
    (error "Time mismatch in ~a, was: ~s, sb: ~s"
           place was sb)))

(defmethod do-processinbox-internal ((server server) db args reqs)
  (let* ((bankid (bankid server))
         (parser (parser server))
         (id (getarg $CUSTOMER args))
         (time (getarg $TIME args))
         (timelist (getarg $TIMELIST args))
         (inboxtimes (explode #\| timelist))
         (spends (make-equal-hash))
         (fees (make-equal-hash))
         (accepts nil)
         (rejects nil)
         (storagemsgs (make-equal-hash))
         (fracmsgs (make-equal-hash))
         (inbox-key (inbox-key id))
         (storage-infos nil)
         (tobecharged nil)
         (inboxmsgs nil)
         (res (bankmsg server $ATPROCESSINBOX (get-parsemsg (car reqs))))
         (outboxtimes nil)
         (outboxhashreq nil)
         (balancehashreq nil)
         (diffs nil))

    ;; Burn the transaction, even if balances don't match.
    (deq-time server id time)

    ;; Collect the inbox items being processed
    (dolist (inboxtime inboxtimes)
      (let* ((item (db-get db inbox-key inboxtime))
             (itemargs (if (null item)
                           (error "No inbox item for time: ~s" inboxtime)
                           (unpack-bankmsg server item $INBOX t)))
             (request (getarg $REQUEST itemargs)))
        (unless (or (equal (getarg $ID itemargs) id)
                    (equal (getarg $ID itemargs) $COUPON))
          (error "Inbox corrupt. Item found for other customer"))
        (cond ((equal request $SPEND)
               (let* ((itemtime (getarg $TIME itemargs))
                      (itemreqs (getarg $UNPACK-REQS-KEY itemargs))
                      (itemcnt (length itemreqs))
                      (feereq (and (> itemcnt 1) (car (last itemreqs)))))
                 (setf (gethash itemtime spends) itemargs)
                 (when feereq
                   (let ((feeargs (match-pattern parser feereq)))
                     (when (equal (getarg $REQUEST feeargs) $ATTRANFEE)
                       (setq feeargs (match-pattern parser (getarg $MSG feeargs))))
                     (unless (equal (getarg $REQUEST feeargs) $TRANFEE)
                       (error "Inbox corrupt. Fee not properly encoded"))
                     (setf (gethash itemtime fees) feeargs)))))
              ((equal request $SPENDACCEPT)
               (push itemargs accepts))
              ((equal request $SPENDREJECT)
               (push itemargs rejects))
              (t (error "Inbox corrupted. Found '~a' item" request)))))

    (setq accepts (nreverse accepts)
          rejects (nreverse rejects))

    ;; Refund the transaction fees for accepted spends
    (dolist (itemargs accepts)
      (let* ((outboxtime (getarg $TIME itemargs))
             (spendfeeargs (get-outbox-args server id outboxtime))
             (feeargs (cdr spendfeeargs)))
        (push outboxtime outboxtimes)
        (when feeargs
          (let ((asset (getarg $ASSET feeargs))
                (amt (getarg $AMOUNT feeargs)))
            (push (cons asset (bcsub amt)) diffs)))))

    ;; Credit the spend amounts for rejected spends, but do NOT
    ;; refund the transaction fees.
    (dolist (itemargs rejects)
      (let* ((outboxtime (getarg $TIME itemargs))
             (spendfeeargs (get-outbox-args server id outboxtime))
             (spendargs (car spendfeeargs))
             (asset (getarg $ASSET spendargs))
             (amt (getarg $AMOUNT spendargs))
             (spendtime (getarg $TIME spendargs)))
        (push outboxtime outboxtimes)
        (push (cons asset (bcsub amt)) diffs)
        (push (list asset amt spendtime) tobecharged)))

    ;; Go through the rest of the processinbox items, collecting
    ;; accept and reject instructions and balances.
    (dolist (req (cdr reqs))
      (let* ((reqmsg (get-parsemsg req))
             (args (match-pattern parser req))
             (request (getarg $REQUEST args))
             (argstime (getarg $TIME args)))
        (unless (equal (getarg $CUSTOMER args) id)
          (error "Item not from same customer as ~s" $PROCESSINBOX))
        (cond ((or (equal request $SPENDACCEPT)
                   (equal request $SPENDREJECT))
               ;; $SPENDACCEPT => array($BANKID,$TIME,$id,$NOTE=>1),
               ;; $SPENDREJECT => array($BANKID,$TIME,$id,$NOTE=>1),
               (let* ((otherid (getarg $ID args))
                      (itemargs (gethash argstime spends)))
                 (unless itemargs
                   (error "'~a' not matched in '~a' item, argstime: ~a"
                          request $PROCESSINBOX argstime))
                 (cond ((equal request $SPENDACCEPT)
                        ;; Accepting the payment. Credit it.
                        (let ((itemasset (getarg $ASSET itemargs))
                              (itemamt (getarg $AMOUNT itemargs))
                              (itemtime (getarg $TIME itemargs)))
                          (push (cons itemasset (bcsub itemamt)) diffs)
                          (push (list itemasset itemamt itemtime) tobecharged))
                        (dotcat res "." (bankmsg server $ATSPENDACCEPT reqmsg)))
                       (t
                        ;; Rejecting the payment. Credit the fee.
                        (let ((feeargs (gethash argstime fees)))
                          (when feeargs
                            (let ((feeasset (getarg $ASSET feeargs))
                                  (feeamt (getarg $AMOUNT feeargs)))
                              (push (cons feeasset (bcsub feeamt)) diffs))))
                        (dotcat res "." (bankmsg server $ATSPENDREJECT reqmsg))))
                 (let (inboxtime inboxmsg)
                   (cond ((equal otherid bankid)
                          (when (and (equal request $SPENDREJECT)
                                     (< (bccomp (getarg $AMOUNT itemargs) 0) 0))
                            (error "You may not reject a bank charge"))
                          (setq inboxtime request
                                inboxmsg itemargs))
                         (t 
                          (setq inboxtime (gettime server)
                                inboxmsg (bankmsg server $INBOX
                                                  inboxtime reqmsg))))
                   (push (list otherid inboxtime inboxmsg) inboxmsgs))))
              ((equal request $STORAGEFEE)
               (checktime argstime time "storagefee item")
               (let ((storageasset (getarg $ASSET args)))
                 (when (gethash storageasset storagemsgs)
                   (error "Duplicate storage fee for asset: ~s" storageasset))
                 (setf (gethash storageasset storagemsgs) reqmsg)
                 (let ((msg (bankmsg server $ATSTORAGEFEE reqmsg)))
                   (dotcat res "." msg))))
              ((equal request $FRACTION)
               (checktime argstime time "fraction item")
               (let ((fracasset (getarg $ASSET args)))
                 (when (gethash fracasset fracmsgs)
                   (error "Duplicate fraction balance for asset: ~s" fracasset))
                 (setf (gethash fracasset fracmsgs) reqmsg)
                 (let ((fracmsg (bankmsg server $ATFRACTION reqmsg)))
                   (dotcat res "." fracmsg)
                   (db-put db (fraction-balance-key id fracasset) fracmsg))))
              ((equal request $OUTBOXHASH)
               (unless (equal id bankid)
                 (when outboxhashreq
                   (error "~s appeared multiple times" $OUTBOXHASH))
                 (checktime argstime time "outboxhash")
                 (setq outboxhashreq req)
                 (let ((item (bankmsg server $ATOUTBOXHASH (get-parsemsg req))))
                   (dotcat res "." item)
                   (db-put db (outbox-hash-key id) item))))
              ((equal request $BALANCE)
               (checktime argstime time "balance item")
               (let ((balmsg (handle-wrapper-balance-msg
                              server db id reqmsg args)))
                 (dotcat res "." balmsg)))
              ((equal request $BALANCEHASH)
               (unless (equal id bankid)
                 (when balancehashreq
                   (error "~s appeared multiple times" $BALANCEHASH))
                 (checktime argstime time "balancehash")
                 (setq balancehashreq req)
                 (let ((item (bankmsg server $ATBALANCEHASH
                                      (get-parsemsg req))))
                   (dotcat res "." item)
                   (db-put db (balance-hash-key id) item))))
              (t
               (error "~s not valid for ~s, only ~s, ~s, ~s, ~s, ~s, ~s, & ~s"
                      request $PROCESSINBOX
                      $SPENDACCEPT $SPENDREJECT
                      $STORAGEFEE $FRACTION $OUTBOXHASH
                      $BALANCE $BALANCEHASH)))))

    ;; No outbox hash maintained for the bank
    (unless (equal id bankid)
      ;; Make sure the outbox hash was included iff needed
      (when (or (and outboxtimes (not outboxhashreq))
                (and (null outboxtimes) outboxhashreq))
        (error (if outboxhashreq
                   "~s included when not needed"
                   "~s missing")
               $OUTBOXHASH)))

    ;; Remove no longer needed inbox and outbox entries.
    ;; Probably should have a bank config parameter to archive these somewhere.
    (let ((inboxkey (inbox-key id)))
      (dolist (inboxtime inboxtimes)
        (setf (db-get db inboxkey inboxtime) "")))

    ;; Clear processed outbox entries
    (let ((outboxkey (outbox-key id)))
      (dolist (outboxtime outboxtimes)
        (setf (db-get db outboxkey outboxtime) "")))

    ;; Verify balances
    (setf storage-infos
          (validate-db-update server db id time diffs tobecharged))

    ;; Update accepted and rejected spenders' inboxes
    (dolist (inboxmsg (reverse inboxmsgs))
      (destructuring-bind (otherid inboxtime inboxmsg) inboxmsg
        (cond ((equal otherid bankid)
               (let ((request inboxtime)
                     (itemargs inboxmsg))
                 (when (equal request $SPENDREJECT)
                   ;; Return the funds to the bank's account
                   ;; (should this write into the wrapped-db instead
                   ;;  of directly to disk? We're just about to commit).
                   (add-to-bank-balance
                    server
                    (getarg $ASSET itemargs)
                    (getarg $AMOUNT itemargs)))))
              (t
               (let ((inboxkey (inbox-key otherid)))
                 (setf (db-get db inboxkey inboxtime) inboxmsg))))))

    (values res storage-infos)))

(defun get-outbox-args (server id spendtime)
  (check-type server server)

  (let* ((db (db server))
         (parser (parser server))
         (bankid (bankid server))
         (outboxkey (outbox-key id))
         (spendmsg (or (db-get db outboxkey spendtime)
                       (error "Can't find outbox item: ~s" spendtime)))
         (reqs (parse parser spendmsg))
         (spendargs (match-pattern parser (car reqs)))
         (feeargs (and (> (length reqs) 1)
                       (match-pattern parser (second reqs)))))
    (unless (and (equal (getarg $CUSTOMER spendargs) bankid)
                 (equal (getarg $REQUEST spendargs) $ATSPEND)
                 (or (null feeargs)
                     (and (equal (getarg $CUSTOMER feeargs) bankid)
                          (equal (getarg $REQUEST feeargs) $ATTRANFEE))))
      (error "Outbox corrupted"))

    (setq spendargs (match-pattern parser (getarg $MSG spendargs)))
    (when feeargs
      (setq feeargs (match-pattern parser (getarg $MSG feeargs))))
    (unless (and (equal (getarg $CUSTOMER spendargs) id)
                 (equal (getarg $REQUEST spendargs) $SPEND)
                 (or (null feeargs)
                     (and (equal (getarg $CUSTOMER feeargs) id)
                          (equal (getarg $REQUEST feeargs) $TRANFEE))))
      (error "Outbox inner messages corrupted"))
    (cons spendargs feeargs)))

(define-message-handler do-storagefees $STORAGEFEES (server args reqs)
  ;; Process a storagefees request
  (let ((db (db server))
        (bankid (bankid server))
        (id (getarg $CUSTOMER args)))
    (with-db-lock (db (acct-time-key id))
      (checkreq server args)
      (let* ((inboxkey (inbox-key id))
             (key (storage-fee-key id))
             (assetids (db-contents db key)))
        (dolist (assetid assetids)
          (let* ((storagefee (db-get db key assetid))
                 (args (unpack-bankmsg server storagefee $STORAGEFEE))
                 (amount (getarg $AMOUNT args)))
            (unless (equal assetid (getarg $ASSET args))
              (error "Asset mismatch, sb: ~s, was: ~s"
                     assetid (getarg $ASSET args)))
            (multiple-value-bind (amount fraction)
                (split-decimal amount)
              (when (> (bccomp amount 0) 0)
                (let* ((time (gettime server))
                       (storagefee (bankmsg server $STORAGEFEE
                                            bankid time assetid fraction))
                       (spend (bankmsg server $SPEND
                                       bankid time id assetid amount
                                       "Storage fees"))
                       (inbox (bankmsg server $INBOX time spend)))
                  (setf (db-get db key assetid) storagefee)
                  (setf (db-get db inboxkey time) inbox)))))))
      (bankmsg server $ATSTORAGEFEES (get-parsemsg (car reqs))))))

(define-message-handler do-getasset $GETASSET (server args reqs)
  (declare (ignore reqs))
  (let ((db (db server))
        (id (getarg $CUSTOMER args)))
    (with-db-lock (db (acct-time-key id))
      (checkreq server args)
      (let ((assetid (getarg $ASSET args)))
        (unless (and assetid (> (length assetid) 0))
          (error "Illegal assetid: ~s" assetid))
        (or (db-get db $ASSET assetid)
            (error "Unknown asset: ~s" assetid))))))

(define-message-handler do-asset $ASSET (server args reqs)
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (bankid (bankid server))
        (parser (parser server)))
    (with-db-lock (db (acct-time-key id))

      (when (< (length reqs) 2)
        (error "No balance items"))

      ;; $ASSET => array($BANKID,$ASSET,$SCALE,$PRECISION,$ASSETNAME),
      (let* ((assetid (getarg $ASSET args))
             (scale (getarg $SCALE args))
             (precision (getarg $PRECISION args))
             (assetname (getarg $ASSETNAME args))
             (storage-msg nil)
             (exists-p (is-asset-p server assetid))
             (tokens (if (equal id bankid) "0" "1"))
             (bals (make-equal-hash))
             (acctbals (make-equal-hash))
             (oldneg (make-equal-hash))
             (newneg (make-equal-hash))
             (tokenid (tokenid server))
             (time nil)
             (state (make-balance-state
                     :acctbals acctbals
                     :bals bals
                     :tokens tokens
                     :oldneg oldneg
                     :newneg newneg
                     ;; 'time' initialized below
                     ))
             (balancehashreq nil)
             (balancehash nil)
             (balancehashcnt nil)
             (balancehashmsg nil)
             )

        (unless (and (is-numeric-p scale t) (is-numeric-p precision t)
                     (> (bccomp scale 0) 0) (> (bccomp precision 0) 0))
          (error "Scale & precision must be integers >= 0"))

        (when (> (bccomp scale 10) 0)
          (error "Maximum scale is 10"))

        ;; Don't really need this restriction. Maybe widen it a bit?
        (unless (every #'is-alphanumeric-or-space-p assetname)
          (error "Asset name must contain only letters and digits"))

        (unless (equal assetid (assetid id scale precision assetname))
          (error "Asset id is not sha1 hash of 'id,scale,precision,name'"))

        (unless exists-p (setf (gethash assetid bals) "-1"))
        (setf (gethash tokenid bals) "0")

        (dolist (req (cdr reqs))
          (let* ((reqargs (match-pattern parser req))
                 (reqid (getarg $CUSTOMER reqargs))
                 (request (getarg $REQUEST reqargs))
                 (reqtime (getarg $TIME reqargs))
                 (reqmsg (get-parsemsg req)))
            (unless time
              ;; Burn the transaction
              (setq time reqtime)
              (deq-time server id time)
              (setf (balance-state-time state) time))
            (unless (equal reqid id) (error "ID mismatch"))
            (cond ((equal request $STORAGE)
                   (when storage-msg (error "Duplicate storage fee"))
                   (let ((percent (getarg $PERCENT reqargs)))
                     (unless (blankp percent)
                       (unless (and (is-numeric-p percent)
                                    (>= (bccomp percent 0) 0)
                                    (< (bccomp percent 10) 0))
                         (error "Storage fee must be a positive number less than 10"))))
                   (setq storage-msg reqmsg))
                  ((equal request $BALANCE)
                   (handle-balance-msg server id reqmsg reqargs state assetid))
                  ((equal request $BALANCEHASH)
                   (when balancehashreq
                     (error "~s appeared multiple times" $BALANCEHASH))
                   (setq balancehashreq req
                         balancehash (getarg $HASH reqargs)
                         balancehashcnt (getarg $COUNT reqargs)
                         balancehashmsg reqmsg))
                  (t (error "~s not valid for asset creation. Only ~s, ~s, & ~s"
                            request $STORAGE $BALANCE $BALANCEHASH)))))

        ;; Check that we have exactly as many negative balances after the transaction
        ;; as we had before, plus one for the new asset.
        (unless exists-p (setf (gethash assetid oldneg) $MAIN))
        (unless (equal (hash-table-count oldneg) (hash-table-count newneg))
          (error "Negative balance count not conserved"))
        (loop
           for asset being the hash-keys of oldneg
           do
           (unless (gethash asset newneg)
             (error "Negative balance assets not conserved")))

        ;; Charge the new file tokens
        (setq tokens (balance-state-tokens state))
        (setf (gethash tokenid bals) (bcsub (gethash tokenid bals 0) tokens))

        (let ((errmsg nil))
          ;; Check that the balances in the spend message, match the current balance,
          ;; minus amount spent minus fees.
          (loop
             for balasset being the hash-key using (hash-value balamount) of bals
             unless (eql 0 (bccomp balamount 0))
             do
             (let ((name (if (equal balasset assetid)
                             assetname
                             (lookup-asset-name server balasset))))
               (if errmsg (dotcat errmsg ", ") (setq errmsg ""))
               (dotcat errmsg name ": " balamount)))
          (when errmsg (error "Balance discrepancies: ~s" errmsg)))

        ;; balancehash must be included
        (unless (equal id bankid)
          (unless balancehashreq (error "~s missing" $BALANCEHASH))

          (multiple-value-bind (hash hashcnt)
              (balancehash db (unpacker server) (balance-key id) acctbals)
            (unless (and (equal balancehash hash)
                         (eql 0 (bccomp balancehashcnt hashcnt)))
              (error "~s mismatch, hash: ~s, sb: ~s, count: ~s, sb: ~s"
                     $BALANCEHASH balancehash hash balancehashcnt hashcnt))))
  
        ;; All's well with the world. Commit this puppy.
        ;; Add asset
        (let ((res (bankmsg server $ATASSET (get-parsemsg (car reqs)))))
          (when storage-msg
            (dotcat res "." (bankmsg server $ATSTORAGE storage-msg)))

          (setf (db-get db $ASSET assetid) res)

          ;; Credit bank with tokens
          (add-to-bank-balance server tokenid tokens)

          ;; Update balances
          (let ((balancekey (balance-key id)))
            (loop
               for acct being the hash-key using (hash-value balances) of acctbals
               for acctkey = (append-db-keys balancekey acct)
               do
               (loop
                  for balasset being the hash-key using (hash-value balance)
                  of balances
                  for balmsg = (bankmsg server $ATBALANCE balance)
                  do
                  (dotcat res "." balmsg)
                  (setf (db-get db acctkey balasset) balmsg))))

          ;; Update balancehash
          (unless (equal id bankid)
            (let ((balancehash-item (bankmsg server $ATBALANCEHASH balancehashmsg)))
              (dotcat res "." balancehash-item)
              (db-put db (balance-hash-key id) balancehash-item)))

          res)))))

(define-message-handler do-getbalance $GETBALANCE (server args reqs)
  (declare (ignore reqs))
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (acct (getarg $ACCT args))
        (assetid (getarg $ASSET args))
        (res "")
        assetnames
        assetkeys
        acctkeys
        assetids)

    (with-db-lock (db (acct-time-key id))
      (checkreq server args)

      (cond ((and acct (not (equal acct "")))
             (setq acctkeys (list (acct-balance-key id acct))))
            (t (let* ((balancekey (balance-key id))
                      (acctnames (db-contents db balancekey)))
                 (dolist (name acctnames)
                   (push (append-db-keys balancekey name) acctkeys))
                 (setf acctkeys (nreverse acctkeys)))))

      (dolist (acctkey acctkeys)
        (setq assetnames
              (if (and assetid (not (equal assetid "")))
                  (list assetid)
                  (db-contents db acctkey))
              assetkeys nil)
        (dolist (name assetnames)
          (push (append-db-keys acctkey name) assetkeys))
        (dolist (assetkey assetkeys)
          (let ((bal (db-get db assetkey)))
            (when bal
                (unless (equal res "") (dotcat res "."))
                (dotcat res bal)))))

      ;; Get the fractions
      (setq assetids
            (if assetid
                (list assetid)
                assetnames))
      (dolist (assetid assetids)
        (let* ((fractionkey (fraction-balance-key id assetid))
               (fraction (db-get db fractionkey)))
          (when fraction (dotcat res "." fraction))))

      (let ((balancehash (db-get db (balance-hash-key id))))
        (when balancehash
          (unless (equal res "") (dotcat res "."))
          (dotcat res balancehash))))

    res))

(define-message-handler do-getoutbox $GETOUTBOX (server args reqs)
  (let ((db (db server))
        (id (getarg $CUSTOMER args)))
    (with-db-lock (db (acct-time-key id))
      (checkreq server args)

      ;; $GETOUTBOX => array($BANKID,$REQ)
      (let* ((id (getarg $CUSTOMER args))
             (msg (bankmsg server $ATGETOUTBOX (get-parsemsg (car reqs))))
             (outboxkey (outbox-key id))
             (contents (db-contents db outboxkey))
             (outboxhash (db-get db (outbox-hash-key id))))

        (dolist (time contents)
          (dotcat msg "." (db-get db outboxkey time)))

        (when outboxhash (dotcat msg "." outboxhash))

        msg))))

;; These are set by trubanc-client-web::save-trubanc-application
(defvar *last-commit* nil)
(defvar *save-application-time* nil)

(define-message-handler do-getversion $GETVERSION (server args reqs)
  (declare (ignore reqs))
  (let ((db (db server))
        (id (getarg $CUSTOMER args)))
    (with-db-lock (db (acct-time-key id))
      (checkreq server args)
      (bankmsg server $VERSION
               (or *last-commit* "")
               (stringify (or *save-application-time* (get-unix-time)) "~d")))))

(defun data-key-hash (key id)
  (sha1 (if id (strcat id "." key) key)))

(define-message-handler do-writedata $WRITEDATA (server args reqs)
  (let* ((db (db server))
         (parser (parser server))
         (tokenid (tokenid server))
         (id (getarg $CUSTOMER args))
         (time (getarg $TIME args))
         (anonymous (getarg $ANONYMOUS args))
         (anonymous-p (not (blankp anonymous)))
         (key (getarg $KEY args))
         (data (getarg $DATA args))
         (keyhash (data-key-hash key (unless anonymous-p id)))
         (contents (unless (blankp data) (bankmsg server $ATREADDATA id time data)))
         (old-contents (db-get db $DATA keyhash))
         (oldargs (and old-contents (match-message parser old-contents)))
         (old-data (and oldargs (getarg $DATA oldargs)))
         (old-cost (if old-data (data-cost old-data) 0))
         (net-cost (- (data-cost data) old-cost))
         balancemsg
         balance
         balancehashmsg
         balancehashcnt
         balancehash)

    ;; Burn the transaction
    (deq-time server id time)

    ;; Get the balance and balancehash messages
    (dolist (req (cdr reqs))
      (let* ((msg (get-parsemsg req))
             (reqargs (match-pattern parser req))
             (request (getarg $REQUEST reqargs))
             (reqtime (getarg $TIME reqargs)))
        (checktime reqtime time request)
        (cond ((equal request $BALANCE)
               (when balancemsg (error "Duplicate balance message"))
               (unless (equal (getarg $ASSET reqargs) tokenid)
                 (error "Asset in balance message not tokenid"))
               (setq balancemsg msg
                     balance (getarg $AMOUNT reqargs)))
              ((equal request $BALANCEHASH)
               (when balancehashmsg (error "Duplication balancehash message"))
               (setq balancehashmsg msg
                     balancehashcnt (getarg $COUNT reqargs)
                     balancehash (getarg $HASH reqargs)))
              (t (error "Bad request in writedata: ~s" request)))))

    ;; Check balance
    (unless balancemsg
      (error "Balance message missing"))
    (let ((bal (bcsub (asset-balance server id tokenid) net-cost)))
      (unless (eql 0 (bccomp bal balance))
        (error "Balance mismatch, sb: ~s, was: ~s" bal balance)))

    ;; Check balance hash
    (unless balancehashmsg
      (error "Balance hash message missing"))
    (let ((acctbals (make-equal-hash $MAIN (make-equal-hash tokenid balance))))
      (multiple-value-bind (hash hashcnt)
          (balancehash db (unpacker server) (balance-key id) acctbals)
        (unless (and (equal balancehash hash)
                     (eql 0 (bccomp balancehashcnt hashcnt)))
          (error "~s mismatch, hash: ~s, sb: ~s, count: ~s, sb: ~s"
                   $BALANCEHASH balancehash hash balancehashcnt hashcnt))))

    ;; All is well.
    ;; Bank wrap the balance and balancehash
    (setq balancemsg (bankmsg server $ATBALANCE balancemsg)
          balancehashmsg (bankmsg server $ATBALANCEHASH balancehashmsg))

    ;; Write the data, balance, and balancehash
    (setf (db-get db $DATA keyhash) contents
          (db-get db (asset-balance-key id tokenid)) balancemsg
          (db-get db (balance-hash-key id)) balancehashmsg)

    ;; And cons up the return message
    (strcat (bankmsg server $ATWRITEDATA id time anonymous key)
            "."
            balancemsg
            "."
            balancehashmsg)))

(define-message-handler do-readdata $READDATA (server args reqs)
  (declare (ignore reqs))
  (let* ((db (db server))
         (id (getarg $CUSTOMER args))
         (id-zero-p (equal id "0"))
         (key (getarg $KEY args))
         (size (getarg $SIZE args)))
    (unless id-zero-p
      (checkreq server args))
    (let* ((keyhash (data-key-hash key (unless id-zero-p id)))
           (contents (or (db-get db $DATA keyhash) (error "No data for key")))
           (args (match-message (parser server) contents))
           (request (getarg $REQUEST args))
           (data-id (getarg $ID args))
           (time (getarg $TIME args))
           (data (getarg $DATA args)))
      (unless (equal request $ATREADDATA)
        (error "Wrong request in readdata db message: ~s" request))
      (unless (or id-zero-p (equal id data-id))
        ;; Bloody unlikely, but possible, I suppose
        (error "Attempt to read data from another id"))
      (let ((res-data (if (blankp size)
                          data
                          (stringify (length data)))))
        (bankmsg server $ATREADDATA id time res-data)))))


(defstruct grant
  id
  toid
  permission
  grant
  msg)

(defun parse-grants (server id permission)
  (let* ((msg (db-get (db server) (permission-key id permission)))
         (parser (parser server))
         (reqs (and msg (parse parser msg)))
         (bankid (bankid server))
         (res nil))
    (dolist (req reqs)
      (let* ((args (match-pattern parser req))
             (toid (getarg $ID args)))
        (unless (equal toid id)
          (error "Grant not for proper id. SB: ~s, was: ~s"
                 id toid))
        (unless (equal bankid (getarg $BANKID args))
          (error "Bad bankid in grant, SB: ~s, was: ~s"
                 bankid (getarg $BANKID args)))
        (unless (equal permission (getarg $PERMISSION args))
          (error "Bad permission in grant, SB: ~s, was: ~s"
                 permission (getarg $PERMISSION args)))
        (push (make-grant
               :id (getarg $CUSTOMER args)
               :toid toid
               :permission permission
               :grant (getarg $GRANT args)
               :msg (get-parsemsg req))
              res)))
    (values (nreverse res) msg)))

(defun ensure-grant-permission (server id permission)
  (unless (or (equal id (bankid server))
              (let ((grants (parse-grants server id permission)))
                (find $GRANT grants :test #'equal :key #'grant-grant)))
    (error "You don't have permission to grant permission: ~s" permission)))

(define-message-handler do-grant $GRANT (server args msgs)
  ;; (<id>,grant,<bankid>,<req#>,<toid>,<permission>,grant=grant)
  (declare (ignore msgs))
  (checkreq server args)
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (bankid (getarg $BANKID args))
        (toid (getarg $ID args))
        (permission (getarg $PERMISSION args)))
    (unless (equal bankid (bankid server))
      (error "Bad bankid in grant request"))
    (unless (db-get (pubkeydb server) toid)
      (error "No account for toid: ~s" toid))
    (ensure-grant-permission server id permission)
    (with-db-lock (db (acct-time-key toid))
      (do-grant-internal server args id toid permission))))

(defun do-grant-internal (server args id toid permission)
  (let* ((db (db server)))
    (let* ((grants (parse-grants server toid permission))
           (msg (bankmsg server $ATGRANT (get-parsemsg args)))
           (res msg))
      (dolist (grant grants)
        (unless (equal id (grant-id grant))
          (dotcat msg "." (grant-msg grant))))
      (db-put db (permission-key toid permission) msg)
      res)))

(define-message-handler do-deny $DENY (server args msgs)
  ;; (<id>,deny,<bankid>,<req#>,<toid>,<permission>)
  (declare (ignore msgs))
  (checkreq server args)
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (bankid (getarg $BANKID args))
        (toid (getarg $ID args))
        (permission (getarg $PERMISSION args)))
    (unless (equal bankid (bankid server))
      (error "Bad bankid in deny request"))
    (with-db-lock (db (acct-time-key toid))
      (do-deny-internal server args id toid permission))))

(defun do-deny-internal (server args id toid permission)
  (let* ((grants (parse-grants server toid permission))
         (grant (find id grants :test #'equal :key #'grant-id)))
    (when grant
      (setf grants (delete grant grants :test #'eq))
      (let ((msg (car grants)))
        (dolist (grant (cdr grants))
          (dotcat msg "." (grant-msg grant)))
        (db-put (db server) (permission-key toid permission) msg))
      (when (and (equal $GRANT (grant-grant grant))
                 (not (find $GRANT grants
                            :test #'equal :key #'grant-grant)))
        (deny-permission-transitively server toid permission))))
  (bankmsg server $ATDENY (get-parsemsg args)))

;; This would likely benefit from computing the transitive closure
;; of the permissions tree in one pass, and then changing permissions
;; in a second pass. This is slower, but simpler.
;; Won't matter unless lots of permissions get bandied about.
;; I'm thinking that servers that use permissions aren't going to
;; hand them out much, and very rarely transtively.
(defun deny-permission-transitively (server toid permission)
  (let* ((db (db server))
         (granters (list toid))
         (added-p t)
         (ids (db-contents db $ACCOUNT)))
    (loop while added-p
       do
         (setf added-p nil)
         (dolist (id ids)
           (unless (member id granters :test #'equal)
             (let ((key (permission-key id permission)))
               (with-db-lock (db key)
                 ;; Can we afford the RAM to store the parses?
                 ;; Might speed things up considerably
                 (let ((grants (parse-grants server id permission))
                       (maybe-added-p nil))
                   (loop for tail on grants
                      for grant = (car tail)
                      do
                        (when (member (grant-id grant) granters :test #'equal)
                          (when (equal $GRANT (grant-grant grant))
                            (setf maybe-added-p t))
                          (if (eq tail grants)
                              (setf (cdr grants) (cdr tail))
                              (setf tail (cdr tail)))))
                   (when maybe-added-p
                     (let ((msg (grant-msg (car grants))))
                       (dolist (grant (cdr grants))
                         (dotcat msg "." (grant-msg grant)))
                       (db-put db key msg))
                     (unless (find $GRANT grants :test
                                   #'equal :key #'grant-grant)
                       (push id granters)
                       (setf added-p t)))))))))))

(define-message-handler do-permission $PERMISSION (server args msgs)
  ;; (<id>,permission,<bankid>,<req#>,permission=<permission>)
  (declare (ignore msgs))
  (checkreq server args)
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (bankid (getarg $BANKID args))
        (permission (getarg $PERMISSION args))
        (msg (bankmsg server $ATPERMISSION (get-parsemsg args))))
    (unless (equal bankid (bankid server))
      (error "Bad bankid in permission request"))
    (cond (permission
           (ensure-grant-permission server id permission)
           (dolist (toid (db-contents db $ACCOUNT))
             (let* ((grants (parse-grants server toid permission))
                    (grant (find id grants :test #'equal :key #'grant-id)))
               (dotcat msg "." (grant-msg grant)))))
          (t (dolist (permission (db-contents db (permission-key id)))
               (dolist (grant (parse-grants server id permission))
                 (dotcat msg "." (grant-msg grant))))))
    msg))

(define-message-handler do-backup $BACKUP (server args reqs)
  (declare (ignore reqs))
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (req (getarg $REQ args))
        (keys&values (getarg :rest args)))
    (unless (equal id (bankid server))
      (error "Backup command only allowed for bankid"))
    (unless (evenp (length keys&values))
      (error "Odd length key&value list"))
    (checkreq server args)
    (loop
       for tail on keys&values by #'cddr
       for key = (car tail)
       for value = (cadr tail)
       do
         ;;(format t "~&~s => ~s~%" key value)
         (setf (db-get db key) value))
    (bankmsg server $ATBACKUP req)))
      
;;;
;;; End request processing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *server-commands* nil)

(defun server-commands ()
  (or *server-commands*
      (let* ((patterns (patterns))
             (names `(,$BANKID
                      ,$ID
                      ,$REGISTER
                      ,$GETREQ
                      ,$GETTIME
                      ,$GETFEES
                      ,$SETFEES
                      ,$SPEND
                      ,$SPENDREJECT
                      ,$COUPONENVELOPE
                      ,$GETINBOX
                      ,$PROCESSINBOX
                      ,$STORAGEFEES
                      ,$GETASSET
                      ,$ASSET
                      ,$GETOUTBOX
                      ,$GETBALANCE
                      ,$GETVERSION
                      ,$WRITEDATA
                      ,$READDATA
                      ,$GRANT
                      ,$DENY
                      ,$PERMISSION
                      ,$BACKUP))
             (commands (make-hash-table :test #'equal)))
      (loop
         for name in names
         for pattern =  (gethash name patterns)
         do
           (setf (gethash name commands) pattern))
      (setq *server-commands* commands))))

(defparameter *backup-mode-commands*
  `(,$BANKID ,$BACKUP ,$GETREQ))

(defparameter *backup-mode-only-commands*
  `(,$BACKUP))

(defun shorten (string maxlen)
  (if (> (length string) maxlen)
      (strcat (subseq string (- maxlen 3)) "...")
      string))

(defmethod process ((server server) msg)
  "Process a message and return the response.
   This is usually all you'll call from outside."
  (handler-bind
      ((error
        (lambda (c)
          (when (debug-stream-p)
            (debugmsg "Server error: ~a~%~a" c (backtrace-string)))
          (return-from process
            (failmsg server msg (format nil "~a" c))))))
    (process-internal server msg)))

(defun process-internal (server msg)
  (let* ((parser (parser server))
         (reqs (parse parser msg))
         (request (gethash 1 (car reqs)))
         (pattern (gethash request (server-commands))))
    (declare (special *break-in-sldb*))
    (unless pattern
      (error "Unknown request: ~s" request))
    (cond ((backup-mode-p server)
           (unless (member request *backup-mode-commands* :test #'equal)
             (error "Request not valid in backup mode: ~s" request)))
          (t
           (when (member request *backup-mode-only-commands* :test #'equal)
             (error "Request only valid in backup mode: ~s" request))))
    (setq pattern (append `(,$CUSTOMER ,$REQUEST) pattern))
    (let* ((args (or (match-args (car reqs) pattern)
                     (error "Can't match message")))
           (args-bankid (getarg $BANKID args))
           (note (getarg $NOTE args))
           (handler (get-message-handler request)))
      (unless (or (null args-bankid)
                  (and (equal "0" args-bankid)
                       (equal $BANKID (getarg $REQUEST args)))
                  (equal args-bankid (bankid server)))
        (error "bankid mismatch"))
      (when (> (length note) 4096)
        (error "Note too long. Max: 4096 chars"))
      (funcall handler server args reqs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009-2010 Bill St. Clair
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
