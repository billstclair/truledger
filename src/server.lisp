; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Truledger server
;;;

(in-package :truledger-server)

(defun make-server (dir passphrase &key
                    (servername "")
                    (serverurl "")
                    (privkey-size 3072))
  "Create a Truledger server instance."
  (let ((db (make-fsdb dir)))
    (make-instance 'server
                   :db db
                   :passphrase passphrase
                   :servername servername
                   :serverurl serverurl
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
   (servername :type (or string null)
             :initarg :servername
             :initform "A Random Truledger"
             :accessor servername)
   (serverurl :type (or string null)
            :initarg :serverurl
            :initform nil
            :accessor serverurl)
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
   (permissions :type list
                :initarg :permissions
                :initform nil
                :accessor permissions)
   (privkey-size :accessor privkey-size
                 :initarg :privkey-size
                 :initform 3072)
   (privkey :accessor privkey)
   (serverid :type (or string null)
           :initform nil
           :accessor serverid)
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
                           :server-getter (lambda () (serverid server))
                           :always-verify-sigs-p (always-verify-sigs-p server)))
    (setup-db server passphrase)
    (set-shutdown-msg server)))

(defmethod gettime ((server server))
  (let* ((db (db server)))
    (with-db-lock (db $TIME)
      (let ((res (next (timestamp server) (db-get db $TIME))))
        (db-put db $TIME res)
        (maybe-purge-transactions res)
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
        (unpack-servermsg server msg $ATBALANCE $BALANCE $AMOUNT))))

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
                  (setq args (unpack-servermsg server msg $ATFRACTION $FRACTION)
                        fraction (getarg $AMOUNT args)
                        fractime (getarg $TIME args)))
                (values percent fraction fractime issuer)))))))))

(defun storage-fee-key (id &optional assetid)
  (let ((res (append-db-keys (account-dir id) $STORAGEFEE)))
    (if assetid
        (append-db-keys res assetid)
        res)))

(defun last-transaction-key (id)
  (append-db-keys (account-dir id) $LASTTRANSACTION))

(defmethod unpacker ((server server))
  #'(lambda (msg) (unpack-servermsg server msg)))

(defmethod outbox-hash ((server server) id &optional newitem removed-items)
  (let ((db (db server)))
    (multiple-value-bind (hash cnt)
        (dirhash db (outbox-key id) (unpacker server) newitem removed-items)
      (values (or hash "") (or cnt 0)))))

(defmethod outbox-hash-msg ((server server) id)
  (multiple-value-bind (hash count) (outbox-hash server id)
    (servermsg server
             $OUTBOXHASH
             (serverid server)
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
      (let ((res (unpack-servermsg server asset $ATASSET $ASSET)))
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

(defun privkey-id (privkey)
  (with-rsa-private-key (key privkey)
    (pubkey-id (encode-rsa-public-key key))))

(defmethod unpack-server-param ((server server) type &optional (key type))
  "Unpack wrapped initialization parameter."
  (unpack-servermsg server (db-get (db server) type) type nil key))

(defun signmsg (privkey msg)
  (let ((sig (sign msg privkey)))
    (strcat msg #.(format nil ":~%") sig)))

(defmethod serversign ((server server) msg)
  "Server sign a message."
  (signmsg (privkey server) msg))

(defmethod servermsg ((server server) &rest req)
  "Make a server signed message from the args."
  (serversign server (apply #'makemsg (parser server) (serverid server) req)))

(defun shorten-failmsg-msg (msg)
  (if (> (length msg) 1024)
      (strcat (subseq msg 0 1021) "...")
      msg))

(defmethod failmsg ((server server) &rest req)
  (when (stringp (car req))
    (setf (car req) (shorten-failmsg-msg (car req))))
  (serversign server (apply #'simple-makemsg (serverid server) $FAILED req)))


(defmethod unpack-servermsg ((server server) msg &optional type subtype idx)
  "Reverse the servermsg() function, optionally picking one field to return.
   IDX can be that field, or :no-error, to not error if SUBTYPE is wrong."
  (let* ((serverid (serverid server))
         (parser (parser server))
         (reqs (parse parser msg))
         (req (elt reqs 0))
         (args (match-pattern parser req)))
    (when (and serverid (not (equal (getarg $CUSTOMER args) serverid)))
      (error "Servermsg not from server"))
    (when (and type (not (equal (getarg $REQUEST args) type)))
      (error "Servermsg wasn't of type: ~s" type))
    (cond ((not subtype)
           (cond (idx
                  (or (getarg idx args)
                      (error "No arg in servermsg: ~s" idx)))
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
                 (return-from unpack-servermsg nil))
               (error "Wrapped message wasn't of type: ~s" subtype))
             (cond ((and idx (not (eq idx :no-error)))
                    (or (getarg idx args)
                        (error "No arg with idx: ~s" idx)))
                   (t (setf (getarg $UNPACK-REQS-KEY args) reqs) ; save parse results
                      args))))))

(defmethod unpack-server-name ((server server))
  (let ((db (db server))
        (serverid (serverid server)))
    (unpack-servermsg
     server (db-get db $PUBKEYSIG serverid) $ATREGISTER $REGISTER $NAME)))

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
               (serverid (pubkey-id pubkey)))
          (setf (privkey server) privkey
                (serverid server) serverid
                (serverurl server) (db-get db $SERVERURL)
                (servername server) (unpack-server-name server)
                (tokenid server) (unpack-server-param server $TOKENID)
                (regfee server) (unpack-server-param server $REGFEE $AMOUNT)
                (tranfee server) (unpack-server-param server $TRANFEE $AMOUNT))
          (load-fees server))
        ;; http://www.rsa.com/rsalabs/node.asp?id=2004 recommends that 3072-bit
        ;; RSA keys are equivalent to 128-bit symmetric keys, and they should be
        ;; secure past 2031.
        (let* ((privkey (rsa-generate-key (or (privkey-size server) 3072)))
               (privkey-text (encode-rsa-private-key privkey passphrase))
               (pubkey (encode-rsa-public-key privkey))
               (serverid (pubkey-id pubkey))
               (servername (servername server))
               (ut "Usage Tokens")
               (token-name (if servername (strcat servername " " ut) ut))
               (zero "0")
               (tokenid (assetid serverid zero zero token-name)))
          (db-put db $PRIVKEY privkey-text)
          (setf (privkey server) privkey
                (serverid server) serverid)
          (db-put db $TIME zero)
          (db-put db $SERVERID (servermsg server $SERVERID serverid))
          (db-put db $SERVERURL (serverurl server))
          (setf (db-get db $PUBKEY serverid) pubkey)
          (setf (db-get db $PUBKEYSIG serverid)
                (servermsg server $ATREGISTER
                         (servermsg server $REGISTER
                                  serverid
                                  (strcat #.(format nil "~%") pubkey)
                                  servername)))
          (setf (tokenid server) tokenid)
          (db-put db $TOKENID (servermsg server $TOKENID tokenid))
          (setf (db-get db $ASSET tokenid)
                (servermsg server $ATASSET
                         (servermsg server $ASSET
                                  serverid tokenid zero zero token-name)))
          (db-put db $REGFEE
                  (servermsg server $REGFEE serverid zero tokenid (regfee server)))
          (db-put db $TRANFEE
                  (servermsg server $TRANFEE serverid zero tokenid (tranfee server)))
          (db-put db (acct-time-key serverid) zero)
          (db-put db (acct-last-key serverid) zero)
          (db-put db (acct-req-key serverid) zero)
          (db-put db (asset-balance-key serverid tokenid)
                  (servermsg server $ATBALANCE
                           (servermsg server $BALANCE
                                    serverid zero tokenid "-1")))))))

(defun load-fees (server)
  (let ((db (db server))
        (parser (parser server))
        (serverid (serverid server))
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
            (unless (and (EQUAL (getarg $CUSTOMER args) serverid)
                         (equal (getarg $REQUEST args) $FEE)
                         (equal (getarg $SERVERID args) serverid)
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
      (servermsg server $BALANCE (serverid server) time asset amount acct)
      (servermsg server $BALANCE (serverid server) time asset amount)))

(defmethod signed-spend ((server server) time id assetid amount &optional note)
  (let ((serverid (serverid server)))
    (if note
        (servermsg server $SPEND serverid time id assetid amount note)
        (servermsg server $SPEND serverid time id assetid amount))))

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

(defmethod add-to-server-balance ((server server) assetid amount
                                &optional (acct $MAIN))
  "Add AMOUNT to the server balance for ASSETID in ACCT, default \"main\".
   Returns new balance, unless you add 0, then it returns nil."
  (let ((serverid (serverid server))
        (db (db server)))
    (unless (eql 0 (bccomp amount 0))
      (let ((key (asset-balance-key serverid assetid acct)))
        (with-db-lock (db key)
          (let* ((balmsg (db-get db key))
                 (balargs (and balmsg
                               (unpack-servermsg
                                server balmsg $ATBALANCE $BALANCE))))
            (unless (or (null balargs)
                        (equal acct (or (getarg $ACCT balargs) $MAIN)))
              (error "Server balance message not for ~s account" acct))
            (let* ((bal (if balargs (getarg $AMOUNT balargs) 0))
                   (newbal (bcadd bal amount))
                   (balsign (bccomp bal 0))
                   (newbalsign (bccomp newbal 0)))
              (when (or (and (>= balsign 0) (< newbalsign 0))
                        (and (< balsign 0) (>= newbalsign 0)))
                (error "Transaction would put server out of balance."))
              ;; $BALANCE => `(,$SERVERID ,$TIME ,$ASSET ,$AMOUNT (,$ACCT))
              (let ((msg (servermsg
                          server $ATBALANCE
                          (apply #'servermsg
                           server $BALANCE
                           serverid (gettime server) assetid newbal
                           (unless (equal acct $MAIN) (list acct)))))
                    (reqkey (acct-req-key serverid))
                    )
                (db-put db key msg)
                ;; Make sure clients update the balance
                ;; Need to figure out another way to do this.
                ;; It can make it impossible for the server
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
        (serverid (serverid server))
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
                (unless (equal id serverid)
                  (setf (balance-state-tokens state)
                        (bcadd (balance-state-tokens state) 1)))
                (compute-storage-charges server id state asset nil nil))
               (t (let* ((acctargs
                          (unpack-servermsg server acctmsg $ATBALANCE $BALANCE))
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
       (db-put db asset-balance-key (servermsg server $ATBALANCE msg)))))

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
        (serverid (serverid server))
        (tokenid (tokenid server))
        (parser (parser server))
        (negative-assets nil)
        (balance-changed-p nil))
    (flet ((unwrap-balance (msg acct asset)
             (let* ((reqs (parse parser msg))
                    (req (elt reqs 0))
                    (args (match-pattern parser req)))
               (when (equal (getarg $REQUEST args) $ATBALANCE)
                 (unless (equal (getarg $CUSTOMER args) serverid)
                   (error "Servermsg not from server"))
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
                 (unless (equal (getarg $CUSTOMER args) serverid)
                   (error "Servermsg not from server"))
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
                 (unless (equal (getarg $CUSTOMER args) serverid)
                   (error "Servermsg not from server"))
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
      (when (and balance-changed-p (not (equal id serverid)))
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

(define-message-handler do-serverid $SERVERID (server inargs reqs)
  "Look up the server's public key"
  (declare (ignore reqs))
  (let* ((db (db server))
         (serverid (serverid server))
         (coupon (getarg $COUPON inargs))
         (msg (db-get db $PUBKEYSIG serverid))
         (args (unpack-servermsg server msg $ATREGISTER))
         (req (getarg $MSG args))
         (res (get-parsemsg req)))

    (when coupon
      ;; Validate a coupon number
      (let* ((coupon-number-hash (sha1 coupon))
             (key (append-db-keys $COUPON coupon-number-hash))
             (coupon (db-get db key)))
        (unless coupon
          (error "Coupon invalid or already redeemed"))
        (setq args (unpack-servermsg server coupon $ATSPEND $SPEND))
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
        (let ((msg (servermsg server $COUPONNUMBERHASH coupon-number-hash)))
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
            (let ((inmsg-args (unpack-servermsg server inmsg nil t)))
              (when (equal $SPEND (getarg $REQUEST inmsg-args))
                (let ((asset (getarg $ASSET inmsg-args))
                      (amount (getarg $AMOUNT inmsg-args)))
                  (when (and (equal asset tokenid) (> (bccomp amount regfee) 0))
                    (return))))))))

      ;; Create the account
      (let* ((msg (get-parsemsg (car reqs)))
             (res (servermsg server $ATREGISTER msg))
             (time (gettime server)))
        (setf (db-get db $PUBKEY id) pubkey
              (db-get db $PUBKEYSIG id) res)
        ;; Post the debit for the registration fee
        (when (> (bccomp regfee 0)  0)
          (let* ((spendmsg (servermsg server
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
    (servermsg server $REQ id (or (db-get (db server) (acct-req-key id server)) "0"))))

(define-message-handler do-gettime $GETTIME (server args reqs)
  "Process a time request."
  (declare (ignore reqs))
  (checkreq server args)
  (let ((db (db server))
        (id (getarg $CUSTOMER args)))
    (with-db-lock (db (acct-time-key id))
      (let ((time (gettime server)))
        (db-put db (acct-time-key id) time)
        (servermsg server $TIME id time)))))

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
    (unless (equal id (serverid server))
      (error "Only the server can set fees"))
    (with-db-lock (db (acct-time-key id))
      (do-setfees-internal server args reqs))
    (servermsg server $ATSETFEES (get-parsemsg (car reqs)))))

(defun do-setfees-internal (server args reqs)
  (let ((db (db server))
        (parser (parser server))
        (time (getarg $TIME args))
        (count (getarg $COUNT args))
        (cnt 0)
        (serverid (serverid server))
        (tokenid (tokenid server))
        (msg-alist nil)
        (fee-alist nil)
        (tranmsg nil)
        (tranfee nil)
        (regmsg nil)
        (regfee "0"))

    ;; Burn the transaction
    (deq-time server serverid time)

    (loop
       for req in (cdr reqs)
       for reqargs = (match-pattern parser req)
       for reqid = (getarg $CUSTOMER reqargs)
       for request = (getarg $REQUEST reqargs)
       for reqserver = (getarg $SERVERID reqargs)
       for reqtime = (getarg $TIME reqargs)
       for reqop = (getarg $OPERATION reqargs)
       for reqasset = (getarg $ASSET reqargs)
       for reqamount = (getarg $AMOUNT reqargs)
       for reqmsg = (get-parsemsg req)
       do
         (incf cnt)
         (unless (and (equal reqid serverid) (equal reqserver serverid))
           (error "Fee record not from and for server"))
         (unless (equal reqtime time) (error "Timestamp mismatch"))
           (unless (equal reqid serverid) (error "ID mismatch"))
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
            tranmsg (servermsg server $TRANFEE serverid time tokenid tranfee)))
    (unless regmsg
      (setf regfee *regfee-default*
            regmsg (servermsg server $REGFEE serverid time tokenid regfee)))
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

(define-message-handler do-getfeatures $GETFEATURES (server args reqs)
  (declare (ignore reqs))
  (checkreq server args)
  (let* ((db (db server))
         (time (db-get db $TIME)))
    (servermsg server $FEATURES (serverid server) time $TWOPHASECOMMIT)))

(defun post-fees-and-storage (server feesdiffs storage-infos)
  ;; Credit the non-refundable fees to the server as storage fees
  (loop
     for (assetid . amount) in feesdiffs
     do
       (post-storage-fee server assetid (serverid server) amount nil))

  ;; This is outside the customer lock to avoid deadlock with the issuer account.
  (loop
     for (assetid . assetinfo) in storage-infos
     for issuer = (storage-info-issuer assetinfo)
     for storage-fee = (storage-info-fee assetinfo)
     for digits = (storage-info-digits assetinfo)
     when (and issuer storage-fee digits)
     do
       (post-storage-fee server assetid issuer storage-fee digits)))

(defvar *transactions* (make-hash-table :test 'equal))
(defvar *transactions-lock* (make-lock "*transactions-lock*"))

(defclass transaction ()
  ((db :initarg :db
       :accessor transaction-db)
   (time :initarg :time
         :accessor transaction-time)
   (feesdiffs :initarg :feesdiffs
              :accessor transaction-feesdiffs)
   (storage-infos :initarg :storage-infos
                  :accessor transaction-storage-infos)))

(defparameter *transaction-purge-period* 600) ; ten minutes
(defvar *next-transaction-purge-time*
  (+ (get-unix-time) *transaction-purge-period*))

(defun maybe-purge-transactions (time)
  (when (>= (bccomp time *next-transaction-purge-time*) 0)
    (let ((min-time (bcsub time *transaction-purge-period*)))
      (with-lock-grabbed (*transactions-lock*)
        (maphash (lambda (key transaction)
                   (when (< (bccomp (transaction-time transaction) min-time) 0)
                     (remhash key *transactions*)))
                 *transactions*)
        (setf *next-transaction-purge-time*
              (bcadd time *transaction-purge-period*))))))

(defun save-transaction (server db id time feesdiffs storage-infos)
  (check-type server server)
  (check-type db fsdb:db-wrapper)
  (let ((transaction (make-instance 'transaction
                                    :db (fsdb:copy-db-wrapper db)
                                    :time time
                                    :feesdiffs feesdiffs
                                    :storage-infos storage-infos)))
    (with-lock-grabbed (*transactions-lock*)
      (setf (gethash id *transactions*) transaction))))

(define-message-handler do-spend $SPEND (server args reqs)
  "Process a spend message."
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        res time two-phase-commit-p feesdiffs storage-infos)
    (with-db-lock (db (acct-time-key id))
      (with-db-wrapper (db (db server))
        (multiple-value-setq (res time two-phase-commit-p feesdiffs storage-infos)
          (do-spend-internal server db args reqs))
        (when two-phase-commit-p
          (save-transaction server db id time feesdiffs storage-infos)
          (return-from do-spend res))))
    (post-fees-and-storage server feesdiffs storage-infos)
    res))

(defun get-two-phase-commit-arg (args whichhash)
  (let ((twophasecommit (getarg $TWOPHASECOMMIT args)))
    (when twophasecommit
      (unless (equal twophasecommit $TWOPHASECOMMIT)
        (error "~a value for ~a not ~s"
               $TWOPHASECOMMIT
               whichhash
               $TWOPHASECOMMIT))
      t)))

;; New implementation, using db-wrapper
;; Need to deal with negative spend, moving issuer to different acct or
;; different ID.
(defmethod do-spend-internal ((server server) db args reqs)
  "Do the work for a spend.
   Returns four values:
    res: The result message to be returned to the client
    time: the transaction time
    feediffs: list of (assetid . amount) pairs for server fees
    storage-infos: list of storage-info instances for storage fees"
  (let* ((serverid (serverid server))
         (parser (parser server))
         (spendmsg (get-parsemsg (car reqs)))
         (id (getarg $CUSTOMER args))
         (time (getarg $TIME args))
         (id2 (getarg $ID args))
         (assetid (getarg $ASSET args))
         (tokenid (tokenid server))
         (amount (getarg $AMOUNT args))
         (note (getarg $NOTE args))
         (asset (lookup-asset server assetid))
         (operation (if (equal id id2) $TRANSFER $SPEND))
         (fees (and (not (equal id serverid)) (getfees server operation)))
         (two-phase-outbox-hash-p nil)
         (two-phase-balance-hash-p nil)
         (two-phase-commit-p nil))

    (cond ((equal id2 $COUPON)
           (ensure-permission server id $MINT-COUPONS)
           (when (equal assetid tokenid)
             (ensure-permission server id $MINT-TOKENS)))
          ((not (db-get db $PUBKEY id2))
           (ensure-permission server id $MINT-TOKENS)))

    ;; Remove fees for assets for which the spender is the issuer
    (setf fees
          (delete-if
           #'(lambda (fee)
               (let ((asset (lookup-asset server (car fee))))
                 (equal id (getarg $CUSTOMER asset))))
           fees))

    ;; Burn the transaction, even if balances don't match.
    (deq-time server id time)

    (when (equal id2 serverid)
      (error "Spends to the server are not allowed."))

    (unless asset
      (error "Unknown asset id: ~s" assetid))

    (unless (is-numeric-p amount)
      (error "Not a number: ~s" amount))

    ;; Make sure there are no server debit inbox entries older
    ;; than the highest timestamp last read from the inbox
    (let ((inbox (scan-inbox server id))
          (last (get-acct-last server id)))
      (dolist (inmsg inbox)
        (let* ((inmsg-args (unpack-servermsg server inmsg))
               (spendreq (getarg $MSG inmsg-args)))
          (when (and (or (< (bccomp last 0) 0)
                         (<= (bccomp (getarg $TIME inmsg-args) last) 0))
                     (let ((spendargs (match-pattern (parser server) spendreq)))
                       (and (equal serverid (getarg $CUSTOMER spendargs))
                            (equal $SPEND (getarg $REQUEST spendargs))
                            (< (bccomp (getarg $AMOUNT spendargs) 0) 0))))
            (error
             "Please process server debits in your account before doing a spend.")))))

    (let* ((tokens (if (and (not (equal id id2))
                            (not (equal id serverid)))
                       (tranfee server)
                       "0"))
           (feesdiffs nil)
           (outbox-item (servermsg server $ATSPEND spendmsg))
           (res outbox-item)
           (feemsg nil)
           (storageamts nil)  ;list of (assetid . amt) for storage fees
           (fracids nil)      ;list of assetid for fractions
           (outboxhash-msg nil)
           (balancehash-msg nil))

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
                  (setq feemsg (servermsg server $ATTRANFEE reqmsg))
                  (dotcat outbox-item "." feemsg)
                  (dotcat res "." feemsg))                   
                 ((equal request $STORAGEFEE)
                  (let ((assetid (getarg $ASSET reqargs))
                        (storagemsg (servermsg server $ATSTORAGEFEE reqmsg)))
                    (when (assocequal assetid storageamts)
                      (error "Duplicate asset in storage fees"))
                    (push (cons assetid (getarg $AMOUNT reqargs)) storageamts)
                    (dotcat res "." storagemsg)))
                 ((equal request $FRACTION)
                  (let* ((fracasset (getarg $ASSET reqargs))
                         (fracmsg (servermsg server $ATFRACTION reqmsg))
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
                  (setf outboxhash-msg (servermsg server $ATOUTBOXHASH reqmsg))
                  (db-put db (outbox-hash-key id) outboxhash-msg)
                  (dotcat res "." outboxhash-msg)
                  (setf two-phase-outbox-hash-p
                        (get-two-phase-commit-arg reqargs "outboxhash")))
                 ((equal request $BALANCEHASH)
                  (when balancehash-msg
                    (error "~s appeared multiple times" $BALANCEHASH))
                  (setq balancehash-msg (servermsg server $ATBALANCEHASH reqmsg))
                  (db-put db (balance-hash-key id) balancehash-msg)
                  (dotcat res "." balancehash-msg)
                  (setf two-phase-balance-hash-p
                        (get-two-phase-commit-arg reqargs "balancehash")))
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
                    (let ((msg (servermsg server $ATFEE reqmsg)))
                      (dotcat res "." msg))))
                 (t
                  (error "~s not valid for spend. Only ~s, ~s, ~s, ~s, ~s, & ~s"
                         request
                         $TRANFEE $STORAGEFEE $FRACTION
                         $BALANCE $OUTBOXHASH $BALANCEHASH))))

      (when (equal id2 $COUPON)
        ;; If it's a coupon request, generate the coupon
        (let* ((coupon-number (random-id))
               (serverurl (serverurl server))
               (coupon
                (if note
                    (servermsg server $COUPON serverurl coupon-number
                             assetid amount note)
                    (servermsg server $COUPON serverurl coupon-number
                             assetid amount)))
               (coupon-number-hash (sha1 coupon-number)))
          (setf (db-get db $COUPON coupon-number-hash) outbox-item)
          (setq coupon
                (servermsg server $COUPONENVELOPE id
                         (pubkey-encrypt
                          coupon (db-get (pubkeydb server) id))))
          (dotcat res "." coupon)
          (dotcat outbox-item "." coupon)))

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
      (unless (or (equal id id2) (equal id serverid))
        (unless outboxhash-msg
          (error "~s missing" $OUTBOXHASH))
        (setf (db-get db (outbox-key id) time) outbox-item))

      ;; balancehash must be included, except on server spends
      (unless (equal id serverid)
        (unless balancehash-msg
          (error "~s missing" $BALANCEHASH)))

      ;; Generate inbox item for recipient, if there is one
      (unless (or (equal id2 $COUPON) (equal id id2))
        (let* ((newtime (gettime server))
               (inbox-item (servermsg server $INBOX newtime spendmsg)))
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
        (when (or two-phase-balance-hash-p two-phase-outbox-hash-p)
          (unless (and two-phase-balance-hash-p two-phase-outbox-hash-p)
            (error "~a value differs between balance hash and outbox hash"
                   $TWOPHASECOMMIT))
          (setf two-phase-commit-p t))
        (values res time two-phase-commit-p feesdiffs storage-infos)))))

(defmethod post-storage-fee ((server server) assetid issuer storage-fee digits)
  "Credit storage fee to an asset issuer.
   Digits can be null when storage-fee has no fraction and you just want
   to preserve the digits in the existing sum."
  (let ((db (db server))
        (parser (parser server))
        (serverid (serverid server)))
    (with-db-lock (db (acct-time-key issuer))
      (let* ((key (storage-fee-key issuer assetid))
             (storage-msg (db-get db key)))
        (when storage-msg
          (let ((reqs (parse parser storage-msg)))
            (unless (eql 1 (length reqs))
              (error "Bad storagefee msg: ~s" storage-msg))
            (let* ((args (match-pattern parser (car reqs)))
                   (amount (getarg $AMOUNT args)))
              (unless (and (equal (getarg $CUSTOMER args) serverid)
                           (equal (getarg $REQUEST args) $STORAGEFEE)
                           (equal (getarg $SERVERID args) serverid)
                           (equal (getarg $ASSET args) assetid))
                (error "Storage fee message malformed"))
              (when (null digits)
                (setf digits (number-precision amount)))
              (setq storage-fee (wbp (digits) (bcadd storage-fee amount))))))
        (let* ((time (gettime server))
               (storage-msg (servermsg server $STORAGEFEE
                                     serverid time assetid storage-fee)))
          (db-put db key storage-msg))))))

(define-message-handler do-commit $COMMIT (server args reqs)
  "Do the second phase of the commit for a spend or processinbox."
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (time (getarg $TIME args))
        (msg (get-parsemsg (car reqs)))
        (transaction nil)
        res)
    (with-db-lock (db (acct-time-key id))
      (with-lock-grabbed (*transactions-lock*)
        (setf transaction (gethash id *transactions*))
        (when transaction
          (remhash id *transactions*)))
      (cond (transaction
             (cond ((equal time (transaction-time transaction))
                    (fsdb:commit-db-wrapper (transaction-db transaction))
                    (setf res (servermsg server $ATCOMMIT msg)
                          (db-get db (last-transaction-key id)) res))
                   (t (setf res (failmsg server msg
                                         (format nil
                                                 "No saved transaction for time: ~d"
                                                 time))
                            transaction nil))))
            (t (setf res (failmsg server msg "No active transaction")))))
    (when transaction
      (post-fees-and-storage server
                             (transaction-feesdiffs transaction)
                             (transaction-storage-infos transaction)))
    res))

(define-message-handler do-spendreject $SPENDREJECT (server args reqs)
  "Process a spend|reject"
  (let ((db (db server))
        (parser (parser server))
        (id (getarg $CUSTOMER args))
        (msg (get-parsemsg (car reqs))))
    (with-db-lock (db (acct-time-key id))
      (let* ((serverid (serverid server))
             (time (getarg $TIME args))
             (key (outbox-key id))
             (item (db-get db key time)))
        (unless item
          (error "No outbox entry for time: ~s" time))
        (let ((args (unpack-servermsg server item $ATSPEND $SPEND)))
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
                     (args (and item (unpack-servermsg
                                      server item $INBOX $SPEND :no-error))))
                (when (and args (equal (getarg $TIME args) time))
                  ;; Calculate the fee, if there is one
                  (let* ((reqs (getarg $UNPACK-REQS-KEY args))
                         (req (second reqs)))
                    (when req
                      (let ((args (match-pattern parser req)))
                        (unless (equal (getarg $CUSTOMER args) serverid)
                          (error "Fee message not from server"))
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
                    (add-to-server-balance server feeasset feeamt))
                  (let* ((newtime (gettime server))
                         (item (servermsg server $INBOX newtime msg))
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
      (servermsg server $ATCOUPONENVELOPE (get-parsemsg (car reqs))))))

(defun do-couponenvelope-raw (server args id)
  "Called by do_register to process coupons there.
   Returns an error string or false."
  (check-type server server)

  (let ((db (db server))
        (serverid (serverid server))
        (parser (parser server))
        (encrypted-to (getarg $ID args)))
    (unless (equal encrypted-to serverid)
      (error "Coupon not encrypted to server"))

    (let* ((coupon (privkey-decrypt (getarg $ENCRYPTEDCOUPON args) (privkey server)))
           (coupon-number (and (coupon-number-p coupon) coupon)))

      (unless coupon-number
        (let ((args (unpack-servermsg server coupon $COUPON)))
          (unless (equal serverid (getarg $CUSTOMER args))
            (error "Coupon not signed by server"))
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
                       (let* ((reqs (parse parser item))
                              (req (find-if
                                    (lambda (req)
                                      (equal $COUPONNUMBERHASH
                                             (gethash 1 req)))
                                    reqs))
                              (msg (and req (match-pattern parser req))))
                         (when (and msg
                                    (equal coupon-number-hash
                                           (getarg $COUPON msg)))
                           ;; Customer already redeemded this coupon.
                           ;; Success if he tries to do it again.
                           (return t))))))
                 (error "Coupon already redeemed"))))
          (t
           (let* ((ok nil)
                  (args (unwind-protect
                             (prog1 (unpack-servermsg server outbox-item $ATSPEND)
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
                  (inbox-item (servermsg server $INBOX newtime spendmsg))
                  (cnhmsg (servermsg server $COUPONNUMBERHASH coupon-number-hash)))
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
             (res (servermsg server $ATGETINBOX (get-parsemsg (car reqs))))
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
        res time two-phase-commit-p storage-infos)

    (with-db-lock (db (acct-time-key id))
      (with-db-wrapper (db (db server))
        (multiple-value-setq (res time two-phase-commit-p storage-infos)
          (do-processinbox-internal server db args reqs))
        (when two-phase-commit-p
          (save-transaction server db id time nil storage-infos)
          (return-from do-processinbox res))))
      
    (post-fees-and-storage server nil storage-infos)

    res))

(defun checktime (was sb place)
  (unless (equal was sb)
    (error "Time mismatch in ~a, was: ~s, sb: ~s"
           place was sb)))

(defmethod do-processinbox-internal ((server server) db args reqs)
  (let* ((serverid (serverid server))
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
         (res (servermsg server $ATPROCESSINBOX (get-parsemsg (car reqs))))
         (outboxtimes nil)
         (outboxhashreq nil)
         (balancehashreq nil)
         (diffs nil)
         (two-phase-balance-hash-p nil)
         (two-phase-outbox-hash-p nil)
         (two-phase-commit-p nil))

    ;; Burn the transaction, even if balances don't match.
    (deq-time server id time)

    ;; Collect the inbox items being processed
    (dolist (inboxtime inboxtimes)
      (let* ((item (db-get db inbox-key inboxtime))
             (itemargs (if (null item)
                           (error "No inbox item for time: ~s" inboxtime)
                           (unpack-servermsg server item $INBOX t)))
             (request (getarg $REQUEST itemargs)))
        (unless (or (equal (getarg $ID itemargs) id)
                    (equal (getarg $ID itemargs) $COUPON))
          (error "Inbox corrupt. Item found for other customer"))
        (cond ((equal request $SPEND)
               (let* ((itemtime (getarg $TIME itemargs))
                      (itemreqs (getarg $UNPACK-REQS-KEY itemargs))
                      (feereq
                       (find-if
                        (lambda (req) (equal $ATTRANFEE (gethash 1 req)))
                        itemreqs)))
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
               ;; $SPENDACCEPT => array($SERVERID,$TIME,$id,$NOTE=>1),
               ;; $SPENDREJECT => array($SERVERID,$TIME,$id,$NOTE=>1),
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
                        (dotcat res "." (servermsg server $ATSPENDACCEPT reqmsg)))
                       (t
                        ;; Rejecting the payment. Credit the fee.
                        (let ((feeargs (gethash argstime fees)))
                          (when feeargs
                            (let ((feeasset (getarg $ASSET feeargs))
                                  (feeamt (getarg $AMOUNT feeargs)))
                              (push (cons feeasset (bcsub feeamt)) diffs))))
                        (dotcat res "." (servermsg server $ATSPENDREJECT reqmsg))))
                 (let (inboxtime inboxmsg)
                   (cond ((equal otherid serverid)
                          (when (and (equal request $SPENDREJECT)
                                     (< (bccomp (getarg $AMOUNT itemargs) 0) 0))
                            (error "You may not reject a server charge"))
                          (setq inboxtime request
                                inboxmsg itemargs))
                         (t 
                          (setq inboxtime (gettime server)
                                inboxmsg (servermsg server $INBOX
                                                    inboxtime reqmsg))))
                   (push (list otherid inboxtime inboxmsg) inboxmsgs))))
              ((equal request $STORAGEFEE)
               (checktime argstime time "storagefee item")
               (let ((storageasset (getarg $ASSET args)))
                 (when (gethash storageasset storagemsgs)
                   (error "Duplicate storage fee for asset: ~s" storageasset))
                 (setf (gethash storageasset storagemsgs) reqmsg)
                 (let ((msg (servermsg server $ATSTORAGEFEE reqmsg)))
                   (dotcat res "." msg))))
              ((equal request $FRACTION)
               (checktime argstime time "fraction item")
               (let ((fracasset (getarg $ASSET args)))
                 (when (gethash fracasset fracmsgs)
                   (error "Duplicate fraction balance for asset: ~s" fracasset))
                 (setf (gethash fracasset fracmsgs) reqmsg)
                 (let ((fracmsg (servermsg server $ATFRACTION reqmsg)))
                   (dotcat res "." fracmsg)
                   (db-put db (fraction-balance-key id fracasset) fracmsg))))
              ((equal request $OUTBOXHASH)
               (unless (equal id serverid)
                 (when outboxhashreq
                   (error "~s appeared multiple times" $OUTBOXHASH))
                 (checktime argstime time "outboxhash")
                 (setf outboxhashreq req
                       two-phase-outbox-hash-p
                       (get-two-phase-commit-arg args "outboxhash"))
                 (let ((item (servermsg server $ATOUTBOXHASH (get-parsemsg req))))
                   (dotcat res "." item)
                   (db-put db (outbox-hash-key id) item))))
              ((equal request $BALANCE)
               (checktime argstime time "balance item")
               (let ((balmsg (handle-wrapper-balance-msg
                              server db id reqmsg args)))
                 (dotcat res "." balmsg)))
              ((equal request $BALANCEHASH)
               (unless (equal id serverid)
                 (when balancehashreq
                   (error "~s appeared multiple times" $BALANCEHASH))
                 (checktime argstime time "balancehash")
                 (setf balancehashreq req
                       two-phase-balance-hash-p
                       (get-two-phase-commit-arg args "balancehash"))
                 (let ((item (servermsg server $ATBALANCEHASH
                                        (get-parsemsg req))))
                   (dotcat res "." item)
                   (db-put db (balance-hash-key id) item))))
              (t
               (error "~s not valid for ~s, only ~s, ~s, ~s, ~s, ~s, ~s, & ~s"
                      request $PROCESSINBOX
                      $SPENDACCEPT $SPENDREJECT
                      $STORAGEFEE $FRACTION $OUTBOXHASH
                      $BALANCE $BALANCEHASH)))))

    ;; No outbox hash maintained for the server
    (unless (equal id serverid)
      ;; Make sure the outbox hash was included iff needed
      (when (or (and outboxtimes (not outboxhashreq))
                (and (null outboxtimes) outboxhashreq))
        (error (if outboxhashreq
                   "~s included when not needed"
                   "~s missing")
               $OUTBOXHASH)))

    ;; Remove no longer needed inbox and outbox entries.
    ;; Probably should have a server config parameter to archive these somewhere.
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
        (cond ((equal otherid serverid)
               (let ((request inboxtime)
                     (itemargs inboxmsg))
                 (when (equal request $SPENDREJECT)
                   ;; Return the funds to the server's account
                   ;; (should this write into the wrapped-db instead
                   ;;  of directly to disk? We're just about to commit).
                   (add-to-server-balance
                    server
                    (getarg $ASSET itemargs)
                    (getarg $AMOUNT itemargs)))))
              (t
               (let ((inboxkey (inbox-key otherid)))
                 (setf (db-get db inboxkey inboxtime) inboxmsg))))))

    (when (or two-phase-balance-hash-p two-phase-outbox-hash-p)
      (unless (or (not balancehashreq) (not outboxhashreq)
                  (and two-phase-balance-hash-p two-phase-outbox-hash-p))
        (error "~a value differs between balance hash and outbox hash"
               $TWOPHASECOMMIT))
      (setf two-phase-commit-p t))

    (values res time two-phase-commit-p storage-infos)))

(defun get-outbox-args (server id spendtime)
  (check-type server server)

  (let* ((db (db server))
         (parser (parser server))
         (serverid (serverid server))
         (outboxkey (outbox-key id))
         (spendmsg (or (db-get db outboxkey spendtime)
                       (error "Can't find outbox item: ~s" spendtime)))
         (reqs (parse parser spendmsg))
         (spendargs (match-pattern parser (car reqs)))
         (feereq (find-if #'(lambda (req) (equal $ATTRANFEE (gethash 1 req)))
                          reqs))
         (feeargs (and feereq
                       (match-pattern parser feereq))))
    (unless (and (equal (getarg $CUSTOMER spendargs) serverid)
                 (equal (getarg $REQUEST spendargs) $ATSPEND)
                 (or (null feeargs)
                     (equal (getarg $REQUEST feeargs) $ATTRANFEE)))
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

(define-message-handler do-lasttransaction $LASTTRANSACTION (server args reqs)
  ;; Process a lasttransaction request
  (let ((db (db server))
        (id (getarg $CUSTOMER args)))
    (with-db-lock (db (acct-time-key id))
      (checkreq server args)
      (let ((res (db-get db (last-transaction-key id))))
        (or res
            (failmsg server
                     (get-parsemsg (car reqs))
                     "No two-phase transactions have been made."))))))

(define-message-handler do-storagefees $STORAGEFEES (server args reqs)
  ;; Process a storagefees request
  (let ((db (db server))
        (serverid (serverid server))
        (id (getarg $CUSTOMER args)))
    (with-db-lock (db (acct-time-key id))
      (checkreq server args)
      (let* ((inboxkey (inbox-key id))
             (key (storage-fee-key id))
             (assetids (db-contents db key)))
        (dolist (assetid assetids)
          (let* ((storagefee (db-get db key assetid))
                 (args (unpack-servermsg server storagefee $STORAGEFEE))
                 (amount (getarg $AMOUNT args)))
            (unless (equal assetid (getarg $ASSET args))
              (error "Asset mismatch, sb: ~s, was: ~s"
                     assetid (getarg $ASSET args)))
            (multiple-value-bind (amount fraction)
                (split-decimal amount)
              (when (> (bccomp amount 0) 0)
                (let* ((time (gettime server))
                       (storagefee (servermsg server $STORAGEFEE
                                            serverid time assetid fraction))
                       (spend (servermsg server $SPEND
                                       serverid time id assetid amount
                                       "Storage fees"))
                       (inbox (servermsg server $INBOX time spend)))
                  (setf (db-get db key assetid) storagefee)
                  (setf (db-get db inboxkey time) inbox)))))))
      (servermsg server $ATSTORAGEFEES (get-parsemsg (car reqs))))))

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
        (serverid (serverid server))
        (parser (parser server)))
    (ensure-permission server id $ADD-ASSET)
    (with-db-lock (db (acct-time-key id))

      (when (< (length reqs) 2)
        (error "No balance items"))

      ;; $ASSET => array($SERVERID,$ASSET,$SCALE,$PRECISION,$ASSETNAME),
      (let* ((assetid (getarg $ASSET args))
             (scale (getarg $SCALE args))
             (precision (getarg $PRECISION args))
             (assetname (getarg $ASSETNAME args))
             (storage-msg nil)
             (exists-p (is-asset-p server assetid))
             (tokens (if (equal id serverid) "0" "1"))
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
        (unless (equal id serverid)
          (unless balancehashreq (error "~s missing" $BALANCEHASH))

          (multiple-value-bind (hash hashcnt)
              (balancehash db (unpacker server) (balance-key id) acctbals)
            (unless (and (equal balancehash hash)
                         (eql 0 (bccomp balancehashcnt hashcnt)))
              (error "~s mismatch, hash: ~s, sb: ~s, count: ~s, sb: ~s"
                     $BALANCEHASH balancehash hash balancehashcnt hashcnt))))
  
        ;; All's well with the world. Commit this puppy.
        ;; Add asset
        (let ((res (servermsg server $ATASSET (get-parsemsg (car reqs)))))
          (when storage-msg
            (dotcat res "." (servermsg server $ATSTORAGE storage-msg)))

          (setf (db-get db $ASSET assetid) res)

          ;; Credit server with tokens
          (add-to-server-balance server tokenid tokens)

          ;; Update balances
          (let ((balancekey (balance-key id)))
            (loop
               for acct being the hash-key using (hash-value balances) of acctbals
               for acctkey = (append-db-keys balancekey acct)
               do
               (loop
                  for balasset being the hash-key using (hash-value balance)
                  of balances
                  for balmsg = (servermsg server $ATBALANCE balance)
                  do
                  (dotcat res "." balmsg)
                  (setf (db-get db acctkey balasset) balmsg))))

          ;; Update balancehash
          (unless (equal id serverid)
            (let ((balancehash-item (servermsg server $ATBALANCEHASH balancehashmsg)))
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

      ;; $GETOUTBOX => array($SERVERID,$REQ)
      (let* ((id (getarg $CUSTOMER args))
             (msg (servermsg server $ATGETOUTBOX (get-parsemsg (car reqs))))
             (outboxkey (outbox-key id))
             (contents (db-contents db outboxkey))
             (outboxhash (db-get db (outbox-hash-key id))))

        (dolist (time contents)
          (dotcat msg "." (db-get db outboxkey time)))

        (when outboxhash (dotcat msg "." outboxhash))

        msg))))

;; These are set by truledger-client-web::save-truledger-application
(defvar *last-commit* nil)
(defvar *save-application-time* nil)

(define-message-handler do-getversion $GETVERSION (server args reqs)
  (declare (ignore reqs))
  (let ((db (db server))
        (id (getarg $CUSTOMER args)))
    (with-db-lock (db (acct-time-key id))
      (checkreq server args)
      (servermsg server $VERSION
               (or *last-commit* (last-commit) "")
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
         (contents (unless (blankp data) (servermsg server $ATREADDATA id time data)))
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
    ;; Server wrap the balance and balancehash
    (setq balancemsg (servermsg server $ATBALANCE balancemsg)
          balancehashmsg (servermsg server $ATBALANCEHASH balancehashmsg))

    ;; Write the data, balance, and balancehash
    (setf (db-get db $DATA keyhash) contents
          (db-get db (asset-balance-key id tokenid)) balancemsg
          (db-get db (balance-hash-key id)) balancehashmsg)

    ;; And cons up the return message
    (strcat (servermsg server $ATWRITEDATA id time anonymous key)
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
        (servermsg server $ATREADDATA id time res-data)))))


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
         (serverid (serverid server))
         (res nil))
    (dolist (req reqs)
      (let* ((args (match-pattern parser req)))
        (unless (and (equal serverid (getarg $CUSTOMER args))
                     (equal $ATGRANT (getarg $REQUEST args)))
          (error "Malformed ~s message" $ATGRANT))
        (setf args (match-pattern parser (getarg $MSG args)))
        (let ((toid (getarg $ID args)))
          (unless (equal toid id)
            (error "Grant not for proper id. SB: ~s, was: ~s"
                   id toid))
          (unless (equal serverid (getarg $SERVERID args))
            (error "Bad serverid in grant, SB: ~s, was: ~s"
                   serverid (getarg $SERVERID args)))
          (unless (equal permission (getarg $PERMISSION args))
            (error "Bad permission in grant, SB: ~s, was: ~s"
                   permission (getarg $PERMISSION args)))
          (push (make-grant
                 :id (getarg $CUSTOMER args)
                 :toid toid
                 :permission permission
                 :grant (getarg $GRANT args)
                 :msg (get-parsemsg req))
                res))))
    (values (nreverse res) msg)))

(defun ensure-grant-permission (server id toid permission)
  (unless (or (and (equal id toid)
                   (equal id (serverid server)))
              (let ((grants (parse-grants server id permission)))
                (find $GRANT grants :test #'equal :key #'grant-grant)))
    (error "You don't have permission to grant or deny permission: ~s"
           permission)))

(defun permission-required-p (server permission)
  (let ((db (db server))
        (cell (assocequal permission (permissions server))))
    (cond (cell (cdr cell))
          (t (let ((serverid (serverid server)))
               (with-db-lock (db (acct-time-key serverid))
                 (setf cell (assocequal permission (permissions server)))
                 (cond
                   (cell (cdr cell))
                   (t (setf cell (cons permission
                                       (not
                                        (null
                                         (parse-grants
                                          server serverid permission)))))
                      (push cell (permissions server))
                      (cdr cell)))))))))

(defun has-permission-p (server id permission)
  (or (not (permission-required-p server permission))
      (not (null (parse-grants server id permission)))))

(defun ensure-permission (server id permission)
  (or (has-permission-p server id permission)
      (error "You do not have ~s permission" permission)))

(define-message-handler do-grant $GRANT (server args msgs)
  ;; (<id>,grant,<serverid>,<time#>,<toid>,<permission>,grant=grant)
  (declare (ignore msgs))
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (serverid (getarg $SERVERID args))
        (time (getarg $TIME args))
        (toid (getarg $ID args))
        (grantp (equal $GRANT (getarg $GRANT args)))
        (permission (getarg $PERMISSION args)))

    ;; Burn the transaction
    (deq-time server id time)

    (unless (equal serverid (serverid server))
      (error "Bad serverid in grant request"))
    (when (and (not (equal id serverid)) (equal toid serverid))
      (error "You are not allowed to grant permissions to the server"))
    (unless (db-get (pubkeydb server) toid)
      (error "No account for toid: ~s" toid))
    (when (and (equal toid serverid) (not grantp))
      (error "Server must be given transitive grant permission"))
    (ensure-grant-permission server id toid permission)
    (with-db-lock (db (acct-time-key toid))
      (do-grant-internal server args id toid permission))))

(defun do-grant-internal (server args id toid permission)
  (let* ((db (db server))
         (serverid (serverid server))
         (grants (parse-grants server toid permission))
         (old-grantp nil)
         (grantp (equal $GRANT (getarg $GRANT args)))
         (msg (servermsg server $ATGRANT (get-parsemsg args)))
         (res msg))
    (dolist (grant grants)
      (cond ((equal id (grant-id grant))
             (setf old-grantp (equal $GRANT (grant-grant grant))))
            (t (when (equal $GRANT (grant-grant grant))
                 (setf grantp t))
               (dotcat msg "." (grant-msg grant)))))
    (db-put db (permission-key toid permission) msg)
    (when (equal toid serverid)
      ;; Clear cache
      (setf (permissions server) nil))
    (when (and old-grantp (or (equal id serverid) (not grantp)))
      (garbage-collect-permission server permission))
    res))

(define-message-handler do-deny $DENY (server args msgs)
  ;; (<id>,deny,<serverid>,<req#>,<toid>,<permission>)
  (declare (ignore msgs))
  (checkreq server args)
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (serverid (getarg $SERVERID args))
        (toid (getarg $ID args))
        (permission (getarg $PERMISSION args)))
    (unless (equal serverid (serverid server))
      (error "Bad serverid in deny request"))
    (when (and (not (equal id serverid)) (equal toid serverid))
      (error "You are not allowed to deny permissions to the server"))
    (ensure-grant-permission server id toid permission)
    (with-db-lock (db (acct-time-key toid))
      (do-deny-internal server args id toid permission))))

(defun do-deny-internal (server args id toid permission)
  (let* ((grants (parse-grants server toid permission))
         (grant (find id grants :test #'equal :key #'grant-id)))
    (when grant
      (setf grants (delete grant grants :test #'eq))
      (let ((msg (and grants (grant-msg (car grants)))))
        (dolist (grant (cdr grants))
          (dotcat msg "." (grant-msg grant)))
        (db-put (db server) (permission-key toid permission) msg)
        (when (equal toid (serverid server))
          ;; Clear cache
          (setf (permissions server) nil)))
      (when (and (equal $GRANT (grant-grant grant))
                 (or (equal id (serverid server))
                     (not (find $GRANT grants
                                :test #'equal :key #'grant-grant))))
        (garbage-collect-permission server permission))))
  (servermsg server $ATDENY (get-parsemsg args)))

(defstruct grant-info
  id
  grants
  grantees
  reachable-grants)

(defun garbage-collect-permission (server permission)
  (let* ((db (db server))
         (serverid (serverid server))
         (info-hash (make-equal-hash))
         (ids (db-contents db $ACCOUNT))
         (cnt 0))

    ;; Collect the grants
    (dolist (id ids)
      (let ((grants (parse-grants server id permission)))
        (when grants
          (setf (gethash id info-hash)
                (make-grant-info
                 :id id
                 :grants grants)))))

    ;; Compute the grantees
    (maphash (lambda (id info)
               (dolist (grant (grant-info-grants info))
                 (let ((grantee-info (gethash (grant-id grant) info-hash)))
                   (when grantee-info
                     (push id (grant-info-grantees grantee-info))))))
             info-hash)

    ;; Compute reachable-grants
    (let* ((server-info (gethash serverid info-hash))
           (grants (and server-info (grant-info-grants server-info))))
      (when server-info
        (setf (grant-info-reachable-grants server-info) grants))
      (labels ((mark-grantees (id grantees)
                 (dolist (grantee grantees)
                   (let ((info (gethash grantee info-hash)))
                     (when (and
                            info
                            (not (find id (grant-info-reachable-grants info)
                                       :test #'equal
                                       :key #'grant-id)))
                       (let ((grant (find id (grant-info-grants info)
                                          :test #'equal
                                          :key #'grant-id)))
                         (when grant
                           (push grant (grant-info-reachable-grants info))
                           (mark-grantees
                            grantee (grant-info-grantees info)))))))))
        (mark-grantees serverid (grant-info-grantees server-info))))
                   
    ;; Update the database
    (maphash
     (lambda (id info)
       (let ((reachable-grants (grant-info-reachable-grants info)))
         (unless (eql (length reachable-grants)
                      (length (grant-info-grants info)))
           (incf cnt)
           (let ((msg (and reachable-grants
                           (grant-msg (car reachable-grants)))))
             (dolist (grant (cdr reachable-grants))
               (dotcat msg "." (grant-msg grant)))
             (db-put (db server)
                     (permission-key id permission)
                     msg)))))
     info-hash)

    cnt))

(defparameter *all-permissions*
  (list $MINT-TOKENS
        $MINT-COUPONS
        $ADD-ASSET))

(define-message-handler do-permission $PERMISSION (server args msgs)
  ;; (<id>,permission,<serverid>,<req#>,grant=grant)
  (declare (ignore msgs))
  (checkreq server args)
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (serverid (getarg $SERVERID args))
        (grant (getarg $GRANT args))
        (msg (servermsg server $ATPERMISSION (get-parsemsg args))))
    (unless (equal serverid (serverid server))
      (error "Bad serverid in permission request"))
    (cond (grant
           (unless (equal grant $GRANT)
             (error "~s arg to ~s command can only be blank or ~s"
                    $GRANT $PERMISSION $GRANT))
           (when (dolist (permission (db-contents db (permission-key id)))
                   (when (find $GRANT (parse-grants server id permission)
                               :test #'equal :key #'grant-grant)
                     (return t)))
             (dolist (toid (db-contents db $ACCOUNT))
               (dolist (permission (db-contents db (permission-key toid)))
                 (let* ((grants (parse-grants server toid permission))
                        (grant (find id grants :test #'equal :key #'grant-id)))
                   (when grant
                     (dotcat msg "." (grant-msg grant))))))))
          (t (let ((permissions (db-contents db (permission-key serverid))))
               (dolist (permission permissions)
                 (let ((grants (parse-grants server id permission)))
                   (unless grants
                     (setf grants (parse-grants server serverid permission)))
                   (dolist (grant grants)
                     (dotcat msg "." (grant-msg grant)))))
               (dolist (permission *all-permissions*)
                 (unless (member permission permissions :test #'equal)
                   (let ((m (servermsg
                               server $ATGRANT
                               (servermsg
                                server $GRANT serverid "" id permission))))
                     (dotcat msg "." m)))))))
    msg))

(define-message-handler do-audit $AUDIT (server args reqs)
  (declare (ignore reqs))
  (let* ((serverid (serverid server))
         (id (getarg $CUSTOMER args))
         (assetid (getarg $ASSET args))
         (req (checkreq server args)))
    (unless (id-p assetid)
      (error "Not an ID: ~s" assetid))
    (multiple-value-bind (total fraction audit-acct-total)
        (audit server id assetid)
      (setf total (bcsub total audit-acct-total))
      (let* ((msg (servermsg server $ATAUDIT (get-parsemsg args)))
             (totalmsg (servermsg
                        server $BALANCE serverid req assetid total "audit"))
             (fractionmsg (unless (bc= fraction 0)
                           (servermsg
                            server $FRACTION serverid req assetid fraction))))
        (dotcat msg "." totalmsg)
        (when fractionmsg
          (dotcat msg "." fractionmsg))
        msg))))      

(defun audit (server id assetid)
  "Audit the given asset. Return three values:
   1) The grand total of all balances for the asset.
   2) The fractional part of the grand total.
   3) The total of the issuer's \"audit\" account.
   If in balance since the last audit correction, the 1 + 2
   will = 3. If really in balance, all will be 0.
   ID is who's asking. Errors if that's not the server or
   the asset issuer."
  (let* ((db (db server))
         (serverid (serverid server))
         (assetmsg (or (db-get db $ASSET assetid)
                       (error "Unknown asset ID: ~s" assetid)))
         (issuer (unpack-servermsg server assetmsg $ATASSET $ASSET $CUSTOMER)))
    (unless (or (equal id serverid) (equal id issuer))
      (error "Only the issuer can audit an asset"))
    (when (equal assetid (tokenid server))
      (error "Auditing not supported for tokenid"))
    (with-write-locked-fsdb (db t)
      (audit-internal server assetid issuer))))

(defun audit-internal (server assetid issuer)
  (let ((db (db server))
        (total "1")                     ; Account for -1 represents 0
        (fraction "0")
        (precision 0)
        (audit-acct-total "0"))
    ;; Add up balances
    (dolist (id (db-contents db $ACCOUNT))
      (let ((key (append-db-keys $ACCOUNT id $BALANCE)))
        (dolist (acct (db-contents db key))
          (let* ((balance (db-get db key acct assetid))
                 (amount
                  (and balance
                       (unpack-servermsg
                        server balance $ATBALANCE $BALANCE $AMOUNT))))
            (when amount
              (setf total (bcadd total amount))
              (when (and (equal id issuer)
                         (equal acct "audit"))
                (setf audit-acct-total
                      (bcadd audit-acct-total amount))))))

        ;; Add up inbox amounts
        (let* ((key (append-db-keys key $INBOX)))
          (dolist (time (db-contents db key))
            (let* ((msg (db-get db key time))
                   (args (unpack-servermsg server msg $INBOX))
                   (msgtime (getarg $TIME args)))
              (unless (equal msgtime time)
                (error "Time mismatch in ~s: SB: ~s, Was: ~s"
                       key time msgtime))
              (setf args (getarg $MSG args))
              (when (and (equal $SPEND (getarg $OPERATION args))
                         (equal assetid (getarg $ASSET args)))
                (setf total (bcadd total (getarg $AMOUNT args)))))))

        ;; Add fraction
        (let* ((fracmsg (db-get db key $FRACTION assetid))
               (args (and fracmsg
                          (unpack-servermsg
                           server fracmsg $ATFRACTION $FRACTION)))
               (frac (and args (getarg $AMOUNT args))))
          (when args
            (assert (and frac (equal assetid (getarg $ASSET args))))
            (setf precision (max precision (number-precision frac)))
            (wbp (precision)
              (setf fraction (bcadd fraction frac)))))

      ))

    ;; Add up coupons
    (dolist (hash (db-contents db $COUPON))
      (let* ((msg (db-get db $COUPON hash))
             (args (unpack-servermsg server msg $ATSPEND $SPEND)))
        (when (equal assetid (getarg $ASSET args))
          (setf total (bcadd total (getarg $AMOUNT args))))))

    ;; Add queued storagefees
    (let* ((msg (db-get db $ACCOUNT issuer $STORAGEFEE assetid))
           (args (and msg (unpack-servermsg server msg $STORAGEFEE)))
           (frac (and args (getarg $AMOUNT args))))
      (when args
        (assert (and frac (equal assetid (getarg $ASSET args))))
        (setf precision (max precision (number-precision frac)))
        (wbp (precision)
          (setf fraction (bcadd fraction frac)))))

    (multiple-value-setq (total fraction)
      (normalize-balance total fraction (number-precision fraction)))
    (values total fraction audit-acct-total)))

(defvar *crypto-session-to-remove* nil)
(defvar *current-crypto-session* nil)

(define-message-handler do-opensession $OPENSESSION (server args reqs)
  (declare (ignore reqs))
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (timeout (getarg $TIMEOUT args))
        (inactivetime (getarg $INACTIVETIME args)))
    (checkreq server args)
    (cond ((blankp timeout) (setf timeout nil))
          (t (setf timeout (parse-integer timeout))))
    (cond ((blankp inactivetime) (setf inactivetime nil))
          (t (setf inactivetime (parse-integer inactivetime))))
    (let* ((crypto-session (new-crypto-session id timeout inactivetime))
           (sessionid (crypto-session-id crypto-session))
           (sessionkey (crypto-session-password-string crypto-session))
           (msg (get-parsemsg args))
           (plaintext (make-square-bracket-string sessionid sessionkey))
           (pubkey (db-get db $PUBKEY id))
           (ciphertext (pubkey-encrypt plaintext pubkey)))
      ;; Can't remove session until we've used it to encrypt the return message
      (setf *crypto-session-to-remove* *current-crypto-session*)
      (servermsg server $ATOPENSESSION msg ciphertext))))

(define-message-handler do-closesession $CLOSESESSION (server args reqs)
  (declare (ignore reqs))
  (let ((id (getarg $CUSTOMER args))
        (sessionid (getarg $SESSIONID args)))
    (checkreq server args)
    (let ((crypto-session (get-crypto-session sessionid)))
      (unless (equal id (crypto-session-userid crypto-session))
        (error "Attempt to close a crypto-session for another user"))
      ;; Can't remove session until we've used it to encrypt the return message
      (setf *crypto-session-to-remove* crypto-session))))

(define-message-handler do-backup $BACKUP (server args reqs)
  (declare (ignore reqs))
  (let ((db (db server))
        (id (getarg $CUSTOMER args))
        (req (getarg $REQ args))
        (keys&values (getarg :rest args)))
    (unless (equal id (serverid server))
      (error "Backup command only allowed for serverid"))
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
    (servermsg server $ATBACKUP req)))
      
;;;
;;; End request processing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *server-commands* nil)

(defun server-commands ()
  (or *server-commands*
      (let* ((patterns (patterns))
             (names `(,$SERVERID
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
                      ,$AUDIT
                      ,$OPENSESSION
                      ,$CLOSESESSION
                      ,$BACKUP
                      ,$COMMIT
                      ,$GETFEATURES
                      ,$LASTTRANSACTION))
             (commands (make-hash-table :test #'equal)))
      (loop
         for name in names
         for pattern =  (gethash name patterns)
         do
           (setf (gethash name commands) pattern))
      (setq *server-commands* commands))))

(defparameter *backup-mode-commands*
  `(,$SERVERID ,$BACKUP ,$GETREQ))

(defparameter *backup-mode-only-commands*
  `(,$BACKUP))

(defun shorten (string maxlen)
  (if (> (length string) maxlen)
      (strcat (subseq string (- maxlen 3)) "...")
      string))

(defmacro with-server-crypto-session-context ((msg-var &optional (msg msg-var))
                                              &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,msg-var) ,@body))
       (call-with-server-crypto-session-context #',thunk ,msg))))

(defvar *in-server-crypto-session-context* nil)

(defvar *enable-debug-backtrace* nil)

(defun call-with-server-crypto-session-context (thunk msg)
  (if *in-server-crypto-session-context*
      (funcall thunk msg)
      (let ((*in-server-crypto-session-context* t)
            (*current-crypto-session* nil)
            (*crypto-session-to-remove* nil))
        (flet ((crypto-session-handler (c)
                 (when (and *enable-debug-backtrace* (debug-stream-p))
                   (debugmsg "Server crypto error: ~a~%~a"
                             c (backtrace-string)))
                 (let ((sessionid
                        (or (and *current-crypto-session*
                                 (crypto-session-id *current-crypto-session*))
                            (ignore-errors
                              (car (parse-square-bracket-string msg)))
                            "<unknown sessionid>")))
                   (return-from call-with-server-crypto-session-context
                     (make-square-bracket-string
                      sessionid "error" (format nil "~a" c))))))
          (when (square-bracket-string-p msg)
            (handler-bind ((error #'crypto-session-handler))
              (multiple-value-bind (plaintext crypto-session)
                  (decrypt-for-crypto-session msg)
                (check-crypto-session-timeout crypto-session)
                (setf msg plaintext
                      *current-crypto-session* crypto-session))))
          (let ((res (funcall thunk msg)))
            (unwind-protect
                 (if *current-crypto-session*
                     ;; Should we just return plaintext if there's
                     ;; an error encrypting?
                     ;; Probably won't ever happen.
                     (handler-bind ((error #'crypto-session-handler))
                       (values
                        (encrypt-for-crypto-session
                         *current-crypto-session* res)))
                     res)
              (when *crypto-session-to-remove*
                ;; Handle delayed session removal
                (ignore-errors
                  (remove-crypto-session *crypto-session-to-remove*)))))))))

(defmethod process ((server server) msg)
  "Process a message and return the response.
   This is usually all you'll call from outside."
  (with-server-crypto-session-context (msg)
    (block nil
      (handler-bind
          ((error
            (lambda (c)
              (when (and *enable-debug-backtrace* (debug-stream-p))
                (debugmsg "Server error: ~a~%~a" c (backtrace-string)))
              (return (failmsg server msg (format nil "~a" c))))))
        (with-read-locked-fsdb ((db server))
          (process-internal server msg))))))

(defun maybe-abort-two-phase-commit (request args)
  (when (and (not (equal request $COMMIT))
             (getarg $TIME args))
    (let ((id (getarg $CUSTOMER args)))
      (with-lock-grabbed (*transactions-lock*)
        (remhash id *transactions*)))))

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
           (args-serverid (getarg $SERVERID args))
           (note (getarg $NOTE args))
           (handler (get-message-handler request)))
      (unless (or (null args-serverid)
                  (and (equal "0" args-serverid)
                       (equal $SERVERID (getarg $REQUEST args)))
                  (equal args-serverid (serverid server)))
        (error "serverid mismatch"))
      (when (> (length note) 4096)
        (error "Note too long. Max: 4096 chars"))
      (maybe-abort-two-phase-commit request args)
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
