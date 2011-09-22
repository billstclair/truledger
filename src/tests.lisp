; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test code for Truledger client and server
;;;

(in-package :truledger-test)

(defvar *test-pathname*
  (or *load-pathname* "~/loom/web/truledger/lisp/src/tests.lisp"))

(defparameter *www-dir*
  (directory-namestring (merge-pathnames "../www/" *test-pathname*)))

(defun make-test-state (db-dir &rest rest &key dont-erase passphrase port network-p)
  (declare (ignore dont-erase passphrase port network-p))
  (apply #'make-instance 'test-state
         :db-dir db-dir
         rest))        

(defclass test-state ()
  ((db-dir :accessor db-dir-of
           :initarg :db-dir)
   (port :accessor port
         :initarg :port
         :initform 8081)
   (passphrase :accessor passphrase
               :initarg :passphrase
               :initform "passphrase")
   (server :accessor server
           :initform nil)
   (client :accessor client
           :initform nil)))

(defmethod initialize-instance :after ((ts test-state) &key
                                       dont-erase
                                       (network-p nil))
  (let* ((db-dir (ensure-directory-pathname (db-dir-of ts)))
         (port (port ts))
         (passphrase (passphrase ts))
         (server-dir (merge-pathnames "serverdb" db-dir))
         (client-dir (merge-pathnames "clientdb" db-dir)))
    (when network-p
      (stop-web-server port))
    (unless dont-erase
      (ignore-errors (recursive-delete-directory server-dir))
      (ignore-errors (recursive-delete-directory client-dir)))
    (setf (server ts) (make-server
                       server-dir passphrase
                       :servername "Test Server"
                       :serverurl (format nil "http://localhost:~d/" port)
                       :privkey-size 512)
          (client ts) (make-client client-dir
                                   :test-server (unless network-p (server ts))))
    (when network-p
      (start-test-web-server ts))))

(defmethod start-test-web-server ((ts test-state))
  (truledger-web-server (server ts) :www-dir *www-dir* :port (port ts)))

(defmethod stop-test-web-server ((ts test-state))
  (stop-web-server (port ts)))

(defmethod getinbox  ((ts test-state) &optional includeraw)
  (getinbox (client ts) includeraw))

(defmethod getoutbox ((ts test-state) &optional includeraw)
  (getoutbox (client ts) includeraw))

(defmethod getbalance ((ts test-state) &optional acct assetid includeraw)
  (getbalance (client ts) acct assetid includeraw))

(defmethod getfraction ((ts test-state) &optional assetid includeraw)
  (getfraction (client ts) assetid includeraw))

(defmethod getstoragefee ((ts test-state) &optional assetid)
  (getstoragefee (client ts) assetid))

(defmethod id ((ts test-state))
  (id (client ts)))

(defmethod serverid ((ts test-state))
  (serverid (server ts)))

(defmethod getassets ((ts test-state))
  (getassets (client ts)))

(defmethod tokenid ((ts test-state))
  (tokenid (client ts)))

(defmethod login-server ((ts test-state))
  (let* ((server (server ts))
         (client (client ts))
         (serverid (serverid server))
         (passphrase (passphrase ts)))
    (handler-case (login client passphrase)
      (error ()
        (let ((privkey (decode-rsa-private-key
                        (encode-rsa-private-key (privkey server)))))
          (newuser client
                   :passphrase passphrase
                   :privkey privkey))))
    (handler-case (setserver client serverid)
      (error ()
        (addserver client (serverurl server))
        (let ((balance (getbalance client))
              (tokenid (tokenid server)))
          (loop
             for (acct . bals) in balance
             do
               (unless (equal acct $MAIN)
                 (error "Found non-main acct: ~s" acct))
               (dolist (bal bals)
                 (let ((assetid (balance-assetid bal))
                       (assetname (balance-assetname bal))
                       (amount (balance-amount bal))
                       (formatted-amount (balance-formatted-amount bal)))
                   (assert (equal tokenid assetid))
                   (assert (equal assetname "Test Server Usage Tokens"))
                   (assert (equal amount "-1"))
                   (assert (equal formatted-amount "-0"))))))))
    (id client)))
  
(defmethod login-user ((ts test-state) passphrase &optional (name passphrase))
  (let ((client (client ts))
        (server (server ts)))
    (handler-case (login client passphrase)
      (error ()
        (newuser client :passphrase passphrase :privkey 512)))
    (handler-case (setserver client (serverid server))
      (error ()
        (handler-case (addserver client (serverurl server) name)
          (error ()
            (let ((id (id client)))
              (login-server ts)
              (spend client id (tokenid server) "200" nil "Welcome to my server")
              (login client passphrase)
              (addserver client (serverurl server) name))))))
    (id client)))

(defmethod accept-inbox ((ts test-state) &optional (accept-p t))
  (loop with client = (client ts)
     for inbox = (getinbox client)
     for directions = nil
     while inbox
     do
      (dolist (item inbox)
        (push (make-process-inbox
               :time (inbox-time item)
               :request (if accept-p $SPENDACCEPT $SPENDREJECT)
               :note (format nil "~a ~a"
                             (if accept-p "Accepting" "Rejecting")
                             (or (inbox-msgtime item) (inbox-time item))))
              directions))
       (processinbox client directions)))
             
(defmethod cancel-outbox ((ts test-state))
  (accept-inbox ts)
  (let* ((client (client ts))
         (outbox (getoutbox client)))
    (dolist (item outbox)
      (when (equal (outbox-request item) $SPEND)
        (let ((time (outbox-time item)))
          (spendreject client time)
          (accept-inbox ts)
          (return time))))))

(defmethod getbal ((ts test-state) asset &optional (acct $MAIN))
  (let* ((client (client ts))
         (bal (getbalance client acct asset)))
    (and bal (balance-amount bal))))

(defmethod give-tokens ((ts test-state) user amount)
  (let* ((client (client ts))
         (id (login-user ts user))
         (fee-asset (fee-assetid (getfees client))))
    (login-server ts)
    (spend client id fee-asset amount)
    (login-user ts user)
    (accept-inbox ts)))

(defun set-standard-fees (ts)
  (let ((client (client ts)))
    (login-server ts)
    (setfees client)
    (let ((permissions (get-permissions client nil t)))
      (dolist (permission permissions)
        (deny client
              (permission-toid permission)
              (permission-permission permission))))))

(defmethod tokens-test ((ts test-state))
  (set-standard-fees ts)
  (let* ((john (prog1 (login-user ts "john") (accept-inbox ts)))
         (bill (login-user ts "bill"))
         (client (client ts))
         (fee (getfees client))
         (fee-asset (fee-assetid fee))
         (fee-amount (fee-amount fee))
         bill-tokens)

    ;; Make sure bill has enough tokens
    (give-tokens ts "bill" "14")

    ;; Spend 10 tokens from bill to john. John accepts.
    (setq bill-tokens (getbal ts fee-asset))
    (spend client john fee-asset "10" nil (strcat john bill))
    (assert (eql 0 (bccomp (getbal ts fee-asset)
                           (bcsub bill-tokens 10 fee-amount)))
            nil
            "Balance mismatch after spend")
    (login-user ts "john")
    (accept-inbox ts)
    (login-user ts "bill")
    (accept-inbox ts)
    (assert (eql 0 (bccomp (getbal ts fee-asset)
                           (bcsub bill-tokens 10)))
            nil
            "Balance mismatch after accept")

    ;; Spend 10 tokens from bill to john. John rejects
    (setq bill-tokens (getbal ts fee-asset))
    (spend client john fee-asset "10" nil (strcat john bill))
    (assert (eql 0 (bccomp (getbal ts fee-asset)
                           (bcsub bill-tokens 10 fee-amount)))
            nil
            "Balance mismatch after spend")
    (login-user ts "john")
    (accept-inbox ts nil)
    (login-user ts "bill")
    (accept-inbox ts)
    (assert (eql 0 (bccomp (getbal ts fee-asset)
                           (bcsub bill-tokens 2)))
            nil
            "Balance mismatch after accept")

    ;; Spend 10 tokens from bill to john. Bill cancels
    (setq bill-tokens (getbal ts fee-asset))
    (spend client john fee-asset "10" nil (strcat john bill))
    (assert (eql 0 (bccomp (getbal ts fee-asset)
                           (bcsub bill-tokens 10 fee-amount)))
            nil
            "Balance mismatch after spend")
    (cancel-outbox ts)
    (assert (eql 0 (bccomp (getbal ts fee-asset)
                           (bcsub bill-tokens 2)))
            nil
            "Balance mismatch after cancel")))

(defmethod bill-goldgrams-assetid ((ts test-state) &optional (percent nil))
  (set-standard-fees ts)
  (login-user ts "bill")
  (accept-inbox ts)
  (let* ((client (client ts))
         (precision "7")
         (scale "3")
         (name "Bill GoldGrams")
         (assetid (assetid (id client) precision scale name))
         (asset (ignore-errors (getasset client assetid))))
    (unless (and asset (equal percent (asset-percent asset)))
      (addasset client precision scale name percent))
    assetid))

(defmethod goldgrams-test ((ts test-state))
  (set-standard-fees ts)
  (let* ((john (prog1 (login-user ts "john") (accept-inbox ts)))
         (bill (login-user ts "bill"))
         (client (client ts))
         (fee (getfees client))
         (fee-asset (fee-assetid fee))
         (fee-amount (fee-amount fee))
         (assetid (bill-goldgrams-assetid ts))
         (scale (asset-scale (getasset client assetid)))
         (formatted-amount "100")
         (amount (bcmul formatted-amount (bcpow 10 scale)))
         bill-tokens
         bill-grams)
    ;; Make sure bill has enough tokens
    (give-tokens ts "bill" "4")

    ;; Spend 10 goldgrams from bill to john. John accepts.
    (setq bill-tokens (getbal ts fee-asset)
          bill-grams (getbal ts assetid))
    (spend client john assetid formatted-amount nil (strcat john bill))
    (assert (and (eql 0 (bccomp (getbal ts fee-asset)
                                (bcsub bill-tokens fee-amount)))
                 (eql 0 (bccomp (getbal ts assetid)
                                (bcsub bill-grams amount))))
            nil
            "Balance mismatch after spend")
    (login-user ts "john")
    (accept-inbox ts)
    (login-user ts "bill")
    (accept-inbox ts)
    (assert (and (eql 0 (bccomp (getbal ts fee-asset) bill-tokens))
                 (eql 0 (bccomp (getbal ts assetid)
                                (bcsub bill-grams amount))))
            nil
            "Balance mismatch after accept")

    ;; Spend 10 goldgrams from bill to john. John rejects
    (setq bill-tokens (getbal ts fee-asset)
          bill-grams (getbal ts assetid))
    (spend client john assetid formatted-amount nil (strcat john bill))
    (assert (and (eql 0 (bccomp (getbal ts fee-asset)
                                (bcsub bill-tokens fee-amount)))
                 (eql 0 (bccomp (getbal ts assetid)
                                (bcsub bill-grams amount))))
            nil
            "Balance mismatch after spend")
    (login-user ts "john")
    (accept-inbox ts nil)
    (login-user ts "bill")
    (accept-inbox ts)
    (assert (and (eql 0 (bccomp (getbal ts fee-asset)
                                (bcsub bill-tokens 2)))
                 (eql 0 (wbp (scale)
                          (bccomp (getbal ts assetid) bill-grams))))
            nil
            "Balance mismatch after reject")

    ;; Spend 10 goldgrams from bill to john. Bill cancels.
    (setq bill-tokens (getbal ts fee-asset)
          bill-grams (getbal ts assetid))
    (spend client john assetid formatted-amount nil (strcat john bill))
    (assert (and (eql 0 (bccomp (getbal ts fee-asset)
                                (bcsub bill-tokens fee-amount)))
                 (eql 0 (bccomp (getbal ts assetid)
                                (bcsub bill-grams amount))))
            nil
            "Balance mismatch after spend")
    (cancel-outbox ts)
    (accept-inbox ts)
    (assert (and (eql 0 (bccomp (getbal ts fee-asset)
                                (bcsub bill-tokens 2)))
                 (eql 0 (bccomp (getbal ts assetid) bill-grams)))
            nil
            "Balance mismatch after reject")
    ))

(defmethod storage-test ((ts test-state))
  (set-standard-fees ts)
  (let* ((john (prog1 (login-user ts "john") (accept-inbox ts)))
         (client (client ts))
         (percent "1.0")
         (assetid (bill-goldgrams-assetid ts percent))
         (bill (id client))
         (scale (asset-scale (getasset client assetid)))
         (formatted-amount "100")
         (amount (bcmul formatted-amount (bcpow 10 scale))))
    (declare (ignore amount))

    ;; Make sure bill has enough tokens
    (give-tokens ts "bill" "4")

    ;; Spend to John. He accepts.
    (spend client john assetid (bcadd formatted-amount 1))
    (login-user ts "john")
    (sleep 1)
    (accept-inbox ts)
    (login-user ts "bill")
    (accept-inbox ts)

    ;; Spend to John. He rejects
    (spend client john assetid formatted-amount)
    (login-user ts "john")
    (sleep 1)
    (accept-inbox ts nil)
    (login-user ts "bill")
    (accept-inbox ts)

    ;; Spend to John. Cancel the spend.
    (spend client john assetid formatted-amount)
    (cancel-outbox ts)
    (accept-inbox ts)

    ;; Make sure john has enough tokens
    (give-tokens ts "john" "4")

    ;; Spend from John. Cancel the spend.
    (spend client bill assetid formatted-amount)
    (sleep 1)
    (cancel-outbox ts)
    (accept-inbox ts)

    ;; Spend from John. Bill rejects.
    (spend client bill assetid formatted-amount)
    (login-user ts "bill")
    (accept-inbox ts nil)
    (sleep 1)
    (login-user ts "john")
    (accept-inbox ts)
    
    ;; Spend from John. Bill accepts
    (spend client bill assetid formatted-amount)
    (login-user ts "bill")
    (accept-inbox ts)
    (login-user ts "john")
    (accept-inbox ts)

    (let ((bill-bal (progn (login-user ts "bill")
                           (getbalance client $MAIN assetid)))
          (bill-fee (getstoragefee client assetid))
          (john-bal (progn (login-user ts "john")
                           (getbalance client $MAIN assetid)))
          (john-frac (getfraction client assetid)))
      (values bill-bal bill-fee john-bal john-frac))))

(defmethod transfer-test ((ts test-state))
  (set-standard-fees ts)
  (give-tokens ts "bill" "3")
  (let* ((bill (login-user ts "bill"))
         (client (client ts))
         (fee (getfees client))
         (tokenid (fee-assetid fee))
         (assetid (bill-goldgrams-assetid ts))
         (tokenbal (getbal ts tokenid))
         (balance (getbalance ts t))
         (acct (loop for i from 0
                  for acct = (format nil "a~d" i)
                  do
                    (unless (assoc acct balance :test #'equal)
                      (return acct)))))
    (spend client bill tokenid "1" (list nil acct))
    (spend client bill assetid "1" (list nil acct))
    (let* ((newbal (getbal ts tokenid))
           (diff (bcsub tokenbal (getbal ts tokenid)))
           (sb (bcadd 1 2)))          ;1 spent, 2 new file fees
      (unless (bc= diff sb)
        (error "Diff sb: ~a, was: ~a, tokenbal: ~a, newbal: ~a"
               sb diff tokenbal newbal)))
    acct))

(defmethod fee-test ((ts test-state))
  (set-standard-fees ts)
  (let* ((john (prog1 (login-user ts "john") (accept-inbox ts)))
         (bill (prog1 (login-user ts "bill") (accept-inbox ts)))
         (client (client ts))
         (tokenid (fee-assetid (getfees client)))
         (fees (list (make-fee :type $TRANFEE :assetid tokenid :amount 1)
                     (make-fee :type $REGFEE :assetid tokenid :amount 10)))
         (goldgrams (bill-goldgrams-assetid ts)))
    (login-server ts)
    (apply #'setfees client
           `(,@fees
             ,(make-fee :type $SPEND :assetid tokenid :amount 2)
             ,(make-fee :type $TRANSFER :assetid tokenid :amount 3)))
    (give-tokens ts "bill" (+ 10 1 2 10 3))
    (let ((server-tokens (progn (login-server ts)
                              (reinit-balances client)
                              (balance-amount (getbalance ts $MAIN tokenid))))
          (tokens (progn (login-user ts "bill")
                         (balance-amount (getbalance ts $MAIN tokenid))))
          (backup-p (getbalance ts "backup" tokenid)))
      (spend client john tokenid "10")
      (let ((sb (bcsub tokens 10 1 2))
            (was (balance-amount (getbalance ts $MAIN tokenid)))
            (server-sb (bcadd server-tokens 2))
            (server-was (progn
                        (login-server ts)
                        (storagefees client)
                        (accept-inbox ts)
                        (prog1 (balance-amount (getbalance ts $MAIN tokenid))
                          (login-user ts "bill")))))
        (unless (bc= sb was)
          (error "Outspend w/token fee mismatch. Old: ~s, SB: ~s, Was: ~s"
                 tokens sb was))
        (setf tokens was)
        (unless (bc= server-sb server-was)
          (error "Outspend w/token fee server mismatch. Old: ~s, SB: ~s, Was: ~s"
                 server-tokens server-sb server-was))
        (setf server-tokens server-was))
      (spend client bill tokenid "10" `(,$MAIN "backup"))
      (let ((sb (bcsub tokens 10 3 (if backup-p 0 1)))
            (was (balance-amount (getbalance ts $MAIN tokenid))))
        (unless (bc= sb was)
          (error "Transfer w/token fee mismatch. Old: ~s SB: ~s, Was: ~s"
                 tokens sb was))))

    ;; Test with goldgrams as a fee
    (login-server ts)
    (apply #'setfees client
           `(,@fees
             ,(make-fee :type $SPEND :assetid goldgrams
                        :formatted-amount "0.001")
             ,(make-fee :type $TRANSFER :assetid goldgrams
                        :formatted-amount "0.002")))
    (login-user ts "bill")
    (let ((gg (balance-formatted-amount (getbalance client $MAIN goldgrams))))
      (spend client john goldgrams "1.1")
      (let ((sb (wbp (7) (bcsub gg "1.1")))
            (was (balance-formatted-amount (getbalance client $MAIN goldgrams))))
        (unless (bc= sb was)
          (error "Spend goldgrams issuer mismatch. Old: ~s, sb: ~s, was: ~s"
                 gg sb was))))
    (login-user ts "john")
    (accept-inbox ts)
    (let ((servergg (progn
                    (login-server ts)
                    (storagefees client)
                    (accept-inbox ts)
                    (let ((bal (getbalance client $MAIN goldgrams)))
                      (if bal
                          (balance-formatted-amount bal)
                          "0"))))
          (gg (progn
                (login-user ts "john")
                (balance-formatted-amount
                 (getbalance client $MAIN goldgrams)))))
      (spend client john goldgrams "1" `(,$MAIN "backup"))
      (let ((serversb (wbp (7) (bcadd servergg ".002")))
            (serverwas (progn
                       (login-server ts)
                       (storagefees client)
                       (accept-inbox ts)
                       (balance-formatted-amount
                        (getbalance client $MAIN goldgrams))))
            (sb (wbp (7) (bcsub gg "1" "0.002")))
            (was (progn
                   (login-user ts "john")
                   (balance-formatted-amount
                    (getbalance client $MAIN goldgrams)))))
        (unless (bc= serversb serverwas)
          (error "Spend goldgrams transfer server mismatch. Old:~s, sb: ~s, was: ~s"
                 servergg serversb serverwas))
        (setf servergg serverwas)
        (unless (bc= sb was)
          (error "Spend goldgrams transfer mismatch. Old: ~s, sb: ~s, was: ~s"
                 gg sb was)))
      (setf gg (balance-formatted-amount
                (getbalance client "backup" goldgrams)))

      (spend client bill goldgrams "0.5" "backup")
      (let ((serversb (wbp (7) (bcadd servergg ".001")))
            (serverwas (progn
                       (login-server ts)
                       (storagefees client)
                       (accept-inbox ts)
                       (balance-formatted-amount
                        (getbalance client $MAIN goldgrams))))
            (sb (wbp (7) (bcsub gg "0.5" "0.001")))
            (was (progn
                   (login-user ts "john")
                   (balance-formatted-amount
                    (getbalance client "backup" goldgrams)))))
        (unless (bc= serversb serverwas)
          (error "Spend goldgrams server mismatch. Old:~s, sb: ~s, was: ~s"
                 servergg serversb serverwas))
        (setf servergg serverwas)
        (unless (bc= sb was)
          (error "Spend goldgrams mismatch. Old: ~s, sb: ~s, was: ~s"
                 gg sb was))
        (setf gg was)))

    ;; Test with goldgrams as a fee with storage fees
    (bill-goldgrams-assetid ts "1")
    (give-tokens ts john "12")
    (login-user ts "john")
    (spend client john goldgrams "0.2" `("backup" ,$MAIN))
    (sleep 0.5)
    (spend client bill goldgrams "0.1")
    (sleep 0.5)
    (spend client bill tokenid "10")
    (login-server ts)
    (storagefees client)
    (accept-inbox ts)
    (values
     (getbalance ts t)
     (progn (login-user ts "john")
            (getbalance ts t)))
))

(defun permissions-test (ts)
  (set-standard-fees ts)
  (let ((client (client ts))
        (serverid (serverid ts))
        (tokenid (tokenid ts))
        (bill (prog1 (login-user ts "bill") (accept-inbox ts)))
        (john (prog1 (login-user ts "john") (accept-inbox ts)))
        (mike (prog1 (login-user ts "mike") (accept-inbox ts)))
        (goldgrams (bill-goldgrams-assetid ts)))
    (login-server ts)
    ;; Test $MINT-TOKENS permission
    (assert (nth-value 1 (ignore-errors (grant client bill $MINT-TOKENS t))))
    (grant client serverid $MINT-TOKENS t)
    (grant client bill $MINT-TOKENS t)
    (give-tokens ts "bill" 12)
    (multiple-value-bind (permissions grant-p)
        (get-permissions client $MINT-TOKENS t)
      (assert grant-p)
      (assert (eql (length permissions) 1))
      (let ((perm (car permissions)))
        (assert (and (equal (permission-id perm) serverid)
                     (equal (permission-toid perm) bill)
                     (equal (permission-permission perm) $MINT-TOKENS)
                     (permission-grant-p perm)))))                         
    (spend client $COUPON tokenid 10)
    (cancel-outbox ts)
    (give-tokens ts "john" 12)
    (assert (null (get-permissions client $MINT-TOKENS t)))
    (assert (nth-value 1 (ignore-errors (spend client $COUPON tokenid 10))))

    ;; Test multiple grants
    (login-server ts)
    (grant client mike $MINT-TOKENS t)
    (login-user ts "mike")
    (grant client john $MINT-TOKENS)
    (login-user ts "bill")
    (grant client john $MINT-TOKENS t)
    (login-user ts "john")
    (multiple-value-bind (permissions grant-p)
        (get-permissions client $MINT-TOKENS t)
      (assert (and (find bill permissions :test #'equal :key #'permission-id)
                   (find mike permissions :test #'equal :key #'permission-id)
                   grant-p)))
    (login-server ts)
    (deny client bill $MINT-TOKENS)
    (login-user ts "john")
    (multiple-value-bind (permissions grant-p)
        (get-permissions client $MINT-TOKENS t)
      (assert (and (eql 1 (length permissions))
                   (find mike permissions :test #'equal :key #'permission-id)
                   (not grant-p))))
    (login-server ts)
    (deny client mike $MINT-TOKENS)
    (login-user ts "john")
    (multiple-value-bind (permissions grant-p)
        (get-permissions client $MINT-TOKENS t)
      (assert (and (null permissions) (null grant-p))))

    ;; Test circular grants
    (login-server ts)
    (grant client bill $MINT-TOKENS t)
    (login-user ts "bill")
    (grant client john $MINT-TOKENS t)
    (login-user ts "john")
    (grant client bill $MINT-TOKENS t)
    (login-server ts)
    (deny client bill $MINT-TOKENS)
    (login-user ts "bill")
    (assert (not (get-permissions client $MINT-TOKENS t)))
    (login-user ts "john")
    (assert (not (get-permissions client $MINT-TOKENS t)))

    ;; Test MINT-COUPON permission
    (login-user ts "bill")
    (assert (nth-value 1 (ignore-errors (spend client $COUPON tokenid 10))))
    (spend client $COUPON goldgrams 1)
    (cancel-outbox ts)
    (login-server ts)
    (grant client serverid $MINT-COUPONS t)
    (login-user ts "bill")
    (assert (nth-value 1 (ignore-errors (spend client $COUPON tokenid 10))))
    (assert (nth-value 1 (ignore-errors (spend client $COUPON goldgrams 1))))
    (login-server ts)
    (grant client bill $MINT-COUPONS t)
    (login-user ts "bill")
    (assert (nth-value 1 (ignore-errors (spend client $COUPON tokenid 10))))
    (let* ((pubkey (rsa-generate-key 512))
           (id (pubkey-id (encode-rsa-public-key pubkey))))
      (rsa-free pubkey)
      (assert (nth-value 1 (ignore-errors (spend client id tokenid 10)))))
    (spend client $COUPON goldgrams 1)
    (cancel-outbox ts)

    ;; Test ADD-ASSET permission
    ;; Already made bill goldgrams asset with no permission in place
    (login-server ts)
    (grant client serverid $ADD-ASSET t)
    (login-user ts "bill")
    (assert (nth-value 1 (ignore-errors
                           (addasset client 7 3 "Bill Silver Grams"))))
    (login-server ts)
    (grant client bill $ADD-ASSET)
    (addasset client 7 3 "Bill Silver Grams")

    nil))

(defun run-all-tests (ts-or-dir &rest rest &key dont-erase passphrase port network-p)
  (declare (ignore dont-erase passphrase port network-p))
  (let ((ts ts-or-dir))
    (unless (typep ts 'test-state)
      (setf ts (apply #'make-test-state ts-or-dir rest)))
    (format t "tokens...") (finish-output)
    (tokens-test ts)
    (format t " goldgrams...") (finish-output)
    (goldgrams-test ts)
    (format t " storage...") (finish-output)
    (storage-test ts)
    (format t " transfer...") (finish-output)
    (transfer-test ts)
    (format t " fees...") (finish-output)
    (fee-test ts)
    (format t " permissions...")
    (permissions-test ts)
    ts))

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
