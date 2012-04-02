; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Truledger JSON webapp server
;;;
;;; See www/doc/json.txt (http://truledger.com/doc/json.txt) for spec.
;;;

(in-package :truledger-json)

;; Called from do-truledger-json in server-web.lisp
;; Returns a JSON string.
(defun json-server ()
  (let* ((client (make-client (client-db-dir))))
    (let* ((res (catch 'error-return
                  (unwind-protect
                       (handler-case
                           (json-server-internal client)
                         (error (c)
                           (json-error "~a" c)))
                    (finalize client))))
           (str (ignore-errors (json:encode-json-to-string res))))
      (or str
          (json:encode-json-to-string
           (catch 'error-return
             (json-error "Unencodable result: ~s" res)))))))

(defun json-error (format-string &rest format-args)
  (throw 'error-return
    `(("@type" . "error")
      ("message" . ,(apply #'format nil format-string format-args)))))

(defparameter *json-commands*
  '(("newuser")
    ("getprivkey")
    ("login")
    "logout"
    "getuser"
    "getpubkey"
    "getserver"
    "getservers"
    "addserver"
    "setserver"
    "current-server"
    "privkey-cached?"
    "cache-privkey"
    "getcontact"
    "getcontacts"
    "addcontact"
    "deletecontact"
    "sync-contacts"
    "getasset"
    "getassets"
    "addasset"
    "getfees"
    "getbalance"
    "getbalances"
    "getrawbalances"
    "getfraction"
    "getfractions"
    "getrawfractions"
    "getstoragefee"
    "getstoragefees"
    "spend"
    "spendreject"
    "is-history-enabled?"
    "set-history-enabled"
    "get-history-times"
    "get-history-items"
    "remove-history-items"
    "getinbox"
    "processinbox"
    "storagefees"
    "getoutbox"
    "redeem"
    "getversion"
    "get-permissions"
    "get-granted-permissions"
    "grant"
    "deny"
    "audit"))

(defparameter *json-dispatch-table* nil)
(defparameter *last-json-commands* nil)

(defun json-dispatch-table ()
  (if (eq *json-commands* *last-json-commands*)
      *json-dispatch-table*
      (let ((hash (make-hash-table :test #'equal :size (length *json-commands*))))
        (mapc
         (lambda (command)
           (setf (gethash command hash)
                 (intern (format nil "JSON-~a"
                                 (string-upcase
                                  (if (listp command) (car command) command)))
                         :truledger-json)))
         *json-commands*)
        (prog1
            (setf *json-dispatch-table* hash)
          (setf *last-json-commands* *json-commands*)))))

(defun get-json-command-function (command)
  (or (gethash command (json-dispatch-table))
      (let ((res (gethash (list command) (json-dispatch-table))))
        (if res
            (values res t)
            (json-error "Unknown command: ~a" command)))))

(defun alistp (x)
  (loop
     (when (null x) (return t))
     (unless (listp x) (return nil))
     (let ((elt (pop x)))
       (unless (and (listp elt) (atom (car elt)))
         (return nil)))))

(defun json-server-internal (client)
  client
  (let* ((json (or (parm "eval") (json-error "Missing eval string")))
         (form (json:decode-json-from-string json)))
    (unless (and (listp form)
                 (stringp (first form))
                 (listp (cdr form))
                 (alistp (second form))
                 (null (cddr form)))
      (json-error "Eval form must be [command,{arg:value,...}]"))
    (let ((args (second form)))
      (multiple-value-bind (fun no-login-p)
          (get-json-command-function (first form))
        (unless no-login-p
          (%login-json client args))
        (funcall fun client args)))))

(defun assoc-json-value (key alist)
  (cdr (assoc key alist :test #'string-equal)))

(defun blank-to-nil (x)
  (if (blankp x) nil x))

(defmacro with-json-args (lambda-list args-alist &body body)
  (let ((args-var (gensym "ARGS")))
    `(let ((,args-var ,args-alist))
       (let ,(loop for var-spec in lambda-list
                for var = (if (listp var-spec) (first var-spec) var-spec)
                for default = (and (listp var-spec) (second var-spec))
                for val-form = `(blank-to-nil
                                  (assoc-json-value
                                   ,(string-downcase (string var)) ,args-var))
                collect `(,var ,(if default
                                    `(or ,val-form ,default)
                                    val-form)))
         ,@body))))

(defun parse-proxy (proxy)
  (check-type proxy (or null string))
  (unless (blankp proxy)
    (let ((colon-pos (position #\: proxy :from-end t)))
      (unless colon-pos
        (json-error "Proxy must be host:port"))
      (let ((host (subseq proxy 0 colon-pos))
            (port (subseq proxy (1+ colon-pos))))
        (unless (ignore-errors
                  (setf port (parse-integer port)))
          (json-error "Proxy port not an integer: ~s" proxy))
        (when (string-equal host "localhost")
          (setf host "127.0.0.1"))
        (list host port)))))

(defun ensure-string (var name &optional optionalp)
  (unless (or (and optionalp (null var))
              (stringp var))
    (json-error "~a must be a string" name)))

(defun ensure-integer (var name &optional optionalp)
  (unless (or (and optionalp (null var))
              (integerp var))
    (json-error "~a must be an integer" name)))

(defun json-newuser (client args)
  (with-json-args (passphrase) args
    (unwind-protect
         (json-newuser-internal client passphrase args)
      (when (stringp passphrase)
        (destroy-password passphrase)))))

(defun json-newuser-internal (client passphrase args)
  (with-json-args (keysize name privkey fetchprivkey url coupon proxy)
      args
    (ensure-string passphrase "passphrase")
    (when (stringp keysize) (setf keysize (parse-integer keysize)))
    (ensure-integer keysize "keysize" t)
    (ensure-string name "name" t)
    (ensure-string privkey "privkey" t)
    (ensure-string url "url" t)
    (ensure-string coupon "coupon" t)
    (ensure-string proxy "proxy" t)
    
    (when (cond (keysize (or privkey fetchprivkey))
                (privkey fetchprivkey)
                ((not fetchprivkey)
                 (json-error
                  "One of keysize, privkey, and fetchprivkey must be included")))
      (json-error
       "Only one of keysize, privkey, and fetchprivkey may be included"))
    (when (and url coupon)
      (error "Only one of url and coupon may be included"))
    (when (passphrase-exists-p client passphrase)
      (json-error "There is already a client account for passphrase"))
    (when proxy
      (setf proxy (parse-proxy proxy)))

    (when fetchprivkey
      (unless url
        (json-error "url required to fetch private key from server"))
      (verify-server client url nil proxy)
      (when fetchprivkey
        (handler-case
            (setf privkey (fetch-privkey client url passphrase
                                         :http-proxy proxy))
          (error (c)
            (json-error "Error fetching private key from ~a: ~a"
                        url c)))))

    (cond ((and privkey url)
           ;; Make sure we've got an account.
           (verify-private-key client privkey passphrase url proxy))
          ((not coupon)
           (when (server-db-exists-p)
             (json-error "Coupon must be included to create new account")))
          (t
           (let ((url (parse-coupon coupon)))
             (handler-case
                 (verify-coupon client coupon nil url :http-proxy proxy)
               (error (c)
                 (json-error "Coupon didn't verify: ~a" c))))))
    (newuser client :passphrase passphrase :privkey (or privkey keysize))
    (let ((session (login-new-session client passphrase)))
      ;; Not calling maybe-start-server here. Maybe I should
      (handler-case
          (addserver client (or coupon url) :name name :couponok t
                     :http-proxy proxy)
        (error (c)
          (logout client)
          (json-error "Failed to add server: ~a" c)))
      (when fetchprivkey
        (setf (privkey-cached-p client) t))
       session)))

(defun json-getprivkey (client args)
  (with-json-args (passphrase) args
    (when (blankp passphrase)
      (json-error "Passphrase required for getprivkey"))
    (login client passphrase)
    (encode-rsa-private-key (privkey client) passphrase)))

(defun json-login (client args)
  (with-json-args (passphrase) args
    (when (blankp passphrase)
      (json-error "Passphrase required for login"))
    (let ((session (login-new-session client passphrase)))
      (%setserver-json client)
      session)))

(defun %setserver-json (client)
  (let ((serverid (user-preference client "serverid")))
    (ignore-errors (setserver client serverid nil))
    (or (serverid client)
        (dolist (server (getservers client))
          (ignore-errors
            (setserver client (server-info-id server))
            (setf (user-preference client "serverid") serverid)
            (return (server-info-id server)))))))

(defun %login-json (client args)
  (with-json-args (session) args
    (unless session
      (json-error "Missing session arg"))
    (login-with-sessionid client session)
    (%setserver-json client)
    (initialize-client-history client)))

(defun json-logout (client args)
  (declare (ignore args))
  (logout client))

(defun json-getuser (client args)
  (with-json-args (id) args
    (unless id (setf id (id client)))
    (multiple-value-bind (pubkeysig name) (get-id client id)
      (unless pubkeysig
        (json-error "There is no user with id: ~a" id))
      `(("@type" . "user")
        ("id" . ,id)
        ,@(%json-optional "name" name)))))

(defun json-getpubkey (client args)
  (declare (ignore args))
  (pubkey client))

(defun json-getserver (client args)
  (with-json-args (serverid) args
    (unless serverid
      (setf serverid (serverid client))
      (unless serverid
        (json-error "There is no current server")))
    (let ((server (or (getserver client serverid)
                      (json-error "There is no server with id: ~a" serverid))))
      (%json-server-alist server client))))

(defun %json-server-alist (server client)
  `(("@type" . "server")
    ("id" . ,(server-info-id server))
     ("name" . ,(server-info-name server))
     ("url" . ,(server-info-url server))
     ,@(let ((host (server-info-proxy-host server)))
            (when host
              `(("proxy"
                 .
                 ,(format nil "~s:~d"
                          host (server-info-proxy-port server))))))
    ("privkeycached" . ,(privkey-cached-p client (server-info-id server)))))

 (defun json-getservers (client args)
   (declare (ignore args))
   (loop for server in (getservers client)
      collect (%json-server-alist server client)))

 (defun json-addserver (client args)
   (with-json-args (coupon name proxy) args
    (ensure-string coupon "coupon")
    (addserver client coupon :name name :http-proxy (parse-proxy proxy))
    (serverid client)))

(defun json-setserver (client args)
  (with-json-args (serverid) args
    (setserver client serverid)
    (setf (user-preference client "serverid") serverid)
    nil))

(defun json-current-server (client args)
  (declare (ignore args))
  (serverid client))

(defun json-privkey-cached? (client args)
  (with-json-args (serverid) args
    (privkey-cached-p client serverid)))

(defun json-cache-privkey (client args)
  (with-json-args (session serverid uncache) args
    (unless (or (null serverid) (equal serverid (serverid client)))
      (setserver client serverid))
    (cache-privkey client session uncache)))
  
(defun json-getcontact (client args)
  (with-json-args (id) args
    (unless id
      (json-error "Missing ID arg"))
    (let ((contact (getcontact client id)))
      (unless contact
        (json-error "There is no contact with id: ~a" id))
      (%json-contact-alist contact))))

(defun %json-optional (name value)
  (unless (blankp value)
    `((,name . ,value))))

(defun %json-contact-alist (contact)
  `(("@type" . "contact")
    ("id" . ,(contact-id contact))
    ("name" . ,(contact-name contact))
    ,@(%json-optional "nickname" (contact-nickname contact))
    ,@(%json-optional "note" (contact-note contact))))

(defun json-getcontacts (client args)
  (declare (ignore args))
  (loop for contact in (getcontacts client)
     collect (%json-contact-alist contact)))

(defun json-addcontact (client args)
  (with-json-args (id nickname note) args
    (addcontact client id nickname note)
    nil))

(defun json-deletecontact (client args)
  (with-json-args (id) args
    (deletecontact client id)))

(defun json-sync-contacts (client args)
  (declare (ignore args))
  (sync-contacts client)
  nil)

(defun json-getasset (client args)
  (with-json-args (assetid forceserver) args
    (let ((asset (getasset client assetid forceserver)))
      (unless asset
        (json-error "There is no asset with id: ~a" assetid))
      (%json-asset-alist asset))))

(defun %json-asset-alist (asset)
  `(("@type" . "asset")
    ("id" . ,(asset-id asset))
    ("assetid" . ,(asset-assetid asset))
    ("scale" . ,(parse-integer (asset-scale asset)))
    ("precision" . ,(parse-integer (asset-precision asset)))
    ("name" . ,(asset-name asset))
    ,@(%json-optional "issuer" (asset-issuer asset))
    ,@(%json-optional "percent" (asset-percent asset))))

(defun json-getassets (client args)
  (declare (ignore args))
  (loop for asset in (getassets client)
     collect (%json-asset-alist asset)))

(defun json-addasset (client args)
  (with-json-args (scale precision assetname percent) args
    (unless (and (typep scale '(integer 0))
                 (typep precision '(integer 0)))
      (json-error "scale and precision must be positive integers"))
    (unless (and (stringp assetname)
                 (not (blankp assetname)))
      (error "assetname must be a non-blank string"))
    (unless (or (null percent) (stringp percent))
      (error "percent must be null or a string"))
    (asset-assetid (addasset client
                             (as-string scale)
                             (as-string precision)
                             assetname percent))))

(defun json-getfees (client args)
  (with-json-args (reload) args
    (multiple-value-bind (tranfee regfee other-fees)
        (getfees client reload)
      (loop for fee in (list* tranfee regfee other-fees)
         collect (%json-fee-alist fee)))))

(defun %json-fee-alist (fee)
  `(("@type" . "fee")
    ("type" . ,(fee-type fee))
    ("assetid" . ,(fee-assetid fee))
    ("assetname" . ,(fee-assetname fee))
    ("amount" . ,(fee-amount fee))
    ("formattedamount" . ,(fee-formatted-amount fee))))

(defun json-getbalance (client args)
  (with-json-args (assetid acct) args
    (unless acct (setf acct $MAIN))
    (let ((bal (getbalance client acct assetid)))
      (%json-balance-alist bal))))

(defun %json-balance-alist (bal)
  `(("@type" . "balance")
    ("acct" . ,(balance-acct bal))
    ("assetid" . ,(balance-assetid bal))
    ("assetname" . ,(balance-assetname bal))
    ("amount" . ,(balance-amount bal))
    ("time" . ,(balance-time bal))
    ("formattedamount" . ,(balance-formatted-amount bal))))

(defun json-getbalances (client args)
  (with-json-args (assetid acct) args
    (let ((bals (getbalance client (or acct t) assetid)))
      (unless (listp bals)
        (setf bals `((,acct ,bals))))
      (loop for acct.bals in bals
         nconc (loop for bal in (cdr acct.bals)
                  collect (%json-balance-alist bal))))))

(defun json-getfraction (client args)
  (with-json-args (assetid) args
    (unless (stringp assetid)
      (json-error "assetid must be a string"))
    (let ((fraction (getfraction client assetid)))
      (%json-fraction-alist fraction))))

(defun %json-fraction-alist (fraction)
  `(("@type" . "fraction")
    ("assetid" . ,(fraction-assetid fraction))
    ("assetname" . ,(fraction-assetname fraction))
    ("amount" . ,(fraction-amount fraction))
    ("sclae" . ,(fraction-scale fraction))))

(defun json-getfractions (client args)
  (declare (ignore args))
  (loop for fraction in (getfraction client)
     collect (%json-fraction-alist fraction)))

(defun json-getstoragefee (client args)
  (with-json-args (assetid) args
    (unless (stringp assetid)
      (json-error "assetid must be a string"))
    (let ((fee (getstoragefee client assetid)))
      (%json-storagefee-alist fee))))

(defun %json-storagefee-alist (fee)
  `(("@type" . "storagefee")
    ("assetid" . ,(balance-assetid fee))
    ("assetname" . ,(balance-assetname fee))
    ("amount" . ,(balance-amount fee))
    ("time" . ,(balance-time fee))
    ("formattedamount". ,(balance-formatted-amount fee))
    ("fraction" . ,(balance+fraction-fraction fee))))

(defun json-getstoragefees (client args)
  (declare (ignore args))
  (loop for fee in (getstoragefee client)
     collect (%json-storagefee-alist fee)))

(defun json-spend (client args)
  (with-json-args (toid assetid formattedamount acct note) args
    (let* ((plist (spend client toid assetid formattedamount acct note)))
      `(("@type" . "spendresult")
        ,@(%json-optional "transaction-fee" (getf plist :transaction-fee))
        ,@(%json-optional "storage-fee" (getf plist :storage-fee))
        ,@(%json-optional "coupon" (getcoupon client))))))

(defun json-spend-reject (client args)
  (with-json-args (time note) args
    (spendreject client time note)
    nil))

(defun json-is-history-enabled? (client args)
  (declare (ignore args))
  (keep-history-p client))

(defun json-set-history-enabled (client args)
  (with-json-args (enabled) args
    (setf (keep-history-p client) enabled)))

(defun json-get-history-times (client args)
  (declare (ignore args))
  (gethistorytimes client))

(defun json-get-history-items (client args)
  (with-json-args (time) args
    (let* ((items (or (gethistoryitems client time)
                      (return-from json-get-history-items nil)))
           (item (car items))
           (request (and item (getarg $REQUEST item)))
           (res nil))
      (cond ((equal request $SPEND)
             (let ((fromid (id client))
                   (toid (getarg $ID item))
                   (amount (getarg $AMOUNT item))
                   (formattedamount (getarg $FORMATTEDAMOUNT item))
                   (assetid (getarg $ASSET item))
                   (assetname (getarg $ASSETNAME item))
                   (note (maybe-decrypt-note client (getarg $NOTE item))))
               (push `(("@type" . "history")
                       ("time" . ,time)
                       ("request" . ,request)
                       ("fromid" . ,fromid)
                       ("toid" . ,toid)
                       ("amount" . ,amount)
                       ("formattedamount" . ,formattedamount)
                       ("assetid" . ,assetid)
                       ("assetname" . ,assetname)
                       ,@(%json-optional "note" note))
                     res)))
            ((equal request $PROCESSINBOX)
             (loop
                with id = (id client)
                while items
                with req
                with cancelp
                with toid
                with coupon-redeemer-p
                with fromid
                with amount
                with formattedamount
                with assetid
                with assetname
                with note
                with response  
                do
                  (loop
                     while items
                     for item = (pop items) 
                     for request = (getarg $REQUEST item)
                     for last-toid = toid
                     do
                       (cond ((or (equal request $SPENDACCEPT)
                                  (equal request $SPENDREJECT))
                              (when req
                                (push item items)
                                (return))
                              (setf req (if (equal request $SPENDACCEPT)
                                            "accept" "reject"))
                              (setf
                               cancelp (equal (getarg $CUSTOMER item) id)
                               response (maybe-decrypt-note
                                         client (getarg $NOTE item))
                               toid (getarg $CUSTOMER item)))
                             ((equal request $SPEND)
                              (setf
                               fromid (getarg $CUSTOMER item)
                               toid (getarg $ID item))
                              (when (and (not (blankp last-toid))
                                         (equal toid $COUPON))
                                (setf coupon-redeemer-p t))
                              (setf amount (getarg $AMOUNT item)
                                    formattedamount (getarg $FORMATTEDAMOUNT item)
                                    assetid (getarg $ASSET item)
                                    assetname (getarg $ASSETNAME item)
                                    note (maybe-decrypt-note
                                          client (getarg $NOTE item)))
                              (when (equal (getarg $ATREQUEST item)
                                           $ATSPEND)
                                (setf req (stringify
                                           req (if cancelp "=~a" "@~a")))))))
                  (when (and req (not (blankp amount)))
                    (push `(("@type" . "history")
                            ("time" . ,time)
                            ("request" . ,req)
                            ("fromid" . ,fromid)
                            ("toid" . ,toid)
                            ("is-coupon?" . ,coupon-redeemer-p)
                            ("amount" . ,amount)
                            ("formattedamount" . ,formattedamount)
                            ("assetid" . ,assetid)
                            ("assetname" . ,assetname)
                            ,@(%json-optional "note" note)
                            ,@(%json-optional "response" response))
                          res))
                  (setf req nil
                        fromid nil
                        toid nil
                        coupon-redeemer-p nil
                        amount nil
                        formattedamount nil
                        assetid nil
                        assetname nil
                        note nil
                        response nil))))
      (nreverse res))))

(defun json-remove-history-items (client args)
  (with-json-args (time) args
    (removehistoryitem client time)
    nil))

(defun json-getinbox (client args)
  (declare (ignore args))
  (let ((items (getinbox client)))
    (loop for item in items
       collect `(("@type" . "inbox")
                 ("request" . ,(inbox-request item))
                 ("id" . ,(inbox-id item))
                 ("time" . ,(inbox-time item))
                 ("msgtime" . ,(inbox-msgtime item))
                 ("assetid" . ,(inbox-assetid item))
                 ("assetname" . ,(inbox-assetname item))
                 ("amount" . ,(inbox-amount item))
                 ("formattedamount" . ,(inbox-formattedamount item))
                 ,@(%json-optional "note" (inbox-note item))
                 ,@(%json-optional "reply" (inbox-reply item))
                 ,@(%json-optional
                    "items"
                    (loop for fee in (inbox-items item)
                       when (typep fee 'inbox)
                       collect
                         `((:@type . "fee")
                           (:time . ,(inbox-time fee))
                           (:request . ,(inbox-request fee))
                           (:assetid . ,(inbox-assetid fee))
                           (:assetname . ,(inbox-assetname fee))
                           (:amount . ,(inbox-amount fee))
                           (:formattedamount ,(inbox-formattedamount fee)))))))))

(defun json-get (key alist)
  (cdr (assoc key alist :test #'equal)))

(defun nil-if-blank (x)
  (if (blankp x) nil x))

(defun json-processinbox (client args)
  (with-json-args (directions) args
    (setf directions
          (loop for dir in directions
             collect (make-process-inbox
                      :time (json-get :time dir)
                      :request (json-get :request dir)
                      :note (nil-if-blank (json-get :note dir))
                      :acct (nil-if-blank (json-get :acct dir)))))
    (processinbox client directions)
    nil))

(defun json-storagefees (client args)
  (declare (ignore args))
  (storagefees client)
  nil)

(defun json-getoutbox (client args)
  (declare (ignore args))
  (loop for item in (getoutbox client)
     collect `((:@type . "outbox")
               (:time . ,(outbox-time item))
               (:id . ,(outbox-id item))
               (:request . ,(outbox-request item))
               (:assetid . ,(outbox-assetid item))
               (:assetname . ,(outbox-assetname item))
               (:amount . ,(outbox-amount item))
               (:formattedamount . ,(outbox-formattedamount item))
               ,@(%json-optional :note (outbox-note item))
               ,@(%json-optional :items
                                 (loop for fee in (outbox-items item)
                                    when (typep fee 'outbox)
                                    collect
                                      `((:@type . "fee")
                                        (:time . ,(outbox-time fee))
                                        (:request . ,(outbox-request fee))
                                        (:assetid . ,(outbox-assetid fee))
                                        (:assetname . ,(outbox-assetname fee))
                                        (:amount . ,(outbox-amount fee))
                                        (:formattedamount ,(outbox-formattedamount fee)))))
               ,@(%json-optional :coupons
                                 (loop for coupon in (outbox-coupons item)
                                    collect coupon)))))

(defun json-redeem (client args)
  (with-json-args (coupon) args
    ;; Allow a coupon number or a coupon
    (unless (coupon-number-p coupon)
      (setf coupon
            (handler-case (nth-value 1 (parse-coupon coupon))
              (error ()
                (json-error "Malformed-coupon: ~a" coupon)))))
    (redeem client coupon)
    nil))

(defun json-getversion (client args)
  (declare (ignore args))
  (multiple-value-bind (version time) (getversion client t)
    `((:@type . "version")
      (:version . ,version)
      (:time . ,time))))

(defun json-get-permissions (client args)
  (with-json-args (permission reload) args
    (loop for perm in (get-permissions client permission reload)
       collect (%json-permission-alist perm))))

(defun %json-permission-alist (perm)
  `((:@type . "permission")
    (:id . ,(permission-id perm))
    (:toid . ,(permission-toid perm))
    (:permission . ,(permission-permission perm))
    (:grant . ,(permission-grant-p perm))
    ,@(%json-optional :time (permission-time perm))))

(defun json-get-granted-permissions (client args)
  (declare (ignore args))
  (loop for perm in (get-granted-permissions client)
     collect (%json-permission-alist perm)))

(defun json-grant (client args)
  (with-json-args (toid permission grant) args
    (grant client toid permission grant)
    nil))

(defun json-deny (client args)
  (with-json-args (toid permission) args
    (deny client toid permission)
    nil))

(defun json-audit (client args)
  (with-json-args (assetid) args
    (multiple-value-bind (formattedamount fraction amount)
        (audit client assetid)
      `((:@type . "audit")
        (:amount . ,amount)
        (:formattedamount . ,formattedamount)
        (:fraction . ,fraction)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2012 Bill St. Clair
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
