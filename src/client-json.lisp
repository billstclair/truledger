; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Truledger JSON webapp server
;;;
;;; See www/docs/json.txt (http://truledger.com/doc/json.txt) for spec.
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
  '("newuser"
    "getprivkey"
    "login"
    "logout"
    "current-user"
    "user-pubkey"
    "getserver"
    "getservers"
    "addserver"
    "setserver"
    "currentserver"
    "privkey-cached-p"
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
    "ishistoryenabled?"
    "sethistoryenabled"
    "gethistorytimes"
    "gethistoryitems"
    "removehistoryitems"
    "getinbox"
    "processinbox"
    "storagefees"
    "getoutbox"
    "redeem"
    "getversion"
    "getpermissions"
    "getgrantedpermissions"
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
                 (intern (format nil "JSON-~a" (string-upcase command))
                         :truledger-json)))
         *json-commands*)
        (prog1
            (setf *json-dispatch-table* hash)
          (setf *last-json-commands* *json-commands*)))))

(defun get-json-command-function (command)
  (or (gethash command (json-dispatch-table))
      (json-error "Unknown command: ~a" command)))

(defun alistp (x)
  (loop
     (when (null x) (return t))
     (unless (listp x) (return nil))
     (let ((elt (pop x)))
       (unless (and (listp elt) (atom (car elt)) (atom (cdr elt)))
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
    (funcall (get-json-command-function (first form)) client (second form))))

(defun assoc-json-value (key alist)
  (cdr (assoc key alist :test #'string-equal)))

(defmacro with-json-args (lambda-list args-alist &body body)
  (let ((args-var (gensym "ARGS")))
    `(let ((,args-var ,args-alist))
       (let ,(loop for var-spec in lambda-list
                for var = (if (listp var-spec) (first var-spec) var-spec)
                for default = (and (listp var-spec) (second var-spec))
                for val-form = `(assoc-json-value
                                 ,(string-downcase (string var)) ,args-var)
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
  (with-json-args (keysize name privkey fetch-privkey? url coupon proxy)
      args
    (ensure-string passphrase "passphrase")
    (ensure-integer keysize "keysize" t)
    (ensure-string name "name" t)
    (ensure-string privkey "privkey" t)
    (ensure-string url "url" t)
    (ensure-string coupon "coupon" t)
    (ensure-string proxy "proxy" t)
    (when (cond (keysize (or privkey fetch-privkey?))
                (privkey fetch-privkey?)
                ((not fetch-privkey?)
                 (json-error
                  "One of keysize, privkey, and fetch-privkey? must be included")))
      (json-error
       "Only one of keysize, privkey, and fetch-privkey? may be included"))
    (when (and url coupon)
      (error "Only one of url and coupon may be included"))
    (when (passphrase-exists-p client passphrase)
      (json-error "There is already a client account for passphrase"))
    (when proxy
      (setf proxy (parse-proxy proxy)))
    (when fetch-privkey?
      (unless url
        (json-error "url required to fetch private key from server"))
      (verify-server client url nil proxy)
      (when fetch-privkey?
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
      (when fetch-privkey?
        (setf (privkey-cached-p client) t))
      session)))

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
