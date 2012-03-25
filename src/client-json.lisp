; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Truledger JSON webapp server
;;;
;;; See www/docs/json.txt for spec
;;;

(in-package :truledger-json)

;; Called from do-truledger-json in server-web.lisp
;; Returns a string with the contents of the client web page.
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
    `((@type . error)
      (message . ,(apply #'format nil format-string format-args)))))

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
    "setfees"
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

(defun json-newuser (client args)
  client
  (with-json-args (passphrase size privkey coupon proxy) args
    (list passphrase size privkey coupon proxy)))

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
