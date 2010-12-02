; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; tokenize the protocol strings
;;;

(in-package :truledger-tokens)

;; db file & directory names
(defconstant $TIME "time")
(defconstant $PRIVKEY "privkey")
(defconstant $SERVERID "serverid")
(defconstant $TOKENID "tokenid")
(defconstant $REGFEE "regfee")
(defconstant $TRANFEE "tranfee")
(defconstant $FEE "fee")
(defconstant $PUBKEY "pubkey")
(defconstant $PUBKEYSIG "pubkeysig")
(defconstant $ASSET "asset")
(defconstant $SHUTDOWNMSG "shutdownmsg")
(defconstant $STORAGE "storage")
(defconstant $STORAGEFEE "storagefee")
(defconstant $FRACTION "fraction")
(defconstant $ACCOUNT "account")
(defconstant $LAST "last")
(defconstant $REQ "req")
(defconstant $BALANCE "balance")
(defconstant $MAIN "main")
(defconstant $OUTBOX "outbox")
(defconstant $OUTBOXHASH "outboxhash")
(defconstant $INBOX "inbox")
(defconstant $INBOXIGNORED "inboxignored")
(defconstant $COUPON "coupon")
(defconstant $DATA "data")
(defconstant $BACKUP "backup")
(defconstant $READINDEX "readindex")
(defconstant $WRITEINDEX "writeindex")
(defconstant $WALKINDEX "walkindex")

;; request names
(defconstant $ID "id")
(defconstant $REGISTER "register")
(defconstant $FAILED "failed")
(defconstant $REASON "reason")
(defconstant $GETREQ "getreq")
(defconstant $GETTIME "gettime")
(defconstant $GETFEES "getfees")
(defconstant $SETFEES "setfees")
(defconstant $TRANSFER "transfer")
(defconstant $SPEND "spend")
(defconstant $GETINBOX "getinbox")
(defconstant $PROCESSINBOX "processinbox")
(defconstant $STORAGEFEES "storagefees")
(defconstant $SPENDACCEPT "spend|accept")
(defconstant $SPENDREJECT "spend|reject")
(defconstant $AFFIRM "affirm")
(defconstant $GETASSET "getasset")
(defconstant $GETOUTBOX "getoutbox")
(defconstant $GETBALANCE "getbalance")
(defconstant $COUPONENVELOPE "couponenvelope")
(defconstant $GETVERSION "getversion")
(defconstant $VERSION "version")
(defconstant $WRITEDATA "writedata")
(defconstant $READDATA "readdata")
(defconstant $GRANT "grant")
(defconstant $DENY "deny")
(defconstant $PERMISSION "permission")
(defconstant $AUDIT "audit")
(defconstant $OPENSESSION "opensession")
(defconstant $CLOSESESSION "closesession")

;; Affirmations
(defconstant $ATREGISTER "@register")
(defconstant $ATOUTBOXHASH "@outboxhash")
(defconstant $ATSTORAGE "@storage")
(defconstant $ATSTORAGEFEE "@storagefee")
(defconstant $ATFRACTION "@fraction")
(defconstant $ATBALANCE "@balance")
(defconstant $ATSETFEES "@setfees")
(defconstant $ATSPEND "@spend")
(defconstant $ATTRANFEE "@tranfee")
(defconstant $ATFEE "@fee")
(defconstant $ATASSET "@asset")
(defconstant $ATGETINBOX "@getinbox")
(defconstant $ATPROCESSINBOX "@processinbox")
(defconstant $ATSTORAGEFEES "@storagefees")
(defconstant $ATSPENDACCEPT "@spend|accept")
(defconstant $ATSPENDREJECT "@spend|reject")
(defconstant $ATGETOUTBOX "@getoutbox")
(defconstant $ATBALANCEHASH "@balancehash")
(defconstant $ATCOUPON "@coupon")
(defconstant $ATCOUPONENVELOPE "@couponenvelope")
(defconstant $ATWRITEDATA "@writedata")
(defconstant $ATREADDATA "@readdata")
(defconstant $ATGRANT "@grant")
(defconstant $ATDENY "@deny")
(defconstant $ATPERMISSION "@permission")
(defconstant $ATAUDIT "@audit")
(defconstant $ATOPENSESSION "@opensession")
(defconstant $ATCLOSESESSION "@closesession")
(defconstant $ATBACKUP "@backup")

;; request parameter names
(defconstant $CUSTOMER "customer")
(defconstant $REQUEST "request")
(defconstant $NAME "name")
(defconstant $NOTE "note")
(defconstant $ACCT "acct")
(defconstant $OPERATION "operation")
(defconstant $TRAN "tran")
(defconstant $AMOUNT "amount")
(defconstant $ASSETNAME "assetname")
(defconstant $SCALE "scale")
(defconstant $PRECISION "precision")
(defconstant $PERCENT "percent")
(defconstant $TIMELIST "timelist")
(defconstant $HASH "hash")
(defconstant $MSG "msg")
(defconstant $ERRMSG "errmsg")
(defconstant $BALANCEHASH "balancehash")
(defconstant $COUNT "count")
(defconstant $SERVERURL "serverurl")
(defconstant $ENCRYPTEDCOUPON "encryptedcoupon")
(defconstant $COUPONNUMBERHASH "couponnumberhash")
(defconstant $ISSUER "issuer")
(defconstant $ANONYMOUS "anonymous")
(defconstant $KEY "key")
(defconstant $SIZE "size")
(defconstant $SESSIONID "sessionid")
(defconstant $CIPHERTEXT "ciphertext")

;; Client database keys
(defconstant $SERVER "server")
(defconstant $SERVERS "servers")
(defconstant $URL "url")
(defconstant $NICKNAME "nickname")
(defconstant $CONTACT "contact")
(defconstant $SESSION "session")
(defconstant $PREFERENCE "preference")
(defconstant $TOKEN "token")
(defconstant $HISTORY "history")
(defconstant $PRIVKEYCACHEDP "privkeycachedp")
(defconstant $NEEDPRIVKEYCACHE "needprivkeycache")

;; Other client tokens
(defconstant $FORMATTEDAMOUNT "formattedamount")
(defconstant $MSGTIME "msgtime")
(defconstant $ATREQUEST "@request")
(defconstant $MINT-TOKENS "mint-tokens")
(defconstant $MINT-COUPONS "mint-coupons")
(defconstant $ADD-ASSET "add-asset")

;; Marker in hash tables
(defconstant $UNPACK-REQS-KEY "unpack-reqs")

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
