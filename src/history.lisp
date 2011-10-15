; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; History display for the client web server
;;;

(in-package :truledger-client-web)

(defun historycount (client)
  ;; The number of history entries per page
  (or (ignore-errors (parse-integer (user-preference client "history/count")))
      30))

(defun (setf historycount) (value client)
  (setf (user-preference client "history/count")
        (stringify value))
  value)

(defun draw-history (cw &optional (start 1) count)
  (handler-case
      (let ((str (draw-history-internal cw start count)))
        (settitle cw "History")
        (setmenu cw "balance")
        (princ str (cw-html-output cw)))        
    (error (c)
      (setf (cw-error cw) (stringify c))))
  (when (cw-error cw)
    (draw-balance cw)))

(defun maybe-decrypt-note (client note)
  (unless (blankp note)
    (ignore-errors
      (setf note (decrypt-note
                  (id client) (privkey client) note)))
    note))

(defun draw-history-internal (cw start count)
  (let* ((client (cw-client cw))
         (times (gethistorytimes client)))
    (unless count (setq count (historycount client)))
    (unless times (error "No saved history"))

    (when (stringp start) (setq start (parse-integer start)))
    (when (stringp count)
      (setq count
            (if (string-equal count "all")
                most-positive-fixnum
                (parse-integer count))))

    ;; Need controls for pagination, and date search.
    ;; Eventually, recipient, note, and amount search, too. 
    (let* ((cnt (length times))
           (count2 (min cnt count))
           (strt (progn (when (>= count2 cnt) (setq start 1))
                        (1- start)))
           (idx 0)
           (nickcnt 0)
           (postcnt-plist (postcnt-plist cw))
           (scroller-html (scroller-text start count cnt postcnt-plist))
           (time-items nil)
           (req nil))
      (dolist (time (nthcdr strt times))
        (let* ((err nil)
               (items (handler-case (gethistoryitems client time)
                        (error (c)
                          (setq err (stringify c))
                          nil)))
               (timestr (hsc time))
               (datestr (datestr time))
               (item (car items))
               (request (and item (getarg $REQUEST item))))
          ;; There are currently three types of history items:
          ;; 1) Spend
          ;; 2) Process Inbox
          ;;   a) Accept or reject of somebody else's spend
          ;;   b) Acknowledgement of somebody else's accept or reject of my spend
          (cond ((equal request $SPEND)
                 (setq req "spend")
                 (let* ((from "You")
                        (toid (getarg $ID item))
                        (amount (getarg $FORMATTEDAMOUNT item))
                        (assetid (getarg $ASSET item))
                        (assetname (getarg $ASSETNAME item))
                        (note (maybe-decrypt-note client (getarg $NOTE item)))
                        (to (namestr-html
                             cw toid
                             (lambda () (1- (incf nickcnt)))
                             "nickid" "nick")))
                   (push (list :timestr (hsc timestr)
                               :datestr (hsc datestr)
                               :req (hsc req)
                               :from (hsc from)
                               :to to
                               :amount (hsc amount)
                               :assetname (highlight-assetname
                                           assetid (hsc assetname))
                               :note (hsc note)
                               :idx idx)
                         time-items)))
                ((equal request $PROCESSINBOX)
                 (pop items)
                 (setq req nil)
                 (loop
                    with rows = nil
                    with id = (id client)
                    while items
                    with cancelp
                    with toid
                    with to
                    with coupon-redeemer-p
                    with fromid
                    with from
                    with amount
                    with assetid
                    with assetname
                    with note
                    with response  
                    do
                      (loop
                         while items
                         for item = (pop items) 
                         for request = (getarg $REQUEST item)
                         do
                           (cond ((or (equal request $SPENDACCEPT)
                                      (equal request $SPENDREJECT))
                                  (when req
                                    (push item items)
                                    (return))
                                  (setq req (if (equal request $SPENDACCEPT)
                                                "accept" "reject"))
                                  (setf
                                   cancelp (equal (getarg $CUSTOMER item) id)
                                   response (maybe-decrypt-note
                                             client (getarg $NOTE item))
                                   toid (getarg $CUSTOMER item)
                                   to (namestr-html
                                       cw toid
                                       (lambda () (1- (incf nickcnt)))
                                       "nickid" "nick" "You")))
                                 ((equal request $SPEND)
                                  (setq
                                   fromid (getarg $CUSTOMER item)
                                   from (namestr-html
                                         cw fromid
                                         (lambda () (1- (incf nickcnt)))
                                         "nickid" "nick" "You")
                                   toid (getarg $ID item))
                                  (cond ((and (not (blankp to))
                                              (equal toid $COUPON))
                                         (setq coupon-redeemer-p t))
                                        (t
                                         (setq to (namestr-html
                                                   cw toid
                                                   (lambda () (1- (incf nickcnt)))
                                                   "nickid" "nick" "You"))))
                                  (setq amount (getarg $FORMATTEDAMOUNT item)
                                        assetid (getarg $ASSET item)
                                        assetname (getarg $ASSETNAME item)
                                        note (maybe-decrypt-note
                                              client (getarg $NOTE item)))
                                  (when (equal (getarg $ATREQUEST item)
                                               $ATSPEND)
                                    (setq req (stringify
                                               req (if cancelp "=~a" "@~a")))))))
                      (when (and req (not (blankp amount)))
                          (push (list req from to coupon-redeemer-p
                                      amount assetid assetname note response)
                                rows))
                      (setq req nil
                            from nil
                            to nil
                            coupon-redeemer-p nil
                            amount nil
                            assetid nil
                            assetname nil
                            note nil
                            response nil)
                    finally
                      (when rows
                        (setq rows (nreverse rows))
                        (let ((rowcnt (length rows))
                              (first t))
                          (dolist (row rows)
                            (destructuring-bind
                                  (req from to coupon-redeemer-p
                                       amount assetid assetname note response)
                                row
                              (push (list :not-first-p (not first)
                                          :rowspan (and first rowcnt)
                                          :timestr (hsc timestr)
                                          :datestr (hsc datestr)
                                          :req (hsc req)
                                          :from from
                                          :to to
                                          :coupon-redeemer-p coupon-redeemer-p
                                          :amount (hsc amount)
                                          :assetname (highlight-assetname
                                                      assetid (hsc assetname))
                                          :note (hsc note)
                                          :response (hsc response))
                                    time-items)
                              (setf first nil)))))))
                (t (push (list :exception-p t
                               :timestr (hsc timestr)
                               :datestr (hsc datestr)
                               :req (hsc request)
                               :err (hsc err))
                         time-items))))
        (incf idx)
        (when (>= idx count) (return)))

      (expand-template
       (list* :start start
              :count count
              :cnt cnt
              :nickcnt nickcnt
              :add-nicknames-p (> nickcnt 0)
              :scroller-html scroller-html
              :time-items (nreverse time-items)
              :hide-instructions-p (hideinstructions cw)
              postcnt-plist)
       "history.tmpl"))))

(defun scroller-text (start count cnt postcnt-plist)
  (let* ((left-disabled-p (<= start 1))
         (right-disabled-p (> (+ start count) cnt)))
    (when (eql count most-positive-fixnum)
      (setq count "ALL" start 1))
    (expand-template
     (list* :cmd "dohistory"
            :cnt cnt
            :left-disabled-p left-disabled-p
            :right-disabled-p right-disabled-p
            :start start
            :count count
            postcnt-plist)
     "scroller.tmpl")))

(defun do-history (cw)
  (handler-case (do-history-internal cw)
    (error (c)
      (setf (cw-error cw) (stringify c))
      (draw-history cw))))

(defun do-history-internal (cw)
  (let ((client (cw-client cw)))
    (bind-parameters (delete deleteolder chkcnt nickcnt
                      top pageup pagedown bottom
                      start count cnt)
      (when nickcnt (setq nickcnt (parse-integer nickcnt)))
      (when chkcnt (setq chkcnt (parse-integer chkcnt)))
      (cond ((or delete deleteolder)
             (dotimes (i nickcnt)
               (let ((nick (parm (stringify i "nick~d"))))
                 (unless (blankp nick)
                   (let ((id (parm (stringify i "nickid~d"))))
                     (addcontact client id nick)))))

             (dotimes (i chkcnt)
               (let ((chk (parm (stringify i "chk~d"))))
                 (when chk
                   (let ((deltime (parm (stringify i "time~d"))))
                     (cond (delete
                            (removehistoryitem client deltime))
                           (deleteolder
                            (let ((times (gethistorytimes client)))
                              (dolist (time times)
                                (when (>= (bccomp deltime time) 0)
                                  (removehistoryitem client time))))
                            (return))))))))
            (t
             (setq start (parse-integer start)
                   count (parse-integer count)
                   cnt (parse-integer cnt))
             (cond (top (setq start 1))
                   (pageup (decf start count))
                   (pagedown (incf start count))
                   (bottom (setq start (1+ (- cnt count)))))
             (cond ((< start 1) (setq start 1))
                   ((> start cnt)
                    (setq start (1+ (- cnt (mod cnt count))))))
             (when (> start cnt) (setq start (1+ (- cnt count))))
             (when (< start 1) (setq start 1))
             (setf (historycount client) (stringify count))))
      (draw-history cw start count))))

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
