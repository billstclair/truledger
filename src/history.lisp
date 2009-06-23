; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; History display for the client web server
;;;

(in-package :trubanc-client-web)

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
           (req nil))
      (whots (s)
        (:br)
        (scroller s start count cnt)
        (:form
         :method "post" :action "./" :autocomplete "off"
         (:input :type "hidden" :name "cmd" :value "dohistory")
         (:input :type "hidden" :name "start" :value start)
         (:input :type "hidden" :name "count" :value count)
         (:input :type "hidden" :name "chkcnt" :value cnt)
         (:table
          :class "prettytable"
          (:caption (:b "History"))
          (:tr
           (:th "Time")
           (:th "Request")
           (:th "From")
           (:th "To")
           (:th :colspan "2" "Amount")
           (:th "Note")
           (:th "Response")
           (:th "x"))
          (dolist (time (nthcdr strt times))
            (let* ((err nil)
                   (items (handler-case (gethistoryitems client time)
                            (error (c)
                              (setq err (stringify c))
                              nil)))
                   (timestr (hsc time))
                   (datestr
                    (whots (s)
                      (:span :title timestr (str (datestr time)))))
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
                            (assetname (getarg $ASSETNAME item))
                            (note (getarg $NOTE item))
                            (to (namestr-html
                                 cw toid
                                 (lambda () (1- (incf nickcnt)))
                                 "nickid" "nick")))
                       (when (blankp note) (setq note "&nbsp;"))
                       (who (s)
                         (:tr
                          (:td (str datestr))
                          (:td (str req))
                          (:td (str from))
                          (:td (str to))
                          (:td :align "right" :style "border-right-width: 0;"
                               (str amount))
                          (:td :style "border-left-width: 0;" (str assetname))
                          (:td (str note))
                          (:td "&nbsp;")
                          (:td
                           (:input :type "hidden"
                                   :name (stringify idx "time~d")
                                   :value timestr)
                           (:input :type "checkbox"
                                   :name (stringify idx "chk~d")))))))
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
                        with fromid
                        with from
                        with amount
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
                                  (setq
                                   cancelp (equal (getarg $CUSTOMER item) id)
                                   response (getarg $NOTE item)
                                   toid (getarg $CUSTOMER item))
                                  to (namestr-html
                                      cw toid
                                      (lambda () (1- (incf nickcnt)))
                                      "nickid" "nick" "You"))
                                 ((equal request $SPEND)
                                  (setq
                                   fromid (getarg $CUSTOMER item)
                                   from (namestr-html
                                         cw fromid
                                         (lambda () (1- (incf nickcnt)))
                                         "nickid" "nick" "You")
                                   toid (getarg $ID item))
                                  (cond ((not (blankp to))
                                         ;; to set by spendaccept/spendredeem code
                                         (when (equal toid $COUPON)
                                           (setq to
                                                 (whots (s)
                                                   "Coupon redeemed by:"
                                                   (:br)
                                                   "to"))))
                                        (t
                                         (setq to (namestr-html
                                                   cw toid
                                                   (lambda () (1- (incf nickcnt)))
                                                   "nickid" "nick" "You"))))
                                  (setq amount (getarg $FORMATTEDAMOUNT item)
                                        assetname (getarg $ASSETNAME item)
                                        note (getarg $NOTE item))
                                  (when (equal (getarg $ATREQUEST item)
                                               $ATSPEND)
                                    (setq req (stringify
                                               req (if cancelp "=~a" "@~a")))))))
                        (when req
                          (push (list req from to amount assetname note response)
                                rows)
                          (setq req nil
                                from nil
                                to nil
                                amount nil
                                assetname nil
                                note nil
                                response nil))
                        finally
                        (when rows
                          (setq rows (nreverse rows))
                          (let ((rowcnt (length rows))
                                (first t))
                            (dolist (row rows)
                              (who (s)
                                (:tr
                                 (when first
                                   (who (s)
                                     (:td :rowspan rowcnt (str datestr))))
                                 (destructuring-bind
                                       (req from to amount assetname note response)
                                     row
                                   (unless note (setq note "&nbsp;"))
                                   (unless response (setq response "&nbsp;"))
                                   (who (s)
                                     (:td (str req))
                                     (:td (str from))
                                     (:td (str to))
                                     (:td :align "right"
                                          :style "border-right-width: 0;"
                                          (str amount))
                                     (:td :style "border-left-width: 0;"
                                          (str assetname))
                                     (:td (str note))
                                     (:td (str response)))
                                   (when first
                                     (setq first nil)
                                     (who (s)
                                       (:td :rowspan rowcnt
                                            (:input :type "hidden"
                                                    :name (stringify idx "time~a")
                                                    :value timestr)
                                            (:input :type "checkbox"
                                                    :name (stringify idx "chk~a")))))))))))))
                    (t (who (s)
                         (:tr
                          (:td (str datestr))
                          (cond (err
                                 (who (s)
                                   (:td :colspan "7"
                                        :style "color: red;"
                                        "Error: " (esc err))))
                                (t
                                 (who (s)
                                   (:td (esc request))
                                   (:td :colspan "6" "Unknown request type"))))
                          (:td
                           (:input :type "hidden"
                                   :name (stringify idx "time~d")
                                   :value timestr)
                           (:input :type "checkbox"
                                   :name (stringify idx "chk~d"))))))))
            (incf idx)))
         (when (> nickcnt 0)
           (who (s)
             (:input :type "hidden" :name "nickcnt" :value nickcnt)))
         (who (s)
           (:br)
           (:input :type "submit" :name "delete"
                   :value (if (> nickcnt 0)
                              (hsc "Delete checked & Add Nicknames")
                              "Delete checked"))
           (:input :type "submit" :name "deleteolder"
                   :value "Delete Checked & Older")))
        (scroller s start count cnt)
      (cond ((hideinstructions cw)
             (form (s "toggleinstructions"
                      :style "margin: 0px;") ; prevent whitespace below button
               (:input :type "hidden" :name "page" :value "history")
               (:input :type "submit" :name "toggleinstructions"
                       :value "Show Instructions")))
            (t
             (who (s)
               (:table
                :class "prettytable"
                (:tr (:th :colspan 2 "Key"))
                (:tr (:td "spend") (:td "You made a spend"))
                (:tr (:td "accept" (:td "You accepted a spend")))
                (:tr (:td "reject" (:td "You rejected a spend")))
                (:tr (:td "@accept")
                     (:td "You acknowledged acceptance of your spend"))
                (:tr (:td "@reject")
                     (:td "You acknowledged rejection of your spend"))
                (:tr (:td "=reject")
                     (:td "You acknowledged your cancel of a spend"))
                (:tr (:td "=accept")
                     (:td "You acknowledged your acceptance of a coupon you spent to yourself")))
               (:br)
               (form (s "toggleinstructions"
                        :style "margin: 0px;") ; prevent whitespace below button
                 (:input :type "hidden" :name "page" :value "history")
                 (:input :type "submit" :name "toggleinstructions"
                         :value "Hide Instructions")))))
      (who (s) (:br))))))

(defun scroller (s start count cnt)
  (let ((count2 (if (<= count 0) cnt count)))
    (when (eql count most-positive-fixnum)
      (setq count "ALL" start 1))
    (who (s)
      (:form
       :method "post" :action "./" :autocomplete "off"
       (:input :type "hidden" :name "cmd" :value "dohistory")
       (:input :type "hidden" :name "cnt" :value cnt)
       (let ((disabled (<= start 1)))
         (who (s)
           (:input :type "submit" :name "top" :value "&lt;&lt;"
                   :disabled disabled
                   :title "Show the first page")
           (:input :type "submit" :name "pageup" :value "&lt;"
                   :disabled disabled
                   :title "Show the previous page")))
       (who (s)
         "Start: "
         (:input :type "text" :name "start" :size "6" :value start)
         (:input :type "submit" :name "show" :value "Show:")
         (:input :type "text" :name "count" :size "4" :value count)
         " of " (str cnt) " entries")

       (let ((disabled (> (+ start count2) cnt)))
         (who (s)
           (:input :type "submit" :name "pagedown" :value "&gt;"
                   :disabled disabled
                   :title "Show the next page")
           (:input :type "submit" :name "bottom" :value "&gt;&gt;"
                   :disabled disabled
                   :title "Show the last page")))))))

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
