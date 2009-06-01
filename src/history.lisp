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
  (handler-case (draw-history-internal cw start count)
    (error (c)
      (setf (cw-error cw) (stringify c))))
  (when (cw-error cw)
    (draw-balance cw)))

(defun draw-history-internal (cw start count)
  (let* ((client (cw-client cw))
         (s (cw-html-output cw))
         (times (gethistorytimes client)))
    (unless count (setq count (historycount client)))
    (unless times (error "No saved history"))

    (settitle cw :History:)
    (setmenu "balance")

    ;; Need controls for pagination, and date search.
    ;; Eventually, recipient, note, and amount search, too. 
    (let* ((cnt (length times))
           (count2 (min cnt count))
           (strt (progn (when (>= count2 cnt) (setq start 1))
                        (1- start)))
           (idx 0)
           (nickcnt 0)
           (req nil))
      (who (s)
        (:br)
        (scroller s start count cnt)
        (:form
         :method "post" :action "./" :autocomplete "off"
         (:input :type "hidden" :name "cmd" :value "dohistory")
         (:input :type "hidden" :name "start" :value start)
         (:input :type "hidden" :name "count" :value count)
         (:input :type "hidden" :name "chkcnt" :value cnt)
         (:table
          :border "1"
          (:caption (:b "=== History ==="))
          (:tr
           (:th "Time")
           (:th "Request")
           (:th "From")
           (:th "th")
           (:th :colspan "2" "Amount")
           (:th "Note")
           (:th "Response")
           (:th "x"))
          (:tr
           (dolist (time (nthcdr strt times))
             (let* ((items (gethistoryitems client time))
                    (timestr (hsc time))
                    (datestr
                     (whots (s)
                       (:span :title timestr (str (datestr time)))))
                    (item (car items))
                    (request (getarg $REQUEST item)))
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
                                            ;; to set spendaccept/spendredeem code
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
                               (who (s)
                                 (:td :rowspan rowcnt (str datestr))
                                 (dolist (row rows)
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
                                                      :name (stringify idx "chk~a"))))))))))))
                     (t (setq request (hsc request))
                        (who (s)
                          (:td (str datestr))
                          (:td (str request))
                          (:td :colspan "6" "Unknown request type")))))))
          (incf idx))
         (when (> nickcnt 0)
           (who (s)
             (:input :type "hidden" :name "nickcnt" :value nickcnt)))
         (who (s)
           (:br)
           (:input :type "submit" :name "delete" :value "Delete checked")
           (:input :type "submit" :name "deleteolder"
                   :value "Delete Checked & Older")))
        (scroller s start count cnt)
      (cond ((hideinstructions cw)
             (who (s)
               (:a :href "./?cmd=toggleinstructions&page=history"
                   "Show Instructions")))
            (t
             (who (s)
               (:table
                :border "1"
                (:caption (:b "=== Key ==="))
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
               (:a :href "./?cmd=toggleinstructions&page=history"
                   "Hide Instructions"))))))))


(defun scroller (s start count cnt)
)

#||
    global $body;

    if (strtolower($count) == 'all') $count = 0;
    $count2 = ($count <= 0) ? $cnt : $count;

    if ($count <= 0) $count = 'ALL';
    $count = hsc($count);
    $start = hsc($start);
    $cnt = hsc($cnt);

    $body .= <<<EOT
<form method="post" action="./" autocomplete="off">
<input type="hidden" name="cmd" value="dohistory">
<input type="hidden" name="cnt" value="$cnt"/>

EOT;
    $disabled = '';
    if ($start <= 1) $disabled = ' disabled="disabled"';
    $body .= <<<EOT
<input type="submit" name="top" value="&lt;&lt;"$disabled title="Show the first page"/>
<input type="submit" name="pageup" value="&lt;"$disabled title="Show the previous page"/>

EOT;

    $body .= <<<EOT
Start:
<input type="text" name="start" size="6" value="$start"/>
<input type="submit" name="show" value="Show:"/>
<input type="text" name="count" size="4" value="$count"/>
of $cnt entries

EOT;

    $disabled = '';
    if (($start + $count2) > $cnt) $disabled = ' disabled="disabled"';
    $body .= <<<EOT
<input type="submit" name="pagedown" value="&gt;"$disabled title="Show the next page"/>
<input type="submit" name="bottom" value="&gt;&gt;"$disabled title="Show the last page"/>

EOT;

    $body .= "</form>\n";
  }

  function do_history() {
    global $client;

    // Delete or set nickname values
    $delete = mqpost('delete');
    $deleteolder = mqpost('deleteolder');

    $chkcnt = mqpost('chkcnt');
    $nickcnt = mqpost('nickcnt');

    // Scroller values
    $top = mqpost('top');
    $pageup = mqpost('pageup');
    $show = mqpost('show');
    $pagedown = mqpost('pagedown');
    $bottom = mqpost('bottom');

    $start = mqpost('start');
    $count = mqpost('count');
    $cnt = mqpost('cnt');

    if ($delete || $deleteolder) {
      for ($i=0; $i<$nickcnt; $i++) {
        $nick = mqpost("nick$i");
        if ($nick) {
          $id = mqpost("nickid$i");
          $client->addcontact($id, $nick);
        }
      }

      for ($i=0; $i<$chkcnt; $i++) {
        $chk = mqpost("chk$i");
        if ($chk) {
          $deltime = mqpost("time$i");
          if ($delete) {
            $client->removehistoryitem($deltime);
          } elseif ($deleteolder) {
            $times = $client->gethistorytimes();
            foreach ($times as $time) {
              if (bccomp($deltime, $time) >= 0) {
                $found = true;
                $client->removehistoryitem($time);
              }
            }
            break;
          }
        }
      }
    } else {
      if ($top) $start = 1;
      elseif ($pageup) $start -= $count;
      elseif ($pagedown) $start += $count;
      elseif ($bottom) $start = $cnt - $count + 1;

      if ($start < 1) $start = 1;
      elseif ($start > $cnt) {
        $start = $cnt - ($cnt % $count) + 1;
        if ($start > $cnt) $start = $cnt - $count + 1;
        if ($start < 1) $start = 1;
      }

      $this->historycount($count);
    }

    $this->draw_history($start, $count);
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
