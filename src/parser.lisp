; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse "(id,[key:]value,...):signature" into a hash table,
;;; verifying signatures.
;;; Can separate multiple top-level forms with periods.
;;; Values can be (id,...):signature forms.
;;;

(in-package :trubanc)

(defconstant $PARSER-MSGKEY "%msg%")

(defclass parser ()
  ((keydb :initarg :keydb
          :type (or db null)
          :accessor parser-keydb
          :initform nil)
   (keydict :initform (make-hash-table :test 'equal)
            :accessor parser-keydict)
   (bank-getter :initarg :bank-getter
                :type (or function null)
                :initform nil
                :accessor parser-bank-getter)
   (always-verify-sigs-p :initform nil
                         :initarg :always-verify-sigs-p
                         :accessor parser-always-verify-sigs-p)
   (verify-sigs-hash :initform (make-weak-hash-table)
                     :accessor parser-verify-sigs-hash)))

(defmethod parser-verify-sigs-p (parser)
  (gethash (current-process) (parser-verify-sigs-hash parser) t))

(defmethod (setf parser-verify-sigs-p) (value parser)
  (let ((hash (parser-verify-sigs-hash parser)))
    (setf (gethash (current-process) hash) value)))

(defmacro with-verify-sigs-p ((parser &optional verify-sigs-p) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-verify-sigs-p #',thunk ,parser ,verify-sigs-p))))

(defun call-with-verify-sigs-p (thunk parser verify-sigs-p)
  (let ((old-value (parser-verify-sigs-p parser)))
    (setf (parser-verify-sigs-p parser) verify-sigs-p)
    (unwind-protect (funcall thunk)
      (setf (parser-verify-sigs-p parser) old-value))))

(defmethod parse ((parser parser) string &optional
                  (verify-sigs-p (parser-verify-sigs-p parser)))
  "Return a hash table, or signal en error, if the parse could not be done,
   or an ID couldn't be found, or a signature was bad.
   left-paren, right-paren, comma, colon, and period are special chars.
   They, and back-slash, are escaped by backslashes
   If VERIFY-SIGS-P is false, don't verify PGP signatures.
   By default, use (PARSER-VERIFY-SIGS-P PARSER)"
  (let* ((tokens (tokenize string))
         (state nil)
         (res nil)
         (dict nil)
         (dictidx nil)
         (id nil)
         (start nil)
         (key nil)
         (value nil)
         (needsig nil)
         (msg nil)
         (stack (make-array 10 :fill-pointer 0 :adjustable t)))
    (flet ((init-dict ()
             (setq dict (make-hash-table :test 'equal)
                   dictidx -1))
           (append-dict (value)
             (setf (gethash (incf dictidx) dict) value)))
      (loop
         for first = t then nil
         for (pos . tok) in tokens
         do
           (when (and first (not (eql tok #\()))
             (error "Message does not begin with left paren"))
           (cond ((eql tok #\()
                  (setq needsig t)
                  (when (and dict state (not (member state '(#\: #\,))))
                    (error "Open paren not after colon or comma at ~d" pos))
                  (when (and key (not (eql state #\:)))
                    (error "Missing key at ~d" pos))
                  (when dict
                    (vector-push-extend `(,state ,dict ,dictidx ,start ,key) stack)
                    (setq state nil
                          dict nil
                          key nil))
                  (setq start pos
                        state #\())
                 ((eql tok #\))
                  (cond ((eql state #\,)
                         (when key
                           (error "Missing key at ~s" pos))
                         (unless dict (init-dict))
                         (append-dict (or value ""))
                         (setq value nil))
                        ((eql state #\:)
                         (unless dict (init-dict))
                         (setf (gethash key dict) (or value "")
                               value nil))
                        (state
                         (error "Close paren not after value at ~d" pos)))
                  (setq msg (subseq string start (1+ pos))
                        state #\)))
                 ((eql tok #\:)
                  (cond ((eql state #\))
                         (setq state :sig))
                        ((null value)
                         (error "Missing key before colon at ~d" pos))
                        (t
                         (setq key value
                               value nil
                               state #\:))))
                 ((eql tok #\,)
                  (cond ((eql state #\:)
                         (unless key
                           (error "Missing key"))
                         (unless dict (init-dict))
                         (setf (gethash key dict) (or value "")
                               key nil))
                        (t (when (and state
                                      (not (member state '(#\, #\())))
                             (error "Misplaced comma at ~d, state: ~s" pos state))
                           (unless dict (init-dict))
                           (append-dict (or value ""))))
                  (setq value nil
                        state #\,))
                 ((eql tok #\.)
                  (when (or state (> (length stack) 0))
                    (error "Misplaced period at ~d" pos))
                  (when dict (push dict res))
                  (setq dict nil
                        key nil))
                 (t 
                  (cond ((member state '(#\( #\,))
                         (setq value tok))
                        ((eql state #\:)
                         (setq value tok))
                        ((eq state :sig)
                         (setq id (and dict (gethash 0 dict)))
                         (when (not (and (equal id "0")
                                         (member (gethash 1 dict)
                                                 '("bankid" "readdata")
                                                 :test 'equal)))
                           (unless id
                             (error "Signature without ID at ~d" pos))
                           (when (or verify-sigs-p
                                     (parser-always-verify-sigs-p parser))
                             (let ((pubkey (parser-get-pubkey parser id dict)))
                               (unless pubkey
                                 (error "No key for id: ~s at ~d" id pos))
                               (unless (verify msg tok pubkey)
                                 (error "Signature verification failed at ~d" pos)))))
                         (setf (gethash $PARSER-MSGKEY dict)
                               (subseq string start (+ pos (length tok))))
                         (cond ((> (length stack) 0)
                                (let ((pop (vector-pop stack)))
                                  (setq value dict
                                        state (pop pop)
                                        dict (pop pop)
                                        dictidx (pop pop)
                                        start (pop pop)
                                        key (pop pop)))
                                (setq needsig t))
                               (t
                                (push dict res)
                                (setq dict nil
                                      needsig nil
                                      state nil))))
                        (t (error "Misplaced value at ~d" pos))))))
      (when needsig
        (error "Premature end of message"))
      (nreverse res))))

(defmethod parser-get-pubkey ((parser parser) id dict)
  (let* ((keydict (parser-keydict parser))
         (keydb (parser-keydb parser))
         (pubkey (gethash id keydict))
         (dict-1 (gethash 1 dict)))
    (when (and (not pubkey)
               (or (equal dict-1 "register")
                   (equal dict-1 "bankid")))
      ;; May be the first time we've seen this ID.
      ;; If it's a key definition message, we're set
      (setq pubkey
            (if (equal dict-1 "register")
                (gethash 3 dict)
                (gethash 2 dict)))
      (let ((pubkeyid (public-key-id pubkey)))
        (if (not (equal id pubkeyid))
            (setq pubkey nil)
            (setf (gethash id keydict) pubkey))))
    (unless pubkey
      (setq pubkey (and keydb (db-get keydb id)))
      (when pubkey
        (unless (equal id (public-key-id pubkey))
          (error "Pubkey doesn't match id: ~s" id))
        (setf (gethash id keydict) pubkey)))
    pubkey))

(defun tokenize (string)
  (let ((res nil)
        (realstart nil)
        (start nil)
        (delims '(#\( #\: #\, #\) #\.))
        (escaped nil)
        (substr ""))
    (loop
       for i from 0
       for chr across string
       do
         (cond ((and (not escaped) (member chr delims))
                (when start
                  (push (cons realstart (strcat substr (subseq string start i)))
                        res)
                  (setq start nil
                        realstart nil
                        substr ""))
                (push (cons i chr) res))
               ((and (not escaped) (eql chr #\\))
                (setq escaped t)
                (when start
                  (setq substr (strcat substr (subseq string start i))
                        start (1+ i))))
               (t
                (unless start
                  (setq start i
                        realstart i))
                (setq escaped nil)))
         finally
         (when start
           (push (cons realstart (strcat substr (subseq string start i)))
                 res)))
    (nreverse res)))

(defun get-parsemsg (parse)
  "Return the message string that parsed into the hash table in PARSE."
  (gethash $PARSER-MSGKEY parse))

(defun unsigned-message (msg)
  "Return just the message part of a signed message, not including the signature.
   Assumes that the message will parse."
  (let ((pos (search "):" msg :from-end t)))
    (if pos
        (subseq msg 0 (1+ pos))
        msg)))

(defun first-message (msg)
  "Return the first message in a list of them.
   Assumes that message parses correctly."
  (let ((pos 0))
    (loop
       (setq pos (position #\. msg :start (1+ pos)))
       (unless pos (return msg))
       (unless (eql (aref msg (1- pos)) #\\)
         (return (subseq msg 0 pos))))))

(defun match-args (parse pattern)
  "parse is a hash table with numeric and string keys.
   pattern is a list of (key . value) pairs.
   a numeric key in pattern must match a numeric key in parse.
   a non-numeric key in pattern must correspond to a numeric key in parse
   at the element number in pattern or the same non-numeric key in parse.
   the result maps the non-numeric keys and values in $pattern and
   their positions, to the matching values in $parse."
  (loop
     with res = (make-hash-table :test 'equal)
     with name
     with optional
     for elt in pattern
     for i from 0
     for key = (if (listp elt) (car elt) i)
     for value = (if (listp elt) (cdr elt) elt)
     do
       (cond ((integerp key)
              (setq name value
                    optional nil))
             (t
              (setq name key
                    optional t)))
       (let ((val (gethash i parse)))
         (unless val (setq val (gethash name parse)))
         (when (and (not optional) (not val)) (return nil))
         (when val
           (setf (gethash name res) val
                 (gethash i res) val)))
     finally
       (maphash (lambda (key value)
                  (declare (ignore value))
                  (unless (or (eq key $PARSER-MSGKEY)
                              (gethash key res))
                    (return nil)))
                parse)
       (setf (gethash $PARSER-MSGKEY res)
             (gethash $PARSER-MSGKEY parse))
     (return res)))

(defun getarg (key args)
  (gethash key args))

(defun (setf getarg) (value key args)
  (setf (gethash key args) value))

(defun format-pattern (pattern)
  (let ((res "(")
        (comma nil))
    (loop for (key . value) across pattern
       do
         (when comma (setq res (strcat res ",")))
         (setq comma t)
       (if (numberp key)
           (setq res (format nil "~a<~a>" res value))
           (setq res (format nil "~a~a=<~a>" res key value))))
    (setq res (strcat res ")"))
    res))

(defun remove-signatures (msg)
  "Remove the signatures from a message"
  (loop
     with res = ""
     do
       (let ((tail (strstr msg #.(format nil "):~%")))
             (extralen 1)
             (matchlen 3)
             (tail2 (strstr msg #.(format nil "\\)\\:~%"))))
         (when (and tail2 (< (length tail2) (length tail)))
           (setq tail tail2
                 extralen 2
                 matchlen 5))
         (let* ((msglen (length msg))
                (i (- msglen (length tail)))
                dotpos leftpos commapos)
           (when (> msglen 0)
             (setq res (strcat res (subseq msg 0 (min msglen (+ i extralen))))))
           (setq msg (and tail (subseq tail matchlen))
                 dotpos (position #\. msg)
                 leftpos (position #\( msg)
                 commapos (position #\, msg))
           (cond ((null dotpos)
                  (setq dotpos leftpos))
                 (leftpos
                  (setq dotpos (min dotpos leftpos))))
           (cond ((null dotpos)
                  (setq dotpos commapos))
                 (commapos
                  (setq dotpos (min dotpos commapos))))
           (let ((parenpos (position #\) msg)))
             (cond ((and parenpos
                         (or (not dotpos) (< parenpos dotpos)))
                    (setq msg (subseq msg parenpos)))
                   (dotpos
                    (setq res (strcat res #.(format nil "~%")))
                    (setq msg (subseq msg dotpos)))
                   (t (loop-finish))))))
     finally
       (return (str-replace ",(" #.(format nil ",~%(") res))))

(defmethod match-pattern ((parser parser) req &optional bankid)
  (let* ((patterns (patterns))
         (request (gethash 1 req))
         (pattern (gethash request patterns)))
    (unless pattern
      (error "Unknown request: ~s" request))
    (setq pattern (nconc `(,$CUSTOMER ,$REQUEST) pattern))
    (let ((args (match-args req pattern)))
      (unless args
        (error "Request doesn't match pattern for ~s: ~s, ~s"
               request
               pattern
               (get-parsemsg req)))
      (let ((args-bankid (gethash $BANKID args)))
        (when args-bankid
          (unless bankid
            (let ((bank-getter (parser-bank-getter parser)))
              (when bank-getter
                (setq bankid (funcall bank-getter)))))
          (unless (or (blankp bankid) (equal bankid args-bankid))
            (error "bankid mismatch, sb: ~s, was: ~s"
                   bankid args-bankid))))
      (when (> (length (gethash $NOTE args)) 4096)
        (error "Note too long. Max: 4096 chars"))
      args)))

(defmethod match-message ((parser parser) (msg string))
  "Parse and match a message.
   Returns a hash table parameter names to values."
  (let ((reqs (parse parser msg)))
    (match-pattern parser (car reqs))))


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
