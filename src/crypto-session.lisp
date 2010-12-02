; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encrypted sessions
;;;

(in-package :truledger)

(defvar *crypto-session-lock*
  (make-lock "*crypto-session-lock*"))

(defvar *crypto-session-locked-p* nil)

(defmacro with-crypto-session-lock ((&optional whostate) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-crypto-session-lock ,whostate #',thunk))))

(defun call-with-crypto-session-lock (whostate thunk)
  (if *crypto-session-locked-p*
      (funcall thunk)
      (with-lock-grabbed (*crypto-session-lock* whostate)
        whostate                        ;eliminate warning
        (let ((*crypto-session-locked-p* t))
          (funcall thunk)))))

(defvar *crypto-session-hash*
  (make-equal-hash)
  "Map a sessionid to a crypto-session")

(defvar *crypto-session-userid-hash*
  (make-equal-hash)
  "Map a userid to a list of crypto-sessions")

(defvar *crypto-session-count* 0)

(defstruct crypto-session
  id
  userid
  creation-time
  last-use-time
  password
  password-string
  aes-key)

(defun new-crypto-session-id ()
  (with-crypto-session-lock ("new-crypto-session-id")
    (cl-base64:string-to-base64-string
     (sha1 (format nil "~d-~d-~d"
                   (incf *crypto-session-count*)
                   (get-universal-time)
                   (get-internal-run-time))
           :string))))

(defun new-crypto-session-password ()
  (let ((pass (random-array 16)))
    (values pass (cl-base64:usb8-array-to-base64-string pass))))

(defvar *max-user-crypto-sessions* 5)

(defun get-user-crypto-sessions (userid)
  (gethash userid *crypto-session-userid-hash*))

(defun (setf get-user-crypto-sessions) (value userid)
  (setf (gethash userid *crypto-session-userid-hash*) value))

(defun trim-user-crypto-sessions (userid &optional
                                  (max-crypto-sessions
                                   *max-user-crypto-sessions*))
  (let* ((crypto-sessions (get-user-crypto-sessions userid))
         (cnt (length crypto-sessions)))
    (let ((delcnt (- cnt max-crypto-sessions)))
      (when (> delcnt 0)
        (setf crypto-sessions (sort (copy-list crypto-sessions) #'<
                             :key #'crypto-session-last-use-time))
        (mapc #'remove-crypto-session (subseq crypto-sessions 0 delcnt))))))

(defun new-crypto-session (userid)
  (multiple-value-bind (password password-string)
      (new-crypto-session-password)
    (with-crypto-session-lock ("new-crypto-session")
      (trim-user-crypto-sessions userid (1- *max-user-crypto-sessions*))
      (let* ((id (new-crypto-session-id))
             (time (get-universal-time))
             (crypto-session (make-crypto-session :id id
                                    :userid userid
                                    :creation-time time
                                    :last-use-time time
                                    :password password
                                    :password-string password-string
                                    :aes-key (cl-crypto:passphrase-to-aes-key
                                              password))))
        (push crypto-session (gethash userid *crypto-session-userid-hash*))
        (setf (gethash id *crypto-session-hash*) crypto-session)))))

(defun get-crypto-session (id)
  (with-crypto-session-lock ("get-crypto-session")
    (gethash id *crypto-session-hash*)))

(defun erase-crypto-session (crypto-session)
  (let ((password (crypto-session-password crypto-session))
        (string (crypto-session-password-string crypto-session)))
    (setf (crypto-session-password crypto-session) nil
          (crypto-session-password-string crypto-session) nil
          (crypto-session-aes-key crypto-session) nil)
    ;; Should erase the aes-key, too
    (when string
      (destroy-password string))
    (when password
      (destroy-password
       password
       (length password)
       (lambda (char buf i)
         (setf (aref buf i) (char-code char)))))))
  
(defun remove-crypto-session (id-or-crypto-session &optional userid)
  (with-crypto-session-lock ("remove-crypto-session")
    (let ((crypto-session (if (typep id-or-crypto-session 'crypto-session)
                       id-or-crypto-session
                       (gethash id-or-crypto-session *crypto-session-hash*)))
          (res nil))
      (when crypto-session
        (unless (or (null userid)
                    (equal userid (crypto-session-userid crypto-session)))
          (error "Can't remove other user's crypto-session"))
        (setf res (remhash (crypto-session-id crypto-session)
                           *crypto-session-hash*)
              (get-user-crypto-sessions userid)
              (delq crypto-session
                    (get-user-crypto-sessions
                     (crypto-session-userid crypto-session))))
        (erase-crypto-session crypto-session)
        res))))

(defun crypto-session-count ()
  (with-crypto-session-lock ("crypto-session-count")
    (hash-table-count *crypto-session-hash*)))

(defun crypto-session-timed-out-p (crypto-session &key
                                   (total-age 3600) (inactive-age 300))
  (let ((time (get-universal-time)))
    (or (> (- time (crypto-session-creation-time crypto-session))
           total-age)
        (> (- time (crypto-session-last-use-time crypto-session))
           inactive-age))))

(defun purge-crypto-sessions (&key (total-age 3600) (inactive-age 300))
  (let ((cnt 0))
    (with-crypto-session-lock ("purge-crypto-sessions")
      (maphash
       (lambda (id crypto-session)
         (when (crypto-session-timed-out-p crypto-session
                                           :total-age total-age
                                           :inactive-age inactive-age)
           (remove-crypto-session id)
           (incf cnt)))
       *crypto-session-hash*))
    cnt))

(defun check-crypto-session-timeout (crypto-session
                                     &key (total-age 3600) (inactive-age 300))
  (when (crypto-session-timed-out-p crypto-session
                                    :total-age total-age
                                    :inactive-age inactive-age)
    (remove-crypto-session crypto-session)
    (error "Crypto session timed out: ~s"
           (crypto-session-id crypto-session))))

(defun make-square-bracket-string (&rest strings)
  (declare (dynamic-extent strings))
  (check-type strings list)
  (with-output-to-string (s)
    (write-char #\[ s)
    (let ((first-p t))
      (dolist (string strings)
        (check-type string string)
        (if first-p
            (setf first-p nil)
            (write-char #\, s))
        (dotimes (i (length string))
          (let ((char (aref string i)))
            (when (member char '(#\] #\, #\\))
              (write-char #\\ s))
            (write-char char s)))))
    (write-char #\] s)))

(defun square-bracket-string-p (string)
  (when (stringp string)
    (let ((len (length string)))
      (dotimes (i len)
        (let ((char (aref string i)))
          (unless (member char *whitespace*)
            (unless (eql char #\[)
              (return-from square-bracket-string-p nil))
            (return))))
      (loop for i from (1- len) downto 1
         for char = (aref string i)
         do
           (unless (member char *whitespace*)
             (return (eql char #\])))))))

(defun parse-square-bracket-string (string)
  (check-type string string)
  (let ((res nil)
        (len (length string))
        (stream (make-string-output-stream))
        (esc-p nil)
        (i 0))
    ;; Skip whitespace and opening "["
    (loop
       (when (>= i len) (error "No opening square bracket"))
       (let ((char (aref string i)))
         (incf i)
         (unless (member char *whitespace*)
           (unless (eql char #\[)
             (error "First non-whitespace char not opening square bracket"))
           (return))))
    ;; Do the parsing
    (loop
       (when (>= i len) (error "No closing square bracket"))
       (let ((char (aref string i)))
         (incf i)
         (cond (esc-p
                (write-char char stream)
                (setf esc-p nil))
               ((eql char #\\)
                (setf esc-p t))
               ((eql char #\,)
                (push (get-output-stream-string stream) res))
               ((eql char #\])
                (push (get-output-stream-string stream) res)
                (return))
               (t (write-char char stream)))))
    ;; Ensure nothing but whitespace at end
    (loop
       (when (>= i len) (return))
       (unless (member (aref string i) *whitespace*)
         (error "Non-whitespace character after closing square bracket"))
       (incf i))
    (nreverse res)))

(defun encrypt-for-crypto-session (crypto-session string)
  "Encrypt string to the crypto-session with the given id
   Return: [<id>,<iv>,<encrypted string>]"
  (when (stringp crypto-session)
    (setf crypto-session
          (or (get-crypto-session crypto-session)
              (get-client-crypto-session crypto-session)
              (error "No crypto-session with id: ~s" crypto-session))))
  (check-type crypto-session crypto-session)
  (let* ((iv (urandom-array 16))
         (iv-str (cl-base64:usb8-array-to-base64-string iv))
         (ciphertext (cl-crypto:aes-encrypt-string
                      string
                      (crypto-session-aes-key crypto-session)
                      :iv iv)))
    (setf (crypto-session-last-use-time crypto-session) (get-universal-time))
    (values
     (make-square-bracket-string
      (crypto-session-id crypto-session)
      iv-str
      ciphertext)
     crypto-session)))

(defun decrypt-for-crypto-session (message)
  (check-type message string)
  (setf message (trim message))
  (assert (square-bracket-string-p message)
          nil
          "Message must be of the form: [<session>,<iv>,<ciphertext>]")
  (destructuring-bind (sessionid iv ciphertext)
      (parse-square-bracket-string message)
    (when (equal iv "error")
      (error ciphertext))
    (let* ((crypto-session (or (get-crypto-session sessionid)
                        (get-client-crypto-session sessionid)
                        (error "No crypto-session with id: ~s" sessionid))))
      (setf (crypto-session-last-use-time crypto-session) (get-universal-time))
      (values (cl-crypto:aes-decrypt-to-string
               ciphertext
               (crypto-session-aes-key crypto-session)
               :iv iv)
              crypto-session))))

(defvar *client-userid-crypto-session-hash*
  (make-equal-hash))

(defvar *client-crypto-session-hash*
  (make-equal-hash))

(defun get-client-userid-crypto-session (userid)
  (gethash userid *client-userid-crypto-session-hash*))

(defun (setf get-client-userid-crypto-session) (value userid)
  (setf (gethash userid *client-userid-crypto-session-hash*) value))

(defun get-client-crypto-session (id)
  (gethash id *client-crypto-session-hash*))

(defun (setf get-client-crypto-session) (value id)
  (setf (gethash id *client-crypto-session-hash*) value))

(defun new-client-crypto-session (id userid password-string)
  (with-crypto-session-lock ("new-client-crypto-session")
    (remove-client-crypto-session userid)
    (let ((crypto-session (make-crypto-session
                    :id id
                    :userid userid
                    :password-string password-string
                    :aes-key (cl-crypto:passphrase-to-aes-key
                              (cl-base64:base64-string-to-usb8-array
                               password-string)))))
      (setf (get-client-crypto-session id) crypto-session
            (get-client-userid-crypto-session userid) crypto-session))))

(defun remove-client-crypto-session (userid)
  (with-crypto-session-lock ("remove-client-crypto-session")
    (let ((crypto-session (get-client-userid-crypto-session userid)))
      (when crypto-session
        (prog1
            (remhash userid *client-userid-crypto-session-hash*)
          (remhash (crypto-session-id crypto-session)
                   *client-crypto-session-hash*)
          (erase-crypto-session crypto-session))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2010 Bill St. Clair
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
