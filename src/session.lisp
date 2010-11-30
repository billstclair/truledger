; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encrypted sessions
;;;

(in-package :truledger)

(defvar *session-lock*
  (make-lock "*session-lock*"))

(defvar *session-locked-p* nil)

(defmacro with-session-lock ((&optional whostate) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-session-lock ,whostate #',thunk))))

(defun call-with-session-lock (whostate thunk)
  (if *session-locked-p*
      (funcall thunk)
      (with-lock-grabbed (*session-lock* whostate)
        whostate                        ;eliminate warning
        (let ((*session-locked-p* t))
          (funcall thunk)))))

(defvar *session-hash*
  (make-equal-hash))

(defvar *session-count* 0)

(defstruct session
  id
  time
  password
  password-string
  aes-key)

(defun new-session-id ()
  (with-session-lock ("new-session-id")
    (cl-base64:string-to-base64-string
     (sha1 (format nil "~d-~d-~d"
                   (incf *session-count*)
                   (get-universal-time)
                   (get-internal-run-time))
           :string))))

(defun new-session-password ()
  (let ((pass (random-array 16)))
    (values pass (cl-base64:usb8-array-to-base64-string pass))))

(defun new-session ()
  (multiple-value-bind (password password-string)
      (new-session-password)
    (with-session-lock ("new-session")
      (let* ((id (new-session-id))
             (session (make-session :id id
                                    :time (get-universal-time)
                                    :password password
                                    :password-string password-string
                                    :aes-key (cl-crypto:passphrase-to-aes-key
                                              password))))
        (setf (gethash id *session-hash*) session)))))

(defun get-session (id)
  (with-session-lock ("get-session")
    (gethash id *session-hash*)))

(defun remove-session (id)
  (with-session-lock ("remove-session")
    (let ((session (gethash id *session-hash*)))
      (when session
        (remhash id *session-hash*)
        (let ((password (session-password session))
              (string (session-password-string session)))
          (setf (session-password session) nil
                (session-password-string session) nil)
          (destroy-password string)
          (destroy-password
           password (length password)
           (lambda (char buf i)
             (setf (aref buf i) (char-code char)))))
        t))))                                                     

(defun session-count ()
  (with-session-lock ("session-count")
    (hash-table-count *session-hash*)))

(defun purge-sessions (&optional (age-in-seconds 300))
  (let ((time (get-universal-time))
        (cnt 0))
    (with-session-lock ("purge-sessions")
      (maphash
       (lambda (id session)
         (when (> (- time (session-time session)) age-in-seconds)
           (remove-session id)
           (incf cnt)))
       *session-hash*))
    cnt))

(defun encrypt-for-session (session string)
  "Encrypt string to the session with the given id
   Return: [<id>,<iv>,<encrypted string>]"
  (when (stringp session)
    (setf session
          (or (get-session session)
              (error "No session with id: ~s" session))))
  (check-type session session)
  (let* ((iv (urandom-array 16))
         (iv-str (cl-base64:usb8-array-to-base64-string iv))
         (ciphertext (cl-crypto:aes-encrypt-string
                      string
                      (session-aes-key session)
                      :iv iv)))
    (format nil "[~a,~a,~a]"
            (session-id session)
            iv-str
            ciphertext)))

(defun session-encrypted-message-p (message)
  (when(stringp message)
    (setf message (trim message))
    (let ((len (length message)))
      (and (eql #\[ (aref message 0))
           (eql #\] (aref message (1- len)))))))

(defun decrypt-for-session (message)
  (check-type message string)
  (setf message (trim message))
  (assert (session-encrypted-message-p message)
          nil
          "Message must be of the form: [<session>,<iv>,<ciphertext>]")
  (let ((len (length message)))
    (destructuring-bind (id iv ciphertext)
        (split-sequence:split-sequence #\, (subseq message 1 (1- len)))
      (let* ((session (or (get-session id)
                          (error "No session with id: ~s" id))))
        (cl-crypto:aes-decrypt-to-string
         ciphertext
         (session-aes-key session)
         :iv iv)))))

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
