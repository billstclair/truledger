; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Truledger mail sending.
;;; A thin wrapper around cl-smtp:send-email, which computes
;;; the smtp host from the to domain.
;;;

(in-package :truledger)

(defun mx-hosts (domain)
  "Return the mail servers for DOMAIN, sorted by priority"
  (let* ((stream (run-program "host" `("-t" "MX" ,domain) :output :stream))
         (lines (loop
                   with needle = "handled by "
                   with needle-len = (length needle)
                   for line = (ignore-errors (read-line stream))
                   while line
                   for idx = (search needle line :test #'string-equal)
                   for tail = (and idx (subseq line (+ idx needle-len)))
                   for pri+host = (and tail (explode #\space tail))
                   when pri+host
                   collect (cons (parse-integer (car pri+host)) (cdr pri+host)))))
    (mapcar #'second (sort lines #'< :key #'car))))

(defun email-mx-hosts (email)
  (mx-hosts (subseq email (1+ (position #\@ email)))))

(defun send-email (from to subject message)
  (dolist (mx-host (email-mx-hosts to))
    (handler-case
        (return (cl-smtp:send-email mx-host from to subject message))
      (error ()))))

                                                        
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
