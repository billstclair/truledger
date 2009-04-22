; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Read and write configurations and initialize them with web pages
;;;

(in-package :trubanc)

(defvar *config-readtable*
  (let ((rt (copy-readtable)))
    (set-syntax-from-char #\# #\a rt)
    rt))

;; Expects the stream to be "(k v k v k v)"
;; Returns ((k . v) (k . v) (k . v))
(defun read-config-from-stream (stream)
  (let ((*readtable* *config-readtable*))
    (read stream)))

(defun read-config-from-file (file)
  (with-open-file (s file)
    (read-config-from-stream s)))

(defun read-config-from-string (string)
  (with-input-from-string (s string)
    (read-config-from-stream s)))

(defun write-config-to-stream (config stream &optional comments)
  (let ((*print-circle* t)
        (*print-case* :downcase)
        (*print-readably* t))
    (princ #\( stream)
    (terpri stream)
    (loop
       for (k v) on config by #'cddr
       for comment = (and comments (getf comments k))
       do
         (when comment
           (format stream ";; ~a~%" comment))
         (prin1 k stream)
         (princ #\space stream)
         (prin1 v stream)
         (terpri stream))
    (princ #\) stream)
    (terpri stream)))

(defun write-config-to-file (config file &key (if-exists :supersede) comments)
  (with-open-file (s file :direction :output :if-exists if-exists)
    (write-config-to-stream config s comments)))

(defun write-config-to-string (config &optional comments)
  (with-output-to-string (s)
    (write-config-to-stream config s comments)))

(defun sign-config (config privkey &optional comments)
  (let ((str (write-config-to-string config comments)))
    (with-rsa-private-key (key privkey)
      (append config `(:signature ,(sign str key))))))

(defun verify-config (config pubkey &optional comments)
  (let* ((sig (getf config :signature)))
    (or (null sig)
        (let ((copy (copy-list config)))
          (remf copy :signature)
          (let ((str (write-config-to-string copy comments)))
            (with-rsa-public-key (key pubkey)
              (verify str sig key)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Server configuration
;;;

(defparameter *server-config-file* "server.cfg")

(defun read-server-config ()
  (and (probe-file *server-config-file*)
       (read-config-from-file *server-config-file*)))

(defun write-server-config (server &key (port 8080) www-dir)
  (check-type server server)
  (let ((dir (or www-dir (port-www-dir port))))
    (when dir
      (setq www-dir (namestring (truename dir)))))
  (let* ((db-dir (fsdb-dir (db server)))
         (privkey (privkey server))
         (config (sign-config `(:db-dir ,db-dir :port ,port :www-dir ,www-dir)
                              privkey)))
    (write-config-to-file config *server-config-file*)))

(defun start-server (passphrase)
  "Read the server config file, start the server, and return the server instance.
   Signal an error, if something goes wrong."
  (let* ((config (read-server-config))
         (db-dir (getf config :db-dir))
         (server-port (getf config :port))
         (server-www-dir (getf config :www-dir))
         server)
    (unless config (error "Couldn't read server config file"))
    (handler-case
        (setq server (make-server db-dir passphrase))
      (error (c)
        (error "Error creating server, db-dir: ~s, msg: ~a" db-dir c)))
    (unless (verify-config config (privkey server))
      (error "Failed to verify configuration signature"))
    (trubanc-web-server server server-www-dir server-port)
    server))

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
