; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Read and write configurations and initialize them with web pages
;;;

(in-package :trubanc)

(defparameter *config-readtable*
  (let ((rt (copy-readtable)))
    (set-syntax-from-char #\# #\a rt)
    (set-syntax-from-char #\` #\' rt)
    rt))

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

(defun get-server-db-dir (config)
  (getf config :server-db-dir))

(defun get-server-port (config)
  (getf config :server-port))

(defun get-server-www-dir (config)
  (getf config :server-www-dir))

(defun write-server-config (server &key
                            (server-port *default-server-port*)
                            server-www-dir)
  (check-type server server)
  (check-type server-port integer)
  (let ((dir (or server-www-dir (port-www-dir server-port))))
    (when dir
      (setq server-www-dir
            (remove-trailing-separator (namestring (truename dir))))))
  (let* ((server-db-dir (fsdb-dir (trubanc-server:db server)))
         (privkey (trubanc-server:privkey server))
         (bankname (trubanc-server:bankname server))
         (bankurl (trubanc-server:bankurl server))
         (config (sign-config `(:bankname ,bankname
                                :bankurl ,bankurl
                                :server-db-dir ,server-db-dir
                                :server-port ,server-port
                                :server-www-dir ,server-www-dir)
                              privkey)))
    (write-config-to-file config *server-config-file*)))

(defun start-server (passphrase &optional
                     (config (read-server-config)))
  "Read the server config file, start the web server, and return the server instance.
   Signal an error if something goes wrong."
  (let* ((db-dir (get-server-db-dir config))
         (server-port (get-server-port config))
         (server-www-dir (get-server-www-dir config))
         server)
    (unless config (error "Couldn't read server config file"))
    (setq server (trubanc-server:make-server db-dir passphrase))
    (unless (verify-config config (trubanc-server:privkey server))
      (error "Failed to verify configuration signature"))
    (trubanc-web-server server :www-dir server-www-dir :port server-port)
    server-port))

(defctype size-t :unsigned-int)

#-windows
(defcfun ("readpassphrase" %read-passphrase) :pointer
  (prompt :pointer)
  (buf :pointer)
  (bufsiz size-t)
  (flags :int))

#-windows
(defun read-passphrase (prompt)
  (let ((bufsize 132)
        (flags 0))    
    (with-foreign-string (p prompt)
      (with-foreign-pointer (buf bufsize)
        (let ((res (%read-passphrase p buf bufsize flags)))
          (when (null-pointer-p res) (error "Error reading passphrase"))
          (prog1 (foreign-string-to-lisp res :encoding :latin-1)
            ;; Erase the passphrase from memory
            (destroy-password buf bufsize 'mem-set-char)))))))

;; Need to figure out how not to echo on Windows
#+windows
(defun read-passphrase (prompt)
  (format t "~a" prompt)
  (finish-output)
  (read-line))

(defun get-server-passphrase ()
  (let (passphrase)
    (loop
       (setq passphrase (read-passphrase "Passphrase: "))
       (when (eql 0 (length passphrase))
         (return nil))
       (when (ignore-errors
               (let* ((config (read-server-config))
                      (db-dir (and config (get-server-db-dir config)))
                      (db (and db-dir (make-fsdb db-dir))))
                 (and db (db-get db $PRIVKEY))))
         (return passphrase))
       (let ((again (read-passphrase "Again: ")))
         (when (eql 0 (length again))
           (return nil))
         (when (equal passphrase again)
           (return passphrase))
         (format t "Passphrase mismatch~%")
         (finish-output)))))

(defun toplevel-function ()
  (run-startup-functions)
  (let ((start-config-server-p nil)
        passphrase)
    (loop
       (unless (setq passphrase (get-server-passphrase))
         (quit))
       (handler-case
           (let ((port (start-server passphrase)))
             (destroy-password passphrase)
             (format t "Server started on port ~d~%" port)
             (finish-output)
             (return))
         (bad-rsa-key-or-password ()
           (format t "Bad passphrase. Try again.~%")
           (finish-output))
         (error (c)
           (format t "Error starting server: ~a~%" c)
           (finish-output)
           (setq start-config-server-p t)
           (return))))
    (when start-config-server-p
      (handler-case
          (let ((port (start-config-server passphrase)))
            (format t "Config web server started on port ~d~%" port)
            (finish-output))
        (error (c)
          (format t "Error starting config server: ~a~%" c)
          (finish-output)
          (quit)))))
  (process-wait "Server shutdown"
                (lambda () (not (web-server-active-p)))))

(defun save-trubanc-application (&optional (filename "trubanc-app"))
  (stop-web-server)
  (save-application filename
                    :toplevel-function #'toplevel-function
                    :prepend-kernel t
                    :clear-clos-caches t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The config web server
;;;

(defparameter *config-server-port* 8000)
(defvar *config-server-passphrase* nil)

(defun start-config-server (passphrase &optional (port *config-server-port*))
  (setq *config-server-passphrase* passphrase)
  (let* ((acceptor (trubanc-web-server nil :port port)))
    (setf (get-web-script-handler "/" acceptor) 'do-config-server)
    port))

(defun tr (stream label type name &optional size value)
  (who (s stream)
    (:tr
     (:th :align "right" (str label))
     (:td (:input :type type :name name :size size :value value)))))

(defun remove-port (host)
  (let ((pos (position #\: host :from-end t)))
    (if pos (subseq host 0 pos) host)))

(defun add-trailing-slash (string)
  (let ((len (length string)))
    (if (and (> len 0)
             (stringp string)
             (not (eql #\/ (aref string (1- len)))))
        (strcat string #\/)
        string)))

(defun create-server-config (bankname bankurl server-db-dir server-port server-www-dir)
  (check-type bankname string)
  (check-type bankurl string)
  (check-type server-db-dir string)
  (check-type server-www-dir string)

  (create-directory (add-trailing-slash server-db-dir))
  (let ((path (probe-file server-db-dir)))
    (unless (and path (cl-fad:directory-pathname-p path))
      (error "Couldn't create DB Dir")))
  (let ((port (ignore-errors (parse-integer server-port))))
    (cond ((and server-www-dir
                (not (equal "" server-www-dir))
                (not (cl-fad:directory-pathname-p
                      (probe-file server-www-dir))))
           (error "WWW Dir is not a directory"))
          ((not (and port (> port 256)))
           (error "Server Port must be an integer > 256"))
          (t (let* ((passphrase *config-server-passphrase*)
                    (server (trubanc-server:make-server
                             server-db-dir passphrase
                             :bankname bankname
                             :bankurl bankurl)))
               (write-server-config server
                                    :server-port port
                                    :server-www-dir server-www-dir)
               (stop-web-server port)
               (start-server passphrase)
               (destroy-password passphrase)
               (setq *config-server-passphrase* nil)
               (format t "The server has been started on port: ~a~%" server-port)
               (stop-web-server (hunchentoot:acceptor-port hunchentoot:*acceptor*))
               (whots (s)
                 (:html
                  (:head
                   (:title "Trubanc Server Configured"))
                  (:body
                   (:p "Your Trubanc Server is configured and running.")
                   (:p 
                    (:a :href (format nil "http://~a:~a/"
                                      (remove-port (hunchentoot:host))
                                      server-port)
                        "Click here")
                    " for the server's home page.")))))))))

(defun do-config-server ()
  (setf (hunchentoot:content-type*) "text/html")
    (bind-parameters (submit cancel bankname bankurl
                             server-db-dir server-port server-www-dir)
      (let ((err nil))
        (cond (cancel
               (let ((res (whots (s)
                            (:html
                             (:head (:title "Config Done"))
                             (:body
                              "Config web server terminated.")))))
                 (stop-web-server
                  (hunchentoot:acceptor-port hunchentoot:*acceptor*))
                 ;; We usually exit from toplevel-function
                 ;; before this gets displayed in the browser.
                 ;; Figure out how to delay.
                 (return-from do-config-server res)))
              (submit
               (handler-case
                   (return-from do-config-server
                     (create-server-config bankname bankurl server-db-dir server-port server-www-dir))
                 (bad-rsa-key-or-password ()
                   (setq err (format nil "Couldn't load private key. ~
                                          Your passphrase may be wrong. ~
                                          Cancel and restart to try again.")))
                 (error (c) (setq err (format nil "~a" c)))))
            (t (unless (and bankname bankurl server-db-dir server-port server-www-dir)
                 (let* ((config (ignore-errors (read-server-config))))
                   (unless bankname
                     (setq bankname (getf config :bankname "My Banc")))
                   (unless server-db-dir
                     (setq server-db-dir (getf config :server-db-dir "serverdb")))
                   (unless server-port
                     (setq server-port
                           (getf config :server-port *default-server-port*)))
                   (unless server-www-dir
                     (setq server-www-dir (getf config :server-www-dir "www")))
                   (unless bankurl
                     (let* ((host (get-host-name))
                            (port (unless (eql 80 server-port) server-port))
                            (url (format nil "http://~a~@[:~d~]/" host port)))
                       (setq bankurl (getf config :bankurl url))))))))
        (whots (s)
          (:html
           (:head
            (:title "Trubanc Configuration"))
           (:body
            (:p "Fill in directories and the port on which to run the server. "
                "Then click the 'Submit' button to start the server "
                "and save the configuration.")
            (:p :style "color: red;" (str (or err "&nbsp;")))
            (:form :method "post" :action "./"
                   (:table
                    (tr s "Bank Name" "text" "bankname" "20" bankname)
                    (tr s "Bank URL:" "text" "bankurl" "30" bankurl)
                    (tr s "DB Dir:" "text" "server-db-dir" 50 server-db-dir)
                    (tr s "WWW Dir:" "text" "server-www-dir" 50 server-www-dir)
                    (tr s "Server Port:" "text" "server-port" "5" server-port)
                    (:tr (:td "&nbsp;")
                         (:td (:input :type "submit" :name "submit" :value "Submit")
                              " "
                              (:input :type "submit" :name "cancel" :value "Cancel")))
                    ))))))))

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
