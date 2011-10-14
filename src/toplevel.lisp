; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The toplevel function for the Truledger application
;;;

(in-package :truledger-client-web)

(defun toplevel-function ()
  (set-interactive-abort-process)
  (invoking-debugger-hook-on-interrupt
    (let ((*debugger-hook* (lambda (condition hook)
                             ;; Here on ctrl-c, SIGINT.
                             ;; Really should do a controlled shut-down
                             ;; of the web server, with a second hook
                             ;; to quit right away on a second ctlr-c
                             (declare (ignore condition hook))
                             (terpri)
                             (finish-output)
                             (quit))))
      (handler-case (toplevel-function-internal)
        (error (c)
          (if (typep c 'usage-error)
              (format t "~&~a~%" c)
              (format t "~&~a~%~a~%" c (backtrace-string)))
          (finish-output)
          (quit -1))))))

(defparameter *allowed-parameters*
  '((("-p" "--port") :port . t)
    ("--dbdir" :dbdir . t)
    ("--key" :key . t)
    ("--cert" :cert . t)
    ("--nonsslport" :nonsslport . t)
    ("--uid" :uid . t)
    ("--gid" :gid . t)
    ("--url-prefix" :url-prefix . t)
    #+loadswank
    ("--slimeport" :slimeport . t)))

(define-condition usage-error (simple-error)
  ())

(defun usage-error (app)
  (error 'usage-error
         :format-control
         "Usage is: ~a [-p port] [--dbdir dbdir] [--key keyfile --cert certfile] [--nonsslport nonsslport] [--uid uid --gid gid] [--url-prefix url-prefix]~a
port defaults to 8785, unless keyfile & certfile are included, then 8786.
If port defaults to 8786, then nonsslport defaults to 8785,
otherwise the application doesn't listen on a non-ssl port.
dbdir is where the 'database' files are stored.
  default: ~a
keyfile is the path to an SSL private key file.
certfile is the path to an SSL certificate file.
uid & gid are the user id and group id to change to after listening on the port.
url-prefix is a prefix of the URL to ignore; useful for reverse proxies.~a
"
         :format-arguments
         (list app
               #-loadswank ""
               #+loadswank " [--slimeport slimeport]"
               (db-dir)
               #-loadswank ""
               #+loadswank (format nil "~%slimeport is a port on which to listen for a connection from the SLIME IDE."))))

(defun parse-args (&optional (args (command-line-arguments)))
  (let ((app (pop args))
        (res nil))
    (loop
       while args
       for switch = (pop args)
       for key-and-argp = (cdr (assoc switch *allowed-parameters*
                                  :test (lambda (x y)
                                          (member x
                                                  (if (listp y) y (list y))
                                                  :test #'string-equal))))
       for key = (car key-and-argp)
       for argp = (cdr key-and-argp)
       do
         (cond (argp
                (when (and argp (null args))
                  (usage-error app))
                (push (cons key (and argp (pop args))) res))
               (t (usage-error app))))
    (values (nreverse res) app)))

(defparameter *default-port-string* "8785")
(defparameter *default-ssl-port-string* "8786")

(defun toplevel-function-internal ()
  (run-startup-functions)
  (let (port dbdir keyfile certfile nonsslport uid gid slimeport)
    (multiple-value-bind (args app) (parse-args)
      (handler-case
          (setq keyfile (cdr (assoc :key args))
                certfile (cdr (assoc :cert args))
                port (parse-integer
                      (or (cdr (assoc :port args))
                          (if keyfile
                              *default-ssl-port-string*
                              *default-port-string*)))
                dbdir (cdr (assoc :dbdir args))
                nonsslport (parse-integer
                            (or (cdr (assoc :nonsslport args))
                                (if (assoc :port args) "0" *default-port-string*)))
                uid (cdr (assoc :uid args))
                gid (cdr (assoc :gid args))
                truledger:*url-prefix* (or (cdr (assoc :http-port args))
                                            (asdf:getenv "TRULEDGER_URL_PREFIX"))
                slimeport (let ((str (cdr (assoc :slimeport args))))
                            (and str (parse-integer str))))
        (error () (usage-error app))))
    (when dbdir
      (setf (db-dir) dbdir))
    (when (xor keyfile certfile)
      (error "Both or neither required of --key & --cert"))
    (when keyfile
      (unless (and (probe-file keyfile) (probe-file certfile))
        (error "Key or cert file missing")))
    (when (eql 0 nonsslport) (setq nonsslport nil))
    (when uid (setq uid (parse-integer uid)))
    (when gid (setq gid (parse-integer gid)))
    #+loadswank
    (when slimeport
      (push (cons '*package* (find-package :truledger))
            swank:*default-worker-thread-bindings*)
      (swank:create-server :port slimeport :dont-close t))
    slimeport                           ;no warning
    (handler-bind
	((error (lambda (c)
		  (break "Error starting server on port ~d: ~a~%" port c)
		  (return-from toplevel-function-internal
		    (quit -1)))))
      (let* ((dispport (or nonsslport port))
	     (url (format nil "http://localhost~@[:~a~]/"
			  (unless (eql dispport 80) dispport))))
	(truledger-web-server
	 nil
	 :port port
	 :ssl-privatekey-file keyfile
	 :ssl-certificate-file certfile
	 :forwarding-port nonsslport
	 :uid uid
	 :gid gid)
	(format t "Client web server started on port ~a.~%"
		(or nonsslport port))
	(format t "Web address: ~a~%" url)
	(when (server-db-exists-p)
	  (format t "REMEMBER TO LOG IN AS THE SERVER TO START THE SERVER!!~%"))
	(finish-output)
	(browse-url url))))
  (process-wait
   "Server shutdown"
   (lambda () (not (web-server-active-p)))))

(defun last-commit ()
  (ignore-errors
    (let* ((s #-windows (run-program "git" '("log") :output :stream)
	      #+windows (progn (run-program "git-log.bat" nil)
			       (open "git.log")))	   
	   (str (unwind-protect (read-line s)
		  (close s))))
      (when str
        ;; str = "commit <number>"
        (let ((commit (second (explode #\space (trim str))))
              (tags (get-git-tags)))
          (or (cdr (assoc commit tags :test #'equal))
              commit))))))

(defun get-git-tags ()
  (ignore-errors
    (let* ((s #-windows (run-program "git" '("show-ref" "--tags" "-d") :output :stream)
              #+windows (progn (run-program "git-tags.bat" nil)
                               (open "git.tags"))))
      (unwind-protect
           (read-git-tags s)
        (close s)))))

(defun read-git-tags (s)
  (loop
     with needle = "refs/tags/"
     with needle-len = (length needle)
     for line = (read-line s nil nil)
     while line
     for tokens = (explode #\space (trim line))
     for commit = (car tokens)
     for tag-str = (cadr tokens)
     for deref-pos = (- (length tag-str) 3)
     when (and (> deref-pos 0)
               (eql deref-pos (search "^{}" tag-str :start2 deref-pos))
               (eql 0 (search needle tag-str)))
     collect (cons commit (subseq tag-str needle-len deref-pos))))

(defun target-suffix ()
  (or
   (progn
     #+darwinx86-target "dx86cl"
     #+darwinx8664-target "dx86cl64"
     #+freebsdx86-target "fx86cl"
     #+freebsdx8664-target "fx86cl64"
     #+linuxx86-target "lx86cl"
     #+linuxx8664-target "lx86cl64"
     #+win32-target "wx86cl.exe"
     #+win64-target "wx86cl64.exe")
   "app"))

(defun write-application-name (file &optional (name (application-name)))
  (with-open-file (s file :direction :output :if-exists :supersede)
    (write-string name s)))

(defun application-name ()
  (stringify (target-suffix) "truledger-~a"))

(defun truledger-application-directory ()
  #+(and unix (not darwin))
  (fsdb:append-db-keys (asdf:getenv "HOME") ".truledger")
  #+windows
  (fsdb:append-db-keys (namestring (pathname (asdf:getenv "APPDATA")))
                       "Truledger")
  #+darwin
  (fsdb:append-db-keys (asdf:getenv "HOME")
                       "Library"
                       "Application Support"
                       "Truledger"))

(defun set-db-dir-for-saved-application ()
  (setf (db-dir)
        (lambda ()
          (fsdb:append-db-keys (truledger-application-directory) "dbs"))
        (ssl-certificates-dir)
        (lambda ()
          (fsdb:append-db-keys (truledger-application-directory)
                               "ssl-certificates"))))

(defun set-db-dir-for-development ()
  (setf (db-dir) "truledger-dbs"
        (ssl-certificates-dir) "ssl-certificates"))

(defun save-truledger-application (&optional (filename (application-name)))
  (stop-web-server)
  (load-template-directory)
  (setq *last-commit* (last-commit))
  (setq *save-application-time* (get-unix-time))
  (set-db-dir-for-saved-application)
  (run-save-application-functions)
  (save-application filename
                    :toplevel-function #'toplevel-function
                    :prepend-kernel t
                    :clear-clos-caches t))

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
