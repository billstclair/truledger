; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The toplevel function for the Trubanc application
;;;

(in-package :trubanc-client-web)

#-windows
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
            (destroy-password buf bufsize #'mem-set-char)))))))

;; Need to figure out how not to echo on Windows
#+windows
(defun read-passphrase (prompt)
  (format t "~a" prompt)
  (finish-output)
  (read-line))

(defun start-server (passphrase port)
  (cond ((server-privkey-file-exists-p)
         (let* ((server (make-server (server-db-dir) passphrase)))
           (trubanc-web-server server :port port)))
        (t (trubanc-web-server nil :port port))))

(defun toplevel-function ()
  (handler-case (toplevel-function-internal)
    (error (c)
      (format t "~a~%" c)
      (finish-output)
      (quit))))

(defun toplevel-function-internal ()
  (run-startup-functions)
  (let ((port (third (command-line-arguments))) ; app - port
        passphrase)
    (setq port (if port (parse-integer port) 8080))
    (loop
       (when (server-privkey-file-exists-p)
         (when (blankp (setq passphrase (read-passphrase "Bank passphrase: ")))
           (quit)))
       (unwind-protect
            (handler-case
                (progn
                  (start-server passphrase port)
                  (destroy-password passphrase)
                  (format t "~a started on port ~d~%"
                          (if (port-server port)
                              "Server"
                              "Client web server")
                          port)
                  (format t "Local web address: http://localhost:~d/client/~%"
                          port)
                  (finish-output)
                  (return))
              (bad-rsa-key-or-password ()
                (format t "Bad passphrase. Try again.~%")
                (finish-output))
              (error (c)
                (format t "Error starting server on port ~d: ~a~%" port c)
                (finish-output)
                (quit)))
         (destroy-password passphrase))))
  (process-wait "Server shutdown"
                (lambda () (not (web-server-active-p)))))

(defun target-suffix ()
  (or
   (progn
     #+darwinx86-target "dx86cl"
     #+darwinx8664-target "dx86cl64"
     #+freebsdx86-target "fx86cl"
     #+freebsdx8664-target "fx86cl64"
     #+linuxx86-target "lx86cl"
     #+linuxx8664-target "lx86cl64"
     #+win32-target "wx86cl"
     #+win64-target "wx86cl64")
   "app"))

(defun application-name ()
  (stringify (target-suffix) "trubanc-~a"))

(defun save-trubanc-application (&optional (filename (application-name)))
  (stop-web-server)
  (save-application filename
                    :toplevel-function #'toplevel-function
                    :prepend-kernel t
                    :clear-clos-caches t))

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
