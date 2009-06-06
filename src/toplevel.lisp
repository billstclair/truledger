; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The toplevel function for the Trubanc application
;;;

(in-package :trubanc-client-web)

(defun toplevel-function ()
  (handler-case (toplevel-function-internal)
    (error (c)
      (format t "~a~%" c)
      (finish-output)
      (quit))))

(defparameter *allowed-parameters*
  '((("-p" "--port") :port . t)
    ("--key" :key . t)
    ("--cert" :cert . t)
    ("--nonsslport" :nonsslport . t)))

(defun usage-error (app)
  (error
"Usage is: ~a [-p port] [--key keyfile --cert certfile] [--nonsslport nonsslport]
port defaults to 8080, unless keyfile & certfile are included, then 8443.
If port defaults to 8443, then nonsslport defaults to 0,
otherwise the application doesn't listen on a non-ssl port.
keyfile is the path to an SSL private key file.
certfile is the path to an SSL certificate file."
         app))

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

(defun toplevel-function-internal ()
  (run-startup-functions)
  (let (port keyfile certfile nonsslport)
    (multiple-value-bind (args app) (parse-args)
      (handler-case
          (setq keyfile (cdr (assoc :key args))
                certfile (cdr (assoc :cert args))
                port (parse-integer (or (cdr (assoc :port args))
                                        (if keyfile "8443" "8080")))
                nonsslport (parse-integer
                            (or (cdr (assoc :nonsslport args))
                                (if (assoc :port args) "0" "8080"))))
        (error () (usage-error app))))
    (when (xor keyfile certfile)
      (error "Both or neither required of --key & --cert"))
    (when keyfile
      (unless (and (probe-file keyfile) (probe-file certfile))
        (error "Key or cert file missing")))
    (when (eql 0 nonsslport) (setq nonsslport nil))
    (handler-case
        (progn (trubanc-web-server
                nil
                :port port
                :ssl-privatekey-file keyfile
                :ssl-certificate-file certfile
                :forwarding-port nonsslport)
               (format t "Client web server started on port ~a.~%"
                       (or nonsslport port))
               (format t "Web address: http://localhost:~a/~%"
                       (or nonsslport port))
               (finish-output))
      (error (c)
        (format t "Error starting server on port ~d: ~a~%" port c)
        (finish-output)
        (quit -1))))
  (process-wait
   "Server shutdown"
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
     #+win32-target "wx86cl.exe"
     #+win64-target "wx86cl64.exe")
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
