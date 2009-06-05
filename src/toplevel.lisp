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
  '((("-p" "--port") :port . t)))

(defun usage-error (app)
  (error "Usage is: ~a [-p port]" app))

(defun parse-args (&optional (args (command-line-arguments)))
  (let ((app (pop args))
        (res nil))
    (loop
       while args
       for switch = (pop args)
       for key-and-argp = (cdr (assoc switch *allowed-parameters*
                                  :test (lambda (x y)
                                          (member x y :test #'string-equal))))
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
  (let (port)
    (multiple-value-bind (args app) (parse-args)
      (setq port (cdr (assoc :port args)))
      (setq port (if port
                     (handler-case (parse-integer port)
                       (error () (usage-error app)))
                     8080)))
    (handler-case
        (progn (trubanc-web-server nil :port port)
               (format t "Client web server started on port ~a.~%" port)
               (format t "Web address: http://localhost:~a/~%" port)
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
