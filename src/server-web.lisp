; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Trubanc server web interface
;;;
;;; (let ((server (make-server "/users/billstclair/testserverdb" "passphrase")))
;;;   (trubanc-web-server server))
;;;

(in-package :trubanc)

(defvar *trubanc-ports-to-servers*
  (make-hash-table :test 'eql))

(defvar *trubanc-ports-to-acceptors*
  (make-hash-table :test 'eql))

(defvar *trubanc-ports-to-www-dirs*
  (make-hash-table :test 'eql))

(defun port-server (port)
  (gethash port *trubanc-ports-to-servers*))

(defun (setf port-server) (server port)
  (if server
      (setf (gethash port *trubanc-ports-to-servers*) server)
      (remhash port *trubanc-ports-to-servers*))
  server)

(defun port-acceptor (port)
  (gethash port *trubanc-ports-to-acceptors*))

(defun (setf port-acceptor) (acceptor port)
  (if acceptor
      (setf (gethash port *trubanc-ports-to-acceptors*) acceptor)
      (remhash port *trubanc-ports-to-acceptors*))
  acceptor)

(defun port-www-dir (port)
  (gethash port *trubanc-ports-to-www-dirs*))

(defun (setf port-www-dir) (www-dir port)
  (if www-dir
      (setf (gethash port *trubanc-ports-to-www-dirs*) www-dir)
      (remhash port *trubanc-ports-to-www-dirs*))
  www-dir)

(defun web-server-active-p ()
  (> (hash-table-count *trubanc-ports-to-acceptors*) 0))

(defmacro bind-parameters ((&rest params) &body body)
  `(let ,(mapcar (lambda (param)
                   `(,param (hunchentoot:parameter
                             ,(string-downcase (string param)))))
                 params)
     ,@body))

(defun do-trubanc-web-server ()
  (let* ((acceptor hunchentoot:*acceptor*)
         (port (hunchentoot:acceptor-port acceptor))
         (server (port-server port)))
    (bind-parameters (msg debug)
      (setf (hunchentoot:content-type*) "text/html")
      (cond ((and msg server)
             (let ((res (process server msg)))
               (when debug
                 (setq res (format nil
                                   "msg: <pre>~a</pre>~%response: <pre>~a</pre>~%"
                                   msg res)))
               res))
            ((not server)
             (hunchentoot:redirect "/client/"))
            (t (do-static-file))))))

(defvar *last-uri* nil)

(defun do-trubanc-web-client ()
  (trubanc-client-web:web-server))
  
(defvar *web-script-handlers*
  (make-hash-table :test 'equal))

(defun get-web-script-handler (port script-name acceptor)
  (gethash (list port script-name acceptor) *web-script-handlers*))

(defun (setf get-web-script-handler) (handler port script-name acceptor)
  (setf (gethash (list port script-name acceptor) *web-script-handlers*)
        handler))

(defun remove-web-script-handlers (port acceptor)
  (loop
     for key being the hash-key of *web-script-handlers*
     do
       (when (and (eql port (car key))
                  (eq acceptor (third key)))
         (remhash key *web-script-handlers*))))

(hunchentoot:define-easy-handler (trubanc-server :uri 'identity) ()
  (let* ((script (hunchentoot:script-name hunchentoot:*request*))
         (acceptor hunchentoot:*acceptor*)
         (port (hunchentoot:acceptor-port acceptor))
         (handler (get-web-script-handler port script acceptor)))
    (cond (handler (funcall handler))
          ((search "/.." script)
           (abort-request))
          (t (do-static-file)))))

(defun abort-request ()
  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
  (hunchentoot:abort-request-handler))

(defun do-static-file ()
  (let* ((acceptor hunchentoot:*acceptor*)
         (port (hunchentoot:acceptor-port acceptor))
         (dir (port-www-dir port)))
    (cond ((not dir)
           (abort-request))
          (t
           (let ((file (merge-pathnames
                        (strcat dir "/."
                                (hunchentoot:request-uri hunchentoot:*request*)))))
             (hunchentoot:handle-static-file
              (if (cl-fad:directory-pathname-p file)
                  (merge-pathnames "index.html" file)
                  file)))))))

(defparameter *default-server-port* 8080)

(defun trubanc-web-server (server &key (www-dir "www") (port *default-server-port*))
  (setf (port-server port) server
        (port-www-dir port) www-dir)
  (or (port-acceptor port)
      (let ((acceptor (make-instance 'hunchentoot:acceptor :port port)))
        (setf (get-web-script-handler port "/" acceptor)
              'do-trubanc-web-server
              (get-web-script-handler port "/client" acceptor)
              #'(lambda () (hunchentoot:redirect "/client/"))
              (get-web-script-handler port "/client/" acceptor)
              'do-trubanc-web-client)
        (setf (port-acceptor port) acceptor)
        (hunchentoot:start acceptor)
        acceptor)))

(defun stop-web-server (&optional (port :all))
  (cond ((eq port :all)
         (let ((ports (loop
                         for port being the hash-keys
                         of *trubanc-ports-to-acceptors*
                         collect port)))
           (mapc 'stop-web-server ports)))
        (t (let ((acceptor (port-acceptor port)))
             (setf (port-server port) nil
                   (port-acceptor port) nil
                   (port-www-dir port) nil)
             (when acceptor
               (remove-web-script-handlers port acceptor)
               (let ((started nil))
                 (process-run-function
                  (format nil "Stop port ~d" port)
                  (lambda ()
                    (setq started t)
                    (hunchentoot:stop acceptor)))
                 (process-wait "Server stopper" (lambda () started)))
               ;; Need to make a request in order to get the server to shut down
               (ignore-errors
                 (dotimes (i 3)
                   (drakma:http-request (format nil "http://localhost:~d/" port))))
               )))))

(defun send-bank-request (uri &optional msg-p)
  (drakma:http-request
   (format nil "http://~a~@[?msg=(bc50c4fd9c228a21f64d34ca644a46c1fe8520e4%2Cbankid%2C-----BEGIN+PUBLIC+KEY-----%0AMFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBAMwfcmkk2coTuYAEbdZ5iXggObNPzbSi%0ADnVtndZFe4%2F4Xg0IQPfpQ04OkhWIftMy1OjFhGlBzzNzdW98KYwKMgsCAwEAAQ%3D%3D%0A-----END+PUBLIC+KEY-----%0A)%3A%0AsLJ9GqFjZ61fq%2FbDFL6rxpY3w2s5dWIAXJCvPKQTPEkrG%2F2I1fwxBfugBmn%2FiPwa%0AjCRtnFDnrn7Mv%2BUY%2BSH4yw%3D%3D~]" uri msg-p)))

(defvar *stop-pounding-flag* nil)
(defvar *pound-lock* (make-lock "Pound lock"))
(defvar *pound-count* 0)

(defun pound (uri &optional msg-p (thread-count 5))
  (setq *stop-pounding-flag* nil
        *pound-count* 0)
  (dotimes (i thread-count)
    (process-run-function (format nil "Pound ~d" i)
      (lambda ()
        (loop
           (when *stop-pounding-flag* (return))
           (ignore-errors
             (send-bank-request uri msg-p)
             (with-lock-grabbed (*pound-lock*)
               (incf *pound-count*))))))))

(defun stop-pounding ()
  (setq *stop-pounding-flag* t))

(defun pound-count ()
  *pound-count*)

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
