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
  (if port
      (setf (gethash port *trubanc-ports-to-servers*) server)
      (remhash port *trubanc-ports-to-servers*))
  port)

(defun port-acceptor (port)
  (gethash port *trubanc-ports-to-acceptors*))

(defun (setf port-acceptor) (acceptor port)
  (if port
      (setf (gethash port *trubanc-ports-to-acceptors*) acceptor)
      (remhash port *trubanc-ports-to-acceptors*))
  port)

(defun port-www-dir (port)
  (gethash port *trubanc-ports-to-www-dirs*))

(defun (setf port-www-dir) (www-dir port)
  (if port
      (setf (gethash port *trubanc-ports-to-www-dirs*) www-dir)
      (remhash port *trubanc-ports-to-www-dirs*))
  port)

(defun web-server-active-p ()
  (> (hash-table-count *trubanc-ports-to-acceptors*) 0))

(defparameter *default-server-port* 8080)

(defun trubanc-web-server (server &key www-dir (port *default-server-port*))
  (setf (port-server port) server
        (port-www-dir port) www-dir)
  (or (port-acceptor port)
      (let ((acceptor (make-instance 'hunchentoot:acceptor :port port)))
        (hunchentoot:start acceptor)
        (setf (port-acceptor port) acceptor))))

(defun stop-web-server (&optional (port :all))
  (cond ((eq port :all)
         (let ((ports (loop
                         for port being the hash-keys
                         of *trubanc-ports-to-servers*
                         collect port)))
           (mapc 'stop-web-server ports)))
        (t (let ((acceptor (port-acceptor port)))
             (when acceptor
               (setf (port-server port) nil
                     (port-acceptor port) nil
                     (port-www-dir port) nil)
               (process-run-function
                (format nil "Stop port ~d" port)
                'hunchentoot:stop acceptor)
               ;; The process above will hang until somebody
               ;; makes the web server do something.
               ;; Need to send it a "GET / HTTP/1.0" or some-such.
               )))))

(defun do-trubanc-web-server (msg debug)
  (let* ((port (hunchentoot:acceptor-port hunchentoot:*acceptor*))
         (server (port-server port)))
    (cond (msg
           (let ((res (process server msg)))
             (when debug
               (setq res (format nil "msg: <pre>~a</pre>~%response: <pre>~a</pre>~%"
                                 msg res)))
             res))
          (t (do-static-file)))))
  
(hunchentoot:define-easy-handler (trubanc-server :uri "/") (msg debug)
  (setf (hunchentoot:content-type*) "text/html")
  (do-trubanc-web-server msg debug))

(hunchentoot:define-easy-handler (static-file :uri 'static-file-request-p) ()
  (do-static-file))

(defun do-static-file ()
  (let* ((acceptor hunchentoot:*acceptor*)
         (port (hunchentoot:acceptor-port acceptor))
         (dir (port-www-dir port)))
    (cond ((not dir)
           #.(strcat "This is a <a href='http://trubanc.com/'>Trubanc</a>"
                     " server with no home page."))
          (t
           (let ((file (merge-pathnames
                        (strcat dir "/."
                                (hunchentoot:request-uri hunchentoot:*request*)))))
             (hunchentoot:handle-static-file
              (if (cl-fad:directory-pathname-p file)
                  (merge-pathnames "index.html" file)
                  file)))))))

(defun static-file-request-p (request)
  (let ((script (hunchentoot:script-name request)))
    (cond ((equal script "/") nil)
          ((search "/.." script) nil)
          (t t))))

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
