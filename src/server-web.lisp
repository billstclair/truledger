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
  (setf (gethash port *trubanc-ports-to-servers*) server))

(defun port-acceptor (port)
  (gethash port *trubanc-ports-to-acceptors*))

(defun (setf port-acceptor) (acceptor port)
  (setf (gethash port *trubanc-ports-to-acceptors*) acceptor))

(defun port-www-dir (port)
  (gethash port *trubanc-ports-to-www-dirs*))

(defun (setf port-www-dir) (www-dir port)
  (setf (gethash port *trubanc-ports-to-www-dirs*) www-dir))

(defparameter *default-server-port* 8080)

(defun trubanc-web-server (server &key www-dir (port *default-server-port*))
  (setf (port-server port) server
        (port-www-dir port) www-dir)
  (or (port-acceptor port)
      (let ((acceptor (make-instance 'hunchentoot:acceptor :port port)))
        (hunchentoot:start acceptor)
        (setf (port-acceptor port) acceptor))))

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
