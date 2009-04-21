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

(defvar *index-file*
  (merge-pathnames "../index.inc" trubanc-loader:*source-directory*))

(defun trubanc-web-server (server &key (port 8080))
  (setf (gethash port *trubanc-ports-to-servers*) server)
  (or (gethash port *trubanc-ports-to-acceptors*)
      (let ((acceptor (make-instance 'hunchentoot:acceptor :port port)))
        (hunchentoot:start acceptor)
        (setf (gethash port *trubanc-ports-to-acceptors*) acceptor)
        acceptor)))

(defun do-trubanc-web-server (msg debug)
  (let* ((port (hunchentoot:acceptor-port hunchentoot:*acceptor*))
         (server (gethash port *trubanc-ports-to-servers*)))
    (cond (msg
           (let ((res (process server msg)))
             (when debug
               (setq res (format nil "msg: <pre>~a</pre>~%response: <pre>~a</pre>~%"
                                 msg res)))
             res))
          (t (file-get-contents *index-file*)))))
  
(hunchentoot:define-easy-handler (trubanc-server :uri "/") (msg debug)
  (setf (hunchentoot:content-type*) "text/html")
  (do-trubanc-web-server msg debug))

(hunchentoot:define-easy-handler (static-file :uri 'static-file-request-p) ()
  (let ((file (merge-pathnames
               (strcat ".." (hunchentoot:request-uri hunchentoot:*request*))
               trubanc-loader:*source-directory*)))
    (hunchentoot:handle-static-file
     (if (cl-fad:directory-pathname-p file)
         (merge-pathnames "index.html" file)
         file))))

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
