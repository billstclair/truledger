; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A Trubanc client API. Talks the protocol of server.lisp
;;;

(in-package :trubanc-test)

(defun make-test-state (db-dir &rest rest &key dont-erase passphrase port)
  (declare (ignore dont-erase passphrase port))
  (apply #'make-instance 'test-state
         :db-dir db-dir
         rest))        

(defclass test-state ()
  ((db-dir :accessor db-dir
           :initarg :db-dir)
   (port :accessor port
         :initarg :port
         :initform 8081)
   (server :accessor server
           :initform nil)
   (client :accessor client
           :initform nil)))

(defmethod initialize-instance :after ((ts test-state) &key
                                       dont-erase
                                       (passphrase "passphrase"))
  (let* ((db-dir (ensure-directory-pathname (db-dir ts)))
         (port (port ts))
         (server-dir (merge-pathnames "serverdb" db-dir))
         (client-dir (merge-pathnames "clientdb" db-dir)))
    (unless dont-erase
      (ignore-errors (recursive-delete-directory db-dir)))
    (setf (server ts) (trubanc-server:make-server
                       server-dir passphrase
                       :bankname "Test Bank"
                       :bankurl (format nil "http://localhost:~d/" port))
          (client ts) (make-client client-dir))))



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
