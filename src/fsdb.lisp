; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File System Database for Trubanc
;;;

(in-package :trubanc)

(defclass fsdb ()
  ((dir :type string
        :initarg :dir
        :accessor fsdb-dir)
   (locks :type hash-table
          :initform (make-hash-table :test 'equal)
          :accessor fsdb-locks)))

(defun normalize-key (key)
  (if (eql (aref key 0) #\/)
      (subseq key 1)
      key))

(defmethod filename ((db fsdb) key)
  (let ((key (normalize-key key)))
    (values (strcat (fsdb-dir db) key)
            key)))

(defmacro with-global-lock ((key) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-global-lock #',thunk ,key))))

(defun call-with-global-lock (thunk key)
  (let ((lock (global-lock key)))
    (unwind-protect
         (funcall thunk)
      (global-unlock lock))))

;; To do
(defun global-lock (key)
  key)

;; To do
(defun global-unlock (lock)
  lock)

;; To do
(defun lock-global-lock (lock)
  lock)

;; To do
(defun lock-exclusive-p (lock)
  lock)

;; To do
(defun upgrade-to-exclusive (lock)
  lock)

(defmacro with-fsdb-filename ((db filename key &optional exclusive-p) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk (,filename ,key)
              (declare (ignorable ,key))
              ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-fsdb-filename #',thunk ,db ,key ,exclusive-p))))

(defun call-with-fsdb-filename (thunk db key exclusive-p)
  (multiple-value-bind (filename key) (filename db key)
    (if (getlock db key exclusive-p)
        (funcall thunk filename key)
        (with-global-lock (key)
          (funcall thunk filename key)))))

(defmethod getlock ((db fsdb) key &optional exclusive-p)
  (let ((lock (gethash key (fsdb-locks db))))
    (and lock
         (progn
           (when (and exclusive-p
                      (not (lock-exclusive-p (lock-global-lock lock))))
             (upgrade-to-exclusive (lock-global-lock lock)))
           lock))))

(defmethod dbput ((db fsdb) key value)
  (with-fsdb-filename (db filename key t)
    (file-put-contents filename value)))

(defmethod dbget ((db fsdb) key)
  (with-fsdb-filename (db filename key)
    (file-get-contents filename)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http:;;;www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
