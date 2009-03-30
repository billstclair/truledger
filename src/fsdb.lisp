; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File System Database for Trubanc
;;;

(in-package :trubanc)

;; All put/get database implementations should extend db
(defclass db ()
  ())

(defun unimplemented-db-method (gf)
  (error "Unimplemented db method: ~s" gf))

(defgeneric db-get (db key)
  (:method ((db db) key)
    (declare (ignore key))
    (unimplemented-db-method 'db-get)))

(defgeneric db-put (db key value)
  (:method ((db db) key value)
    (declare (ignore key value))
    (unimplemented-db-method 'db-put)))

(defgeneric db-lock (db key)
  (:method ((db db) key)
    (declare (ignore key))
    (unimplemented-db-method 'db-lock)))
  
(defgeneric db-unlock (db lock)
  (:method ((db db) lock)
    (declare (ignore lock))
    (unimplemented-db-method 'db-unlock)))

(defgeneric db-contents (db &optional key)
  (:method ((db db) &optional key)
    (declare (ignore key))
    (unimplemented-db-method 'db-contents)))

(defgeneric db-subdir (db key)
  (:method ((db db) key)
    (declare (ignore key))
    (unimplemented-db-method 'db-subdir)))
  
;;;
;;; Implement the db protocol using the file system
;;;

(defclass fsdb (db)
  ((dir :type string
        :initarg :dir
        :accessor fsdb-dir)))

(defmethod print-object ((db fsdb) stream)
  (print-unreadable-object (db stream :type t)
    (format stream "~s" (fsdb-dir db))))

(defmethod initialize-instance :after ((db fsdb) &rest ignore)
  (declare (ignore ignore))
  (let* ((dir (namestring (truename (fsdb-dir db))))
         (idx (1- (length dir))))
    (when (and (>= idx 0) (eql #\/ (aref dir idx)))
      (setq dir (subseq dir 0 idx)))
    (setf (fsdb-dir db) dir)))

(defun normalize-key (key)
  (if (eql (aref key 0) #\/)
      (subseq key 1)
      key))

(defmethod db-filename ((db fsdb) key)
  (let ((key (normalize-key key)))
    (values (strcat (fsdb-dir db) "/" key)
            key)))

(defmacro with-fsdb-filename ((db filename key) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk (,filename ,key)
              (declare (ignorable ,key))
              ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-fsdb-filename #',thunk ,db ,key))))

(defun call-with-fsdb-filename (thunk db key)
  (multiple-value-bind (filename key) (db-filename db key)
    (with-file-locked (filename)
      (funcall thunk filename key))))

(defmethod db-put ((db fsdb) key value)
  (with-fsdb-filename (db filename key)
    (if (or (null value) (equal value ""))
        (delete-file filename)
        (file-put-contents filename value))))

(defmethod db-get ((db fsdb) key)
  (with-fsdb-filename (db filename key)
    (let ((res (file-get-contents filename)))
      (and (not (equal "" res))
           res))))

(defmethod db-lock ((db fsdb) key)
  (grab-file-lock (db-filename db key)))

(defmethod db-unlock ((db fsdb) lock)
  (release-file-lock lock))

(defun file-namestring-or-last-directory (path)
  (if (or (pathname-name path) (pathname-type path))
      (file-namestring path)
      (car (last (pathname-directory path)))))

;; This doesn't work. Directories come with "/" at the end, so
;; file-namestring returns "".
(defmethod db-contents ((db fsdb) &optional key)
  (let ((dir (directory (db-filename db (strcat key "/*.*"))
                        :directories t
                        :all nil)))
    (mapcar 'file-namestring-or-last-directory dir)))

(defmethod db-subdir ((db fsdb) key)
  (make-instance 'fsdb :dir (strcat (fsdb-dir db) "/" key)))
    
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
