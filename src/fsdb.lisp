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

(defgeneric db-get (db key &rest more-keys)
  (:method ((db db) key &rest more-keys)
    (declare (ignore key more-keys))
    (unimplemented-db-method 'db-get)))

(defgeneric (setf db-get) (value db key &rest more-keys)
  (:method (value (db db) key &rest more-keys)
    (declare (ignore value key more-keys))
    (unimplemented-db-method '(setf db-get))))

(defun db-put (db key value)
  (setf (db-get db key) value))

(defgeneric db-lock (db key)
  (:method ((db db) key)
    (declare (ignore key))
    (unimplemented-db-method 'db-lock)))
  
(defgeneric db-unlock (db lock)
  (:method ((db db) lock)
    (declare (ignore lock))
    (unimplemented-db-method 'db-unlock)))

(defgeneric db-contents (db &rest keys)
  (:method ((db db) &rest keys)
    (declare (ignore keys))
    (unimplemented-db-method 'db-contents)))

(defgeneric db-subdir (db key)
  (:method ((db db) key)
    (declare (ignore key))
    (unimplemented-db-method 'db-subdir)))
  
;;;
;;; Implement the db protocol using the file system
;;;

(defun make-fsdb (dir)
  "Create an fsdb isstance for the given file system directory."
  (make-instance 'fsdb :dir dir))

(defclass fsdb (db)
  ((dir :type string
        :initarg :dir
        :accessor fsdb-dir)))

(defmethod print-object ((db fsdb) stream)
  (print-unreadable-object (db stream :type t)
    (format stream "~s" (fsdb-dir db))))

(defmethod initialize-instance :after ((db fsdb) &rest ignore)
  (declare (ignore ignore))
  (ignore-errors (create-directory (strcat (fsdb-dir db) "/")))
  (let* ((dir (remove-trailing-separator (namestring (truename (fsdb-dir db))))))
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

(defun %append-db-keys (key &optional more-keys)
  (if (null more-keys)
      key
      (let* ((len (+ (length key)
                     (reduce #'+ more-keys :key #'length)
                     (length more-keys)))
             (res (make-string len :element-type (array-element-type key)))
             (i -1))
        (dolist (str (cons key more-keys))
          (unless (eql i -1) (setf (aref res (incf i)) #\/))
          (dotimes (j (length str))
            (setf (aref res (incf i)) (aref str j))))
        res)))

(defun append-db-keys (key &rest more-keys)
  (declare (dynamic-extent more-keys))
  (apply '%append-db-keys key more-keys))

(defmethod db-get ((db fsdb) key &rest more-keys)
  (declare (dynamic-extent more-keys))
  (let ((key (%append-db-keys key more-keys)))
    (with-fsdb-filename (db filename key)
      (let ((res (file-get-contents filename)))
        (and (not (equal "" res))
             res)))))

(defmethod (setf db-get) (value (db fsdb) key &rest more-keys)
  (declare (dynamic-extent more-keys))
  (let ((key (%append-db-keys key more-keys)))
    (with-fsdb-filename (db filename key)
      (if (or (null value) (equal value ""))
          (when (probe-file filename) (delete-file filename))
          (file-put-contents filename value)))))

(defmethod db-lock ((db fsdb) key)
  (grab-file-lock (db-filename db key)))

(defmethod db-unlock ((db fsdb) lock)
  (release-file-lock lock))

(defmacro with-db-lock ((db key) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-db-lock #',thunk ,db ,key))))

(defun call-with-db-lock (thunk db key)
  (let ((lock (db-lock db key)))
    (unwind-protect
         (funcall thunk)
      (db-unlock db lock))))

(defun file-namestring-or-last-directory (path)
  (if (or (pathname-name path) (pathname-type path))
      (file-namestring path)
      (car (last (pathname-directory path)))))

(defmethod db-contents ((db fsdb) &rest keys)
  (let* ((key (if keys
                 (%append-db-keys (car keys) (append (cdr keys) '("*.*")))
                 "*.*"))
         (dir (directory (db-filename db key)
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
