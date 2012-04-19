; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Templates for Truledger
;;;

(in-package :truledger)

(defvar *template-db* nil)
(defvar *template-custom-db* nil)
(defvar *template-hash* nil)

(defun make-template-dbs ()
  (setf *template-db* (make-fsdb "templates")
        *template-custom-db* (make-fsdb "templates-custom")))

(make-template-dbs)
(add-startup-function 'make-template-dbs)

(defun validate-template-keys (key keys)
  (flet ((pred (x) (and x (search "../" x))))
    (when (or (funcall #'pred key)
              (some #'pred keys))
      (error "Parent dirs not allowed in template-get"))))

(defmethod template-get ((db fsdb) key &rest keys)
  (validate-template-keys key keys)
  (apply #'db-get db key keys))

(defmethod (setf template-get) (value (db fsdb) key &rest keys)
  (validate-template-keys key keys)
  (apply #'(setf db-get) value db key keys))

;; This is so we can put the default templates in the distributed image
(defun load-template-directory (&key
                                (db *template-db*)
                                (custom-db *template-custom-db*))
  (when (stringp db) (setf db (make-fsdb db)))
  (when (stringp custom-db) (setf custom-db (make-fsdb custom-db)))
  (let ((hash (make-hash-table :test #'equal)))
    (labels ((traverse (db base)
               (dolist (key (db-contents db base))
                 (let ((path (append-db-keys base key)))
                   (cond ((db-dir-p db path)
                          (traverse db path))
                         (t (setf (gethash path hash) (db-get db path))))))))
      (traverse db "")
      (when custom-db
        (traverse custom-db "")))
    (setf *template-hash* hash)))

(defun fill-and-print-to-string (template plist)
  (with-output-to-string (s)
    (let ((template:*string-modifier* 'identity))
      (template:fill-and-print-template template plist :stream s))))

(defun load-template (key &key
                      (template-custom-db *template-custom-db*)
                      (template-db *template-db*)
                      wired-in-p)
  (let* ((hash *template-hash*)
         (not-wired-in-p (not (and hash wired-in-p))))
    (or (and not-wired-in-p
             template-custom-db
             (template-get template-custom-db key))
        (and hash (gethash key hash))
        (and not-wired-in-p
             template-db
             (template-get template-db key)))))

(defun expand-template (plist key &key
                        (template-custom-db *template-custom-db*)
                        (template-db *template-db*))
  (fill-and-print-to-string
   (load-template key
                  :template-custom-db template-custom-db
                  :template-db template-db)
   plist))

(defparameter *client-properties-key*
  "client-properties.sexp")

(defvar *client-properties* nil)

(defun read-client-properties ()
  (ignore-errors
    (let ((*read-eval* nil))
      (read-from-string
       (load-template *client-properties-key* :wired-in-p t)))))

(defun client-properties (&optional reload-p)
  (or (and (not reload-p) *template-hash* *client-properties*)
      (setf *client-properties* (read-client-properties))))

(defun get-client-property (property)
  (getf (client-properties) property))

(defun get-highlighted-asset-color (assetid)
  (let ((alist (get-client-property :highlighted-assets)))
    (second (assocequal assetid alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2011 Bill St. Clair
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
