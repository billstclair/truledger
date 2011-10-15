; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Templates for Truledger
;;;

(in-package :truledger)

(defvar *template-db* (make-fsdb "templates"))
(defvar *template-custom-db* (make-fsdb "templates-custom"))
(defvar *template-hash* nil)

(defmethod template-get ((db fsdb) key &rest keys)
  (apply #'db-get db key keys))

(defmethod (setf template-get) (value (db fsdb) key &rest keys)
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
                      (template-db *template-db*))
  (let* ((hash *template-hash*))
    (or (and template-custom-db
             (template-get template-custom-db key))
        (template-get template-db key)
        (and hash (gethash key hash)))))
  

(defun expand-template (plist key &key
                        (template-custom-db *template-custom-db*)
                        (template-db *template-db*))
  (fill-and-print-to-string
   (load-template key
                  :template-custom-db template-custom-db
                  :template-db template-db)
   plist))

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
