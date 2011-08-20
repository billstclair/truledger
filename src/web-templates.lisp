; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Templates for Truledger
;;;

(in-package :truledger-client-web)

(defvar *template-db* (make-fsdb "templates"))

(defmethod template-get ((db fsdb) key &rest keys)
  (apply #'db-get db key keys))

(defmethod (setf template-get) (value (db fsdb) key &rest keys)
  (apply #'(setf db-get) value db key keys))

(defmethod template-get ((hash hash-table) key &rest keys)
  (gethash (apply #'append-db-keys key keys) hash))

(defmethod (setf template-get) (value (hash hash-table) key &rest keys)
  (setf (gethash (apply #'append-db-keys key keys) hash) value))

;; This is so we can put the default templates in the distributed image
(defun load-template-directory (&optional (db *template-db*))
  (when (stringp db) (setf db (make-fsdb db)))
  (let ((hash (make-hash-table :test #'equal)))
    (labels ((traverse (base)
               (dolist (key (db-contents db base))
                 (let ((path (append-db-keys base key)))
                   (cond ((db-dir-p db path)
                          (traverse path))
                         (t (setf (gethash path hash) (db-get db path))))))))
      (traverse ""))
    hash))

(defun fill-and-print-to-string (template plist)
  (with-output-to-string (s)
    (let ((template:*string-modifier* 'identity))
      (template:fill-and-print-template template plist :stream s))))

(defun expand-template (plist key &optional (template-db *template-db*))
  (unless template-db
    (setf template-db *template-db*))
  (let ((template (template-get template-db key)))
    (fill-and-print-to-string template plist)))

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
