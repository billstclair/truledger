; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Read-write locks
;;;

(in-package #:trubanc)

(defvar *file-locks-lock*
  (make-lock))

(defvar *file-locks*
  (make-hash-table :test 'equal))

(defclass file-lock ()
  ((syslock :initarg :syslock
            :accessor lock-syslock)
   (filename :initarg :filename
             :accessor lock-filename)
   (count :initform 0
          :accessor lock-count)
   (process-count :initform 0
                  :accessor lock-process-count)
   (process :initform nil
            :accessor lock-process)))

(defmethod print-object ((lock file-lock) stream)
  (print-unreadable-object (lock stream :type t)
    (format stream "~d:~d ~s ~s"
            (lock-process-count lock)
            (lock-count lock)
            (lock-filename lock)
            (lock-process lock))))

(defun grab-file-lock (filename)
  (let (lock)
    (with-lock-grabbed (*file-locks-lock*)
      (unless (setq lock (gethash filename *file-locks*))
        (setq lock (make-instance
                   'file-lock
                   :syslock (make-lock filename)
                   :filename filename))
        (setf (gethash filename *file-locks*) lock))
      (unless (eq (current-process) (lock-process lock))
        (incf (lock-count lock))
        (grab-lock (lock-syslock lock)))
      (incf (lock-process-count lock))
      (setf (lock-process lock) (current-process)))
    lock))

(defun release-file-lock (lock)
  (unless (eq (current-process) (lock-process lock))
    (error "Lock not owned: ~s" lock))
  (when (<= (decf (lock-process-count lock)) 0)
    (with-lock-grabbed (*file-locks-lock*)
      (setf (lock-process-count lock) 0
            (lock-process lock) nil)
      (when (<= (decf (lock-count lock)) 0)
        (remhash (lock-filename lock) *file-locks*))
      (release-lock (lock-syslock lock)))
    t))      

(defmacro with-file-locked ((filename) &body body)
  (let ((lock (gensym)))
    `(let ((,lock (grab-file-lock ,filename)))
       (unwind-protect
            (progn ,@body)
         (release-file-lock ,lock)))))

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
