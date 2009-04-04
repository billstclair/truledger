; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CCL interface functions 
;;;

(in-package :trubanc)

(defun run-program (program args &key input output)
  (ccl:run-program program args :input input :output output))

(defun quit (&optional (exit-status 0))
  (ccl:quit exit-status))

(defun df (x) (disassemble x))

(defun add-startup-function (f)
  (pushnew f ccl:*lisp-startup-functions*))

;;;
;;; Locking.
;;;
;;; A read-write-lock may NOT be locked recursively.
;;; A regular lock MAY be locked recursively.
;;;

(defun make-lock (&optional name)
  (ccl:make-lock name))

(defun grab-lock (lock)
  (ccl:grab-lock lock))

(defun release-lock (lock)
  (ccl:release-lock lock))

(defmacro with-lock-grabbed ((lock &optional (whostate "Lock")) &body body)
  `(ccl:with-lock-grabbed (,lock ,whostate) ,@body))

(defun make-read-write-lock ()
  (ccl:make-read-write-lock))

(defun read-lock-rwlock (rwlock)
  (ccl::read-lock-rwlock rwlock))

(defun write-lock-rwlock (rwlock)
  (ccl::write-lock-rwlock rwlock))

(defun unlock-rwlock (rwlock)
  (ccl::unlock-rwlock rwlock))

(defmacro with-read-lock ((lock) &body body)
  `(ccl:with-read-lock (,lock) ,@body))

(defmacro with-write-lock ((lock) &body body)
  `(ccl:with-write-lock (,lock) ,@body))

;;;
;;; Processes
;;;

(defun current-process ()
  ccl:*current-process*)

(defun all-processes ()
  (ccl:all-processes))

(defun process-run-function (name function &rest args)
  (declare (dynamic-extent args))
  (apply #'ccl:process-run-function name function args))

(defun process-wait (whostate function &rest args)
  (declare (dynamic-extent args))
  (apply #'ccl:process-wait whostate function args))

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
