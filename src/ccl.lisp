; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CCL interface functions 
;;;

(in-package :truledger)

;; This noticeably speeds things up
(ccl:egc nil)

(defun run-program (program args &key input output (wait (not (eq output :stream))))
  (let ((proc (ccl:run-program program args :input input :output output :wait wait)))
    (if (eq output :stream)
        (ccl:external-process-output-stream proc)
        proc)))

(defun quit (&optional (exit-status 0))
  (ccl:quit exit-status))

(defun df (x) (disassemble x))

(defun arglist (x &optional include-bindings)
  (ccl:arglist x include-bindings))

(defun make-weak-hash-table ()
  (make-hash-table :test 'eq :weak t))

(defun gc ()
  (ccl:gc))

(defclass truledger-application (ccl::application)
  ())

(defun set-interactive-abort-process (&optional (process ccl:*current-process*))
  (check-type process (or ccl:process null))
  (setq ccl::*interactive-abort-process* process))

(defmacro invoking-debugger-hook-on-interrupt (&body body)
  `(let ((ccl::*invoke-debugger-hook-on-interrupt* t))
     ,@body))

(defun save-application (filename &rest rest &key
                         toplevel-function
                         init-file
                         error-handler
                         (application-class 'truledger-application)
                         clear-clos-caches
                         purify
                         impurify
                         prepend-kernel)
  (declare (ignore toplevel-function init-file error-handler
                   clear-clos-caches purify
                   impurify prepend-kernel))
  (apply 'ccl:save-application
         filename
         :application-class application-class
         rest))

(defun command-line-arguments ()
  ccl:*command-line-argument-list*)

(defun backtrace-string ()
  (let ((bt (ccl::backtrace-as-list))
        (i 0))
    (with-output-to-string (s)
      (dolist (b (cdr bt))              ;Don't print the backtrace-string frame
        (format s "~d:~{ ~a~}~%" i b)
        (incf i)))))

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
;;; Semaphores
;;;

(defun make-semaphore ()
  (ccl:make-semaphore))

(defun signal-semaphore (semaphore)
  (ccl:signal-semaphore semaphore))

(defun wait-on-semaphore (semaphore)
  (ccl:wait-on-semaphore semaphore))

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

(defun create-directory (dir &key mode)
  (if mode
      (ccl:create-directory dir :mode mode)
      (ccl:create-directory dir)))

(defun recursive-delete-directory (path &rest rest &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (apply #'ccl::recursive-delete-directory path rest))

(defun ensure-directory-pathname (path)
  (ccl::ensure-directory-pathname path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009-2010 Bill St. Clair
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
