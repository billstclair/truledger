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

;; A utility to change all instances of "bank" to "server" in file
;; and directory names in an old Trubanc database
(defun file-name-replace (dir &rest rest &key (from "bank") (to "server") verbose)
  (let* ((pattern (merge-pathnames "*.*" (ensure-directory-pathname dir)))
         (files (directory pattern :directories t :all nil))
         (fromlen (length from)))
    (dolist (file files)
      (when (ccl:directory-pathname-p file)
        (apply #'file-name-replace file rest)
        (setf file (cl-fad:pathname-as-file file)))
      (let ((name (pathname-name file)))
        (let ((pos (search from name :test #'string=)))
          (when pos
            (let* ((newname (concatenate
                             'string
                             (subseq name 0 pos)
                             to
                             (subseq name (+ pos fromlen))))
                   (newfile (merge-pathnames newname file)))
              (when verbose
                (format t "Renaming ~s to ~s~%"
                        file newfile))
              (rename-file file newfile))))))))
              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009-2011 Bill St. Clair
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
