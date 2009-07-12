; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simple timer package
;;;

(in-package :trubanc)

(defvar *timer-lock*
  (make-lock "Timer"))

(defvar *timer-times* nil)

(defun timer (thunk &key delay time)
  (when delay (setq time (+ delay (get-universal-time))))
  (with-lock-grabbed (*timer-lock*)
    (let ((last nil)
          (next *timer-times*)
          (found (cons time thunk)))
      (loop
         (unless next (return))
         (when (< time (caar next)) (return)))
      (cond (last (setf (cdr last) (cons found next)))
            (t (push found *timer-times*)))))
  (values thunk time))

(defun cancel-timer (thunk)
  (with-lock-grabbed (*timer-lock*)
    (setq *timer-times*
          (delete thunk *timer-times* :key #'cdr :test #'eq))))

(defun timer-thread ()
  (loop
     (sleep 1)
     (let ((time (get-universal-time)))
       (loop
          (let ((found (and *timer-times*
                            (with-lock-grabbed (*timer-lock*)
                              (and (<= (caar *timer-times*) time)
                                   (pop *timer-times*))))))
            (unless found (return))
            (ignore-errors (funcall (cdr found))))))))

(defun start-timer-thread ()
  (process-run-function "Timer" #'timer-thread))

(defun startup-timer ()
  (setq *timer-times* nil)
  (start-timer-thread))

(add-startup-function 'startup-timer)

(start-timer-thread)
             
                                                        
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
