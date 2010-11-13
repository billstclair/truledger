(in-package :truledger)

(defstruct read-write-lock
  (lock (truledger:make-lock))
  (readers 0)
  write-waiting-p
  (write-semaphore (truledger:make-semaphore)))

(defun read-lock-rwlock (lock)
  (loop
     (truledger:with-lock-grabbed ((read-write-lock-lock lock))
       (unless (read-write-lock-write-waiting-p lock)
         (incf (read-write-lock-readers lock))
         (return)))
     (truledger:process-wait
      "read-lock-rwlock"
      (lambda (lock)
        (not (read-write-lock-write-waiting-p lock)))
      lock)))

(defun read-unlock-rwlock (lock)
  (truledger:with-lock-grabbed ((read-write-lock-lock lock))
    (assert (not (eql 0 (read-write-lock-readers lock))))
    (when (eql 0 (decf (read-write-lock-readers lock)))
      (when (read-write-lock-write-waiting-p lock)
        (truledger:signal-semaphore
         (read-write-lock-write-semaphore lock))))))

(defun write-lock-rwlock (lock &optional reading-p)
  (truledger:with-lock-grabbed ((read-write-lock-lock lock))
    (assert (not (read-write-lock-write-waiting-p lock)))
    (setf (read-write-lock-write-waiting-p lock) t)
    (when reading-p
      (decf (read-write-lock-readers lock))))
  (let ((done nil))
    (unwind-protect
         (unless (eql 0 (read-write-lock-readers lock))
           (truledger:wait-on-semaphore
            (read-write-lock-write-semaphore lock))
           (setf done t))
      (unless done
        (setf (read-write-lock-write-waiting-p lock) nil)))))

(defun write-unlock-rwlock (lock &optional reading-p)
  (truledger:with-lock-grabbed ((read-write-lock-lock lock))
    (setf (read-write-lock-write-waiting-p lock) nil)
    (when reading-p
      (incf (read-write-lock-readers lock)))))

(defun unlock-rwlock (lock)
  (if (read-write-lock-write-waiting-p lock)
      (write-unlock-rwlock lock)
      (read-unlock-rwlock lock)))
              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2010 Bill St. Clair
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
