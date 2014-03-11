; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cryptographically secure random number generation
;;;

(in-package :truledger)

(defvar *use-urandom* t)
(defvar *use-random* t)

#+windows
(defvar *windows-random-stream*
  (cl-crypto:make-random-stream))

#+windows
(add-startup-function 'cl-crypto:initialize-windows-crypto-library)

(defun urandom-stream ()
  (and *use-urandom*
       (or (ignore-errors
	     #-windows (open "/dev/urandom" :external-format :ISO-8859-1)
	     #+windows *windows-random-stream*)
           (setq *use-urandom* nil))))

(defun random-stream ()
  (and *use-random*
       (or (ignore-errors
	     #-windows (open "/dev/random" :external-format :ISO-8859-1)
	     #+windows *windows-random-stream*)
           (setq *use-random* nil))))

(defun random-bytes (num &optional (stream (random-stream)))
  "Return NUM random bytes from /dev/random as a string"
  (when (< num 0)
    (error "Number of bytes must be non-negative"))
  (unwind-protect
       (with-output-to-string (s)
         (if stream
             (dotimes (i num) (write-char (read-char stream) s))
             (dotimes (i num) (write-char (code-char (random 256)) s))))
    (when stream (close stream))))

(defun urandom-bytes (num)
  "Return $num random bytes from /dev/urandom as a string"
  (random-bytes num (urandom-stream)))

(defun random-array (num &optional (stream (random-stream)))
  "Return $num random bytes from /dev/random as a string
   as an (unsigned-byte 8) array"
  (when (< num 0)
    (error "Number of bytes must be non-negative"))
  (unwind-protect
       (let ((res (make-array num :element-type '(unsigned-byte 8))))
         (if stream
             (dotimes (i num)
               (setf (aref res i) (char-code (read-char stream))))
             (dotimes (i num)
               (setf (aref res i) (random 256))))
         res)
    (when stream (close stream))))

(defun urandom-array (num)
  (random-array num (urandom-stream)))

(defun random-id ()
  "Return a random 128-bit location, as hex"
  (let ((res (bin2hex (urandom-bytes 16))))
    (if (>= (length res) 32)
        res
        (strcat (zero-string (- 32 (length res))) res))))

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
