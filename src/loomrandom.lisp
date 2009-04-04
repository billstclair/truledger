; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cryptographically secure random number generation
;;;

(in-package :trubanc)

(defvar *urandom-stream* nil)
(defvar *use-urandom* t)

(defun urandom-stream ()
  (or *urandom-stream*
      (and *use-urandom*
          (or (setq *urandom-stream* (ignore-errors (open "/dev/urandom")))
              (setq *use-urandom* nil)))))

(defun urandom-bytes (num)
  "Return $num random bytes from /dev/urandom as a string"
  (when (< num 0)
    (error "Number of bytes must be non-negative"))
  (let ((stream (urandom-stream)))
    (with-output-to-string (s)
      (if stream
          (dotimes (i num) (write-char (read-char stream) s))
          (dotimes (i num) (write-char (code-char (random 256)) s))))))

(defun random-id ()
  "Return a random 128-bit location, as hex"
  (let ((res (bin2hex (urandom-bytes 16))))
    (if (>= (length res) 32)
        res
        (strcat (zero-string (- 32 (length res))) res))))

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
