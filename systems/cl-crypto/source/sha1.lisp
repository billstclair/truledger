(in-package :cl-crypto)

;;;
;;; Local declaims
;;;


;;;
;;; Constants
;;;

;; SHA-1 uses a 160-bit (20-byte) message size
(define-constant +sha1-message-size+ 20)

;; Constants for each set of 20 rounds (80 total)
(define-constant +sha1-rcons+
    '(#x5A827999 #x6ED9EBA1 #x8F1BBCDC #xCA62C1D6))

;; Initial state values 
(define-constant +sha1-initial-state-vals+
    '(#x67452301 #xEFCDAB89 #x98BADCFE #x10325476 #xC3D2E1F0))


;;;
;;; Test values TODO - refactor
;;;

;; Test message is "abc"
(define-constant +sha1-test-message+ "abc")

(define-constant +sha1-padded-test-mesage+
    (make-array 16
		:element-type 'uint-32
		:initial-contents
		'(#x61626380 #x00000000 #x00000000 #x00000000
		  #x00000000 #x00000000 #x00000000 #x00000000
		  #x00000000 #x00000000 #x00000000 #x00000000
		  #x00000000 #x00000000 #x00000000 #x00000018)))

;;;
;;; Classes
;;;

(defclass sha1-state ()
  ((num-rounds
    :accessor num-rounds-of
    :initarg :num-rounds)
   (state
    :type list
    :accessor state-of
    :initarg :state)
   (schedule
    :type (simple-array uint-32)
    :accessor schedule-of
    :initarg :schedule)))
  


;;;
;;; SHA-1 implementation
;;;

(defun ch (x y z)
  (logxor (logand x y)
	  (logand (logand #xFFFFFFFF (lognot x))
		  z)))

(defun parity (x y z)
  (logxor x y z))

(defun maj (x y z)
  (logxor (logand x y) (logand x z) (logand y z)))

(defun mod+ (&rest values)
  (logand #xFFFFFFFF (apply #'+ values)))



(defun sha1-get-initial-state ()
  (make-instance 'sha1-state
		 :num-rounds 0
		 :state +sha1-initial-state-vals+
		 :schedule (make-array
			    80
			    :element-type 'uint-32)))

(defun sha1-pad-message (state m)
  (setf (num-rounds-of state) 1))

(defun sha1-expand-schedule (state m)
  (dotimes (i 16)
    (setf (aref (schedule-of state) i) (aref m i)))
  (for (i 16 79)
    (setf (aref (schedule-of state) i)
	  (rot-uint-32-L
	   (logxor (aref (schedule-of state) (- i 3))
		   (aref (schedule-of state) (- i 8))
		   (aref (schedule-of state) (- i 14))
		   (aref (schedule-of state) (- i 16)))
	   1))))

(defun sha1-process-message (state m &optional verbose-p)
  (sha1-pad-message state m)
  (sha1-expand-schedule state m)
  (dotimes (i (num-rounds-of state))
    (sha1-process-round state verbose-p)))

(defun sha1-process-round (state &optional verbose-p)
  (destructuring-bind (a b c d e) (state-of state)
    (let ((chunk-num 0) (tmp 0))
      (flet ((foo (fn rcon)
	       (do* ((i (* chunk-num 20) (1+ i))
		     (end (+ i 20)))
		    ((= i end) (incf chunk-num))
		 (setq tmp (mod+ (rot-uint-32-L a 5)
				 (funcall fn b c d)
				 e
				 rcon
				 (aref (schedule-of state) i))
		       e d
		       d c
		       c (rot-uint-32-L b 30)
		       b a
		       a tmp)
                 (when verbose-p
                   (format
                    t "~%~2,'0d:~2,'0d  ~8,'0X ~8,'0X ~8,'0X ~8,'0X ~8,'0X"
                    chunk-num i a b c d e)))))
	(declare (dynamic-extent #'foo))
	(map nil #'foo '(ch parity maj parity) +sha1-rcons+))
      (setf (state-of state)
	    (mapcar #'mod+ (state-of state) (list a b c d e))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2010 TSC AG, Postfach 73, CH 6314 Unterageri, Switzerland
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
