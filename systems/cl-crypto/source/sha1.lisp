(in-package :cl-crypto)

;;;
;;; Local declaims
;;;


;;;
;;; Constants
;;;

;; Word size in bytes
(define-constant +word-size+ 4)

;; Constants for each set of 20 rounds (80 total)
(define-constant +sha1-rcons+
    '(#x5A827999 #x6ED9EBA1 #x8F1BBCDC #xCA62C1D6))

;; SHA-1 uses a 160-bit (20-byte) message size    
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
  ((round-num
    :accessor round-num-of
    :initarg :round-num)
   (state
    :type (simple-array uint-32)
    :accessor state-of
    :initarg :state)
   (schedule
    :type (simple-array uint-32)
    :accessor schedule-of
    :initarg :schedule)))
  


;;;
;;; SHA-1 implementation
;;;

(defun get-initial-state ()
  (make-instance 'sha1-state
		 :round-num 0
		 :state (make-array
			 5
			 :element-type 'uint-32
			 :initial-contents
			 +sha1-initial-state-vals+)
		 :schedule (make-array
			    80
			    :element-type 'uint-32)))

				    

(defun ch (x y z)
  (logxor (logand x y)
	  (logand (logand #xFFFFFFFF (lognot x))
		  z)))

(defun parity (x y z)
  (logxor x y z))

(defun maj (x y z)
  (logxor (logand x y) (logand x z) (logand y z)))


(defun expand-schedule (state m)
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
  (expand-schedule state m)
  (let ((a (aref (state-of state) 0))
	(b (aref (state-of state) 1))
	(c (aref (state-of state) 2))
	(d (aref (state-of state) 3))
	(e (aref (state-of state) 4))
	(f 'ch)
	(rcon (car +sha1-rcons+))
	(tmp nil))
    (for (i 0 19)
      (setq tmp (logand #xFFFFFFFF
			(+ (rot-uint-32-L a 5)
			   (funcall f b c d)
			   e
			   rcon
			   (aref (schedule-of state) i))))
      (setq e d)
      (setq d c)
      (setq c (rot-uint-32-L b 30))
      (setq b a)
      (setq a tmp)
      (when verbose-p
        (format t "~%~2,'0d  ~8,'0X ~8,'0X ~8,'0X ~8,'0X ~8,'0X"
                i a b c d e)))
    
    (setq f 'parity)
    (setq rcon (second +sha1-rcons+))
    (for (i 20 39)
      (setq tmp (logand #xFFFFFFFF
			(+ (rot-uint-32-L a 5)
			   (funcall f b c d)
			   e
			   rcon
			   (aref (schedule-of state) i))))
      (setq e d)
      (setq d c)
      (setq c (rot-uint-32-L b 30))
      (setq b a)
      (setq a tmp)
      (when verbose-p
        (format t "~%~2,'0d  ~8,'0X ~8,'0X ~8,'0X ~8,'0X ~8,'0X"
                i a b c d e)))

    (setq f 'maj)
    (setq rcon (third +sha1-rcons+))
    (for (i 40 59)
      (setq tmp (logand #xFFFFFFFF
			(+ (rot-uint-32-L a 5)
			   (funcall f b c d)
			   e
			   rcon
			   (aref (schedule-of state) i))))
      (setq e d)
      (setq d c)
      (setq c (rot-uint-32-L b 30))
      (setq b a)
      (setq a tmp)
      (when verbose-p
        (format t "~%~2,'0d  ~8,'0X ~8,'0X ~8,'0X ~8,'0X ~8,'0X"
                i a b c d e)))

    (setq f 'parity)
    (setq rcon (fourth +sha1-rcons+))
    (for (i 60 79)
      (setq tmp (logand #xFFFFFFFF
			(+ (rot-uint-32-L a 5)
			   (funcall f b c d)
			   e
			   rcon
			   (aref (schedule-of state) i))))
      (setq e d)
      (setq d c)
      (setq c (rot-uint-32-L b 30))
      (setq b a)
      (setq a tmp)
      (when verbose-p
        (format t "~%~2,'0d  ~8,'0X ~8,'0X ~8,'0X ~8,'0X ~8,'0X"
                i a b c d e)))
    (setf (aref (state-of state) 0)
	  (logand #xFFFFFFFF (+ (aref (state-of state) 0) a))
	  (aref (state-of state) 1)
	  (logand #xFFFFFFFF (+ (aref (state-of state) 1) b))
	  (aref (state-of state) 2)
	  (logand #xFFFFFFFF (+ (aref (state-of state) 2) c))
	  (aref (state-of state) 3)
	  (logand #xFFFFFFFF (+ (aref (state-of state) 3) d))
	  (aref (state-of state) 4)
	  (logand #xFFFFFFFF (+ (aref (state-of state) 4) e)))))
    
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
