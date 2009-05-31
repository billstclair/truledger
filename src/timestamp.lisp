; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; An ever-increasing timestamp that stays pretty close to the actual time
;;;

(in-package :trubanc)

(defclass timestamp ()
  ((lasttime :type string
             :initarg :lasttime
             :initform "0"
             :accessor timestamp-lasttime)))

(defparameter *time-offset*
  (- (encode-universal-time 0 0 0 1 1 1970)
     (encode-universal-time 0 0 0 1 1 1900)))

(defun get-unix-time ()
  (- (get-universal-time) *time-offset*))

(defmethod next ((timestamp timestamp) &optional
                 (lasttime (timestamp-lasttime timestamp)))
  "Get the next timestamp.
   This is the unix timestamp if > the last result.
   Otherwise, we add some fractions to make it bigger."
  (let ((time (bcadd (get-unix-time))))
    (cond ((<= (bccomp time lasttime) 0)
           (let ((pos (position #\. lasttime))
                 (n 2)
                 (inc 1))
             (cond ((not pos)
                    (let ((zeroes (make-string (1- n) :initial-element #\0)))
                      (setq time (strcat lasttime "." zeroes "1"))))
                   (t (let* ((fract (subseq lasttime (1+ pos)))
                             (fractlen (length fract))
                             (nfract (bcadd fract 1)))
                        (cond ((<= (length nfract) fractlen)
                               (let ((zeroes (make-string
                                             (- fractlen (length nfract))
                                             :initial-element #\0)))
                                 (setq time (strcat (subseq lasttime 0 pos)
                                                    "." zeroes nfract))))
                              (t (let ((l n))
                                   (loop
                                     (when (> l fractlen)
                                       (let ((zeros (zero-string (- l fractlen 1))))
                                         (setq time (strcat lasttime zeros "1"))
                                         (return)))
                                      (incf n inc)
                                      (incf l n)))))))))))
    (setf (timestamp-lasttime timestamp) time)))

(defun strip-fract (time)
  "Return the integer part of a time returned by (next) above"
  (let ((dotpos (position #\. time)))
    (if dotpos
        (subseq time 0 dotpos)
        time)))
