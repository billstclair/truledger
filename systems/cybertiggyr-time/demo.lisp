;;; -*- Mode: Lisp -*-
;;;
;;; $Header: /home/gene/library/website/docsrc/pdl/RCS/demo.lisp,v 395.1 2008/04/20 17:25:47 gene Exp $
;;;
;;; Copyright (c) 2004 Gene Michael Stover.  All rights reserved.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 2.1 of the
;;; License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;;; USA

; requires cybertiggyr-time
; (import 'cybertiggyr-time:format-time)
; (import 'cybertiggyr-time:parse-time)

(in-package "CYBERTIGGYR-TIME")

(defvar *demo-lst*
  (list (format-time nil *format-time-iso8601-long*)
	"now"
	"today"
	"2004 05 30"
	"2004-05-30"
	"2004 May 30"
	"1980-jun-1T12:30:00 gmt"
	"1980-jun-1T12:30:00 est"
	"1980-jun-1T12:30:00 pst"
	"2000-jan-01T05:59:59+00:00"
	"2000-01-01T00:59:59 est"
	"1999-12-31T23:59:59 -6"
	"1999-12-31T22:29:59-07:30"
	"1999-12-31T21:59:59 -8"
	"1999-12-31T21:59:59-8:00"
	"Mar 4, 05"
	"3/4/05" ; dumb American date
	"19951025"))

(defun demo (&optional (lst *demo-lst*))
  (format t "~&~27@A & ~27@A \\\\ \\hline" "{\\bf input}" "{\\bf output}")
  (dolist (x lst)
    (format t "~&~27@A" x)
    (finish-output *standard-output*)
    (format t " & ~27@A" (format-time nil *format-time-iso8601-long*
				   (parse-time x)))
    (format t " \\\\ \\hline"))
  'demo)

;;; --- end of file ---
