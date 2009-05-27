;;; -*- Mode: Lisp -*-
;;;
;;; $Header: /home/gene/library/website/docsrc/pdl/RCS/time.lisp,v 395.1 2008/04/20 17:25:47 gene Exp $
;;;
;;; Copyright (c) 2004, 2006 Gene Michael Stover.  All rights reserved.
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

(defpackage "CYBERTIGGYR-TIME"
  (:documentation "CyberTiggyr's Time-related library")
  (:use "COMMON-LISP"))
(in-package "CYBERTIGGYR-TIME")
(export '*default-day*)
(export '*default-hour*)
(export '*default-language*)
(export '*default-minute*)
(export '*default-month*)
(export '*default-recognizers*)
(export '*default-second*)
(export '*default-year*)
(export '*default-zone*)
(export '*format-time-date*)
(export '*format-time-full*)
(export '*format-time-iso8601-long*)
(export '*format-time-iso8601-short*)
(export '*format-time-time*)
(export 'format-time)
(export 'parse-time)
(export 'recognize-fmt)

(defvar *debug* nil)

(defvar *default-second* (constantly 0))
(proclaim '(type function *default-second*))

(defvar *default-minute* (constantly 0))
(proclaim '(type function *default-minute*))

(defvar *default-hour* (constantly 12)
  "Function which returns the hour to assume when there is no hour.
The default value of *DEFAULT-HOUR* is a function which returns noon,
which is 12.")
(proclaim '(type function *default-hour*))

(defvar *default-day*
  #'(lambda ()
      (multiple-value-bind
	  (ss mm hh dd) (decode-universal-time (get-universal-time))
	(declare (ignore ss mm hh))
	dd))
  "Function which returns the day to assume when there is no day.  The
default value of *DEFAULT-DAY* is a function which returns the current
day.")
(proclaim '(type function *default-day*))

(defvar *default-month*
  #'(lambda ()
      (multiple-value-bind
	  (ss mm hh dd mo) (decode-universal-time (get-universal-time))
	(declare (ignore ss mm hh dd))
	mo))
  "Function which returns the month to assume when there is no month.  The
default value of *DEFAULT-MONTH* is a function which returns the current
month.")
(proclaim '(type function *default-month*))

(defvar *default-year*
  #'(lambda ()
      (multiple-value-bind
	  (ss mm hh dd mo yy) (decode-universal-time (get-universal-time))
	(declare (ignore ss mm hh dd mo))
	yy))
  "Function which returns the year to assume when there is no year.  The
default value of *DEFAULT-YEAR* is a function which returns the current
year.")
(proclaim '(type function *default-year*))

(defstruct broken-time
  ;; Seconds.  Can be a fractional number of seconds, though when converted
  ;; to a Lisp Universal Time, you might loose the fractional part.
  (ss 0 :type number)

  (mm 0 :type integer)
  (hh 0 :type integer)
  (dd 0 :type integer)
  (mo 0 :type integer)
  (yr 0 :type integer)

  dow
  dst?
  zone

  ;; AM, PM, or o'clock flag.  Should be the symbol AM for AM,
  ;; the symbol PM for PM, the symbol OCLOCK for o'clock, or
  ;; nil for literal hours.
  ampm)

(defun create-broken (x)
  "Return a new BROKEN with members initialized from X.  X may be a
universal time or an association list."
  (etypecase x
    (number                             ; X is a universal time.
     (multiple-value-bind (ss mm hh dd mo yr dow dst? zone)
	 (decode-universal-time x)
       (make-broken-time :ss ss :mm mm :hh hh :dd dd :mo mo :yr yr
			 :dow dow :dst? dst? :zone zone :ampm nil)))
    (list                               ; X is an assoc-list
     (labels
	 ((moo (field default)
	       (or (cdr (assoc field x)) (funcall default))))
       (make-broken-time
	:ss   (moo      :second  *default-second*)
	:mm   (moo      :minute  *default-minute*)
	:hh   (moo        :hour  *default-hour*)
	:dd   (moo         :day  *default-day*)
	:mo   (moo       :month  *default-month*)
	:yr   (moo        :year  *default-year*)
	:dow  (moo :day-of-week  (constantly nil))
	:dst? (moo        :dst?  (constantly nil))
	:zone (moo        :zone  (constantly nil))
	:ampm (moo        :ampm  (constantly nil)))))))

(defvar *format-time-months*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash '(1 :english) ht) '("January" "Jan")
	  (gethash '(2 :english) ht) '("February" "Feb")
	  (gethash '(3 :english) ht) '("March" "Mar")
	  (gethash '(4 :english) ht) '("April" "Apr")
	  (gethash '(5 :english) ht) '("May" "May")
	  (gethash '(6 :english) ht) '("June" "Jun")
	  (gethash '(7 :english) ht) '("July" "Jul")
	  (gethash '(8 :english) ht) '("August" "Aug")
	  (gethash '(9 :english) ht) '("September" "Sep")
	  (gethash '(10 :english) ht) '("October" "Oct")
	  (gethash '(11 :english) ht) '("November" "Nov")
	  (gethash '(12 :english) ht) '("December" "Dec"))
    ht))

(defvar *format-time-weekdays*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash '(0 :english) ht) '("Monday"     "Mon")
	  (gethash '(1 :english) ht) '("Tuesday"    "Tue")
	  (gethash '(2 :english) ht) '("Wednesday"  "Wed")
	  (gethash '(3 :english) ht) '("Thursday"   "Thu")
	  (gethash '(4 :english) ht) '("Friday"     "Fri")
	  (gethash '(5 :english) ht) '("Saturday"   "Sat")
	  (gethash '(6 :english) ht) '("Sunday"     "Sun"))
    ht))

(defvar *format-time-fns* (make-hash-table :test #'equal))

(macrolet ((deffmt (key str fn)
	     `(setf (gethash ,key *format-time-fns*)
                    #'(lambda (broken language strm)
			(format strm ,str (funcall ,fn broken language))))))
  ;; Abbreviated weekday
  (deffmt "%a" "~A"
    #'(lambda (broken language)
	(second (gethash (list (broken-time-dow broken) language)
			 *format-time-weekdays*))))

  ;; Full weekday
  (deffmt "%A" "~A"
    #'(lambda (broken language)
	(first (gethash (list (broken-time-dow broken) language)
			*format-time-weekdays*))))

  ;; Abbreviated month
  (deffmt "%b" "~A"
    #'(lambda (broken language)
	(second
	 (gethash (list (broken-time-mo broken) language) *format-time-months*))))

  ;; Full month
  (deffmt "%B" "~A"
    #'(lambda (broken language)
	(first
	 (gethash (list (broken-time-mo broken) language) *format-time-months*))))

  ;; Day of month, two digits
  (deffmt "%d" "~2,'0D"
    #'(lambda (broken language)
	(declare (ignore language))
	(broken-time-dd broken)))

  ;; Hour, 00 to 23, two digits
  (deffmt "%H" "~2,'0D" #'(lambda (broken language)
			    (declare (ignore language))
			    (broken-time-hh broken)))

  ;; Hour, 01 to 12, two digits
  (deffmt "%I" "~2,'0D" #'(lambda (broken language)
			    (declare (ignore language))
			    (if (zerop (mod (broken-time-hh broken) 12))
				12
			      (mod (broken-time-hh broken) 12))))

  ;; Day of year.  Todo.  Use "%j" key.

  ;; Month as two-digit number
  (deffmt "%m" "~2,'0D" #'(lambda (broken language)
			    (declare (ignore language))
			    (broken-time-mo broken)))

  ;; Minute, two digits
  (deffmt "%M" "~2,'0D" #'(lambda (broken language)
			    (declare (ignore language))
			    (broken-time-mm broken)))

  ;; AM or PM.  Hard-coded to those two values, but should alter for the
  ;; language.  Do other languages divide the hours into more than English's
  ;; two, 12-hour groups?
  (deffmt "%p" "~A" #'(lambda (broken language)
			(declare (ignore language))
			(if (<= 1 (broken-time-hh broken) 12)
			    "AM"
			  "PM")))

  ;; Seconds.  Two digits.
  (deffmt "%S" "~2,'0D" #'(lambda (broken language)
			    (declare (ignore language))
			    (broken-time-ss broken)))

  ;; Year, two digits.  DON'T Todo.  Two-digit years are wrong.

  ;; Year.  Four digits.
  (deffmt "%Y" "~4D" #'(lambda (broken language)
			 (declare (ignore language))
			 (broken-time-yr broken)))

  ;; Time zone.  This should be language-dependant.  It should lookup from
  ;; a table.  I'll just print whatever Lisp decoded into the BROKEN-ZONE
  ;; for now, but later, I'll need to print something more useful.
  ;; According to ISO, it can be the number of hours ahead of GMT.  That
  ;; number depends on the time zone from the BROKEN time, & also on
  ;; Daylight Savings Time.  It's reasonable to trust that Common Lisp
  ;; set the DST flag correctly in the BROKEN time, but Common Lisp does
  ;; not give us information about the size of the DST offset, so we'll
  ;; assume one hour.  (This is yet another reason that Daylight Savings
  ;; time is Daylight Stupid time.)
  (deffmt "%Z" "~@D" #'(lambda (broken language)
			    (declare (ignore language))
			    (- 0
			       (broken-time-zone broken)
			       (if (broken-time-dst? broken) -1 0)))))

(labels
    ;; end-of-token returns true when the next character is a % or we're
    ;; at end of input.
    ((end-of-token (strm)
		   (or (eq (peek-char nil strm nil strm) strm)
		       (eql (peek-char nil strm nil strm) #\%)))
     ;; Next-token returns the next token, whether it is a two-char token
     ;; beginning with % or all characters up to but excluding the next
     ;; % or the end of input.
     (next-token (strm)
		 (cond ((eq (peek-char nil strm nil strm) strm)
			;; End of input.
			strm)
		       ((eql (peek-char nil strm nil strm) #\%)
			;; Percent character.  So the token is this %
			;; character & the character which follows it.
			(coerce (make-array
				 2
				 :element-type 'character
				 :initial-contents (list
						    (read-char strm)
						    (read-char strm)))
				'string))
		       (t
			;; The next character is not %, so the next token
			;; is all characters until the next % or the end
			;; of input.
			(do ((lst () (cons (read-char strm) lst)))
			    ((end-of-token strm)
			     (coerce (nreverse lst) 'string)))))))
  (defun convert-fmt-string-to-list (fmt)
    "Given a FMT string for FORMAT-TIME, return a list of substrings parsed
from the FMT string."
    (with-input-from-string (strm fmt)
      (do ((lst () (cons (next-token strm) lst)))
	  ((eq (peek-char nil strm nil strm) strm)
	   (nreverse lst))))))

(defvar *default-language* :english)
(defvar *default-zone* nil)

(defun format-time (strm fmt
		      &optional
		      (ut (get-universal-time))
		      (zone *default-zone*)
		      (language *default-language*))
  (declare (type number ut) (type symbol language))
  (assert (or (eq t strm) (eq nil strm) (output-stream-p strm)))
  (cond ((null strm)
	 ;; When STRM is NIL, we write the output to a new string &
	 ;; return that.  Easy way to accomplish that is recursively.
	 (with-output-to-string (x)
	   (format-time x fmt ut zone language)))
	((eq t strm)
	 ;; When STRM is T, we write to standard output.
	 (format-time *standard-output* fmt ut zone language))
	((null fmt)
	 ;; FMT is the empty list, so we don't do anything.  There's
	 ;; nothing to output.
	 nil)
	((stringp fmt)
	 ;; Need to convert FMT from a string to a list that describes
	 ;; the output, then process the list recursively.
	 (format-time strm (convert-fmt-string-to-list fmt) ut zone language))
	((and (consp fmt) (gethash (first fmt) *format-time-fns*))
	 ;; FMT is a list, & its FIRST is in the table of functions.  So we
	 ;; use the associated function, then process the rest of FMT
	 ;; recursively.
	 (let ((fn (gethash (car fmt) *format-time-fns*)))
	   (declare (type function fn))
	   (funcall fn (create-broken ut) language strm))
	 (format-time strm (rest fmt) ut language zone))
	((consp fmt)
	 ;; FMT is a list, but its FIRST is not in the table, so we print
	 ;; the FIRST, the recurse on the REST.
	 (format strm "~A" (first fmt))
	 (format-time strm (rest fmt) ut language zone))
	(t
	 ;; Whatever FMT is, we don't know how to deal with it explicitly,
	 ;; so we output it verbatim.
	 (format strm "~A" fmt))))

(defvar *format-time-iso8601-short* '("%Y" "%m" "%d" "T" "%H" "%M" "%S" " " "%Z")
  "Format list for FORMAT-TIME to print a date-&-time in the compact
ISO 8061 format.  It's compact because it's all numbers (as required
by the ISO format), & there are no field separators except for the T
between the date & the time.")

(defvar *format-time-iso8601-long*
  '("%Y" "-" "%m" "-" "%d" "T" "%H" ":" "%M" ":" "%S" " " "%Z")
  "Format list for FORMAT-TIME to print a date-&-time in the verbose
ISO 8061 format.  It's verbose because it separates the fields
of the date with -, fields of the time with :, & the date from
the time with T.  So it is arguably human-readable.")

(defvar *format-time-date* '("%d" " " "%b" " " "%Y")
  "Format list for FORMAT-TIME to print a date in a compact, human readable
format.  It's the day-of-month, abbreviated month name, & the year.")

(defvar *format-time-time* '("%H" ":" "%M" " " "%Z")
  "Format list for FORMAT-TIME to print a human-readable time.  The hours
are on a 24-hour clock.")

(defvar *format-time-full*
  '("%A" ", " "%Y" " " "%B" " " "%d" ", " "%H" ":" "%M" " " "%Z")
  "It's like ISO format except that it's supposed to be readable by
humans.")

(defun end-of-stream? (strm)
  "Return true if STRM is at its end.  Does not consume characters.  STRM
is a character input stream."
  (eq (peek-char nil strm nil strm) strm))

(defun xdigit? (x)
  "Return true if X is a character AND is a digit."
  (and (characterp x) (digit-char-p x)))

(defun normalize-hour (broken)
  (cond ((eq (broken-time-ampm broken) :am)
	 (broken-time-hh broken))
	((eq (broken-time-ampm broken) :pm)
	 (mod (+ (broken-time-hh broken) 12) 24))
	((eq (broken-time-ampm broken) :oclock)
	 (broken-time-hh broken))
	(t
	 (broken-time-hh broken))))

(defun normalize-broken (x)
  "Given a BROKEN-TIME, some components of
which may be missing, some of which may be screwy -- like a 2-digit
year, this function inserts all missing components, possibly performs
some other adjustments, & returns a new BROKEN-TIME.  An exception is
time-zone; if it's missing, it won't be inserted."
  (make-broken-time
   :ss (broken-time-ss x)
   :mm (broken-time-mm x)
   :hh (normalize-hour x)
   :dd (broken-time-dd x)
   :mo (broken-time-mo x)
   :yr (broken-time-yr x)
   :zone (broken-time-zone x)))

(defun broken-to-ut (x)
  "Given a BROKEN time structure, conver them
to a universal time & return the universal time.  All of the date-&-time
components must be present in the BROKEN time."
  ;; Notice how we handle the time zone.  First, it must already be a
  ;; number of hours or NIL.  Second, the time zone in the BROKEN-TIME is
  ;; the difference, in hours, between GMT & the time zone of the
  ;; BROKEN-TIME, while ENCODE-UNIVERSAL-TIME represents time zones in an
  ;; opposite way.  So we must negate the time zone.
  (let ((y (normalize-broken x)))
    (encode-universal-time (broken-time-ss y)
			   (broken-time-mm y)
			   (broken-time-hh y)
			   (broken-time-dd y)
			   (broken-time-mo y)
			   (broken-time-yr y)
			   (if (broken-time-zone y)
			       (- (broken-time-zone y))
			     nil))))

(defvar *default-day* #'(lambda ()
			  (fourth
			   (multiple-value-list
			       (decode-universal-time
				(get-universal-time)))))
  "Function which returns the day of month to assume when there is no
day of month.  The default value of *DEFAULT-DAY* is a function which
returns today's day of month.")

(proclaim '(type function *default-day*))
  
(defvar *default-month* #'(lambda ()
			    (fifth
			     (multiple-value-list
				 (decode-universal-time
				  (get-universal-time)))))
  "Function which returns the month to assume when there is no
month.  The default value of *DEFAULT-MONTH* is a function which
returns the current month.")

(proclaim '(type function *default-month*))

(defvar *default-year* #'(lambda ()
			   (sixth
			    (multiple-value-list
				(decode-universal-time
				 (get-universal-time)))))
  "Function which returns the year to assume when there is no
year.  The default value of *DEFAULT-YEAR* is a function which
returns the current year.")

(proclaim '(type function *default-year*))
  
(defun next-fn (strm fn)
  "Consume & collect characters from STRM as long as they satisfy
FN.  FN is a function of one argument which should be a character."
  (declare (type function fn))
  (labels ((xend-p (strm)
		   (or (eq (peek-char nil strm nil strm) strm)
		       (not (funcall fn (peek-char nil strm nil strm))))))
    (if (xend-p strm)
	;; Stream is already at end.
	nil
      (do ((lst () (cons (read-char strm) lst)))
	  ((xend-p strm)
	   (coerce (nreverse lst) 'string))))))

(defun next-number (strm)
  "Consume characters from STRM as long as they are digits.  Then
convert to a number & return the number."
  (let ((x (next-fn strm #'(lambda (ch)
			     (or (digit-char-p ch)
				 (char-equal ch #\.))))))
    (and x (car (multiple-value-list (read-from-string x))))))

(defun next-word (strm)
  "Consume & collect characters from STRM as long as they are alphanumeric.
Converts all characters to upper case. Returns the token as a string.
Return NIL if the stream is already at the end when you call this function."
  (let ((x (next-fn strm #'(lambda (ch)
			     (or (alpha-char-p ch)
				 (char-equal ch #\'))))))
    (and x (string-upcase x))))

(defun next-token (strm)
  ;; Discard white-space.
  (peek-char t strm nil strm)
  (cond ((eq (peek-char nil strm nil strm) strm)
	 ;; End of input
	 nil)
	((digit-char-p (peek-char nil strm nil strm))
	 (next-number strm))
	((alpha-char-p (peek-char nil strm nil strm))
	 (next-word strm))
	((member (peek-char nil strm nil strm) '(#\, #\: #\- #\+))
	 (coerce (list (read-char strm)) 'string))
	(t
	 ;; This character is a token unto itself.
	 (read-char strm))))

(defun tokenize (str)
  "Return a list of tokens.  Where possible & convenient, tokens are
converted to symbols & numbers.  Otherwise, tokens are strings or
single characters, always upper case."
  (with-input-from-string (strm str)
    (do ((  lst                 ()   (cons token lst))
	 (token  (next-token strm)  (next-token strm)))
	((null token)
	 (nreverse lst)))))

(defun is-second? (x)
  (and (numberp x) (<= 0 x 59)))

(defun is-minute? (x)
  (and (numberp x) (<= 0 x 59)))

(defun is-hour? (x)
  (and (numberp x) (<= 0 x 24)))

(defun is-day? (x)
  (and (numberp x) (<= 1 x 31)))

(let ((ht (make-hash-table :test #'equal)))
  (setf (gethash           1 ht)    1
	(gethash           2 ht)    2
	(gethash           3 ht)    3
	(gethash           4 ht)    4
	(gethash           5 ht)    5
	(gethash           6 ht)    6
	(gethash           7 ht)    7
	(gethash           8 ht)    8
	(gethash           9 ht)    9
	(gethash          10 ht)   10
	(gethash          11 ht)   11
	(gethash          12 ht)   12
	(gethash   "january" ht)    1
	(gethash  "february" ht)    2
	(gethash     "march" ht) 3
	(gethash     "april" ht) 4
	(gethash       "may" ht) 5
	(gethash      "june" ht) 6
	(gethash      "july" ht) 7
	(gethash    "august" ht) 8
	(gethash "september" ht) 9
	(gethash   "october" ht) 10
	(gethash  "november" ht) 11
	(gethash  "december" ht) 12
	(gethash       "jan" ht) 1
	(gethash       "feb" ht) 2
	(gethash       "mar" ht) 3
	(gethash       "apr" ht) 4
	;; (gethash      "may" ht) 5
	(gethash       "jun" ht) 6
	(gethash       "jul" ht) 7
	(gethash       "aug" ht) 8
	(gethash       "sep" ht) 9
	(gethash       "oct" ht) 10
	(gethash       "nov" ht) 11
	(gethash       "dec" ht) 12)
  (defun make-month (x)
    (car (multiple-value-list
	     (if (stringp x)
		 (gethash (string-downcase x) ht)
	       (gethash x ht))))))

(defun is-year? (x)
  (numberp x))

(defvar *zones*
  (let ((ht (make-hash-table :test #'equal)))
    (setf
     (gethash "+0" ht) 0
     (gethash "+00" ht) 0
     (gethash "+0000" ht) 0
     (gethash "+0030" ht) (/ 30 60)
     (gethash "+00:00" ht) 0
     (gethash "+00:30" ht) (/ 30 60)
     (gethash "+01" ht) 1
     (gethash "+0100" ht) 1
     (gethash "+0130" ht) (+ 1 (/ 30 60))
     (gethash "+01:00" ht) 1
     (gethash "+01:30" ht) (+ 1 (/ 30 60))
     (gethash "+02" ht) 2
     (gethash "+0200" ht) 2
     (gethash "+0230" ht) (+ 2 (/ 30 60))
     (gethash "+02:00" ht) 2
     (gethash "+02:30" ht) (+ 2 (/ 30 60))
     (gethash "+03" ht) 3
     (gethash "+0300" ht) 3
     (gethash "+0330" ht) (+ 3 (/ 30 60))
     (gethash "+03:00" ht) 3
     (gethash "+03:30" ht) (+ 3 (/ 30 60))
     (gethash "+04" ht) 4
     (gethash "+0400" ht) 4
     (gethash "+0430" ht) (+ 4 (/ 30 60))
     (gethash "+04:00" ht) 4
     (gethash "+04:30" ht) (+ 4 (/ 30 60))
     (gethash "+05" ht) 5
     (gethash "+0500" ht) 5
     (gethash "+0530" ht) (+ 5 (/ 30 60))
     (gethash "+05:00" ht) 5
     (gethash "+05:30" ht) (+ 5 (/ 30 60))
     (gethash "+06" ht) 6
     (gethash "+0600" ht) 6
     (gethash "+0630" ht) (+ 6 (/ 30 60))
     (gethash "+06:00" ht) 6
     (gethash "+06:30" ht) (+ 6 (/ 30 60))
     (gethash "+07" ht) 7
     (gethash "+0700" ht) 7
     (gethash "+0730" ht) (+ 7 (/ 30 60))
     (gethash "+07:00" ht) 7
     (gethash "+07:30" ht) (+ 7 (/ 30 60))
     (gethash "+08" ht) 8
     (gethash "+0800" ht) 8
     (gethash "+0830" ht) (+ 8 (/ 30 60))
     (gethash "+08:00" ht) 8
     (gethash "+08:30" ht) (+ 8 (/ 30 60))
     (gethash "+09" ht) 9
     (gethash "+0900" ht) 9
     (gethash "+0930" ht) (+ 9 (/ 30 60))
     (gethash "+09:00" ht) 9
     (gethash "+09:30" ht) (+ 9 (/ 30 60))
     (gethash "+0:30" ht) (/ 30 60)
     (gethash "+1" ht) 1
     (gethash "+10" ht) 10
     (gethash "+1000" ht) 10
     (gethash "+1030" ht) (+ 10 (/ 30 60))
     (gethash "+10:00" ht) 10
     (gethash "+10:30" ht) (+ 10 (/ 30 60))
     (gethash "+11" ht) 11
     (gethash "+1100" ht) 11
     (gethash "+1130" ht) (+ 11 (/ 30 60))
     (gethash "+11:00" ht) 11
     (gethash "+11:30" ht) (+ 11 (/ 30 60))
     (gethash "+12" ht) 12
     (gethash "+1200" ht) 12
     (gethash "+1230" ht) (+ 12 (/ 30 60))
     (gethash "+12:00" ht) 12
     (gethash "+12:30" ht) (+ 12 (/ 30 60))
     (gethash "+2" ht) 2
     (gethash "+3" ht) 3
     (gethash "+4" ht) 4
     (gethash "+5" ht) 5
     (gethash "+6" ht) 6
     (gethash "+7" ht) 7
     (gethash "+8" ht) 8
     (gethash "+9" ht) 9
     (gethash "-0" ht) -0
     (gethash "-00" ht) -0
     (gethash "-0000" ht) -0
     (gethash "-0030" ht) (- -0 (/ 30 60))
     (gethash "-00:00" ht) -0
     (gethash "-00:30" ht) (- (/ 30 60))
     (gethash "-01" ht) -1
     (gethash "-0100" ht) -1
     (gethash "-0130" ht) (- -1 (/ 30 60))
     (gethash "-01:00" ht) -1
     (gethash "-01:30" ht) (- -1 (/ 30 60))
     (gethash "-02" ht) -2
     (gethash "-0200" ht) -2
     (gethash "-0230" ht) (- -2 (/ 30 60))
     (gethash "-02:00" ht) -2
     (gethash "-02:30" ht) (- -2 (/ 30 60))
     (gethash "-03" ht) -3
     (gethash "-0300" ht) -3
     (gethash "-0330" ht) (- -3 (/ 30 60))
     (gethash "-03:00" ht) -3
     (gethash "-03:30" ht) (- -3 (/ 30 60))
     (gethash "-04" ht) -4
     (gethash "-0400" ht) -4
     (gethash "-0430" ht) (- -4 (/ 30 60))
     (gethash "-04:00" ht) -4
     (gethash "-04:30" ht) (- -4 (/ 30 60))
     (gethash "-05" ht) -5
     (gethash "-0500" ht) -5
     (gethash "-0530" ht) (- -5 (/ 30 60))
     (gethash "-05:00" ht) -5
     (gethash "-05:30" ht) (- -5 (/ 30 60))
     (gethash "-06" ht) -6
     (gethash "-0600" ht) -6
     (gethash "-0630" ht) (- -6 (/ 30 60))
     (gethash "-06:00" ht) -6
     (gethash "-06:30" ht) (- -6 (/ 30 60))
     (gethash "-07" ht) -7
     (gethash "-0700" ht) -7
     (gethash "-0730" ht) (- -7 (/ 30 60))
     (gethash "-07:00" ht) -7
     (gethash "-07:30" ht) (- -7 (/ 30 60))
     (gethash "-08" ht) -8
     (gethash "-0800" ht) -8
     (gethash "-0830" ht) (- -8 (/ 30 60))
     (gethash "-08:00" ht) -8
     (gethash "-08:30" ht) (- -8 (/ 30 60))
     (gethash "-09" ht) -9
     (gethash "-0900" ht) -9
     (gethash "-0930" ht) (- -9 (/ 30 60))
     (gethash "-09:00" ht) -9
     (gethash "-09:30" ht) (- -9 (/ 30 60))
     (gethash "-0:00" ht) -0
     (gethash "-0:30" ht) (- (/ 30 60))
     (gethash "-1" ht) -1
     (gethash "-10" ht) -10
     (gethash "-1000" ht) -10
     (gethash "-1030" ht) (- -10 (/ 30 60))
     (gethash "-10:00" ht) -10
     (gethash "-10:30" ht) (- -10 (/ 30 60))
     (gethash "-11" ht) -11
     (gethash "-1100" ht) -11
     (gethash "-1130" ht) (- -11 (/ 30 60))
     (gethash "-11:00" ht) -11
     (gethash "-11:30" ht) (- -11 (/ 30 60))
     (gethash "-12" ht) -12
     (gethash "-1200" ht) -12
     (gethash "-1230" ht) (- -12 (/ 30 60))
     (gethash "-12:00" ht) -12
     (gethash "-12:30" ht) (- -12 (/ 30 60))
     (gethash "-1:00" ht) -1
     (gethash "-1:30" ht) (- -1 (/ 30 60))
     (gethash "-2" ht) -2
     (gethash "-2:00" ht) -2
     (gethash "-2:30" ht) (- -2 (/ 30 60))
     (gethash "-3" ht) -3
     (gethash "-3:00" ht) -3
     (gethash "-3:30" ht) (- -3 (/ 30 60))
     (gethash "-4" ht) -4
     (gethash "-4:00" ht) -4
     (gethash "-4:30" ht) (- -4 (/ 30 60))
     (gethash "-5" ht) -5
     (gethash "-5:00" ht) -5
     (gethash "-5:30" ht) (- -5 (/ 30 60))
     (gethash "-6" ht) -6
     (gethash "-6:00" ht) -6
     (gethash "-6:30" ht) (- -6 (/ 30 60))
     (gethash "-7" ht) -7
     (gethash "-7:00" ht) -7
     (gethash "-7:30" ht) (- -7 (/ 30 60))
     (gethash "-8" ht) -8
     (gethash "-8:00" ht) -8
     (gethash "-8:30" ht) (- -8 (/ 30 60))
     (gethash "-9" ht) -9
     (gethash "-9:00" ht) -9
     (gethash "-9:30" ht) (- -9 (/ 30 60))
     (gethash "0" ht) 0
     (gethash "1" ht) 1
     (gethash "10" ht) 10
     (gethash "11" ht) 11
     (gethash "12" ht) 12
     (gethash "2" ht) 2
     (gethash "3" ht) 3
     (gethash "4" ht) 4
     (gethash "5" ht) 5
     (gethash "6" ht) 6
     (gethash "7" ht) 7
     (gethash "8" ht) 8
     (gethash "9" ht) 9
     (gethash "CDT" ht) -5   ; Central Daylight stupid Time (U.S.) ???
     (gethash "CST" ht) -6	    ; Central Standard Time (U.S.) ???
     (gethash "EDT" ht) -4		; Eastern Daylight Time (U.S.)
     (gethash "EST" ht) -5		; Eastern Standard Time (U.S.)
     (gethash "GMT" ht) 0
     (gethash "PDT" ht) -7	 ; Pacific Daylight stupid Time (U.S.)
     (gethash "PST" ht) -8		; Pacific Standard Time (U.S.)
     (gethash -1 ht) -1
     (gethash -10 ht) -10
     (gethash -11 ht) -11
     (gethash -12 ht) -12
     (gethash -2 ht) -2
     (gethash -3 ht) -3
     (gethash -4 ht) -4
     (gethash -5 ht) -5
     (gethash -6 ht) -6
     (gethash -7 ht) -7
     (gethash -8 ht) -8
     (gethash -9 ht) -9
     (gethash 0 ht) 0
     (gethash 1 ht) 1
     (gethash 10 ht) 10
     (gethash 11 ht) 11
     (gethash 12 ht) 12
     (gethash 2 ht) 2
     (gethash 3 ht) 3
     (gethash 4 ht) 4
     (gethash 5 ht) 5
     (gethash 6 ht) 6
     (gethash 7 ht) 7
     (gethash 8 ht) 8
     (gethash 9 ht) 9
     )
    ht))

(defun make-zone (x)
  (car
   (multiple-value-list (gethash (typecase x
				   (number x)
				   (string (string-upcase x))
				   (t x))
				 *zones*))))

(defun recognize-minimal-iso (str)
  "The string is minimal ISO if, after trimming leading & trailing crap,
the string is 14 characters & they are all digits."
  (declare (type string str))
  (and (eql 14 (length str))
       (every #'digit-char-p str)
       (list (cons 'year (read-from-string (subseq str 0 4)))
	     (cons 'month (read-from-string (subseq str 4 6)))
	     (cons 'day (read-from-string (subseq str 6 8)))
	     (cons 'hour (read-from-string (subseq str 8 10)))
	     (cons 'minute (read-from-string (subseq str 10 12)))
	     (cons 'second (read-from-string (subseq str 12))))))

(defun recognize-tee-iso (str)
  "Tee-ISO is like minimal ISO except that is has a T character in the
middle of it."
  ;; If it looks like it might be Tee ISO, we remove the T, which hopefully
  ;; converts it to minimal ISO.  Then call the minimal ISO function on it.
  (declare (type string str))
  (and (eql (length str) 15)
       (char-equal (char str 8) #\T)
       (recognize-minimal-iso (concatenate 'string
					   (subseq str 0 8)
					   (subseq str 9)))))

(defun recognize-verbose-iso (str tokens)
  (declare (ignore str) (type list tokens))
  (let* (ss mm hh dd mo yy zone)
    (and (is-year? (setq yy (pop tokens)))
	 (equal (pop tokens) "-")
	 (setq mo (make-month (pop tokens)))
	 (equal (pop tokens) "-")
	 (is-day? (setq dd (pop tokens)))
	 (stringp (first tokens))
	 (string-equal (pop tokens) "T")
	 (is-hour? (setq hh (pop tokens)))
	 (equal (pop tokens) ":")
	 (is-minute? (setq mm (pop tokens)))
	 (equal (pop tokens) ":")
	 (is-second? (setq ss (pop tokens)))
	 ;; Time zone is another special case.  The tokenizer turns time
	 ;; zones such as "-7" into two tokens: the string "-" & the
	 ;; number 7.  It does the same for "+7", which becomes "+" and
	 ;; 7. We must merge them into a single token again.  Some
	 ;; time zones will still be a single token already; "PST"
	 ;; is an example.
	 (setq zone (pop tokens))
	 (if zone
	     (setq zone
		   (make-zone
		    (cond ((and (stringp zone) (string-equal zone "+"))
			   (with-input-from-string (strm (format nil "~A"
								 (pop tokens)))
			     (ignore-errors (read strm nil nil))))
			  ((and (stringp zone) (string-equal zone "-"))
			   (with-input-from-string (strm (format nil "-~A"
								 (pop tokens)))
			     (ignore-errors (read strm nil nil))))
			  (t zone))))
	   t) ; Zone is nil, unspecified
	 ;; end of time zone special case
	 (endp tokens)
	 (make-broken-time :ss ss :mm mm :hh hh :dd dd :mo mo :yr yy
			   :zone zone))))

(defun recognize-now (str tokens)
  "Parse the string 'now', in any mix of case & with or without leading or
trailing crap ... er, I mean whitespace characters.  NOW is the current
time with resolution to the second."
  (declare (ignore tokens))
  (when (and (stringp str)
	     (string-equal (string-trim '(#\Tab #\Space #\Newline) str) "now"))
    (get-universal-time)))

(defun recognize-today (str tokens)
  "Parse the string 'today', in any mix of case & with or without leading or
trailing crap ... er, I mean whitespace characters.  Today is the current
year, month, & day, the default hour, 0 for minutes, & 0 for seconds.  It
assumes GMT so that today executed at the same time in different time
zones will give you the same universal time."
  (declare (ignore tokens))
  (when (and (stringp str)
	     (string-equal
	      (string-trim '(#\Tab #\Space #\Newline) str) "today"))
    (multiple-value-bind (ss mm hh dd mo yy)
	(decode-universal-time (get-universal-time))
      (setq ss 0
	    mm 0
	    hh (funcall *default-hour*))
      (encode-universal-time ss mm hh dd mo yy 0))))

(defun recognize-yyyymmdd-nw (str tokens)
  "Recognize YYYY MM DD, with only whitespace separating the tokens.
There is no time zone."
  (declare (ignore str) (type list tokens))
  (let* (dd mo yy)
    (and (is-year? (setq yy (pop tokens)))
	 (setq mo (make-month (pop tokens)))
	 (is-day? (setq dd (pop tokens)))
	 (endp tokens)
	 (make-broken-time :ss 0 :mm 0 :hh (funcall *default-hour*)
			   :dd dd :mo mo :yr yy :zone nil))))

(defun recognize-yyyymmdd (str tokens)
  "Recognize YYYYMMDD with no whitespace.  All characters in the string
must be digits, & the string must be of length 8."
  (declare (type string str) (ignore tokens))
  (and (eql (length str) 8)
       (every #'digit-char-p str)
       (make-broken-time :ss 0 :mm 0 :hh (funcall *default-hour*)
			 :dd (parse-integer str :start 6 :end 8)
			 :mo (parse-integer str :start 4 :end 6)
			 :yr (parse-integer str :start 0 :end 4)
			 :zone nil)))

(defun collect-token (width good-char? strm)
  "Collect characters from character input stream STRM until we have
WIDTH characters or the next character is not acceptable according to
GOOD-CHAR?, which is a function.  Return the characters we collected,
as a string."
  (peek-char t strm nil)		; skip leading space characters
  (do ((lst    ()  (cons (read-char strm) lst))
       (count   0                   (1+ count)))
      ((or (> count width)
	   (end-of-stream? strm)
	   (not (funcall good-char? (peek-char nil strm))))
       ;; Return the digits we've collected, as a string.
       (string-upcase (coerce (nreverse lst) 'string)))))

(defun make-numeric-parser (width min max field)
  "Return a function which parses a numeric token of at most WIDTH digits,
translating to a number not less than MIN & not more than MAX.  For example,
if WIDTH, MIN, & MAX are 4, 100, & 9000, you get a function which parses at
most 4 digits that form a number N in the range 100 <= N <= 9000."
  (declare (type integer width min max) (type symbol field))
  #'(lambda (strm)
      (let ((n
	     (with-input-from-string
		 (strm2 (collect-token width #'digit-char-p strm))
	       (ignore-errors (read strm2 nil nil)))))
	(if (and (numberp n) (<= min n max))
	    ;; It's a number & within our range, so return a CONS.
	    (cons field n)
	  ;; Else, it's not a number, or it's out of our range, so fail.
	  nil))))
(proclaim
 '(ftype (function (integer integer integer symbol) function)
	 make-numeric-parser))

(defun make-word-parser (width ht field)
  "Return a function which parses consecutive alpha- & numeric characters,
up to WIDTH of them, & then converts them to a Lisp object via the HT
hash table."
  (declare (type integer width) (type hash-table ht) (type symbol field))
  #'(lambda (strm)
      (let* ((token (collect-token width #'alphanumericp strm))
	     x)
	;; Ensure that any characters in TOKEN are upper-case.
	(when (stringp token)
	  (setq token (string-upcase token)))
	(setq x (gethash token ht))
	(if x
	    ;; The token is in the hash table, so return the value from
	    ;; the hash table.
	    (cons field x)
	  ;; Else, the token isn't in the hash table, so fail.
	  nil))))
(proclaim
 '(ftype (function (integer hash-table symbol) function) make-word-parser))

(defun parse-literal (literal strm)
  "Match & consume the literal characters in the string LITERAL from the
input stream STRM.  If all the charactrs in LITERAL match the next
characters from STRM, return CONS :LITERAL LITERAL.  Otherwise, Nil."
  (declare (type string literal))
  (with-input-from-string (lstrm literal)
    (peek-char t strm nil)              ; skip leading space characters
    (peek-char t lstrm nil)             ; ditto
    (do ()
	((or (end-of-stream? lstrm)
	     (end-of-stream? strm)
	     (not (char-equal (peek-char nil lstrm) (peek-char nil strm)))))
	;; Not end of stream(s), & the next characters are equivalent, so
	;; consume them.  A special case is space characters.  If the next
	;; characters are spaces, consume them with PEEK-CHAR so that we
	;; consume consecutive white-space characters.
	(cond ((member (peek-char nil lstrm nil) '(#\Space #\Tab #\Newline)
		       :test #'char-equal)
	       ;; Next character is a white-space, so use PEEK-CHAR to
	       ;; consume consecutive space characters from both streams.
	       (peek-char t lstrm nil)
	       (peek-char t strm nil))
	      (t
	       ;; Next character is not white-space, so consume just it.
	       (read-char lstrm nil)
	       (read-char strm nil))))
    ;; If we're at the end of the LITERAL string's input stream, then we
    ;; matched everything in it, which is success.  Otherwise, fail.
    (if (end-of-stream? lstrm)
	(cons :literal literal)
      ;; Else, the match failed.
      nil)))

(defun parse-percent-percent (strm)
  "Recognize the literal '%' character.  This is for the '%%' format token."
  (parse-literal "%" strm))

(defun parse-time-zone-minus-hour (strm hour)
  "Parse the minutes, assuming we've already parsed the hour."
  ;; If the next character is a colon, skip it.
  (declare (type number hour))
  (when (eql (peek-char nil strm nil) #\:)
    (read-char strm))
  ;; Now we expect exactly two digits.
  (let* ((min1 (when (xdigit? (peek-char nil strm nil))
		  (read-char strm)))
	 (min2 (when (xdigit? (peek-char nil strm nil))
		  (read-char strm))))
    (cond ((and (xdigit? min1) (xdigit? min2))
	   ;; We have two digits for the minute.  We also have the hour, which
	   ;; is a number.  Convert the two to a scalar.  That's the zone.
	   (cons :zone
		 (+ hour
		    (/ (read-from-string (format nil "~C~C" min1 min2))
		       60))))
	  ((and (null min1) (null min2))
	   ;; We didn't get any digits.  This is not an error.  It means
	   ;; there were no digits for the minute at all.  So we return
	   ;; just the hour as it is.
	   (cons :zone hour))
	  (t
	   ;; We got just one digit.  This is an error, so we fail.
	   nil))))

(defun parse-time-zone-minus (strm)
  "Parse the rest of a time zone assuming it begins with a - character.
It starts with a two-digit hour."
  (let* ((hour1 (when (xdigit? (peek-char nil strm nil))
		  (read-char strm)))
	 (hour2 (when (xdigit? (peek-char nil strm nil))
		  (read-char strm))))
    (cond ((and (xdigit? hour1) (xdigit? hour2))
	   ;; We have two digits for the hour.  That's good.  We try to
	   ;; parse a minute part, too, but if we can't get that, we
	   ;; still return the hour that we have.
	   (parse-time-zone-minus-hour strm
				       (- (read-from-string
						(format nil "~C~C" hour1 hour2)))))
	  ((and (xdigit? hour1) (eql (peek-char nil strm nil) #\:))
	   ;; We got one digit, & the next character is a colon, so we
	   ;; go for the minutes.
	   (parse-time-zone-minus-hour strm (- (read-from-string
						(format nil "~C" hour1)))))
	  ((xdigit? hour1) 
	   ;; We got one digt & the next character is not a digit & not a
	   ;; colon, so we stop here with just the hour.
	   (cons :zone
		 (- 
		  (with-input-from-string (strm (format nil "~C" hour1))
		    (read strm)))))
	  (t ;; Else we didn't get two digits for the hour, so fail.
	   nil))))

(defun parse-time-zone-plus (strm)
  "Parse a time zone assuming the first character of it was a + character."
  (let ((x (parse-time-zone-minus strm)))
    (if (numberp (cdr x))
	(cons (car x) (- (cdr x)))
      x)))

(defun time-zone-char? (x)
  "Return true if & only if X is a character & is acceptable in a time
zone."
  (and (characterp x)
       (or (eql x #\+) (eql x #\-) (eql x #\:) (alphanumericp x))))

(defun parse-time-zone (strm)
  "Jeezus fucking Krist I hate time zones.  A bitch to parse.  And a
stupid idea to begin with.  Fuuuuuck.
Recognize a time zone token from STRM.  If the next character is + or -,
we expect a two-digit number of hours to follow, such as 7 or 5.  After
those numbers, the next character may be ':' (which is ignored), & then
two-digit number of minutes.  If the first token of STRM is alpha instead
of + or -, we collect alpha-numeric characters into a token, then translate
them to a numeric time zone via a hash table."
  (peek-char t strm nil)		; skip space characters
  ;; Width is 6 because the longest time zone you'll see is something
  ;; like "+08:30", which is 6.
  (let* ((token (collect-token 6 #'time-zone-char? strm))
	 (x (gethash token *zones*)))
    (when *debug*
      (format t "~&~A: debug:" 'parse-time-zone)
      (format t "~&    token is ~S" token)
      (format t "~&    x is ~S" x))
    (if x
	(cons :zone x)
      nil)))

(defvar *months*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "1" ht) 1
	  (gethash "2" ht) 2
	  (gethash "3" ht) 3
	  (gethash "4" ht) 4
	  (gethash "5" ht) 5
	  (gethash "6" ht) 6
	  (gethash "7" ht) 7
	  (gethash "8" ht) 8
	  (gethash "9" ht) 9
	  (gethash "10" ht) 10
	  (gethash "11" ht) 11
	  (gethash "12" ht) 12
	  (gethash "JAN" ht) 1
	  (gethash "FEB" ht) 2
	  (gethash "MAR" ht) 3
	  (gethash "APR" ht) 4
	  (gethash "MAY" ht) 5
	  (gethash "JUN" ht) 6
	  (gethash "JUL" ht) 7
	  (gethash "AUG" ht) 8
	  (gethash "SEP" ht) 9
	  (gethash "SEPT" ht) 9
	  (gethash "OCT" ht) 10
	  (gethash "NOV" ht) 11
	  (gethash "DEC" ht) 12
	  (gethash "JANUARY" ht) 1
	  (gethash "FEBRUARY" ht) 2
	  (gethash "MARCH" ht) 3
	  (gethash "APRIL" ht) 4
	  ;; MAY 5
	  (gethash "JUNE" ht) 6
	  (gethash "JULY" ht) 7
	  (gethash "AUGUST" ht) 8
	  (gethash "SEPTEMBER" ht) 9
	  (gethash "OCTOBER" ht) 10
	  (gethash "NOVEMBER" ht) 11
	  (gethash "DECEMBER" ht) 12)
    ht)
  "Map month names & abbreviations to their numbers.")

(defvar *weekdays*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "SUN" ht) t
	  (gethash "MON" ht) t
	  (gethash "TUE" ht) t
	  (gethash "TUES" ht) t
	  (gethash "WED" ht) t
	  (gethash "THU" ht) t
	  (gethash "THUR" ht) t
	  (gethash "FRI" ht) t
	  (gethash "SAT" ht) t
	  (gethash "SUNDAY" ht) t
	  (gethash "MONDAY" ht) t
	  (gethash "TUESDAY" ht) t
	  (gethash "WEDNESDAY" ht) t
	  (gethash "THURSDAY" ht) t
	  (gethash "FRIDAY" ht) t
	  (gethash "SATURDAY" ht) t)
    ht)
  "Map weekday names & abbreviations to truth.  We don't use the weekday
when figuring the universal time, so we don't map to anything other than
true.  The true simply indicates that we recognize the term as a weekday
name.")

(defvar *ampm*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "AM" ht) :am
	  (gethash "A" ht) :am
	  (gethash "PM" ht) :pm
	  (gethash "P" ht) :pm
	  (gethash "O'CLOCK" ht) :oclock)
    ht)
  "Map AM, PM, & O'CLOCK strings to symbols for use when figuring the
universal time from a broken time.")

(defvar *term-parsers*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "%%" ht) #'parse-percent-percent
	  (gethash "%A" ht) (make-word-parser 20 *weekdays*   :weekday)
	  (gethash "%B" ht) (make-word-parser 20 *months*     :month)
	  (gethash "%H" ht) (make-numeric-parser 2 0 24      :hour)
	  (gethash "%M" ht) (make-numeric-parser 2 0 59       :minute)
	  (gethash "%Y" ht) (make-numeric-parser 4 0 9999     :year)
	  (gethash "%Z" ht) #'parse-time-zone
	  (gethash "%a" ht) (make-word-parser 20 *weekdays*   :weekday)
	  (gethash "%b" ht) (make-word-parser 20 *months*     :month)
	  (gethash "%d" ht) (make-numeric-parser 2 0 31       :day)
	  (gethash "%m" ht) (make-numeric-parser 2 1 12       :month)
	  (gethash "%p" ht) (make-word-parser 2 *ampm*        :ampm)
	  (gethash "%S" ht) (make-numeric-parser 2 0 59       :second)
	  (gethash "%y" ht) (make-numeric-parser 2 0 99       :year)
	  ;; (gethash "%I" ht) need to convert the 12-hour hour to 24-hour hour
	  )
    ht)
  "Maps format descriptors to the functions that parse them.  Keys are
format descriptors, which are strings such as '%Y'.  Values are functions
which extract & return a CONS for an assoc-list or NIL.")

(defun recognize-fmt (strm fmt-lst)
  "STRM is an input stream to parse.  FMT-LST is list of terms from fmt
string."
  ;; Apply funcs for terms, collecting results.
  (let ((x (mapcar #'(lambda (term)
		       (let ((fn (gethash term *term-parsers*))
			     x)
			 (if fn
			     ;; Call the FN to parse the term.  It'll
			     ;; return a pair or Nil.
			     (setq x (funcall fn strm))
			   ;; Else there is no function, so assume the
			   ;; term is literal.
			   (setq x (parse-literal term strm)))
			 x))
		   fmt-lst)))
    (when *debug*
      (format t "~&~A: trace: ~S" 'recognize-fmt x))
    (peek-char t strm nil)              ; consume remaining spaces, if any
    (cond ((not (end-of-stream? strm))
	   ;; Not at end of STRM.  means we didn't consume all input.  Fail.
	   (when *debug*
	     (format t "~&~A: debug: not at end of stream" 'recognize-fmt)
	     (format t "~&    Next character is ~S." (peek-char nil strm)))
	   nil)
	  ((member nil x)
	   (when *debug*
	     (format t "~&~A: debug: At least one term didn't parse."
		     'recognize-fmt))
	   nil)
	  (t
	   ;; Good
	   (create-broken x)))))

(defun make-fmt-recognizer (fmt)
  (let ((fmt-lst (convert-fmt-string-to-list fmt)))
    #'(lambda (str tokens)
	(declare (type string str) (ignore tokens))
	(with-input-from-string (strm str)
	  (recognize-fmt strm fmt-lst)))))

(defvar *default-recognizers*
      (list (make-fmt-recognizer "%Y-%m-%dT%H:%M:%S")
	    (make-fmt-recognizer "%Y-%m-%dT%H:%M:%S%Z")
	    (make-fmt-recognizer "%Y-%B-%dT%H:%M:%S")
	    (make-fmt-recognizer "%Y-%B-%dT%H:%M:%S%Z")
	    (make-fmt-recognizer "%Y%B%d%Z")
	    (make-fmt-recognizer "%Y-%B-%d%Z")
	    (make-fmt-recognizer "%Y%m%d%Z")
	    (make-fmt-recognizer "%Y-%m-%d%Z")
	    (make-fmt-recognizer "%Y%B%d")
	    (make-fmt-recognizer "%Y-%B-%d")
	    (make-fmt-recognizer "%Y%m%d")
	    (make-fmt-recognizer "%Y-%m-%d")
	    (make-fmt-recognizer "%B %d, %Y, %H:%M %p")
	    'recognize-now
	    'recognize-today
	    'recognize-yyyymmdd-nw
	    'recognize-yyyymmdd
	    (make-fmt-recognizer "%Y-%m-%dT%H:%M")
	    (make-fmt-recognizer "%Y-%m-%dT%H:%M%Z")
	    (make-fmt-recognizer "%Y-%B-%dT%H:%M")
	    (make-fmt-recognizer "%Y-%B-%dT%H:%M%Z")
	    (make-fmt-recognizer "%B%d,%Y")    ; silly American date
	    (make-fmt-recognizer "%m/%d/%Y"))) ; stupid American date

(defun parse-time (str &optional (recognizers *default-recognizers*))
  "Parse a string containing a date-&-time.  Return a universal time.
If the string can't be recognized, return NIL."
  ;; Find a function which can parse the string.
  (declare (type string str) (type list recognizers))
  (let ((x (find-if #'(lambda (fn)
			(funcall fn str (tokenize str)))
		    recognizers)))
    (if x
	;; Get the result from the recongizer.
	(let ((y (funcall x str (tokenize str))))
	  ;; If the result is a number, assume it's a universal time &
	  ;; return it as-is.  If it's a BROKEN-TIME, must convert it
	  ;; to a universal time.  Anything else is an error, for which
	  ;; we return NIL.
	  (typecase y
	    (integer y)
	    (broken-time (broken-to-ut y))
	    (list (broken-to-ut (create-broken y)))
	    (t nil)))
      ;; Else, we couldn't find a function that parsed it
      nil)))
       
;;; --- end of file ---
