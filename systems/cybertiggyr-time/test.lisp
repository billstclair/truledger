;;; -*- Mode: Lisp -*-
;;;
;;; $Header: /home/gene/library/website/docsrc/pdl/RCS/test.lisp,v 395.1 2008/04/20 17:25:47 gene Exp $
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

(in-package "CYBERTIGGYR-TIME")

(defvar *tests* nil)
(setq *tests* nil)

(defmacro deftest (name &rest args-doc-body)
  `(progn
     (unless (member ',name *tests*)
       (setq *tests* (append *tests* (list ',name))))
     (defun ,name ,@args-doc-body)))

(defun check (&optional (is-verbose t) (lst *tests*))
  "Executes all the tests in LST until they all execute or one of
them fails."
  (declare (type list lst))
  (let ((count 0))
    (labels 
	((failed (test)
		 (declare (type (or symbol function) test))
		 (when is-verbose
		   (format t "~&~2D%  ~A"
			   (floor (* (/ count (length lst)) 100))
			   test)
		   (force-output *standard-output*))
		 (let ((x (funcall test)))
		   (unless x
		     (if is-verbose
			 (format t "~&*** failed ***")
		       (format t "~&~A~&*** failed ***" test))
		     (force-output *standard-output*))
		   (incf count)
		   (null x))))
  (null (find-if #'failed lst)))))

(defun show-time-diff (x expect)
  "Print a human-readable description of a time a function returned & the
time we expected it to return.  Some of these test programs use this."
  (if (numberp x)
      (let* ((ds (- x expect))		; delta seconds
	     (dm (/ ds 60))		; delta minutes
	     (dh (/ dm 60)))		; delta hours
	(format t "~&    Difference is ~D seconds." ds)
	(format t "~&    Difference is ~D minutes." dm)
	(format t "~&    Difference is ~D hours." dh))))

(deftest test0000 ()
  "Null test.  Always succeeds."
  'test0000)

(deftest test0001 ()
  "Test :NEXT-FN to slurp up all characters in a short,
non-empty string."
  (let ((x "abc"))
    (equal (with-input-from-string (strm x)
	     (cybertiggyr-time::next-fn strm #'identity))
	   x)))

(deftest test0002 ()
  "Test :NEXT-FN to slurp all characters from an empty
string."
  (let ((x (with-input-from-string (strm "")
	     (cybertiggyr-time::next-fn strm #'identity))))
    (when x
      (format t "~&~A returned ~S.  Expected ~S." 'next-fn x nil))
    (null x)))

(deftest test0003 ()
  "Test :NEXT-FN to collect alphanum characters from a string
that begins with a space.  :NEXT-FN should skip the space &
collect the other characters."
  (with-input-from-string (strm "abc ")
    (let ((expected "abc")
	  (x (cybertiggyr-time::next-fn strm #'alphanumericp)))
      (if (equal x expected)
	  (if (char-equal (read-char strm) #\Space)
	      t
	    (progn
	      (format t "~&~A did not consume characters as expected."
		      'cybertiggyr-time::next-fn)
	      nil))
	(progn
	  (format t "~&~A returned ~S.  Expected ~S."
		  'cybertiggyr-time::next-fn x expected)
	  nil)))))

(deftest test0004 ()
  "Text :NEXT-NUMBER on a string that contains just one
number & no other characters."
  (with-input-from-string (strm "123")
    (let ((x (cybertiggyr-time::next-number strm))
	  (expected 123))
      (cond ((not (eql x expected))
	     (format t "~&~A returned ~S.  Expected ~S."
		     'cybertiggyr-time::next-number x expected)
	     nil)
	    ((not (eq (read-char strm nil strm) strm))
	     (format t "~&~A did not leave the stream at its end."
		     'cybertiggyr-time::next-number)
	     nil)
	    (t t)))))

(deftest test0005 ()
  "Test :NEXT-NUMBER on a string that has a non-numeric
character after the number."
  (with-input-from-string (strm "123z")
    (let ((x (cybertiggyr-time::next-number strm))
	  (expected 123))
      (cond ((not (eql x expected))
	     (format t "~&~A returned ~S.  Expected ~S."
		     'cybertiggyr-time::next-number x expected)
	     nil)
	    ((not (char-equal (read-char strm nil strm) #\z))
	     (format t "~&~A did not leave a ~S character in the stream."
		     'cybertiggyr-time::next-number #\z)
	     nil)
	    (t t)))))

(deftest test0006 ()
  "Test :NEXT-WORD on a string that contains just the word."
  (with-input-from-string (strm "abc")
    (let ((x (cybertiggyr-time::next-word strm))
	  (expected "ABC"))
      (cond ((not (equal x expected))
	     (format t "~&~A returned ~S.  Expected ~S."
		     'cybertiggyr-time::next-word x expected)
	     nil)
	    ((not (eq (read-char strm nil strm) strm))
	     (format t "~&~A did not leave the stream at its end."
		     'cybertiggyr-time::next-word)
	     nil)
	    (t t)))))

(deftest test0007 ()
  "Test :NEXT-WORD on a string that contains a space after
the word."
  (with-input-from-string (strm "abc ")
    (let ((x (cybertiggyr-time::next-word strm))
	  (expected "ABC"))
      (cond ((not (equal x expected))
	     (format t "~&~A returned ~S.  Expected ~S."
		     'cybertiggyr-time::next-word x expected)
	     nil)
	    ((not (char-equal (read-char strm nil strm) #\Space))
	     (format t "~&~A did not leave a ~S in the stream."
		     'cybertiggyr-time::next-word #\Space)
	     nil)
	    (t t)))))

(deftest test0010 ()
  "Test :NEXT-TOKEN on a string with just one token."
  (with-input-from-string (strm "123")
    (let ((x (cybertiggyr-time::next-token strm))
	  (expected 123))
      (cond ((not (equal x expected))
	     (format t "~&~A returned ~S.  Expected ~S."
		     'cybertiggyr-time::next-token x expected)
	     nil)
	    ((not (eq (read-char strm nil strm) strm))
	     (format t "~&~A did not leave the stream at its end."
		     'cybertiggyr-time::next-token)
	     nil)
	    (t t)))))

(deftest test0011 ()
  "Test :NEXT-TOKEN on a string with some white space before its only
token."
  (with-input-from-string (strm " abc")
    (let ((x (cybertiggyr-time::next-token strm))
	  (expected "ABC"))
      (cond ((not (equal x expected))
	     (format t "~&~A returned ~S.  Expected ~S."
		     'cybertiggyr-time::next-token x expected)
	     nil)
	    ((not (eq (read-char strm nil strm) strm))
	     (format t "~&~A did not leave the stream at its end."
		     'cybertiggyr-time::next-token)
	     nil)
	    (t t)))))

(deftest test0012 ()
  "Test :NEXT-TOKEN on a string with two tokens."
  (with-input-from-string (strm "abc 123")
    (if (equal (cybertiggyr-time::next-token strm) "ABC")
	(if (eql (cybertiggyr-time::next-token strm) 123)
	    t
	  (progn
	    (format t "~&~A did not return the expected ~S."
		    'cybertiggyr-time::next-token 123)
	    nil))
      (progn 
	(format t "~&~A did not return the expected ~S."
		'cybertiggyr-time::next-token "ABC")
	nil))))

(deftest test0013 ()
  "Test :TOKENIZE on a simple string."
  (let ((expected '(12 "APRIL" 2004))
	(x (cybertiggyr-time::tokenize "12 april 2004")))
    (cond ((not (equal x expected))
	   (format t "~&~A returned ~S.  Expected ~S."
		   'cybertiggyr-time::tokenize x expected)
	   nil)
	  (t t))))

(labels ((moo (str expected)
	      (let ((x (cybertiggyr-time::tokenize str)))
		(cond ((not (equal x expected))
		       (format t "~&(~A ~S) returned ~S.  Expected ~S."
			       'cybertiggyr-time::tokenize str x expected)
		       nil)
		      (t t)))))
  (let ((cases-lst (list (list "12 apr 2004" (list 12 "APR" 2004))
			 (list "apr 12, 2004" (list "APR" 12 "," 2004))
			 (list "12:13" (list 12 ":" 13)))))
    (deftest test0014 ()
      "Test :TOKENIZE on many cases."
      (let ((good? 'test0014))
	(find-if #'(lambda (lst)
		     (setq good? (apply #'moo lst))
		     (not good?))
		 cases-lst)
	good?))))

(deftest test0020 ()
  "Test that :MAKE-BROKEN-TIME doesn't crash."
  (cybertiggyr-time::make-broken-time))

(deftest test0025 ()
  "Test that :NORMALIZE-BROKEN doesn't crash."
  (cybertiggyr-time::normalize-broken (cybertiggyr-time::make-broken-time)))

(deftest test0026 ()
  "Test that :NORMALIZE-BROKEN leaves no element as NIL,
except ZONE, which may be NIL."
  (let* ((bro (cybertiggyr-time::normalize-broken
	       (cybertiggyr-time::make-broken-time)))
	 ;; Don't check :broken-time-zone or
	 ;; :broken-time-ampm because they could be NIL.
	 ;; We could check that they are bound, not undefined.
	 (x (and (cybertiggyr-time::broken-time-ss bro)
		 (cybertiggyr-time::broken-time-mm bro)
		 (cybertiggyr-time::broken-time-hh bro)
		 (cybertiggyr-time::broken-time-dd bro)
		 (cybertiggyr-time::broken-time-mo bro)
		 (cybertiggyr-time::broken-time-yr bro))))
    (unless x
      (format t "~&Normalized ~A has a NIL where it shouldn't.~&It is ~S."
	      (type-of bro) bro))
    x))

(deftest test0030 ()
  "Test :BROKEN-TO-UT.  Test it by getting the current time, then
creating a BROKEN-TIME from that.  Then calling :BROKEN-TO-UT.  The
value from :BROKEN-TO-UT should be the same as that current time we
originally fetched."
  (let ((time (get-universal-time))
	bro
	time2)
    (multiple-value-bind (ss mm hh dd mo yr) (decode-universal-time time)
      (setq bro (cybertiggyr-time::make-broken-time :ss ss :mm mm :hh hh
						    :dd dd :mo mo :yr yr))
      (setq time2 (cybertiggyr-time::broken-to-ut bro))
      (= time time2))))

(deftest test0031 ()
  "Test :BROKEN-TO-UT on a hard-coded time with a time zone of 0."
  ;; The time is noon on 1 January 1900 GMT.  So the expected value
  ;; is 12 hour * 60 minute/hour * 60 second/minute.
  (let* ((expect  (* 12 60 60))
	 (x       (cybertiggyr-time::broken-to-ut
		   (cybertiggyr-time::make-broken-time :ss 0 :mm 0 :hh 12
						       :dd 1 :mo  1 :yr 1900
						       :zone 0))))
    (unless (eql x expect)
      (format t "~&~A: ~A returned ~G, expected ~G." 'test0031 'parse-time
	      x expect))
    (eql x expect)))

(deftest test0032 ()
  "Test :BROKEN-TO-UT on a hard-coded time with a time zone of -8."
  ;; The time is noon on 1 January 1900 Pacific Standard time zone.
  ;; So the expected value is 12 hour * 60 minute/hour * 60 second/minute,
  ;; plus 8 hours because Pacific Standard time is 8 hours behind GMT.
  (let* ((expect  (* (+ 12 8) 60 60))
	 (x       (cybertiggyr-time::broken-to-ut
		   (cybertiggyr-time::make-broken-time :ss 0 :mm 0 :hh 12
						       :dd 1 :mo  1 :yr 1900
						       :zone -8))))
    (unless (eql x expect)
      (format t "~&~A: ~A returned ~G, expected ~G." 'test0032 'parse-time
	      x expect))
    (eql x expect)))

(deftest test0033 ()
  "Test :BROKEN-TO-UT on a hard-coded time with a time zone of 0."
  ;; The time is noon on 1 January 1900 Pacific Standard time zone.
  ;; So the expected value is 12 hour * 60 minute/hour * 60 second/minute,
  ;; plus 8 hours because Pacific Standard time is 8 hours behind GMT.
  (let* ((expect  (* (+ 12 8) 60 60))
	 (x       (cybertiggyr-time::broken-to-ut
		   (cybertiggyr-time::make-broken-time
		    :ss 0 :mm 0 :hh 12 :dd 1 :mo  1 :yr 1900 :zone -8))))
    (unless (eql x expect)
      (format t "~&~A: ~A returned ~G, expected ~G." 'test0033 'parse-time
	      x expect))
    (eql x expect)))

(deftest test0040 ()
  "Test :PARSE-PERCENT-PERCENT on an empty string.  It should return NIL."
  (with-input-from-string (strm "")
    (not (cybertiggyr-time::parse-percent-percent strm))))

(deftest test0041 ()
  "Test :PARSE-PERCENT-PERCENT on a hard-coded '%' string.  It should match
& leave the stream at its end."
  (with-input-from-string (strm "%")
    (and (equal (cybertiggyr-time::parse-percent-percent strm)
		'(:literal . "%"))
	 (eq (read strm nil strm) strm))))

(deftest test0042 ()
  "Test :PARSE-PERCENT-PERCENT on a hard-coded '%a' string.  It should match
& leave the stream pointing at the 'a' character."
  (with-input-from-string (strm "%a")
    (and (equal (cybertiggyr-time::parse-percent-percent strm)
		'(:literal . "%"))
	 (eql (read-char strm nil strm) #\a))))

(deftest test0043 ()
  "Test :PARSE-PERCENT-PERCENT on a hard-coded 'x%' string.  It should not
match."
  (with-input-from-string (strm "x%")
    (not (cybertiggyr-time::parse-percent-percent strm))))

(deftest test0044 ()
  "Test :PARSE-PERCENT-PERCENT on a hard-coded ' %' string.  Notice the
space at the beginning of that string.  It should match."
  (with-input-from-string (strm " %")
    (equal (cybertiggyr-time::parse-percent-percent strm)
	   '(:literal . "%"))))

(deftest test0046 ()
  "Test that :MAKE-NUMERIC-PARSER returns a function without crashing."
  (functionp (cybertiggyr-time::make-numeric-parser 4 0 9999 :year)))

(deftest test0047 ()
  "Test the function returned by :MAKE-NUMERIC-PARSER.  We'll make a
function to parse a year & test it on an empty string.  It should return
NIL."
  (with-input-from-string (strm "")
    (null (funcall (cybertiggyr-time::make-numeric-parser 4 0 9999 :year)
		   strm))))

(deftest test0048 ()
  "Test the function returned by :MAKE-NUMERIC-PARSER.  We'll make a
function to parse a year & test it on a string that holds a year.  It
should return a CONS."
  (with-input-from-string (strm "1999")
    (equal
     (funcall (cybertiggyr-time::make-numeric-parser 4 0 9999 :year) strm)
     '(:year . 1999))))
  
(deftest test0049 ()
  "Test the function returned by :MAKE-NUMERIC-PARSER.  We'll make a
function to parse a year & test it on a string that holds a year preceeded
by a space.  It should match & return a CONS."
  (with-input-from-string (strm " 1999")
    (equal
     (funcall (cybertiggyr-time::make-numeric-parser 4 0 9999 :year) strm)
     '(:year . 1999))))

(deftest test0050 ()
  "Test the function returned by :MAKE-NUMERIC-PARSER.  We'll make a
function to parse a year & test it on a string that holds a year followed
by a space.  It should match, return a CONS, & leave the stream pointing
at the space."
  (with-input-from-string (strm "1999 ")
    (and (equal
	  (funcall (cybertiggyr-time::make-numeric-parser 4 0 9999 :year) strm)
	  '(:year . 1999))
	 (eql (read-char strm nil strm) #\Space))))

(deftest test0051 ()
  "Test the function returned by :MAKE-NUMERIC-PARSER.  We'll make a
function to parse a year & test it on a string that holds a year preceeded
by a letter.  It should not match.  It should return NIL & leave the
stream pointing at the letter."
  (with-input-from-string (strm "x1999")
    (and 
     (null (funcall (cybertiggyr-time::make-numeric-parser 4 0 9999 :year)
		    strm))
     (eql (read-char strm nil strm) #\x))))

(deftest test0056 ()
  "Test that :MAKE-WORD-PARSER returns a function without crashing."
  (functionp (cybertiggyr-time::make-word-parser 4 (make-hash-table) :year)))

(deftest test0057 ()
  "Test the function returned by :MAKE-WORD-PARSER.  We'll make a
function to parse a month name & test it on an empty string.  It should return
NIL."
  (with-input-from-string (strm "")
    (null
     (funcall
      (cybertiggyr-time::make-word-parser
       20
       cybertiggyr-time::*months* :month)
      strm))))

(deftest test0058 ()
  "Test the function returned by :MAKE-WORD-PARSER.  We'll make a
function to parse a month & test it on a string that holds a month.  It
should return a CONS."
  (with-input-from-string (strm "january")
    (equal
     (funcall (cybertiggyr-time::make-word-parser 20
						  cybertiggyr-time::*months*
						  :month) strm)
     '(:month . 1))))
  
(deftest test0059 ()
  "Test the function returned by :MAKE-WORD-PARSER.  We'll make a
function to parse a month & test it on a string that holds a month preceeded
by a space.  It should match & return a CONS."
  (with-input-from-string (strm " may")
    (equal
     (funcall
      (cybertiggyr-time::make-word-parser
       20
       cybertiggyr-time::*months* :month)
      strm)
     '(:month . 5))))

(deftest test0060 ()
  "Test the function returned by :MAKE-WORD-PARSER.  We'll make a
function to parse a month & test it on a string that holds a month followed
by a space.  It should match, return a CONS, & leave the stream pointing
at the space."
  (with-input-from-string (strm "jan ")
    (and (equal
	  (funcall (cybertiggyr-time::make-word-parser
		    20
		    cybertiggyr-time::*months* :month)
		   strm)
	  '(:month . 1))
	 (eql (read-char strm nil strm) #\Space))))

(deftest test0061 ()
  "Test the function returned by :MAKE-WORD-PARSER.  We'll make a
function to parse a month & test it on a string that holds a month preceeded
by a letter.  It should not match.  It should return NIL."
  (with-input-from-string (strm "xjan")
    (null
     (funcall
      (cybertiggyr-time::make-word-parser
       20
       cybertiggyr-time::*months* :month)
      strm))))

;;; Here comes the fucked section of these tests.  We're going to
;;; test time zones.  I hate time zones.  Time zones suck.  Time
;;; zones MAY have been a good idea at one time (unlike daylight
;;; stupid time, which was never a good idea), but at this point,
;;; time zones suck.  Try to parse the little fuckers.  Yeah, they
;;; suck suck suck.  Fucking stupid pieces of un-parsable hacked
;;; shite.

(deftest test0062 ()
  "Test that :PARSE-TIME-ZONE returns NIL on an empty string."
  (with-input-from-string (strm "")
    (null (cybertiggyr-time::parse-time-zone strm))))

(deftest test0063 ()
  "Test that :PARSE-TIME-ZONE returns a good time zone for
+07.  It should return CONS :ZONE . -7."
  (let ((x (with-input-from-string (strm "+07")
	     (cybertiggyr-time::parse-time-zone strm)))
	(expect (cons :zone 7)))
    (unless (equal x expect)
      (format t "~&~A: ~A returned ~A.  Expected ~A." 'test0063
	      'cybertiggyr-time::parse-time-zone x expect))
    (equal x expect)))

(deftest test0096 ()
  "Test that RECOGNIZE-FMT doesn't crash when parsing an empty input
string with a hard-coded format string."
  (with-input-from-string (strm "")
    (recognize-fmt
     strm
     (list "%Y" "%m" "%d" "T" "%H" "%M" "%S" "%Z")))
  ;; If we get here, it didn't crash, so it worked as far as this test
  ;; is concerned.
  'test0096)

(deftest test0097 ()
  "Test RECOGNIZE-FMT with %Y-%B-%dT%H:%M:%S and a string that it should
be able to parse.  Just verify that it parses, not that it returns the
correct time."
  (with-input-from-string (strm "1999-jan-1T12:11:10")
    (recognize-fmt
     strm
     '("%Y" "-" "%B" "-" "%d" "T" "%H" ":" "%M" ":" "%S"))))

(deftest test0098 ()
  "Test RECOGNIZE-FMT with %Y-%B-%dT%H:%M:%S%Z and a string that it should
be able to parse.  Just verify that it parses, not that it returns the
correct time."
  (with-input-from-string (strm "1999-jan-1T12:11:10+0800")
    (recognize-fmt
     strm
     '("%Y" "-" "%B" "-" "%d" "T" "%H" ":" "%M" ":" "%S" "%Z"))))

(deftest test0099 ()
  "Test :MAKE-FMT-RECOGNIZER with %Y-%B-%dT%H:%M:%S%Z and a string that it
should be able to parse."
    (funcall (cybertiggyr-time::make-fmt-recognizer "%Y-%B-%dT%H:%M:%S%Z") 
	     "1999-jan-1T12:11:10+0800"
	     nil))

(deftest test0100 ()
  "Test that we can parse full ISO times.  Uses a hard-coded time.  Doesn't
check that the return value is correct, just that it's a universal time,
which is a number."
  (let* ((x (parse-time "1999-jan-1T12:11:10+0800")))
    (unless (numberp x)
      (format t "~&~A: ~A returned ~S.  Expected a number." 'test0100
	      'pare-date x))
    (numberp x)))

(deftest test0140 ()
  "Test that we can parse full ISO times.  Uses the current time for
the test."
  (let* ((now  (get-universal-time))
	 (x    (parse-time
		(format-time
		 nil
		 *format-time-iso8601-long*
		 now))))
    (unless (eql x now)
      (let* ((ds (- x now))
	     (dm (/ ds 60))
	     (dh (/ dm 60)))
	(format t "~&~A: ~A returned ~G, expected ~G." 'test0140
		'parse-time x now)
	(format t "~&    Difference is ~F seconds" ds)
	(format t "~&    Difference is ~F minutes" dm)
	(format t "~&    Difference is ~F hours" dh)))
    (eql x now)))

(deftest test0141 ()
  "Test PARSE-TIME on a hard-coded string with a time zone of +0,
which is GMT."
  ;; The time is noon on 1 January 1900 GMT.  The expected value is
  ;; 12 hour * 60 minute/hour * 60 second/minute.
  (let* ((expect  (* 12 60 60))
	 (x       (parse-time "1900-01-01T12:00:00 +0")))
    (unless (eql x expect)
      (format t "~&~A: ~A returned ~D, expected ~D." 'test0141
	      'parse-time x expect))
    (eql x expect)))

(deftest test0142 ()
  "Test PARSE-TIME on a hard-coded string with a time zone of PST,
which is 8 hours behind GMT."
  ;; The time is noon on 1 January 1900 GMT.  The expected value is
  ;; 12 hour * 60 minute/hour * 60 second/minute, plus 8 hours because
  ;; PST is 8 hours west of GMT.
  (let* ((expect  (* (+ 8 12) 60 60))
	 (x       (parse-time "1900-01-01T12:00:00 PST")))
    (unless (eql x expect)
      (format t "~&~A: ~A returned ~D, expected ~D." 'test0142
	      'parse-time x expect))
    (eql x expect)))

(deftest test0143 ()
  "Test PARSE-TIME on a hard-coded string with a time zone of +0000,
which is another way of specifying GMT."
  ;; The time is noon on 1 January 1900 GMT.  The expected value is
  ;; 12 hour * 60 minute/hour * 60 second/minute.
  (let* ((expect  (* 12 60 60))
	 (x       (parse-time "1900-01-01T12:00:00 +0000")))
    (unless (eql x expect)
      (format t "~&~A: ~A returned ~D, expected ~D." 'test0143
	      'parse-time x expect)
      (show-time-diff x expect))
    (eql x expect)))

(deftest test0150 ()
  "Test PARSE-TIME on the hard-coded string '1900-01-01 +0000', which
is a date-only ISO 8601 string."
  ;; We must take into account the default hours.
  (let* ((expect  (* (funcall *default-hour*) 60 60))
	 (x       (parse-time "1900-01-01  +0000")))
    (unless (eql x expect)
      (format t "~&~A returned ~D, expected ~D."
	      'parse-time x expect))
    (eql x expect)))

(deftest test0151 ()
  "Test PARSE-TIME on the hard-coded string '1900-01-01 GMT', which
is a date-only ISO 8601 string."
  ;; We must take into account the default hours.
  (let* ((expect  (* (funcall *default-hour*) 60 60))
	 (x       (parse-time "1900-01-01  +0000")))
    (unless (eql x expect)
      (format t "~&~A returned ~D, expected ~D." 'parse-time x expect))
    (eql x expect)))

(deftest test0160 ()
  "Test PARSE-TIME on the hard-coded string 'now', with gratuitous
whitespace characters."
  (equal (parse-time "  now ") (get-universal-time)))

(deftest test0165 ()
  "Test PARSE-TIME on the hard-coded string 'today'."
  (let ((x (parse-time "today"))
	(now (get-universal-time)))
    (multiple-value-bind (now-ss now-mm now-hh now-dd now-mo now-yy)
	(decode-universal-time now)
      (declare (ignore now-ss now-mm now-hh))
      (multiple-value-bind (x-ss x-mm x-hh x-dd x-mo x-yy)
	  (decode-universal-time x)
	(declare (ignore x-hh))
	(let ((is-good (and (eql x-ss 0)
			    (eql x-mm 0)
			    ;; (eql x-hh (funcall *default-hour*))
			    (eql x-dd now-dd)
			    (eql x-mo now-mo)
			    (eql x-yy now-yy))))
	  (unless is-good
	    (format t "~&~A returned ~D, expected ~D." 'parse-time x now)
	    (format t "~&Expanded ~D is ~S." x
		    (multiple-value-list (decode-universal-time x)))
	    (format t "~&Expanded ~D is ~S." now
		    (multiple-value-list (decode-universal-time now))))
	  is-good)))))

(deftest test0170 ()
  "Test PARSE-TIME on YYYY-MM-DD, where YYYY, MM, & DD are the current
year, month, & day.  Notice there is no time zone.  Just ensures that it
doesn't crash."
  (parse-time (format-time nil "%Y-%m-%d")))

(deftest test0171 ()
  "Test PARSE-TIME on YYYY-MM-DD and verify the results."
  (let ((fmt "%Y-%m-%d"))
    (let ((x (format-time nil fmt (parse-time (format-time nil fmt))))
	  (expect (format-time nil fmt)))
      (string-equal x expect))))

(deftest test0180 ()
  "Test PARSE-TIME on 'YYYY MM DD'.  Notice that the components are
separated by spaces, & there is no time zone.  Test that PARSE-TIME can
parse the date, not that it parses it correctly."
    (parse-time (format-time nil "%Y %m %d")))

(deftest test0181 ()
  "Test PARSE-TIME on 'YYYY MM DD'.  Notice that the components are
separated by spaces, & there is no time zone.  Test that PARSE-TIME can
parse the date, not that it parses it correctly."
  (let ((fmt "%Y %m %d"))
    (let ((x (format-time nil fmt (parse-time (format-time nil fmt))))
	  (expect (format-time nil fmt)))
      (string-equal x expect))))

(deftest test0200 ()
  "Test that :MAKE-FMT-RECOGNIZER doesn't crash."
  (cybertiggyr-time::make-fmt-recognizer "%Y-%m-%d"))

(deftest test0205 ()
  "Test PARSE-TIME on a hard-coded string of the form YYYYMMDD.  Just
test that PARSE-TIME thinks it can parse that string without crashing;
do not test that the resulting universal time has the correct value."
  (parse-time "19951012"))

;;; --- end of file ---
