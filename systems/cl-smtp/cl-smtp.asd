;;; -*- mode: Lisp -*-
	
;;; This file is part of CL-SMTP, the Lisp SMTP Client

;;; Copyright (C) 2004/2005/2006/2007 Jan Idzikowski

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU General Public License for more details.

;;; File: cl-smtp.asd
;;; Description: cl-smtp ASDF system definition file

(defpackage :cl-smtp
   	(:use :cl :asdf)
	(:export :send-email))

(in-package :cl-smtp)

(defparameter *debug* nil)

(defmacro print-debug (str)
  `(when *debug*
      (print ,str)))

(asdf:defsystem :cl-smtp
        :version "20071018.1"
	:depends-on (:usocket #-allegro :cl-base64)
	:components ((:file "cl-smtp" :depends-on ("attachments"))
		     (:file "attachments")))
