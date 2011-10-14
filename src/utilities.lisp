; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Various utility functions
;;;

(in-package :truledger)

(defun hex (integer)
  "Return a string encoding integer as hex"
  (format nil "~x" integer))

(defun as-hex (byte)
  (when (or (< byte 0) (> byte 15))
    (error "Not between 0 and 15: ~s" byte))
  (code-char
   (if (< byte 10)
       (+ byte #.(char-code #\0))
       (+ (- byte 10) #.(char-code #\a)))))

(defun as-bin (hex-char)
  (let ((code (char-code hex-char)))
    (cond ((< code #.(char-code #\0))
           (error "Not a hex character: ~s" hex-char))
          ((<= code #.(char-code #\9)) (- code #.(char-code #\0)))
          ((and (>= code #.(char-code #\a))
                (<= code #.(char-code #\f)))
           (+ 10 (- code #.(char-code #\a))))
          ((and (>= code #.(char-code #\A))
                (<= code #.(char-code #\F)))
           (+ 10 (- code #.(char-code #\A))))
          (t (error "Not a hex character: ~s" hex-char)))))

(defun bin2hex (thing)
  "Convert an integer or byte array or string to a hex string"
  (if (integerp thing)
      (format nil "~x" thing)
      (let ((stringp (stringp thing)))
        (with-output-to-string (s)
          (dotimes (i (length thing))
            (let* ((elt (aref thing i))
                   (byte (if stringp (char-code elt) elt))
                   (hi (ash byte -4))
                   (lo (logand byte #xf)))
              (write-char (as-hex hi) s)
              (write-char (as-hex lo) s)))))))

(defun hex2bin (hex &optional res-type)
  "Convert a hex string to binary.
   Result is a byte-string if res-type is :bytes,
   a string if res-type is :string,
   or an integer otherwise (the default)."
  (let* ((len (length hex))
         (bytes (ash (1+ len) -1))
         (res (cond ((eq res-type :string) (make-string bytes))
                    ((eq res-type :bytes)
                     (make-array bytes :element-type '(unsigned-byte 8)))
                    (t nil)))
         (accum 0)
         (cnt (if (evenp len) 2 1))
         (idx -1))
    (dotimes (i len)
      (setq accum (+ (ash accum 4) (as-bin (aref hex i))))
      (when (and res (eql 0 (decf cnt)))
        (setf (aref res (incf idx))
              (if (eq res-type :bytes)
                  accum
                  (code-char accum)))
        (setq accum 0
              cnt 2)))
    (or res accum)))

(defun copy-memory-to-lisp (pointer len byte-array-p)
  (let ((res (if byte-array-p
                 (make-array len :element-type '(unsigned-byte 8))
                 (make-string len))))
    (dotimes (i len)
      (let ((byte (mem-ref pointer :unsigned-char i)))
        (setf (aref res i)
              (if byte-array-p byte (code-char byte)))))
    res))

(defun copy-lisp-to-memory (array pointer &optional (start 0) (end (length array)))
  (loop
     with stringp = (typep array 'string)
     for i from start below end
     for p from 0
     for elt = (aref array i)
     for byte = (if stringp (char-code elt) elt)
     do
       (setf (mem-ref pointer :unsigned-char p) byte)))

(defun base64-encode (string &optional (columns 64))
  (string-to-base64-string string :columns columns))

(defun base64-decode (string)
  (base64-string-to-string string))

(defun zero-string (len)
  (make-string len :initial-element #\0))

(defun integer-string-sort (array)
  "Sort a sequence of integers represented as strings.
   Doesn't use bignum math, just prepends leading zeroes.
   Does NOT clobber the array. Returns a new one."
  (let ((maxlen 0)
        (res (copy-seq array)))
    (map nil
         (lambda (item)
           (let ((len (length item)))
             (when (> len maxlen)
               (setq maxlen len))))
         res)
    (let ((i 0))
      (map nil
           (lambda (item)
             (let ((len (length item)))
               (when (< len maxlen)
                 (setf (elt res i)
                       (strcat (zero-string (- maxlen len))
                               item)))
               (incf i)))
           res))
    (sort res 'string-lessp)))

(defun hsc (x)
  (and x (hunchentoot:escape-for-html x)))

(defun parm (name &rest args)
  (hunchentoot:parameter
   (if args (apply #'format nil name args) name)))

(defun parms (&key (post t) (get nil))
  (let ((req hunchentoot:*request*))
    (append (and post (hunchentoot:post-parameters req))
            (and get (hunchentoot:get-parameters req)))))

(defmacro with-parms ((&rest parms) &body body)
  (let ((bindings (loop for parm in parms
                       collect `(,parm (parm ,(string-downcase parm))))))
    `(let ,bindings ,@body)))

(defun post-parm (name &rest args)
  (hunchentoot:post-parameter
   (if args (apply #'format nil name args) name)))

(defun get-host-name ()
  (usocket::get-host-name))

(defvar *startup-functions* nil)

(defun add-startup-function (function)
  (pushnew function *startup-functions*))

(defun run-startup-functions ()
  (dolist (function (reverse *startup-functions*))
    (funcall function)))

(defvar *save-application-functions* nil)

(defun add-save-application-function (function)
  (pushnew function *save-application-functions*))

(defun run-save-application-functions ()
  (dolist (function (reverse *save-application-functions*))
    (funcall function)))

(defun xor (&rest args)
  "True if an even number of args are true"
  (let ((res nil))
    (dolist (arg args)
      (when arg (setq res (not res))))
    res))

(defun xor-salt (string salt)
  (if (blankp salt)
      string
      (let* ((strlen (length string))
             (saltlen (length salt))
             (saltstr (make-string strlen))
             (idx 0))
        (dotimes (i strlen)
          (setf (aref saltstr i) (aref salt idx))
          (when (>= (incf idx) saltlen) (setq idx 0)))
        (xor-strings string saltstr))))

(defun xor-strings (s1 s2)
  (with-output-to-string (s)
    (let ((s1-len (length s1))
          (s2-len (length s2))
          s-tail s-tail-offset s-tail-len)
      (dotimes (i (min s1-len s2-len))
        (write-char
         (code-char
          (logxor (char-code (aref s1 i)) (char-code (aref s2 i))))
         s))
      (if (> s1-len s2-len)
          (setq s-tail s1
                s-tail-offset s2-len
                s-tail-len (- s1-len s2-len))
          (setq s-tail s2
                s-tail-offset s1-len
                s-tail-len (- s2-len s1-len)))
      (dotimes (i s-tail-len)
        (write-char (aref s-tail (+ s-tail-offset i)) s)))))

(defun browse-url (url)
  (declare (ignorable url))
  #+darwin
  (run-program "open" (list url))
  #+windows
  (run-program
   "/windows/system32/rundll32"
   (list "url.dll,FileProtocolHandler" (format nil "\"~a\"" url)))
  #+linux
  (run-program "firefox" (list url))    ; support only Firefox for now
  )

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
