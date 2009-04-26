; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Various utility functions
;;;

(in-package :trubanc)

(defun file-get-contents (file)
  (with-open-file (stream file :if-does-not-exist nil)
    (when stream
      (let* ((len (file-length stream))
             (s (make-string len)))
        (read-sequence s stream)
        s))))

(defun file-put-contents (file contents)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-sequence contents stream)
    contents))

(defun hex (integer)
  "Return a string encoding integer as hex"
  (format nil "~x" integer))

(defparameter *whitespace* '(#\newline #\return #\tab #\space))

(defun trim (string)
  (string-left-trim *whitespace* (string-right-trim *whitespace* string)))

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

(defun base64-encode (string)
  (string-to-base64-string string :columns 64))

(defun base64-decode (string)
  (base64-string-to-string string))

(defun assocequal (item alist)
  (assoc item alist :test 'equal))

(defun make-equal-hash (&rest keys-and-values)
  (let ((hash (make-hash-table :test 'equal)))
    (loop
       (when (null keys-and-values) (return))
       (let ((key (pop keys-and-values))
             (value (pop keys-and-values)))
         (setf (gethash key hash) value)))
    hash))

(defun strcat (&rest strings)
  "Concatenate a bunch of strings"
  (apply #'concatenate 'string (mapcar 'string strings)))

;; This should probably be smart enough to not eval PLACE twice,
;; but I only ever use it on symbols, so it doesn't really matter.
(defmacro dotcat (place &rest strings)
  `(setf ,place (strcat ,place ,@strings)))

(defun remove-trailing-separator (string &optional (separator #\/))
  (let ((len (length string)))
    (if (and (> len 0) (eql separator (aref string (1- len))))
        (subseq string 0 (1- len))
        string)))      

(defun implode (separator &rest strings)
  (declare (dynamic-extent strings))
  (let ((res (if (null strings) "" (car strings))))
    (dolist (item (cdr strings))
      (setq res (strcat res separator item)))
    res))

(defun explode (separator string)
  (let* ((len (length string))
         (res
          (loop
             with len = (length string)
             for start = 0 then (1+ end)
             while (< start len)
             for end = (or (position separator string :start start) len)
             while end
             collect (subseq string start end))))
    (if (and (> len 0) (eql separator (aref string (1- len))))
        (nconc res (list ""))
        res)))t

(defun strstr (haystack needle)
  "Find NEEDLE in HAYSTACK. Return the tail of HAYSTACK including NEEDLE."
  (let ((pos (search needle haystack)))
    (and pos (subseq haystack pos))))

(defun str-replace (old new string)
  "Change all instance of OLD to NEW in STRING"
  (loop with pos = 0
        with old-len = (length old)
        with new-len = (length new)
        with res = string
        for idx = (search old res :start2 pos)
        do
     (when (null idx) (return res))
     (setq res (strcat (subseq res 0 idx) new (subseq res (+ idx old-len)))
           pos (+ idx new-len))))

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

(defun get-host-name ()
  (usocket::get-host-name))

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
