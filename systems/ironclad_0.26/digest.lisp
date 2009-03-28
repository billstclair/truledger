;;;; digest.lisp -- common functions for hashing

(in-package :crypto)


;;; defining digest (hash) functions

(eval-when (:compile-toplevel :load-toplevel)
(defconstant +buffer-size+ (* 128 1024))
) ; EVAL-WHEN

(deftype buffer-index () `(integer 0 (,+buffer-size+)))

(defun update-digest-from-stream (digest stream &key buffer (start 0) end)
  (cond
    ((let ((element-type (stream-element-type stream)))
       (or (equal element-type '(unsigned-byte 8))
           (equal element-type '(integer 0 255))))
     (flet ((frob (read-buffer start end)
              (loop for last-updated = (read-sequence read-buffer stream
                                                      :start start :end end)
                 do (update-digest digest read-buffer
                                   :start start :end last-updated)
                 until (< last-updated end)
                 finally (return digest))))
       (if buffer
           (frob buffer start (or end (length buffer)))
           (let ((buffer (make-array +buffer-size+
                                     :element-type '(unsigned-byte 8))))
             (declare (dynamic-extent buffer))
             (frob buffer 0 +buffer-size+)))))
    (t
     (error "Unsupported stream element-type ~S for stream ~S."
            (stream-element-type stream) stream))))

;;; Storing a length at the end of the hashed data is very common and
;;; can be a small bottleneck when generating lots of hashes over small
;;; quantities of data.  We assume that the appropriate locations have
;;; already been zeroed if necessary.  LENGTH is also assumed to be an
;;; (effectively) 64-bit quantity.
(declaim (inline store-data-length))
(defun store-data-length (block length offset &optional big-endian-p)
  (let ((lo (if big-endian-p (1+ offset) offset))
        (hi (if big-endian-p offset (1+ offset))))
    #+(and sbcl 32-bit)
    (cond
      ((sb-int:fixnump length)
       (setf (aref block lo) length))
      ;; Otherwise, we have a bignum.
      (t
       (locally (declare (optimize (safety 0))
                         (type sb-bignum:bignum-type length))
         (cond
           ((= (sb-bignum:%bignum-length length) 1)
            (setf (aref block lo) (sb-bignum:%bignum-ref length 0)))
           (t
            (setf (aref block lo) (sb-bignum:%bignum-ref length 0)
                  (aref block hi) (sb-bignum:%bignum-ref length 1)))))))
    #+(and cmu 32-bit)
    (cond
      ((ext:fixnump length)
       (setf (aref block lo) length))
      ;; Otherwise, we have a bignum.
      (t
       (locally (declare (optimize (safety 0))
                         (type bignum:bignum-type length))
         (cond
           ((= (bignum:%bignum-length length) 1)
            (setf (aref block lo) (bignum:%bignum-ref length 0)))
           (t
            (setf (aref block lo) (bignum:%bignum-ref length 0)
                  (aref block hi) (bignum:%bignum-ref length 1)))))))
    #-(or (and sbcl 32-bit)
          (and cmu 32-bit))
    (setf (aref block lo) (ldb (byte 32 0) length)
          (aref block hi) (ldb (byte 32 32) length))))

;;; macros for "mid-level" functions

(defmacro define-digest-registers ((digest-name &key (endian :big) (size 4)) &rest registers)
  (let* ((struct-name (intern (format nil "~A-REGS" digest-name)))
         (constructor (intern (format nil "INITIAL-~A" struct-name)))
         (copier (intern (format nil "%COPY-~A" struct-name)))
         (digest-fun (intern (format nil "~AREGS-DIGEST" digest-name)))
         (register-bit-size (* size 8))
         (digest-size (* size (length registers)))
         (ref-fun (ubref-fun-name register-bit-size (eq endian :big))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defstruct (,struct-name
                      (:type (vector (unsigned-byte ,register-bit-size)))
                      (:constructor ,constructor ())
                      (:copier ,copier))
           ,@registers)
         ;; LispWorks properly defines STRUCT-NAME as a type with DEFSTRUCT,
         ;; so just avoid gratuitous warnings here.
         #-lispworks
         (deftype ,struct-name ()
           '(simple-array (unsigned-byte ,register-bit-size) (,(length registers)))))
       (defun ,digest-fun (regs buffer start)
         (declare (type ,struct-name regs)
                  (type (simple-array (unsigned-byte 8) (*)))
                  (type (integer 0 ,(- array-dimension-limit digest-size)) start)
                  ,(burn-baby-burn))
         ,(let ((inlined-unpacking
                 `(setf ,@(loop for (reg value) in registers
                             for index from 0 by size
                             nconc `((,ref-fun buffer (+ start ,index))
                                     (,(intern (format nil "~A-REGS-~A" digest-name reg)) regs))))))
               (cond
                 #+(and sbcl :little-endian)
                 ((eq endian :little)
                  `(if (= start 0)
                       (sb-kernel:ub8-bash-copy regs 0 buffer 0 ,digest-size)
                       ,inlined-unpacking))
                 #+(and sbcl :big-endian)
                 ((eq endian :big)
                  `(if (= start 0)
                       (sb-kernel:ub8-bash-copy regs 0 buffer 0 ,digest-size)
                       ,inlined-unpacking))
                 (t inlined-unpacking)))
         buffer))))

(defmacro define-digest-updater (digest-name &body body)
  (destructuring-bind (maybe-doc-string &rest rest) body
    `(defmethod update-digest ((state ,digest-name) (sequence vector) &key (start 0) (end (length sequence)))
       ,@(when (stringp maybe-doc-string)
               `(,maybe-doc-string))
       (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
       (declare (type index start end))
       ,(hold-me-back)
       ,@(if (stringp maybe-doc-string)
             rest
             body))))

(defmacro define-digest-finalizer (digest-name digest-size &body body)
  (let ((inner-fun-name (intern (format nil "%FINALIZE-~A-STATE" digest-name)))
        (reg-digest-fun (intern (format nil "~AREGS-DIGEST" digest-name))))
    (destructuring-bind (maybe-doc-string &rest rest) body
      `(progn
         (defmethod finalize-digest ((state ,digest-name)
                                     &optional buffer buffer-start)
           ,@(when (stringp maybe-doc-string)
                   `(,maybe-doc-string))
           (declare (type (or (simple-array (unsigned-byte 8) (*)) cl:null) buffer))
           (cond
             (buffer
              ;; verify that the buffer is large enough
              (let ((buffer-start (or buffer-start 0)))
                (if (<= ,digest-size (- (length buffer) buffer-start))
                    (,inner-fun-name state buffer buffer-start)
                    (error 'insufficient-buffer-space
                           :buffer buffer :start buffer-start
                           :length ,digest-size))))
             (t
              (,inner-fun-name state
                               (make-array ,digest-size
                                           :element-type '(unsigned-byte 8))
                               0))))
         (defun ,inner-fun-name (state %buffer buffer-start)
           ,(hold-me-back)
           (macrolet ((finalize-registers (state regs)
                        (declare (ignore state))
                        `(,',reg-digest-fun ,regs %buffer buffer-start)))
             ,@(if (stringp maybe-doc-string)
                   rest
                   body)))))))

;;; high-level generic function drivers

;;; These three functions are intended to be one-shot ways to digest
;;; an object of some kind.  You could write these in terms of the more
;;; familiar digest interface below, but these are likely to be slightly
;;; more efficient, as well as more obvious about what you're trying to
;;; do.
(defgeneric digest-file (digest-spec pathname &rest args
                                     &key buffer start end
                                     digest digest-start)
  (:documentation "Return the digest of the contents of the file named by PATHNAME using
the algorithm DIGEST-NAME.

If DIGEST is provided, the digest will be placed into DIGEST starting at
DIGEST-START.  DIGEST must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
An error will be signaled if there is insufficient room in DIGEST.

If BUFFER is provided, the portion of BUFFER between START and END will
be used to hold data read from the stream."))

(defmethod digest-file ((digest-name symbol) pathname &rest kwargs)
  (apply #'digest-file (make-digest digest-name) pathname kwargs))

(defmethod digest-file (state pathname &key buffer (start 0) end
                        digest (digest-start 0))
  (with-open-file (stream pathname :element-type '(unsigned-byte 8)
                          :direction :input
                          :if-does-not-exist :error)
    (update-digest-from-stream state stream
                               :buffer buffer :start start :end end)
    (produce-digest state :digest digest :digest-start digest-start)))

(defgeneric digest-stream (digest-spec stream &rest args
                                       &key buffer start end
                                       digest digest-start)
  (:documentation "Return the digest of the contents of STREAM using the algorithm
DIGEST-NAME.  STREAM-ELEMENT-TYPE of STREAM should be (UNSIGNED-BYTE 8).

If DIGEST is provided, the digest will be placed into DIGEST starting at
DIGEST-START.  DIGEST must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
An error will be signaled if there is insufficient room in DIGEST.

If BUFFER is provided, the portion of BUFFER between START and END will
be used to hold data read from the stream."))

(defmethod digest-stream ((digest-name symbol) stream &rest kwargs)
  (apply #'digest-stream (make-digest digest-name) stream kwargs))

(defmethod digest-stream (state stream &key buffer (start 0) end
                          digest (digest-start 0))
  (update-digest-from-stream state stream
                               :buffer buffer :start start :end end)
  (produce-digest state :digest digest :digest-start digest-start))

(defgeneric digest-sequence (digest-spec sequence &rest args
                                         &key start end digest digest-start)
  (:documentation "Return the digest of the subsequence of SEQUENCE specified by START
and END using the algorithm DIGEST-NAME.  For CMUCL and SBCL, SEQUENCE
can be any vector with an element-type of (UNSIGNED-BYTE 8); for other
implementations, SEQUENCE must be a SIMPLE-ARRAY.

If DIGEST is provided, the digest will be placed into DIGEST starting at
DIGEST-START.  DIGEST must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
An error will be signaled if there is insufficient room in DIGEST."))

(defmethod digest-sequence ((digest-name symbol) sequence &rest kwargs)
  (apply #'digest-sequence (make-digest digest-name) sequence kwargs))

(defmethod digest-sequence (state sequence &key (start 0) end
                            digest (digest-start 0))
  (declare (type (vector (unsigned-byte 8)) sequence) (type index start))
  #+cmu
  ;; respect the fill-pointer
  (let ((end (or end (length sequence))))
    (declare (type index end))
    (lisp::with-array-data ((data sequence) (real-start start) (real-end end))
      (declare (ignore real-end))
      (update-digest state data
                     :start real-start :end (+ real-start (- end start)))))
  #+sbcl
  ;; respect the fill-pointer
  (let ((end (or end (length sequence))))
    (sb-kernel:with-array-data ((data sequence) (real-start start) (real-end end))
      (declare (ignore real-end))
      (update-digest state data
                     :start real-start :end (+ real-start (- end start)))))
  #-(or cmu sbcl)
  (let ((real-end (or end (length sequence))))
    (declare (type index real-end))
    (update-digest state sequence
                   :start start :end (or real-end (length sequence))))
  (produce-digest state :digest digest :digest-start digest-start))

;;; These four functions represent the common interface for digests in
;;; other crypto toolkits (OpenSSL, Botan, Python, etc.).  You obtain
;;; some state object for a particular digest, you update it with some
;;; data, and then you get the actual digest.  Flexibility is the name
;;; of the game with these functions.
(defun make-digest (digest-name)
  "Return a digest object which uses the algorithm DIGEST-NAME."
  (typecase digest-name
    (symbol
     (let ((name (massage-symbol digest-name)))
       (if (digestp name)
           (funcall (the function (get name '%make-digest)))
           (error 'unsupported-digest :name digest-name))))
    (t
     (error 'type-error :datum digest-name :expected-type 'symbol))))

(defgeneric copy-digest (digester)
  (:documentation "Return a copy of DIGESTER.  The copy is a deep copy,
not a shallow copy as might be returned by COPY-STRUCTURE."))

(defgeneric update-digest (digester thing &key &allow-other-keys)
  (:documentation "Update the internal state of DIGESTER with THING.
The exact method is determined by the type of THING."))

(defgeneric produce-digest (digester &key digest digest-start)
  (:documentation "Return the hash of the data processed by
DIGESTER so far. This function does not modify the internal state
of DIGESTER.

If DIGEST is provided, the hash will be placed into DIGEST starting at
DIGEST-START.  DIGEST must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
An error will be signaled if there is insufficient room in DIGEST."))

(defmethod produce-digest (digester &key digest (digest-start 0))
  (finalize-digest digester digest digest-start))
 

;;; the digest-defining macro

(defclass digest ()
  ((amount :accessor amount-processed :type (unsigned-byte 64)
           :initform 0)
   (buffer-index :accessor buffer-index :initform 0)
   (finalized-p :accessor finalized-p :initform nil)))

(defun digestp (sym)
  (get sym '%digest-length))

(defun list-all-digests ()
  (loop for symbol being each external-symbol of (find-package :ironclad)
     if (digestp symbol)
     collect symbol))

(defun digest-supported-p (name)
  "Return T if the digest NAME is a valid digest name."
  (and (symbolp name)
       (not (cl:null (digestp name)))))

(defgeneric digest-length (digest)
  (:documentation "Return the number of bytes in a digest generated by DIGEST."))

(defun massage-symbol (symbol)
  (let ((package (symbol-package symbol))
        (ironclad (load-time-value (find-package :ironclad))))
    (cond
      ((eq package ironclad) symbol)
      ((eq package (load-time-value (find-package :keyword)))
       (find-symbol (symbol-name symbol) ironclad))
      (t nil))))

(defmethod digest-length ((digest-name symbol))
  (or (digestp (massage-symbol digest-name))
      (error 'unsupported-digest :name digest-name)))

(defmethod digest-length ((digest digest))
  (digest-length (class-name (class-of digest))))

(defmethod digest-length (digest-name)
  (error 'unsupported-digest :name digest-name))

(defmethod update-digest (digester (stream stream) &key buffer (start 0) end
                          &allow-other-keys)
  (update-digest-from-stream digester stream
                             :buffer buffer :start start :end end))

(defun optimized-maker-name (name)
  (let ((*package* (find-package :ironclad)))
    (intern (format nil "%MAKE-~A-DIGEST" name))))

(defmacro defdigest (name &key digest-length)
  (let ((optimized-maker-name (optimized-maker-name name)))
    `(progn
       (export ',name :ironclad)
       (setf (get ',name '%digest-length) ,digest-length)
       (setf (get ',name '%make-digest) (symbol-function ',optimized-maker-name))
       (defmethod digest-length ((digest ,name))
         ,digest-length))))

;;; If we pass a constant argument to MAKE-DIGEST, convert the
;;; MAKE-DIGEST call to a direct call to the state creation function.
(define-compiler-macro make-digest (&whole form &environment env
                                           name)
  (declare (ignore env))
  (cond
    ((or (keywordp name)
         (and (quotationp name) (symbolp name)))
     (let ((name (massage-symbol (unquote name))))
       (if (digestp name)
           `(,(optimized-maker-name name))
           form)))
    (t form)))

;;; And do the same for various one-shot digest functions.
(defun maybe-expand-one-shot-call (form funname name 2nd-arg keys)
  (cond
    ((or (keywordp name)
         (and (quotationp name) (symbolp name)))
     (let ((name (massage-symbol (unquote name))))
       (if (digestp name)
           `(,funname (,(optimized-maker-name name)) ,2nd-arg ,@keys)
           form)))
    (t form)))

(define-compiler-macro digest-sequence (&whole form &environment env
                                               name sequence &rest keys)
  (declare (ignore env))
  (maybe-expand-one-shot-call form 'digest-sequence name sequence keys))

(define-compiler-macro digest-stream (&whole form &environment env
                                             name stream &rest keys)
  (declare (ignore env))
  (maybe-expand-one-shot-call form 'digest-stream name stream keys))

(define-compiler-macro digest-file (&whole form &environment env
                                           name file &rest keys)
  (declare (ignore env))
  (maybe-expand-one-shot-call form 'digest-file name file keys))
