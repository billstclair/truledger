(in-package :crypto-tests)

(defun hex-string-to-byte-array (string &key (start 0) (end nil))
  ;; This function disappears from profiles if SBCL can inline the
  ;; POSITION call, so declare SPEED high enough to trigger that.
  (declare (type string string) (optimize (speed 2)))
  (let* ((end (or end (length string)))
         (length (/ (- end start) 2))
         (key (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) key))
    (flet ((char-to-digit (char)
             (let ((x (position char "0123456789abcdef" :test #'char-equal)))
               (or x (error "Invalid hex key ~A specified" string)))))
      (loop for i from 0
            for j from start below end by 2
            do (setf (aref key i)
                     (+ (* (char-to-digit (char string j)) 16)
                        (char-to-digit (char string (1+ j)))))
            finally (return key)))))


;;; test vector files

(defun test-vector-filename (ident)
  (merge-pathnames (make-pathname :directory '(:relative "test-vectors")
                                  :name (format nil "~(~A~)" ident)
                                  :type "testvec")
                   #.*compile-file-pathname*))

(defun run-test-vector-file (name function-map)
  (let ((filename (test-vector-filename name)))
    (with-open-file (stream filename :direction :input
                            :element-type 'character
                            :if-does-not-exist :error)
      (loop for form = (read stream nil stream)
         until (eq form stream) do
         (cond
           ((not (listp form))
            (error "Invalid form in test vector file ~A: ~A" filename form))
           (t
            (let ((test-function (cdr (assoc (car form) function-map))))
              (unless test-function
                (error "No test function defined for ~A" (car form)))
              (apply test-function name (cdr form)))))
         finally (return t)))))

;;; cipher testing

(defun ecb-mode-test (cipher-name hexkey hexinput hexoutput)
  (cipher-test-guts cipher-name :ecb hexkey hexinput hexoutput))

(defun stream-mode-test (cipher-name hexkey hexinput hexoutput)
  (cipher-test-guts cipher-name :stream hexkey hexinput hexoutput))

(defparameter *cipher-tests*
  (list (cons :ecb-mode-test #'ecb-mode-test)
        (cons :stream-mode-test #'stream-mode-test)))

(defun cipher-test-guts (cipher-name mode hexkey hexinput hexoutput)
  (labels ((frob-hex-string (func hexinput)
             (let* ((key (hex-string-to-byte-array hexkey))
                    (input (hex-string-to-byte-array hexinput))
                    (cipher (crypto:make-cipher cipher-name :key key
                                                :mode mode))
                    (scratch (copy-seq input)))
               (funcall func cipher input scratch)
               scratch))
           (cipher-test (func hexinput hexoutput)
             (let ((output (hex-string-to-byte-array hexoutput)))
               (not (mismatch (frob-hex-string func hexinput) output)))))
    (unless (cipher-test #'crypto:encrypt hexinput hexoutput)
      (error "encryption failed for ~A on key ~A, input ~A, output ~A"
             cipher-name hexkey hexinput hexoutput))
    (unless (cipher-test #'crypto:decrypt hexoutput hexinput)
      (error "decryption failed for ~A on key ~A, input ~A, output ~A"
             cipher-name hexkey hexoutput hexinput))))

;;; encryption mode consistency checking

;;; tests from NIST

(defun mode-test (mode cipher-name hexkey hexiv hexinput hexoutput)
  (labels ((frob-hex-string (cipher func hexinput)
             (let* ((input (hex-string-to-byte-array hexinput))
                    (scratch (copy-seq input)))
               (funcall func cipher input scratch)
               scratch))
           (cipher-test (cipher func hexinput hexoutput)
             (let ((output (hex-string-to-byte-array hexoutput)))
               (not (mismatch (frob-hex-string cipher func hexinput) output)))))
    (let* ((key (hex-string-to-byte-array hexkey))
           (iv (hex-string-to-byte-array hexiv))
           (cipher (crypto:make-cipher cipher-name :key key :mode mode
                                       :initialization-vector iv)))
      (unless (cipher-test cipher #'crypto:encrypt hexinput hexoutput)
        (error "encryption failed for ~A on key ~A, input ~A, output ~A"
               cipher-name hexkey hexinput hexoutput))
      (reinitialize-instance cipher :key key :mode mode
                             :initialization-vector iv)
      (unless (cipher-test cipher #'crypto:decrypt hexoutput hexinput)
        (error "decryption failed for ~A on key ~A, input ~A, output ~A"
               cipher-name hexkey hexoutput hexinput)))))

(defparameter *mode-tests*
  (list (cons :mode-test #'mode-test)))


;;; digest testing routines

(defun digest-test (digest-name string hexdigest)
  (let ((digest (crypto:make-digest digest-name)))
    (unless (digest-test-one-shot-guts digest string hexdigest)
      (error "one-shot ~A digest of ~S failed" digest-name string))
    (reinitialize-instance digest)
    (unless (digest-test-incremental-guts digest string hexdigest)
      (error "incremental ~A digest of ~S failed"
             digest-name string))
    #+(or sbcl cmucl)
    (reinitialize-instance digest)
    #+(or sbcl cmucl)
    (unless (digest-test-fill-pointer-guts digest string hexdigest)
      (error "fill-pointer'd ~A digest of ~S failed"
             digest-name string))
    #+(or lispworks sbcl cmucl openmcl allegro)
    (reinitialize-instance digest)
    #+(or lispworks sbcl cmucl openmcl allegro)
    (unless (digest-test-stream-guts digest-name string hexdigest)
      (error "stream-y ~A digest of ~S failed" digest-name string))))

(defun digest-bit-test (digest-name leading byte trailing hexdigest)
  (unless (digest-bit-test-guts digest-name leading byte
                                trailing hexdigest)
    (error "individual bit test ~A digest of (~D #x~2,'0X ~D) failed"
           digest-name leading byte trailing)))

(defparameter *digest-tests*
  (list (cons :digest-test #'digest-test)
        (cons :digest-bit-test #'digest-bit-test)))

#+(or lispworks sbcl cmucl openmcl allegro)
(defun digest-test-stream-guts (digest string hexdigest)
  (let* ((input (crypto:ascii-string-to-byte-array string))
         (expected-digest (hex-string-to-byte-array hexdigest))
         (stream (crypto:make-digesting-stream digest)))
    (write-sequence input stream)
    (not (mismatch (crypto:produce-digest stream) expected-digest))))

(defun digest-test-one-shot-guts (digest string hexdigest)
  (let* ((input (crypto:ascii-string-to-byte-array string))
         (expected-digest (hex-string-to-byte-array hexdigest))
         (result (crypto:digest-sequence digest input)))
    (not (mismatch result expected-digest))))

(defun digest-test-incremental-guts (digester string hexdigest)
  (let* ((input (crypto:ascii-string-to-byte-array string))
         (expected-digest (hex-string-to-byte-array hexdigest))
         (length (length input)))
    (loop for i from 0 below length
          do (crypto:update-digest digester input :start i :end (1+ i))
          finally (let ((result (crypto:produce-digest digester)))
                    (return (not (mismatch result expected-digest)))))))

(defun digest-test-fill-pointer-guts (digest string hexdigest)
  (let* ((octets (crypto:ascii-string-to-byte-array string))
         (input (let ((x (make-array (* 2 (length octets))
                                     :fill-pointer 0
                                     :element-type '(unsigned-byte 8))))
                  (dotimes (i (length octets) x)
                    (vector-push (aref octets i) x))))
         (expected-digest (hex-string-to-byte-array hexdigest))
         (result (crypto:digest-sequence digest input)))
    (not (mismatch result expected-digest))))

(defun digest-bit-test-guts (digest leading byte trailing hexdigest)
  (let* ((input (let ((vector (make-array (+ 1 leading trailing)
                                          :element-type '(unsigned-byte 8)
                                          :initial-element 0)))
                  (setf (aref vector leading) byte)
                  vector))
         (expected-digest (hex-string-to-byte-array hexdigest))
         (result (crypto:digest-sequence digest input)))
    (not (mismatch result expected-digest))))


;;; mac testing routines

(defun hmac-test (name digest-name key data expected-digest)
  (declare (ignore name))
  (let ((hmac (ironclad:make-hmac (hex-string-to-byte-array key) digest-name)))
    (ironclad:update-hmac hmac (hex-string-to-byte-array data))
    (when (mismatch (hex-string-to-byte-array expected-digest)
                    (ironclad:hmac-digest hmac))
      (error "HMAC/~A failed on key ~A, input ~A, output ~A"
             digest-name key data expected-digest))
    (loop with data = (hex-string-to-byte-array data)
       initially (reinitialize-instance hmac :key (hex-string-to-byte-array key))
       for i from 0 below (length data)
       do (ironclad:update-hmac hmac data :start i :end (1+ i))
       (ironclad:hmac-digest hmac)
       finally (when (mismatch (hex-string-to-byte-array expected-digest)
                               (ironclad:hmac-digest hmac))
                 (error "progressive HMAC/~A failed on key ~A, input ~A, output ~A"
                        digest-name key data expected-digest)))))

(defun cmac-test (name cipher-name key data expected-digest)
  (declare (ignore name))
  (let ((cmac (ironclad:make-cmac (hex-string-to-byte-array key) cipher-name)))
    (ironclad:update-cmac cmac (hex-string-to-byte-array data))
    (when (mismatch (hex-string-to-byte-array expected-digest)
                    (ironclad:cmac-digest cmac))
      (error "CMAC/~A failed on key ~A, input ~A, output ~A"
             cipher-name key data expected-digest))))

(defparameter *mac-tests*
  (list (cons :hmac-test #'hmac-test)
        (cons :cmac-test #'cmac-test)))
