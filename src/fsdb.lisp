; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File System Database for Truledger
;;;

(in-package :truledger)

;; All put/get database implementations should extend db
(defclass db ()
  ())

(defun unimplemented-db-method (gf)
  (error "Unimplemented db method: ~s" gf))

(defgeneric db-get (db key &rest more-keys)
  (:method ((db db) key &rest more-keys)
    (declare (ignore key more-keys))
    (unimplemented-db-method 'db-get)))

(defgeneric (setf db-get) (value db key &rest more-keys)
  (:method (value (db db) key &rest more-keys)
    (declare (ignore value key more-keys))
    (unimplemented-db-method '(setf db-get))))

(defun db-put (db key value)
  (setf (db-get db key) value))

(defgeneric db-lock (db key)
  (:method ((db db) key)
    (declare (ignore key))
    (unimplemented-db-method 'db-lock)))
  
(defgeneric db-unlock (db lock)
  (:method ((db db) lock)
    (declare (ignore lock))
    (unimplemented-db-method 'db-unlock)))

(defgeneric db-contents (db &rest keys)
  (:method ((db db) &rest keys)
    (declare (ignore keys))
    (unimplemented-db-method 'db-contents)))

(defgeneric db-subdir (db key)
  (:method ((db db) key)
    (declare (ignore key))
    (unimplemented-db-method 'db-subdir)))

(defgeneric db-dir-p (db &rest keys)
  (:method ((db db) &rest keys)
    (declare (ignore keys))
    (unimplemented-db-method 'db-dir-p)))
  
;;;
;;; Implement the db protocol using the file system
;;;

(defun make-fsdb (dir)
  "Create an fsdb isstance for the given file system directory."
  (make-instance 'fsdb :dir dir))

(defclass fsdb (db)
  ((dir :initarg :dir
        :accessor fsdb-dir)))

(defmethod print-object ((db fsdb) stream)
  (print-unreadable-object (db stream :type t)
    (format stream "~s" (fsdb-dir db))))

(defmethod initialize-instance :after ((db fsdb) &rest ignore)
  (declare (ignore ignore))
  (let ((dir (ensure-directory-pathname (fsdb-dir db))))
    (ignore-errors (create-directory dir))
    (setq dir (remove-trailing-separator (namestring (truename (fsdb-dir db)))))
    (setf (fsdb-dir db) dir)))

(defun normalize-key (key)
  (if (eql (aref key 0) #\/)
      (subseq key 1)
      key))

(defmethod db-filename ((db fsdb) key)
  (if (blankp key)
      (values (fsdb-dir db) "")
      (let ((key (normalize-key key)))
        (values (strcat (fsdb-dir db) "/" key)
                key))))

(defmacro with-fsdb-filename ((db filename key) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk (,filename ,key)
              (declare (ignorable ,key))
              ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-fsdb-filename #',thunk ,db ,key))))

(defun call-with-fsdb-filename (thunk db key)
  (multiple-value-bind (filename key) (db-filename db key)
    (with-file-locked (filename)
      (funcall thunk filename key))))

(defun %append-db-keys (key &optional more-keys)
  (if (null more-keys)
      key
      (let* ((len (+ (length key)
                     (reduce #'+ more-keys :key #'length)
                     (length more-keys)))
             (res (make-string len :element-type (array-element-type key)))
             (i -1))
        (dolist (str (cons key more-keys))
          (unless (eql i -1) (setf (aref res (incf i)) #\/))
          (dotimes (j (length str))
            (setf (aref res (incf i)) (aref str j))))
        res)))

(defun append-db-keys (key &rest more-keys)
  (declare (dynamic-extent more-keys))
  (%append-db-keys key more-keys))

(defmethod db-get ((db fsdb) key &rest more-keys)
  (declare (dynamic-extent more-keys))
  (let ((key (%append-db-keys key more-keys)))
    (with-fsdb-filename (db filename key)
      (let ((res (file-get-contents filename)))
        (and (not (equal "" res))
             res)))))

(defmethod (setf db-get) (value (db fsdb) key &rest more-keys)
  (declare (dynamic-extent more-keys))
  (let ((key (%append-db-keys key more-keys)))
    (with-fsdb-filename (db filename key)
      (if (or (null value) (equal value ""))
          (when (probe-file filename) (delete-file filename))
          (file-put-contents filename value)))))

(defmethod db-lock ((db fsdb) key)
  (grab-file-lock (db-filename db key)))

(defmethod db-unlock ((db fsdb) lock)
  (release-file-lock lock))

(defmacro with-db-lock ((db key) &body body)
  (let ((thunk (gensym)))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-db-lock #',thunk ,db ,key))))

(defun call-with-db-lock (thunk db key)
  (let ((lock (db-lock db key)))
    (unwind-protect
         (funcall thunk)
      (db-unlock db lock))))

(defun file-namestring-or-last-directory (path)
  (if (or (pathname-name path) (pathname-type path))
      (file-namestring path)
      (car (last (pathname-directory path)))))

(defmethod db-contents ((db fsdb) &rest keys)
  (let* ((key (if keys
                 (%append-db-keys (car keys) (append (cdr keys) '("*.*")))
                 "*.*"))
         (dir (directory (db-filename db key)
                        :directories t
                        :all nil)))
    ;; DIRECTORY doesn't necessarily return sorted on FreeBSD
    (sort (mapcar 'file-namestring-or-last-directory dir) #'string-lessp)))

(defmethod db-subdir ((db fsdb) key)
  (make-instance 'fsdb :dir (strcat (fsdb-dir db) "/" key)))

(defmethod db-dir-p ((db fsdb) &rest keys)
  (declare (dynamic-extent keys))
  (let ((key (if keys (%append-db-keys (car keys) (cdr keys)) "")))
    (with-fsdb-filename (db filename key)
      (let ((path (probe-file filename)))
        (and path (cl-fad:directory-pathname-p path))))))
    
;;;
;;; Multiple readers, one writer for an fsdb dir.
;;; Best if the writer doesn't run very often, as the readers
;;; busy-wait with process-wait.
;;; Pretty brittle, but I only grab the read lock
;;; around the server dispatch code and the write lock
;;; around do-audit.
;;; Maybe I should use CCL's read-write locks instead.
;;;

(defstruct lock-count
  (readers 0)
  write-waiting-p
  (write-semaphore (make-semaphore)))

;; dir -> lock-count
(defvar *dir-lock-counts*
  (make-equal-hash))

(defvar *dir-lock-counts-lock*
  (make-lock "*dir-lock-counts-lock*"))

(defun get-dir-lock-count (dir)
  (or (gethash dir *dir-lock-counts*)
      (setf (gethash dir *dir-lock-counts*) (make-lock-count))))

(defun read-lock-dir (dir)
  (let ((count nil))
    (loop
       (with-lock-grabbed (*dir-lock-counts-lock* "read-lock-dir")
         (unless count
           (setf count (get-dir-lock-count dir)))
         (unless (lock-count-write-waiting-p count)
           (incf (lock-count-readers count))
           (return)))
       (process-wait
        "read-lock-dir"
        (lambda (count)
          (not (lock-count-write-waiting-p count)))
        count))))

(defun read-unlock-dir (dir)
  (with-lock-grabbed (*dir-lock-counts-lock* "read-unlock-dir")
    (let ((count (get-dir-lock-count dir)))
      (assert (not (eql 0 (lock-count-readers count))))
      (when (eql 0 (decf (lock-count-readers count)))
        (when (lock-count-write-waiting-p count)
          (signal-semaphore (lock-count-write-semaphore count)))))))

(defun write-lock-dir (dir &optional not-reading-p)
  (let (count)
    (with-lock-grabbed (*dir-lock-counts-lock* "write-lock-dir")
      (setf count (get-dir-lock-count dir))
      (assert (not (lock-count-write-waiting-p count)))
      (setf (lock-count-write-waiting-p count) t)
      (unless not-reading-p
        (decf (lock-count-readers count))))
    (let ((done nil))
      (unwind-protect
           (unless (eql 0 (lock-count-readers count))
             (wait-on-semaphore (lock-count-write-semaphore count))
             (setf done t))
        (unless done
          (setf (lock-count-write-waiting-p count) nil))))))

(defun write-unlock-dir (dir &optional not-reading-p)
  (let (count)
    (with-lock-grabbed (*dir-lock-counts-lock* "write-unlock-dir")
      (setf count (get-dir-lock-count dir))
      (setf (lock-count-write-waiting-p count) nil)
      (unless not-reading-p
        (incf (lock-count-readers count))))))

(defmacro with-read-locked-dir ((dir) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-read-locked-dir #',thunk ,dir))))

(defun call-with-read-locked-dir (thunk dir)
  (read-lock-dir dir)
  (unwind-protect
       (funcall thunk)
    (read-unlock-dir dir)))

(defmacro with-write-locked-dir ((dir &optional not-reading-p) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-write-locked-dir #',thunk ,dir ,not-reading-p))))

(defun call-with-write-locked-dir (thunk dir &optional not-reading-p)
  (write-lock-dir dir not-reading-p)
  (unwind-protect
       (funcall thunk)
    (write-unlock-dir dir not-reading-p)))

(defmacro with-read-locked-fsdb ((fsdb) &body body)
  `(with-read-locked-dir ((fsdb-dir ,fsdb))
     ,@body))

(defmacro with-write-locked-fsdb ((fsdb) &body body)
  `(with-write-locked-dir ((fsdb-dir ,fsdb))
     ,@body))

(defun test-dir-lock (&optional (iterations 3) (readers 5))
  (let ((stop nil)
        (dir "dir")
        (stream *standard-output*))
    (dotimes (i readers)
      (process-run-function
       (format nil "Reader ~s" i)
       (lambda (cnt)
         (loop
            (with-read-locked-dir (dir)
              (format stream "Start reader ~s~%" cnt)
              (sleep 0.5)
              (format stream "Stop reader ~s~%" cnt))
            (when stop (return))))
       i))
    (unwind-protect
         (dotimes (i iterations)
           (sleep 0.1)
           (with-read-locked-dir (dir)
             (with-write-locked-dir (dir)
               (format t "Start writer~%")
               (sleep 0.1)
               (format t "Stop writer~%"))))
      (setf stop t))))

;;;
;;; A wrapper for a db that saves writes until commit
;;;

(defclass db-wrapper (db)
  ((db :initarg :db
       :accessor db-wrapper-db)
   (dirs :initform (list nil :dir)
         :accessor db-wrapper-dirs)
   (locks :initform nil
          :accessor db-wrapper-locks)))

(defmethod print-object ((db db-wrapper) stream)
  (print-unreadable-object (db stream :type t)
    (format stream "~s" (db-wrapper-db db))))

(defun make-db-wrapper (db)
  (make-instance 'db-wrapper :db db))

;; This doesn't protect against treating a file system directory
;; as a file or vice-versa. If you do it, you'll get an error
;; from commit-db-wrapper
(defun get-db-wrapper-cell (db key more-keys &key create-p dir-cell-p)
  (assert (not (and create-p dir-cell-p)))
  ;; Eliminate trailing slashes
  (let* ((keystr (%append-db-keys key more-keys))
         (keys (split-sequence:split-sequence
                #\/ keystr :remove-empty-subseqs t)))
    (setf keys (nreverse keys))
    (let ((file (pop keys))
          (dirs (nreverse keys))
          (parent (db-wrapper-dirs db)))
      (when (null file)
        (return-from get-db-wrapper-cell (cdr parent)))
      (dolist (dir dirs)
        (let  ((cell (assoc dir (cddr parent) :test #'equal)))
          (cond (cell
                 (when create-p
                   (assert (eq (cadr cell) :dir))))
                (create-p
                 (setf cell (list dir :dir))
                 (push cell (cddr parent)))
                (t (return-from get-db-wrapper-cell nil)))
          (setf parent cell)))
      (let ((cell (assoc file (cddr parent) :test #'equal)))
        (cond (cell
               (if dir-cell-p
                   (when (eq (cadr cell) :file)
                     (assert (not create-p))
                     (return-from get-db-wrapper-cell nil))
                   (assert (eq (cadr cell) :file))))
              (create-p
               (setf cell (list file :file))
               (push cell (cddr parent))))
        (cdr cell)))))

(defmethod db-get ((db db-wrapper) key &rest more-keys)
  (declare (dynamic-extent more-keys))
  (multiple-value-bind (val val-p)
      (apply #'db-wrapper-get db key more-keys)
    (if val-p
        val
        (apply #'db-get (db-wrapper-db db) key more-keys))))

(defun db-wrapper-get (db key &rest more-keys)
  "Returns two values, the value and whether it was found in the db-wrapper"
  (declare (dynamic-extent more-keys))
  (check-type db db-wrapper)
  (let ((cell (get-db-wrapper-cell db key more-keys)))
    (and cell (values (cdr cell) t))))

(defmethod (setf db-get) (value (db db-wrapper) key &rest more-keys)
  (declare (dynamic-extent more-keys))
  (when (equal value "")
    (setf value nil))
  (let ((cell (get-db-wrapper-cell db key more-keys :create-p t)))
    (setf (cdr cell) value)))
      
(defmethod db-contents ((db db-wrapper) &rest keys)
  (declare (dynamic-extent keys))
  (let ((cell-res (apply #'db-wrapper-contents db keys))
        (res (apply #'db-contents (db-wrapper-db db) keys)))
    (if cell-res
        (sort (union cell-res res :test #'equal) #'string<)
        res)))

(defun db-wrapper-contents (db &rest keys)
  (declare (dynamic-extent keys))
  (let ((cell (get-db-wrapper-cell db (car keys) (cdr keys) :dir-cell-p t)))
    (when cell
      (mapcar #'car (cdr cell)))))

(defun rollback-db-wrapper (db)
  (check-type db db-wrapper)
  (setf (db-wrapper-dirs db) (list nil :dir))
  nil)

;; This sure conses a lot. May need to fix that at some point.
(defun commit-db-wrapper (db)
  (check-type db db-wrapper)
  (let ((wrapped-db (db-wrapper-db db))
        (cnt 0))
    (labels ((do-dir-cell (dir-cell path)
               (dolist (cell dir-cell)
                 (let ((type (cadr cell)))
                   (ecase type
                     (:dir
                      (do-dir-cell (cddr cell) (cons (car cell) path)))
                     (:file
                      (let* ((path (reverse (cons (car cell) path)))
                             (key (%append-db-keys (car path) (cdr path))))
                        (db-put wrapped-db key (cddr cell))
                        (incf cnt))))))))
      (do-dir-cell (cddr (db-wrapper-dirs db)) nil))
    (rollback-db-wrapper db)
    (let ((locks (db-wrapper-locks db))
          (wrapped-db (db-wrapper-db db)))
      (setf (db-wrapper-locks db) nil)
      (dolist (key.lock locks)
        (ignore-errors
          (db-unlock wrapped-db (cdr key.lock)))))
    cnt))
    
;; db-wrapper locking. All locks held until commit
(defmethod db-lock ((db db-wrapper) key)
  (unless (assoc key (db-wrapper-locks db) :test #'equal)
    (let ((lock (db-lock (db-wrapper-db db) key)))
      (push (cons key lock) (db-wrapper-locks db))
      lock)))

(defmethod db-unlock ((db db-wrapper) lock)
  (declare (ignore lock)))

(defmacro with-db-wrapper ((db &optional (db-form db)) &body body)
  (check-type db symbol)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,db) ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-db-wrapper #',thunk ,db-form))))

(defun call-with-db-wrapper (thunk db)
  (let ((db (make-db-wrapper db)))
    (multiple-value-prog1
        (funcall thunk db)
    ;; Non-local exit causes commit to be skipped.
      (commit-db-wrapper db))))


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
