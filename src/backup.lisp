; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trubanc server backup
;;;

(in-package :trubanc-server)

(defun make-backup-db (db)
  (let ((res (make-instance 'backup-db :wrapped-db db))
        (readindex (parse-integer (db-get db $BACKUP $READINDEX)))
        (writeindex (parse-integer (db-get db $BACKUP $WRITEINDEX))))
    (when readindex (setf (read-index res) readindex))
    (when writeindex (setf (write-index res) writeindex))
    res))    

(defclass backup-db (db)
  ((wrapped-db :initarg :wrapped-db
               :accessor wrapped-db
               :type db)
   (lock :initform (make-lock "backup-lock")
         :accessor backup-db-lock)
   (write-index :initform 0
                :accessor write-index)
   (read-index :initform 0
               :accessor read-index)))

(defmethod print-object ((db backup-db) stream)
  (print-unreadable-object (db stream :type t)
    (format stream "wrapping: ~s" (wrapped-db db))))

(defmethod initialize-instance :before ((db backup-db) &key wrapped-db)
  (unless (typep wrapped-db 'db)
    (error "wrapped-db must be specified")))

(defmethod db-get ((db backup-db) key &rest more-keys)
  (declare (dynamic-extent more-keys))
  (apply #'db-get (wrapped-db db) key more-keys))

(defmethod (setf db-get) (value (db backup-db) key &rest more-keys)
  (backup-write db value key more-keys)
  (apply #'(setf db-get) value (wrapped-db db) key more-keys))

(defmethod db-lock ((db backup-db) key)
  (db-lock (wrapped-db db) key))

(defmethod db-unlock ((db backup-db) lock)
  (db-unlock (wrapped-db db) lock))

(defmethod db-contents ((db backup-db) &rest keys)
  (declare (dynamic-extent keys))
  (apply #'db-contents (wrapped-db db) keys))

(defmethod db-subdir ((db backup-db) key)
  (make-instance 'backup-db
                 :wrapped-db (db-subdir (wrapped-db db) key)
                 :lock (backup-db-lock db)))

(defconstant $BACKUP-DELIMITER #.(format nil "///~%"))

;; Here's where we write the data to the log.
;; It's taken out by the backup-reader thread below.
(defmethod backup-write ((db backup-db) value key more-keys)
  (with-lock-grabbed ((backup-db-lock db))
    (let ((index (stringify (incf (write-index db)))))
      (prog1 (setf (db-get (wrapped-db db) $BACKUP index)
                   (format nil "~a~a~a"
                           (apply #'append-db-keys key more-keys)
                           $BACKUP-DELIMITER
                           (or value "")))
        (setf (db-get (wrapped-db db) $BACKUP $WRITEINDEX) index)))))

;; Returns two values: key & value.
;; Null return means the read-index has caught up with the write-index.
(defmethod backup-read ((db backup-db))
  (with-lock-grabbed ((backup-db-lock db))
    (let ((index (1+ (read-index db))))
      (loop
         while (<= index (write-index db))
         do
           (let* ((idx (stringify index))
                  (key (append-db-keys $BACKUP idx))
                  (str (db-get (wrapped-db db) key)))
             (cond (str
                    (let ((pos (search $BACKUP-DELIMITER str)))
                      (assert pos nil "No newline found in backup string: ~s" str)
                      (setf (db-get (wrapped-db db) key) nil)
                      (setf (read-index db) index)
                      (return
                        (values (subseq str 0 pos)
                                (subseq str (+ pos (length $BACKUP-DELIMITER)))))))
                   (t (incf index))))))))

(defmethod can-read-p ((db backup-db))
  (<= (1+ (read-index db)) (write-index db)))

(defmethod save-readindex ((db backup-db))
  (with-lock-grabbed ((backup-db-lock db))
    (setf (db-get (wrapped-db db) $BACKUP $READINDEX)
          (stringify (read-index db)))))

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
