; -*- mode: lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trubanc server backup
;;;

(in-package :trubanc-server)

(defun make-backup-db (db)
  (let ((res (make-instance 'backup-db :wrapped-db db))
        (readindex (parse-integer (or (db-get db $BACKUP $READINDEX) "0")))
        (writeindex (parse-integer (or (db-get db $BACKUP $WRITEINDEX) "0"))))
    (when readindex (setf (read-index res) readindex))
    (when writeindex (setf (write-index res) writeindex))
    res))    

(defclass backup-db (db)
  ((wrapped-db :initarg :wrapped-db
               :accessor wrapped-db
               :type db)
   (write-lock :initform (make-lock "backup-write-lock")
              :reader write-lock)
   (latch :initform (make-latch)
          :reader backup-db-latch)
   (write-index :initform 0
                :accessor write-index)
   (read-index :initform 0
               :accessor read-index)
   (last-read-key :initform nil
                  :accessor last-read-key)))

(defmethod print-object ((db backup-db) stream)
  (print-unreadable-object (db stream :type t)
    (format stream "wrapping: ~s" (wrapped-db db))))

(defmethod initialize-instance :before ((db backup-db) &key wrapped-db)
  (unless (typep wrapped-db 'db)
    (error "wrapped-db must be specified")))

(defmethod wrapped-db ((db db))
  db)

(defmethod wrapped-db ((server server))
  (wrapped-db (db server)))

(defmethod db-get ((db backup-db) key &rest more-keys)
  (declare (dynamic-extent more-keys))
  (apply #'db-get (wrapped-db db) key more-keys))

(defmethod (setf db-get) (value (db backup-db) key &rest more-keys)
  (declare (dynamic-extent more-keys))
  (let ((key (%append-db-keys key more-keys)))
    (with-fsdb-filename ((wrapped-db db) filename key)
      (with-lock-grabbed ((write-lock db))
        (if (or (null value) (equal value ""))
            (when (probe-file filename) (delete-file filename))
            (file-put-contents filename value))
        (backup-write db value key))))
  value)

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
                 :write-lock (write-lock db)))

(defmethod db-dir-p ((db backup-db) &rest keys)
  (declare (dynamic-extent keys))
  (apply #'db-dir-p (wrapped-db db) keys))

(defconstant $BACKUP-DELIMITER #.(format nil "///~%"))

;; Here's where we write the data to the log.
;; It's taken out by BACKUP-READ thread below.
(defmethod backup-write ((db backup-db) value key)
  (let ((index (stringify (incf (write-index db)))))
    (setf (db-get (wrapped-db db) $BACKUP index)
          (format nil "~a~a~a"
                  key
                  $BACKUP-DELIMITER
                  (or value "")))
    (setf (db-get (wrapped-db db) $BACKUP $WRITEINDEX) index)
    (signal-latch (backup-db-latch db))))

;; Returns three values: key, value, and index
;; Null return means the read-index has caught up with the write-index.
(defmethod backup-read ((db backup-db))
  (save-readindex db)
  (let ((index (1+ (read-index db))))
    (loop
       while (<= index (write-index db))
       do
       (let* ((idx (stringify index))
              (key (append-db-keys $BACKUP idx))
              (str (db-get (wrapped-db db) key)))
         (cond (str
                (let ((pos (search $BACKUP-DELIMITER str)))
                  (assert pos nil "No delimiter found in backup string: ~s" str)
                  (setf (last-read-key db) key
                        (read-index db) index)
                  (return
                    (values (subseq str 0 pos)
                            (subseq str (+ pos (length $BACKUP-DELIMITER)))
                            idx))))
               (t (incf index)))))))

(defmethod can-read-p ((db backup-db))
  (<= (1+ (read-index db)) (write-index db)))

(defmethod save-readindex ((db backup-db))
  (let ((key (last-read-key db)))
    (when key
      (setf (db-get (wrapped-db db) key) nil
            (db-get (wrapped-db db) $BACKUP $READINDEX) (stringify (read-index db))
            (last-read-key db) nil))))

(defun make-backup-client (server remote-url)
  (let* ((client (trubanc-client:make-client (client-db-dir)))
         (bankid (bankid server))
         (remote-bankid (trubanc-client:bankid-for-url client remote-url)))
    (unless (equal bankid remote-bankid)
      (error "Remote bank not for same bankid as local bank"))
    (setf (id client) bankid
          (bankid client) bankid
          (privkey client) (privkey server)
          (server client) (trubanc-client:make-server-proxy client remote-url))
    ;; Ensure remote server is in backup mode
    (trubanc-client:getreq client t)
    (trubanc-client:backup client)
    client))

(defun backup-existing-db (db)
  (check-type db backup-db)
  (let ((wrapped-db (wrapped-db db)))
    (labels ((walk (dir walk-index starting-up-p)
               ;;(format t "(walk ~s ~s ~s)~%" dir walk-index starting-up-p)
               (let ((contents (if dir
                                   (db-contents wrapped-db dir)
                                   (db-contents wrapped-db))))
                 (dolist (file contents)
                   (let ((key (if dir (append-db-keys dir file) file)))
                     (cond ((db-dir-p wrapped-db key)
                            (unless (or (and (blankp dir)
                                             (equal file $BACKUP))
                                        (and walk-index
                                             (string-lessp file (car walk-index))))
                              (setq starting-up-p
                                    (walk key (cdr walk-index) starting-up-p))))
                           (t
                            (when starting-up-p
                              (unless (and walk-index
                                          (string-lessp file (car walk-index)))
                                (setq starting-up-p nil)))
                            (unless (or starting-up-p (equal file (car walk-index)))
                              (with-db-lock (wrapped-db key)
                                (with-lock-grabbed ((write-lock db))
                                  (backup-write db (db-get wrapped-db key) key)
                                  ;;(format t "key: ~a~%" key)
                                  (setf (db-get wrapped-db $BACKUP $WALKINDEX)
                                        key)))))))))
               starting-up-p))
      (let ((walk-index (explode #\/ (or (db-get db $BACKUP $WALKINDEX) ""))))
        (walk nil walk-index (not (null walk-index)))))))

(defvar *backup-processes* (make-hash-table :test 'eq))
(defvar *backup-process-dbs* (make-hash-table :test 'eq))
(defvar *backup-process-urls* (make-hash-table :test 'eq))
(defvar *stop-backup-process-flags* (make-hash-table :test 'eq))

(defun backup-process (server)
  (gethash server *backup-processes*))

(defun (setf backup-process) (process server)
  (if process
      (setf (gethash server *backup-processes*) process)
      (remhash server *backup-processes*))
  server)

(defun backup-process-db (server)
  (gethash server *backup-process-dbs*))

(defun (setf backup-process-db) (db server)
  (if db
      (setf (gethash server *backup-process-dbs*) db)
      (remhash server *backup-process-dbs*))
  db)

(defun backup-process-url (server)
  (gethash server *backup-process-urls*))

(defun (setf backup-process-url) (url server)
  (if url
      (setf (gethash server *backup-process-urls*) url)
      (remhash server *backup-process-urls*))
  url)

(defun stop-backup-process-flag (server)
  (gethash server *stop-backup-process-flags*))

(defun (setf stop-backup-process-flag) (boolean server)
  (if boolean
      (setf (gethash server *stop-backup-process-flags*) boolean)
      (remhash server *stop-backup-process-flags*))
  boolean)

(defparameter *max-backup-data-size* (* 32 1024))
(defparameter *backup-retry-count* 5)
(defparameter *backup-retry-sleep-seconds* 1)

(defun do-backup-process (db client server)
  (check-type db backup-db)
  (check-type client trubanc-client:client)
  (check-type server server)
  (unwind-protect
       (progn
         ;; Maybe we should spawn a separate process to do this.
         (backup-existing-db db)
         (signal-latch (backup-db-latch db))
         (loop
            named outer
            do
              (save-readindex db)
              (wait-on-latch (backup-db-latch db))
              (loop
                 (when (stop-backup-process-flag server)
                   (return-from outer))
                 (handler-case
                     (when (do-backup-process-body db client)
                       (return))
                   (error ()
                     ;; Rebackup the whole database.
                     (setf (db-get (wrapped-db db) $BACKUP $WALKINDEX) nil)
                     (return-from outer))))))
    (save-readindex db)
    (setf (db server) (wrapped-db db)
          (backup-process server) nil
          (backup-process-db server) nil
          (backup-process-url server) nil)))

(defun do-backup-process-body (db client)
  (let ((keys&values nil)
        (data-size 0)
        (done nil)
        (bankreq-key (append-db-keys $ACCOUNT (id client) $REQ))
        (initreq-p nil))
    (loop
       (multiple-value-bind (key value) (backup-read db)
         (unless key
           (setq done t)
           (return))
         (unless (equal key bankreq-key)
           (push key keys&values)
           (push value keys&values)
           (incf data-size (+ (length key) (length value)))
           (when (>= data-size *max-backup-data-size*)
             (return)))))
    (when keys&values
      (setq keys&values (nreverse keys&values))
      (dotimes (i *backup-retry-count* (error "Server send retries failed"))
        (handler-case
            (progn
              (when initreq-p
                (setq initreq-p nil)
                (trubanc-client:getreq client t))
              (trubanc-client:backup* client keys&values)
              (return))
          (error (c)
            (setq initreq-p t)
            (save-readindex db)
            (format t "~&~a~%" c)
            (sleep *backup-retry-sleep-seconds*)))))
    done))

(defun start-backup-process (server remote-url)
  (when (backup-process server) (error "Backup process already running"))
  (setf (stop-backup-process-flag server) nil)
  (let ((client (make-backup-client server remote-url))
        (db (make-backup-db (db server))))
    (setf (db server) db
          (backup-process-db server) db
          (backup-process-url server) remote-url
          (backup-process server) (process-run-function
                                   "Trubanc Backup"
                                   #'do-backup-process
                                   db client server))))

(defun stop-backup-process (server &optional (rebackup t))
  (when (backup-process server)
    (setf (stop-backup-process-flag server) t)
    (signal-latch (backup-db-latch (backup-process-db server)))
    (process-wait "Backup termination" (lambda () (null (backup-process server))))
    (setf (stop-backup-process-flag server) nil
          (backup-process-db server) nil)
    (when rebackup
      (setf (db-get (db server) $BACKUP $WALKINDEX) nil))))
                                                        
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
