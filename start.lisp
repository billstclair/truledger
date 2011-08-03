(in-package :cl-user)

;; One-time only
;(load "http://beta.quicklisp.org/quicklisp.lisp")
;(quicklisp-quickstart:install)

(unless (find-package "QUICKLISP")
  (load "~/quicklisp/setup"))

(defvar *source-directory*
  (make-pathname :name nil :type nil
                 :defaults (or *load-pathname* *default-pathname-defaults*))
  "The directory that holds the Truledger source files, which is assumed
   to be the same directory that this file is being loaded from.")

(defun add-to-registry (&rest paths)
  (dolist (path paths)
    (setf asdf:*central-registry*
          (adjoin (truename (merge-pathnames path *source-directory*))
                  asdf:*central-registry*
                  :test #'equal))))

(let ((systems-wildcard
       (merge-pathnames
        (make-pathname :directory "systems" :name :wild :type :wild)
        *source-directory*)))
  (apply 'add-to-registry
         (directory systems-wildcard :directories t :files nil))
  (add-to-registry *source-directory*))

(defun reload ()
  (ql:quickload "truledger" :verbose t))

;; Remove this to switch to the new all-lisp crypto code
(pushnew :openssl-cffi *features*)

(reload)

(defun tl ()
  (let ((set-package (ignore-errors (find-symbol "SET-PACKAGE" :swank))))
    (when set-package
      (funcall set-package :truledger))))

(defun start-swank (&optional port)
  (when port
    (ql:quickload "swank")
    (funcall (find-symbol "CREATE-SERVER" :swank)
             :port port :dont-close t)))

(tl)

(let ((port (ignore-errors (parse-integer (ccl:getenv "TRULEDGER_PORT")))))
  (when port
    (truledger:truledger-web-server nil :port port)))
