(in-package :cl-user)

(unless (find-package "QUICKLISP")
  (cond ((probe-file "~/quicklisp/setup.lisp")
         (load "~/quicklisp/setup"))
        (t (format t "Installing Quicklisp...")
           (load "http://beta.quicklisp.org/quicklisp.lisp")
           (funcall (find-symbol "INSTALL" :quicklisp-quickstart)))))

(defun update-registry ()
  ;; Adapt for new asdf package structure
  (when (eq :inherited (nth-value 1 (find-symbol "GETENV" :asdf)))
    (export 'asdf::getenv :asdf))
  (load "truledger.asd")
  (asdf:initialize-source-registry
   (asdf:system-relative-pathname "truledger" "truledger-asd.conf")))

(update-registry)

(defun reload ()
  (ql:quickload "truledger" :verbose t))

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

(let ((port (ignore-errors (parse-integer (asdf:getenv "TRULEDGER_PORT")))))
  (when port
    (truledger:truledger-web-server nil :port port)))
