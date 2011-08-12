(in-package :cl-user)

;; One-time only
;(load "http://beta.quicklisp.org/quicklisp.lisp")
;(quicklisp-quickstart:install)

(unless (find-package "QUICKLISP")
  (load "~/quicklisp/setup"))

(defun update-registry ()
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

(let ((port (ignore-errors (parse-integer (ccl:getenv "TRULEDGER_PORT")))))
  (when port
    (truledger:truledger-web-server nil :port port)))
