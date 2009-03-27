;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CCL interface functions 
;;;

(in-package #:trubanc)

(defun run-program (program args &key input output)
  (ccl:run-program program args :input input :output output))

(defun quit (&optional (exit-status 0))
  (ccl:quit exit-status))

(defun df (x) (disassemble x))

(defun add-startup-function (f)
  (pushnew f ccl:*lisp-startup-functions*))
