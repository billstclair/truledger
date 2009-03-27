(cl:defpackage #:trubanc-loader
    (:use #:cl :ccl)
  (:export #:add-to-registry
           #:loadsys))

(in-package #:trubanc-loader)

(defvar *source-directory*
  (make-pathname :name nil :type nil
                 :defaults (or *load-pathname* *default-pathname-defaults*))
  "The directory that holds the Trubanc source files, which is assumed
   to be the same directory that this file is being loaded from.")

(require "asdf")

(defun add-to-registry (&rest paths)
  (dolist (path paths)
    (pushnew (truename (merge-pathnames path *source-directory*))
             asdf:*central-registry*)))

(defun loadsys (system)
  (asdf:oos 'asdf:load-op system))

(add-to-registry "./cl-base64"
                 "./ironclad"
                 "./cffi"
                 "./trivial-features"
                 "./babel"
                 "./alexandria"
                 *source-directory*)

(asdf:oos 'asdf:load-op :trubanc)

