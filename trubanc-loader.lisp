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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2009 Bill St. Clair
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http:;;;www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
