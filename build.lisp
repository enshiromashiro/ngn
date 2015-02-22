;;;; How to build executable
;;;; 1. load this file to Impl

(require :asdf)
(require :ngn)

(defvar *app-name* "ngn")

(defun build-app ()
  #+sbcl (save-lisp-and-die
          *app-name*
          :toplevel #'ngn:app
          :executable t)
  #+ccl (save-application
         *app-name*
         :toplevel-function #'ngn:app
         :prepend-kernel t))


;; build app
(build-app)

