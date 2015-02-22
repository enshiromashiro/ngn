;;;; How to build executable
;;;; 1. put ngn into PATH
;;;; 2. load this file to Impl
;;;; 3. run `(build-app)` on REPL

(require :asdf)
(require :ngn)

(format t "load this file and run `(build-app)` on REPL~%")

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

