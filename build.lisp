;;;; How to build executable
;;;; 1. load this file to Impl

(require :asdf)
(require :ngn)

(defvar *app-name*
  #-windows "ngn"
  #+windows "ngn.exe")
(defvar *app-toplevel*
  #'ngn:app)

;;;; building app
#+sbcl
(save-lisp-and-die
 *app-name*
 :toplevel *app-toplevel*
 :executable t)
#+ccl
(save-application
 *app-name*
 :toplevel-function *app-toplevel*
 :prepend-kernel t)
#+ecl
(asdf:make-build
 :ngn
 :type :program
 :move-here "."
 :monolithic t
 :epilogue-code '(progn
                  (ngn:app)
                  (quit)))
