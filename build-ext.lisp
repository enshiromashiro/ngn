;;;; How to build executable
;;;; 1. load this file to Impl

(require :asdf)
(require :ngn-ext)

(defvar *app-name*
  #-windows "ngn-ext"
  #+windows "ngn-ext.exe")
(defvar *app-toplevel*
  #'ngn-ext:app)

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
 :ngn-ext
 :type :program
 :move-here "."
 :monolithic t
 :epilogue-code `(progn
                   (ngn-ext:app)
                   (quit)))

