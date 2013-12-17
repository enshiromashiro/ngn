#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage util
  (:use :cl
        :cl-annot))
(in-package :util)

(cl-annot:enable-annot-syntax)


@export
(defun quit (&optional code)
  "quit _implementation independently_.
http://www.cliki.net/portable%20exit"
  ;; This group from "clocc-port/ext.lisp"
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
  #+lispworks (lw:quit :status code)
  #+sbcl (sb-ext:quit
		  :unix-code (typecase code (number code) (null 0) (t 1)))
  #+(or openmcl mcl) (ccl::quit)
  #+abcl (cl-user::quit)
  #+ecl (si:quit)

  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
		kcl scl openmcl mcl abcl ecl)
  (error 'not-implemented :proc (list 'quit code)))


@export
(defun gen-keyword (name)
  "generate keyword from string."
  (car (multiple-value-list (intern (string-upcase name) :keyword))))


@export
(defparameter *debug* nil
  "debug switch. it's default value is nil")

@export
(defparameter *debug-output* *standard-output*
  "output stream for function `debug`")

@export
(defun debug (str)
  "print debug message if *debug* is t.
*args
str: it's string or list of strings."
  (and *debug*
	   (if (listp str)
		   (dolist (e str)
			 (format *debug-output* "~a~%" e))
		   (format *debug-output* "~a~%" str))))
