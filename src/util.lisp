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
(defun args ()
  #+allegro (system:command-line-arguments)
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+ecl (si:command-args)
  #+cmu ext:*command-line-words*
  #+ccl ccl:*command-line-argument-list*
  #+lispworks system:*line-arguments-list*)
