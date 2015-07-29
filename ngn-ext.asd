#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

#|
  ngn-tag extracter

  Author: subaru45
|#

(in-package :cl-user)
(defpackage ngn-ext-asd
  (:use :cl :asdf))
(in-package :ngn-ext-asd)

(defsystem ngn-ext
  :version "1.0.0"
  :author "subaru45"
  :license "NYSL"
  :depends-on (:cl-ppcre
               :alexandria
               :inquisitor
               :trivial-shell
               :unix-options)
  :components ((:module "src"
                :components
                ((:file "error")
                 (:file "parser" :depends-on ("error"))
                 (:file "ngn-ext" :depends-on ("parser")))))
  :description "ngn-tag extracter"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op ngn-test))))
