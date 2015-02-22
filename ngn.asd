#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

#|
  novel page generator

  Author: subaru45
|#

(in-package :cl-user)
(defpackage ngn-asd
  (:use :cl :asdf))
(in-package :ngn-asd)

(defsystem ngn
  :version "0.9.0"
  :author "subaru45"
  :license "NYSL"
  :depends-on (:cl-ppcre
               :cl-annot
               :alexandria
               :inquisitor
               :trivial-shell
               :unix-options)
  :components ((:module "src"
                :components
                ((:file "parser")
                 (:file "render-dsl")
                 (:file "renderer" :depends-on ("render-dsl"))
                 (:file "ngn" :depends-on ("parser" "renderer")))))
  :description "novel page generator"
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
