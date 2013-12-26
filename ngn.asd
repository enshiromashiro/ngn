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
  :version "0.7"
  :author "subaru45"
  :license "NYSL"
  :depends-on (:cl-ppcre
			   :cl-annot
			   :alexandria
			   :unix-options
			   :guess)  ;; https://github.com/t-sin/guess
  :components ((:module "src"
                :components
                ((:file "ngn" :depends-on ("generator" "tag-parser" "text-io" "util"))
				 (:file "tag-parser" :depends-on ("util"))
				 (:file "text-io" :depends-on ("util"))
				 (:file "generator" :depends-on ("tag-parser"))
				 (:file "util"))))
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
