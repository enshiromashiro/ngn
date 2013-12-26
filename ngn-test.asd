#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn-test-asd
  (:use :cl :asdf))
(in-package :ngn-test-asd)

(defsystem ngn-test
  :author "subaru45"
  :license "NYSL"
  :depends-on (:ngn
			   :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "ngn" :depends-on ("tag-parser" "text-io" "generator" "util"))
				 (:file "tag-parser")
				 (:file "text-io")
				 (:file "generator" :depends-on ("tag-parser"))
				 (:file "util"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
