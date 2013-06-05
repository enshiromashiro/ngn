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
  :license "MIT"
  :depends-on (:ngn
			   :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "ngn"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
