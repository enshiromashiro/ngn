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
			   :prove)
  :components ((:module "t"
                :components
                ((:file "parser")
                 (:module "renderers"
                  :components
                  ((:file "html-renderer")
                   (:file "pixiv-renderer")))
                 (:file "dsl" :depends-on ("renderers"))
                 (:file "renderer"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
