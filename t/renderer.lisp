#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.renderer-test
  (:use :cl
        :ngn.parser
        :prove))
(in-package :ngn.renderer-test)


(plan nil)


(run-test-all)

(finalize)
