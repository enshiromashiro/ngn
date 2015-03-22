#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.renderer.pixiv-renderer-test
  (:use :cl
        :prove))
(in-package :ngn.renderer.pixiv-renderer-test)


(plan 9)

;;; loading pixiv-renderer
(load "renderer/pixiv-renderer.lisp")

(subtest "per-line rendering"
  (is (render-per-line "") "")
  (is (render-per-line "line") "line"))

(subtest "rendering ruby"
  (is (render-ruby "" "") "[[rb: > ]]")
  (is (render-ruby "text" "ruby") "[[rb:text > ruby]]"))

(subtest "rendering emphasis"
  (is (render-emphasis "") "")
  (is (render-emphasis "text") "text"))

(subtest "rendering bold text"
  (is (render-bold "") "")
  (is (render-bold "text") "text"))
 
(subtest "rendering italic text"
  (is (render-italic "") "")
  (is (render-italic "text") "text"))

(subtest "rendering bold-italic text"
  (is (render-bold-italic "") "")
  (is (render-bold-italic "text") "text"))

(subtest "rendering underlined text"
  (is (render-underline "") "")
  (is (render-underline "text") "text"))

(subtest "rendering header"
  (is (render-header "" 0) "[chapter:]")
  (is (render-header "text" 1) "[chapter:text]"))

(subtest "rendering quotation"
  (is (render-quote "text" 0) "text
")
  (is (render-quote "text" 1) "　text
")
  (is (render-quote "line1
line2" 2)
      "　　line1
　　line2
"))


(finalize)
