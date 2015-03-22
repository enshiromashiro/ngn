#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.renderer.html-renderer-test
  (:use :cl
        :prove))
(in-package :ngn.renderer.html-renderer-test)


(plan 9)

;;; loading html-renderer
(load "renderer/html-renderer.lisp")

(subtest "per-line rendering"
  (is (render-per-line "") "<br>")
  (is (render-per-line "line") "line<br>")
  (is (render-per-line "# header") "# header")
  (is (render-per-line "##### header") "##### header<br>")
  (is (render-per-line "#header") "#header<br>"))

(subtest "rendering ruby"
  (is (render-ruby "" "")
      "<ruby><rp>（</rp><rt></rt><rp>）</rp></ruby>")
  (is (render-ruby "text" "ruby")
      "<ruby>text<rp>（</rp><rt>ruby</rt><rp>）</rp></ruby>"))

(subtest "rendering emphasis"
  (is (render-emphasis "")
      "<span class=\"emphasis\"></span>")
  (is (render-emphasis "text")
      "<span class=\"emphasis\">text</span>"))

(subtest "rendering bold text"
  (is (render-bold "")
      "<span class=\"bold\"></span>")
  (is (render-bold "text")
      "<span class=\"bold\">text</span>"))
 
(subtest "rendering italic text"
  (is (render-italic "")
      "<span class=\"italic\"></span>")
  (is (render-italic "text")
      "<span class=\"italic\">text</span>"))

(subtest "rendering bold-italic text"
  (is (render-bold-italic "")
      "<span class=\"bold-italic\"></span>")
  (is (render-bold-italic "text")
      "<span class=\"bold-italic\">text</span>"))

(subtest "rendering underlined text"
  (is (render-underline "")
      "<span class=\"underline\"></span>")
  (is (render-underline "text")
      "<span class=\"underline\">text</span>"))

(subtest "rendering header"
  (is (render-header "" 1) "<h1></h1>")
  (is (render-header "text" 1) "<h1>text</h1>"))

(subtest "rendering quotation"
  (is (render-quote "" 1)
      "<p class=\"quote1\">

</p>
")
  (is (render-quote "text" 1) "<p class=\"quote1\">
text
</p>
")
  (is (render-quote "line1
line2" 2)
      "<p class=\"quote2\">
line1
line2
</p>
"))


(finalize)
