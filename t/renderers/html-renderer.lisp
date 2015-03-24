#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.renderer.html-renderer-test
  (:use :cl
        :prove))
(in-package :ngn.renderer.html-renderer-test)


(plan 12)

;;; loading html-renderer
(load "renderer/html-renderer.lisp")


;;; for auto indexing
(subtest "%add-leaf"
  (subtest "adding a leaf; to empty-tree (=nil)"
    (is (%add-leaf 'x 1 nil) '(nil x))
    (is (%add-leaf 'x 2 nil) '(nil (nil x)))
    (is (%add-leaf 'x 4 nil) '(nil (nil (nil (nil x))))))

  (subtest "adding a leaf; is always added to _next position of last leaf_"
    (is (%add-leaf 'x 1 '(nil a))
        '(nil a x))
    (is (%add-leaf 'x 1 '(nil a b c))
        '(nil a b c x))
    (is (%add-leaf 'x 1 '(nil (a b) c d))
        '(nil (a b) c d x)))

  (subtest "adding a leaf; to any depth"
    (is (%add-leaf 'x 2 '(nil a))
        '(nil (a x)))
    (is (%add-leaf 'x 3 '(nil a))
        '(nil (a (nil x))))
    (is (%add-leaf 'x 2 '(nil (a b) (c d)))
        '(nil (a b) (c d x)))
    (is (%add-leaf 'x 3 '(nil (a (b c)) (d (e f g))))
        '(nil (a (b c)) (d (e f g x))))
    (is (%add-leaf 'x 4 '(nil (a (b c)) (d (e f g))))
        '(nil (a (b c)) (d (e f (g x)))))))

(subtest "add-header"
  (setf *-index-* nil
        *header-number* 1)
  (flet ((idx (id txt)
           (make-index-elem :id id :text txt)))
    (is *-index-* nil)
    (is *header-number* 1)

    (add-header 1 "header1")
    (is *-index-* `(nil ,(idx 1 "header1"))
      :test #'equalp)
    (is *header-number* 2)

    (add-header 2 "header2")
    (is *-index-* `(nil (,(idx 1 "header1")
                         ,(idx 2 "header2")))
        :test #'equalp)
    (is *header-number* 3)))

(subtest "rendering index"
  (flet ((idx (id txt)
           (make-index-elem :id id :text txt)))
    (is (render-index nil) "")
    (is (render-index '(nil)) "")

    (is (render-index `(nil ,(idx 1 "chapter1")
                            (,(idx 2 "chapter2")
                             ,(idx 3 "chapter2.1")
                             ,(idx 4 "chapter2.2"))
                            (nil
                             ,(idx 5 "chapter3.1"))))
        "<ol>
<li><a href=\"#sec1\">chapter1</a></li>
<li><a href=\"#sec2\">chapter2</a>
  <ol>
  <li><a href=\"#sec3\">chapter2.1</a></li>
  <li><a href=\"#sec4\">chapter2.2</a></li>
  </ol>
</li>
<li>
  <ol>
  <li><a href=\"#sec5\">chapter3.1</a></li>
  </ol>
</li>
</ol>
")))



;;; normal renderer api
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
  (setf *-index-* nil
        *header-number* 1)
  (is (render-header "" 1) "<h1><a name=\"sec1\"></a></h1>")
  (is *-index-* `(nil ,(make-index-elem :id 1 :text ""))
      :test #'equalp)
  (is *header-number* 2)

  (is (render-header "text" 1) "<h1><a name=\"sec2\">text</a></h1>")
  (is *-index-* `(nil ,(make-index-elem :id 1 :text "")
                      ,(make-index-elem :id 2 :text "text"))
      :test #'equalp)
  (is *header-number* 3))

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
