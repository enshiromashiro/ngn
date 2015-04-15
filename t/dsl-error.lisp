#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.dsl-error-test
  (:use :cl
        :ngn.parser
        :prove))
(in-package :ngn.dsl-error-test)


(plan 3)

(defmacro tests-with-syntax-error (linum tagname msg &body body)
  `(handler-case
       (progn
         (ngn.dsl::init-linum)
         (ngn.dsl::init-tag-name)
         ,@body)
     (ngn.dsl::ngn-syntax-error (c)
       (let ((linum-got (ngn.dsl::syntax-error-linum c))
             (tagname-got (ngn.dsl::syntax-error-tagname c))
             (msg-got (ngn.dsl::syntax-error-message c)))
         (is linum-got ,linum)
         (is tagname-got ,tagname)
         (is msg-got ,msg)))
     (condition (c)
       (fail (format nil "unexpected error ~a" c)))
     (:no-error (v)
       (declare (ignore v))
       (fail "no error occurs"))))

(defun dsl-reader (str)
  (with-input-from-string (in str)
    (ngn.dsl::dsl-reader in)))

(defmacro render-tags (&rest tags)
  `(let ((hash (make-hash-table)))
     (setf ,@(loop
                for e in tags
                for i = 0 then (incf i)
                while (< i (length tags))
                collect (if (zerop (mod i 2))
                            `(gethash ,e hash) e)))
     (ngn.dsl::render-tags hash "t/test-renderer.lisp")))

(let ((*package* (find-package :ngn.dsl)))
  (load "t/test-renderer.lisp"))


(subtest "basics"
  (subtest "bracket"
    (tests-with-syntax-error
      1 nil "'EOF' is not '['"
      (dsl-reader "#bd"))
    (tests-with-syntax-error
      1 nil "Unexpected EOF while looking for ']'"
      (dsl-reader "#bd["))
    (tests-with-syntax-error
      1 nil "Unexpected NEWLINE while looking for ']'"
      (dsl-reader "#bd[
")))

  (subtest "ngn-element"
    (tests-with-syntax-error
      1 nil "'aa' is not ngn-element"
      (dsl-reader "#aa[test]")))

  (subtest "header"
    (tests-with-syntax-error
      1 nil "'li' is not ngn-element"
      (dsl-reader "#line"))
    (is (dsl-reader "# line") "hd1-line"))

  (subtest "quotation"
    (is (dsl-reader ">line") ">line")
    (is (dsl-reader "> line") "qt1-line")
    (is (dsl-reader "> line1
> line2")
        "qt1-line1
line2")))


(subtest "line number"
  (tests-with-syntax-error
    3 nil "'(' is not '['"
    (dsl-reader "line1
#bd[line2]
#it(italic]"))

  (tests-with-syntax-error
    5 nil "Unexpected EOF while looking for ']'"
    (dsl-reader "line1
line2
# header
line4
#bd[text"))

  (tests-with-syntax-error
    3 nil "'aa' is not ngn-element"
    (dsl-reader "> line1
> line2
#aa[text]"))

  (tests-with-syntax-error
   3 nil "Unexpected EOF while looking for ']'"
   (dsl-reader "> line1
> line2
> line3 #bd[text")))


(subtest "tag name"
  (tests-with-syntax-error
   1 'text1 "'bb' is not ngn-element"
   (render-tags 'text1
                "#bb[]"))

  (tests-with-syntax-error
   1 'text2 "Unexpected EOF while looking for ']'"
   (render-tags 'text1 "title"
                'text2 "#bd["
                'text3 "#bd(")))
    

(finalize)
