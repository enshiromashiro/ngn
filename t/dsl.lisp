#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.dsl-test
  (:use :cl
        :ngn.parser
        :prove))
(in-package :ngn.dsl-test)


(plan 8)

(subtest "Testing read-to"
  (flet ((call-it (ch str)
           (with-input-from-string (in str)
             (ngn.dsl::read-to ch in))))
    (is (call-it #\# "#") "")
    (is (call-it #\# "0123456789#") "0123456789")
    (is (call-it #\# "012345
6789#") "012345
6789")))


(subtest "Testing get-level"
  (flet ((call-it (ch str)
           (with-input-from-string (in str)
             (ngn.dsl::get-level ch in))))
    (multiple-value-bind (lev str)
        (call-it #\# "#### ")
      (is lev 4)
      (is str "####"))
    (multiple-value-bind (lev str)
        (call-it #\# "####a")
      (is lev 4)
      (is str "####"))))


(subtest "Testing paren-reader"
  (flet ((call-it (str)
           (with-input-from-string (in str)
             (ngn.dsl::paren-reader in))))
    (is (call-it "()") "")
    (is (call-it "(string)") "string")
    (is (call-it "(string)s") "string")

    (subtest "error case"
      (is-error (call-it "(") 'error))))


(subtest "Testing element-reader"
  (flet ((call-it (str)
           (with-input-from-string (in str)
             (ngn.dsl::element-reader in))))
    (let ((*package* (find-package :ngn.dsl)))
      (load "t/test-renderer.lisp"))

    (is (call-it "rb(str1)(str2)") "rb-str1str2")
    (is (call-it "em(str)") "em-str")
    (is (call-it "bd(str)") "bd-str")
    (is (call-it "it(str)") "it-str")
    (is (call-it "bi(str)") "bi-str")
    (is (call-it "ul(str)") "ul-str")
 
    (subtest "error case"
      (is-error (call-it "aa") 'error))))


(subtest "Testing sharp-reader"
  (flet ((call-it (str lhp)
           (with-input-from-string (in str)
             (ngn.dsl::sharp-reader in lhp))))
    (let ((*package* (find-package :ngn.dsl)))
      (load "t/test-renderer.lisp"))

    (subtest "non line-head"
      (is (call-it "rb(s1)(s2)" nil) "rb-s1s2")
      (is (call-it " header" nil) "#")
      (is (call-it "# header" nil) "##"))

    (subtest "line-head"
      (is (call-it "rb(s1)(s2)" t) "rb-s1s2")
      (is (call-it " header" t) "hd1-header")
      (is (call-it "# header" t) "hd2-header"))

    (subtest "boundary"
      (is (call-it "## header" t) "hd3-header")
      (is (call-it "### header" t) "hd4-header")
      (is (call-it "#### header" t) "#####"))))


(subtest "Testing greater-reader"
  (flet ((call-it (str lhp)
           (with-input-from-string (in str)
             (ngn.dsl::greater-reader in lhp))))
    (let ((*package* (find-package :ngn.dsl)))
      (load "t/test-renderer.lisp"))

    (is (call-it "\"str\"" nil) ">")
    (is (call-it "\"str\"" t) "qt1-str")
    (is (call-it "\"line1
line2\"" t)
        "qt1-line1
line2")

    (subtest "boundary"
      (is (call-it ">\"str\"" t) "qt2-str")
      (is (call-it ">>\"str\"" t) "qt3-str")
      (is (call-it ">>>\"str\"" t) "qt4-str")
      (is (call-it ">>>>\"str\"" t) ">>>>>"))))


(subtest "Testing dsl-reader"
  (flet ((call-it (str)
           (with-input-from-string (in str)
             (ngn.dsl::dsl-reader in))))
    (let ((*package* (find-package :ngn.dsl)))
      (load "t/test-renderer.lisp"))

    (subtest "non line-head"
      (is (call-it " #rb(s1)(s2)") " rb-s1s2")
      (is (call-it " >\"s\"") " >\"s\""))

    (subtest "line-head"
      (is (call-it "#rb(s1)(s2)") "rb-s1s2")
      (is (call-it ">\"s\"") "qt1-s"))))


(subtest "Testing render-tag"
  (flet ((call-it (str)
           (ngn.dsl::render-tag str)))
    (let ((*package* (find-package :ngn.dsl)))
      (load "t/test-renderer.lisp"))

    (subtest "non line-head"
      (is (call-it " #rb(s1)(s2)") " rb-s1s2")
      (is (call-it " # rb(s1)(s2)") " # rb(s1)(s2)"))

    (subtest "line-head"
      (is (call-it "#rb(s1)(s2)") "rb-s1s2")
      (is (call-it "# rb(s1)(s2)") "hd1-rb(s1)(s2)"))

    (subtest "multi-line"
      (is (call-it "#em(str1)
#bd(str2)")
          "em-str1$
bd-str2$
")
      (is (call-it "# header1
# header2")
          "hd1-header1
hd1-header2
"))))


(finalize)
