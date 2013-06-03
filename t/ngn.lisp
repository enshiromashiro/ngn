#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn-test
  (:use :cl
        :ngn
        :cl-test-more))
(in-package :ngn-test)

(plan nil)


;;; tag-parser
;; ngn.tag-parser::gen-keyword [2]
(is (ngn.tag-parser::gen-keyword "keyword")
	:keyword)
(is (ngn.tag-parser::gen-keyword "") :||)
(is-error (ngn.tag-parser::gen-keyword nil))		  


;; empty-string-p
;; (ok (empty-string-p ""))
;; (ok (not (empty-string-p "string")))


;; ngn.tag-parser::parse-tag [8]
(ok (null (ngn.tag-parser::parse-tag 42 "")))
(ok (null (ngn.tag-parser::parse-tag "" "")))

(ok (null (ngn.tag-parser::parse-tag "" 42)))
(ok (null (ngn.tag-parser::parse-tag "" "")))

(ok (null (ngn.tag-parser::parse-tag "a line." "li")))
(ok (null (ngn.tag-parser::parse-tag "a line." "(li)")))

(is (ngn.tag-parser::parse-tag "*eval*" "\*(.+)\*") :eval)
(is (ngn.tag-parser::parse-tag "*eval:loop*" "\*(.+):(.+)\*" '(:eval "loop")))


;; ngn.tag-parser::parse-oneline-tags [7]
(is (ngn.tag-parser::parse-oneline-tags nil) nil)
(is-error (ngn.tag-parser::parse-oneline-tags ""))

(is (ngn.tag-parser::parse-oneline-tags
	 '("line 1"
	   "line 2"
	   "line 3"))
	nil)

(is (ngn.tag-parser::parse-oneline-tags
	 '(":tagname" "next line")
	 nil)

(is (ngn.tag-parser::parse-oneline-tags
	 '(":TAGNAME data"
	   ":TAG-NAME data"
	   ":tag+name data"
	   ":tag_name data"))
	nil)
(is (ngn.tag-parser::parse-oneline-tags
	 '(":tagname data-1"
	   ":tag-name -data2"
	   ":-tagname data3-"
	   ":tagname- データ4"
	   ":tagname3001 data5 "))
	'((:tagname "data-1")
	  (:tag-name "-data2")
	  (:-tagname "data3-")
	  (:tagname- "データ4")
	  (:tagname3001 "data5 ")))

(is (ngn.tag-parser::parse-oneline-tags
	 '(":tag["
	   "hoge"
	   ":hoge data"
	   ":tag]"))
	nil)


;; ngn.tag-parser::parse-block-tags [9]
(is (ngn.tag-parser::parse-block-tags
	 '(":tag["
	   "data"
	   ":tag]"))
	 '((:tag ("data"))))

(is (ngn.tag-parser::parse-block-tags
	 '(":tag["
	   "line1"
	   "line2"
	   ":tag]"))
	'((:tag ("line1" "line2"))))

(is (ngn.tag-parser::parse-block-tags
	 '(":tag["
	   "日本語データ"
	   ":tag]"))
	'((:tag ("日本語データ"))))

(is (ngn.tag-parser::parse-block-tags
	 '("line1"
	   ":tag]"
	   "line2"
	   ":tag["
	   "line3"
	   ":tag]"
	   "line4"))
	'((:tag ("line3"))))

(is (ngn.tag-parser::parse-block-tags
	 '(":tag["
	   "line1"
	   ":nested-tag["
	   "line2"
	   ":nested-tag]"
	   "line3"
	   ":tag]"))
	'((:tag ("line1"
			 ":nested-tag["
			 "line2"
			 ":nested-tag]"
			 "line3"))))

(is (ngn.tag-parser::parse-block-tags
	 '(":tag["
	   "line1"))
	nil)
(is (ngn.tag-parser::parse-block-tags
	 '(":tag1["
	   "line1"
	   ":tag]"
	   ":tag2["
	   "line2"))
	:parse-error-at-eof)

(is (ngn.tag-parser::parse-block-tags
	 '(":tag1["
	   "line1"
	   ":tag]"
	   ":tag2["
	   "line2"
	   ":tag3["
	   "line3"
	   ":tag3]"))
	:parse-error-at-eof)

(is (ngn.tag-parser::parse-block-tags
	 '(":block-tag["
	   "line1"
	   ":oneline-tag data"
	   "line2"
	   ":block-tag]"))
	 '((:block-tag ("line1" "line2"))))


;; ngn.tag-parser::parse-tags
(is (ngn.tag-parser::parse-tags nil) nil)
(is-error (ngn.tag-parser::parse-tags ""))

(is (ngn.tag-parser::parse-tags
	 '(":title Soundness"
	   ":author subaru45"
	   ""
	   ":body["
	   ":hoge fuga"
	   "bunshou ga tsuduku..."
	   ":body]"))
	'((:title "Soundness")
	  (:author "subaru45")
	  (:body ("bunshou ga tsuduku..."))))



(finalize)
