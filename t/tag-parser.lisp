#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.tag-parser-test
  (:use :cl
		:ngn.tag-parser
        :cl-test-more))
(in-package :ngn.tag-parser-test)


(plan nil)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro deftest-with-handler (test-name &rest forms)
  	`(deftest ,test-name
  		 (diag ,(format nil "test for ~a..." test-name))
  	   (handler-case 
  		   (progn ,@forms)
  		 (condition (c) 
  		   (fail "unexpected condition caused!: ~a~%" c))))))

(diag "*** ngn.tag-parser test ***")

;;; tag-parser
;; gen-keyword
(deftest-with-handler
	tag-parser.gen-keyword
  (is (ngn.tag-parser::gen-keyword "keyword")
	  :keyword)
  (is (ngn.tag-parser::gen-keyword "") :||)
;; Deleted. because of the value: (string-upcase nil) => "NIL"
;;  (is-error (ngn.tag-parser::gen-keyword nil) 'type-error)
  (is (ngn.tag-parser::gen-keyword nil) :nil))


;; tag-parser::parse-tag
(deftest-with-handler
	tag-parser.parse-tag
  (is-error (ngn.tag-parser::parse-tag 42 "") type-error)
  (ok (null (ngn.tag-parser::parse-tag "" "")))
  
  (ok (null (ngn.tag-parser::parse-tag "" 42)))
  (ok (null (ngn.tag-parser::parse-tag "" "")))
  
  (ok (null (ngn.tag-parser::parse-tag "a line." "li")))
  (ok (null (ngn.tag-parser::parse-tag "a line." "(li)")))
  
  (is (ngn.tag-parser::parse-tag "*eval*" "\\*(.+)(\\*)") '(:eval "*"))
  (is (ngn.tag-parser::parse-tag "*eval:loop*" "\\*(.+):(.+)\\*") '(:eval "loop")))


;; parse-oneline-tags
(deftest-with-handler
	tag.parser.parse-oneline-tags
  (is (ngn.tag-parser::parse-oneline-tags nil) nil)
  (is-error (ngn.tag-parser::parse-oneline-tags "") type-error)
  
  (is (ngn.tag-parser::parse-oneline-tags
	   '("line 1"
		 "line 2"
		 "line 3"))
	  nil)
  
  (is (ngn.tag-parser::parse-oneline-tags
	   '(":tagname" "next line"))
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

  ;; Deleted. Because of changing the specification to detect one-line-tags in block-tags.
  ;; (is (parse-oneline-tags
  ;; 	   '(":tag["
  ;; 		 "hoge"
  ;; 		 ":hoge data"
  ;; 		 ":tag]"))
  ;; 	  nil))
  (is (ngn.tag-parser::parse-oneline-tags
  	   '(":tag["
  		 "hoge"
  		 ":hoge data"
  		 ":tag]"))
	  '((:hoge "data"))))

;; parse-block-tags
(deftest-with-handler
	tag-parser.parse-block-tags
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
	  :parse-error-at-eof)
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
	  '((:block-tag ("line1" "line2")))))


;; parse-tags
(deftest-with-handler
	tag-parser.parse-tags
  (is (ngn.tag-parser::parse-tags nil) nil)
  (is-error (ngn.tag-parser::parse-tags "") type-error)
  
  ;; Deleted. Because of changing the specification to detect one-line-tags in block-tags.
  ;; (is (parse-tags
  ;; 	   '(":title Soundness"
  ;; 		 ":author subaru45"
  ;; 		 ""
  ;; 		 ":body["
  ;; 		 ":hoge fuga"
  ;; 		 "bunshou ga tsuduku..."
  ;; 		 ":body]"))
  ;; 	  '((:title "Soundness")
  ;; 		(:author "subaru45")
  ;; 		(:body ("bunshou ga tsuduku...")))))
  (is (parse-tags
  	   '(":title Soundness"
  		 ":author subaru45"
  		 ""
  		 ":body["
  		 ":hoge fuga"
  		 "bunshou ga tsuduku..."
  		 ":body]"))
  	  '((:title "Soundness")
  		(:author "subaru45")
		(:hoge "fuga")
  		(:body ("bunshou ga tsuduku...")))))


;; get-tag-data
(deftest-with-handler
  tag-parser.get-tag-data
  (is (get-tag-data "hoge" nil) nil)
  (let ((tags '((:hoge "*hoge*")
				(:fuga ("DAN" "DON" "FUGA"))
				(:piyo "ぴよ")
				(:hoge "+hoge+"))))
	(is (get-tag-data nil tags) nil)
	(is (get-tag-data "" tags) nil)
	(is (get-tag-data "hoge" tags) "*hoge*")
	(is (get-tag-data "piyo" tags) "ぴよ")
	(is (get-tag-data "fuga" tags)
		'("DAN" "DON" "FUGA"))))
  

(finalize)
