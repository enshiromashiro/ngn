#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.generator-test
  (:use :cl
        :ngn.generator
        :cl-test-more))
(in-package :ngn.generator-test)

(plan nil)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro deftest-with-handler (test-name &rest forms)
  	`(deftest ,test-name
  		 (diag ,(format nil "test for ~a..." test-name))
  	   (handler-case 
  		   (progn ,@forms)
  		 (condition (c) 
  		   (diag (concatenate 'string
  							  "unexpected condition caused!: "
  							  (format nil "~a" c)
  							  )))))))

(diag "ngn-test")


(deftest-with-handler
  generator.remove-empty-string
  (flet ((rm-empty-str (strs)
		   (ngn.generator::remove-empty-string strs)))
	(is (rm-empty-str nil) nil)
	(is (rm-empty-str '("")) nil)
	(is (rm-empty-str '("hoge" "")) '("hoge"))
	(is (rm-empty-str '("" "hoge" "" "fuga")) '("hoge" "fuga"))))

(deftest-with-handler
  generator.-split-line
  (flet ((-splitl (line pos)
		   (ngn.generator::-split-line line pos)))
	(is (-splitl nil nil) '(nil))
	(is-error (-splitl nil '(0 1)) simple-error)
	(is-error (-splitl "hoge-fuga" '(0 10)) simple-error)
	(is (-splitl "hoge-fuga" '(0 9)) '("" "hoge-fuga" ""))
	(is (-splitl "hoge-fuga" '(0 4 5 8)) '("" "hoge" "-" "fug" "a"))
	(is (-splitl "++hoge--fuga**" '(2 6 8 12)) '("++" "hoge" "--" "fuga" "**"))))

(deftest-with-handler
  generator.split-line
  (is (ngn.generator::split-line "0123456789" '(0 10)) '("0123456789")))



(deftest-with-handler
  generator.replace-last
	(let ((lis '(hoge fuga piyo)))
	  (is (ngn.generator::replace-last lis "piyo") '(hoge fuga "piyo"))
	  (is lis '(hoge fuga piyo))))  ; checking for side effect

(deftest-with-handler
  generator.flatten-inner-list
  (is (ngn.generator::flatten-inner-list '() '("111" "222")) '("111" "222"))
  (is (ngn.generator::flatten-inner-list '("hoge") '("111" "222")) '("111" "222hoge"))
  (is (ngn.generator::flatten-inner-list '("hoge" "fuga") '("111" "222")) '("111" "222hoge" "fuga"))
  (is (ngn.generator::flatten-inner-list '("hoge" "fuga" "piyo") '("111" "+++")) '("111" "+++hoge" "fuga" "piyo")))

(deftest-with-handler
  generator.flatten-block-tag
  (is (ngn.generator::flatten-block-tag nil) '(""))
  (is (ngn.generator::flatten-block-tag '("hoge" "fuga" "piyo")) '("hogefugapiyo"))
  (is (ngn.generator::flatten-block-tag '(("foo" "bar") "hoge" ("spam" "egg")))
	  '("foo" "barhogespam" "egg"))
  (is (ngn.generator::flatten-block-tag '(("hoge" "fuga" "piyo")))
	  '("hoge" "fuga" "piyo")))



(deftest-with-handler
  generator.insert-tags-into-line
  (let ((regex "#\\|([a-z0-9-]+)\\|#")
		(tags '((:hoge "*hoge*")
				(:fuga ("DAN" "DON" "FUGA"))
				(:foo "*foo*")
				(:bar ("BAR" "BEE" "BOO")))))
	(flet ((insert-tags (line)
			 (ngn.generator::insert-tags-into-line line tags regex)))
      (is (insert-tags "") "")
	  (is (insert-tags "the quick brown fox jumps over the red lazy dog.")
		  "the quick brown fox jumps over the red lazy dog.")
	  (is (insert-tags "aaa #|hoge|# bbb")
		  "aaa *hoge* bbb")
	  (is (insert-tags "aaa #|hoge|# : #|foo|# bbb")
		  "aaa *hoge* : *foo* bbb")
	  (is (insert-tags "aaa #|fuga|# bbb")
		  '("aaa DAN" "DON" "FUGA bbb"))
	  (is (insert-tags "aaa #|fuga|# : #|bar|# bbb")
		  '("aaa DAN" "DON" "FUGA : BAR" "BEE" "BOO bbb"))
	  (is (insert-tags "aaa #|hoge|# - #|fuga|# bbb")
		  '("aaa *hoge* - DAN" "DON" "FUGA bbb"))
	  (is (insert-tags "aaa #|bar|# - #|foo|# bbb")
		  '("aaa BAR" "BEE" "BOO - *foo* bbb"))
	  (is (insert-tags "+++#|bar|#---#|fuga|#***")
		  '("+++BAR" "BEE" "BOO---DAN" "DON" "FUGA***")))))

			 

(finalize)
