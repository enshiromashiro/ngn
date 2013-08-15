# commands 
CCL = ccl
RM = rm

# compile
define ccl-compile
	$(CCL)  --eval '(progn $1)' \
			--eval '(progn $2)'
endef



ngn: src/ngn.lisp src/text-io.lisp src/tag-parser.lisp src/generator.lisp
	$(call ccl-compile, \
			(load "ngn.asd")(ql:quickload :ngn), \
			(ccl:save-application "ngn" \
								:toplevel-function (function ngn:call-main) \
								:prepend-kernel t))
default: ngn
all: ngn

clean:
	RM ngn