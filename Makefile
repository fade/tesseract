ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
PACKAGE=tesseract
PACKAGEUTILS=tesseract.app-utils
OUT=tesseract
ENTRY=-main

$(OUT): buildapp *.lisp quicklisp-manifest.txt
	./buildapp  --manifest-file quicklisp-manifest.txt \
				--load-system asdf \
				--eval '(push "$(ROOT_DIR)/" asdf:*central-registry*)' \
				--load-system $(PACKAGE) \
				--eval '($(PACKAGEUTILS)::internal-disable-debugger)' \
				--compress-core \
				--output $(OUT) --entry $(PACKAGE):$(ENTRY)

quicklisp-manifest.txt: *.asd
	sbcl --non-interactive \
		--eval '(push #P"$(ROOT_DIR)/" asdf:*central-registry*)'\
		--eval '(ql:quickload "$(PACKAGE)")'\
		--eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'

buildapp:
	sbcl --eval '(ql:quickload "buildapp")' --eval '(buildapp:build-buildapp)' --non-interactive

clean:
	rm -f *.fasl $(OUT) buildapp quicklisp-manifest.txt
