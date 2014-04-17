patch-syntax:
	@echo "Appling jlfe syntax patch to LFE ..."
	@git apply --check patches/jlfe-syntax.patch && \
	echo $$(git apply patches/jlfe-syntax.patch && echo "Patch applied!") || \
	echo "Skipping; patch already applied.\n"

patch-string:
	@echo "Appling pretty-print java.lang.String workaround patch to LFE ..."
	@git apply --check patches/string-pretty-print-workaround.patch && \
	echo $$(git apply patches/string-pretty-print-workaround.patch && \
	echo "Patch applied!") || \
	echo "Skipping; patch already applied.\n"

compile: get-deps patch-syntax patch-string clean-ebin
	@echo "Compiling project code and dependencies ..."
	@rebar compile
