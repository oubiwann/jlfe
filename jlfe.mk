patch:
	@echo "Appling jlfe path to LFE ..."
	@git apply --check patches/jlfe-syntax.patch && \
	echo $$(git apply patches/jlfe-syntax.patch && echo "Patch applied!") || \
	echo "Skipping; patch already applied.\n"

compile: get-deps patch clean-ebin
	@echo "Compiling project code and dependencies ..."
	@rebar compile
