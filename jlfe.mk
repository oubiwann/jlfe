patch:
	@git apply --check patches/jlfe-syntax.patch && \
	echo $$(git apply patches/jlfe-syntax.patch && echo "Patch applied!") || \
	echo \
	"\nPlease submit a bug report to the jlfe project:\n" \
	"https://github.com/oubiwann/jlfe/issues/new"
