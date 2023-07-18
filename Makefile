export IDRIS2 ?= idris2

.PHONY: page
page:
	pack build docs/docs.ipkg
	cp docs/build/exec/mvc.js js/mvc.js
