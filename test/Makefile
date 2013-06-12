
SRCDIR=../src
SEARCH_PATH=../machines/src:$(SRCDIR)
COMMON_OPTIONS=-i$(SEARCH_PATH)

tests: bin/test
	bin/test

bin/test: Main.hs $(SRCDIR)/Control/Arrow/Machine.hs bin
	ghc -o $@ $(COMMON_OPTIONS) Main.hs 

.PHONY: ghci setenv tests

bin:
	mkdir bin


ghci: setenv
	ghci $(COMMON_OPTIONS)

setenv:
	export GHC_PACKAGE_PATH=../

