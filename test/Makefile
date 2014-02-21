
SRCDIR=../src
SEARCH_PATH=../machines/src:$(SRCDIR)
COMMON_OPTIONS=-i$(SEARCH_PATH)

SOURCES=../src/Control/Arrow/Machine/Utils.hs \
../src/Control/Arrow/Machine/Detail.hs \
../src/Control/Arrow/Machine/Utils.hs \
../src/Control/Arrow/Machine/Types.hs \
../src/Control/Arrow/Machine/Event.hs \
../src/Control/Arrow/Machine/Plan.hs \
../src/Control/Arrow/Machine.hs 


tests: bin/test
	bin/test

bin/test: Main.hs $(SOURCES)
	ghc -o $@ $(COMMON_OPTIONS) Main.hs 

.PHONY: ghci setenv tests

bin:
	mkdir bin


ghci: setenv
	ghci $(COMMON_OPTIONS)

setenv:
	export GHC_PACKAGE_PATH=../

