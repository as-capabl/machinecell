
SRCDIR=../../../src
SEARCH_PATH=../../../machines/src:../lib:$(SRCDIR)
COMMON_OPTIONS=-i$(SEARCH_PATH)

SOURCES=../../../src/Control/Arrow/Machine/Utils.hs \
../../../src/Control/Arrow/Machine/Running.hs \
../../../src/Control/Arrow/Machine/Types.hs \
../../../src/Control/Arrow/Machine/Event.hs \
../../../src/Control/Arrow/Machine.hs \
../lib/WxHandler.hs 

bin/stone: bin Main.hs $(SOURCES)
	ghc -o $@ $(COMMON_OPTIONS) Main.hs 

.PHONY: ghci setenv tests

bin:
	mkdir bin


ghci: setenv
	ghci $(COMMON_OPTIONS)

setenv:
	export GHC_PACKAGE_PATH=../../

