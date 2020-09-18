# CABAL="$$HOME/ext/code/dist-newstyle/build/x86_64-linux/ghc-8.4.3/cabal-install-2.4.1.0/build/cabal/cabal"
CABAL="$$HOME/ext/code/cabal/dist-newstyle/build/x86_64-linux/ghc-8.4.3/cabal-install-3.2.0.0/build/cabal/cabal"
THRIFT_COMPILE = $(CABAL) new-run thrift-compiler --

compiler::
	$(CABAL) build thrift-compiler

server::
	$(CABAL) build thrift-server

thrift:: thrift-cpp thrift-hs

thrift-hs::
	$(THRIFT_COMPILE) --hs \
		lib/if/RpcOptions.thrift \
		-o lib
	$(THRIFT_COMPILE) --hs \
		lib/if/ApplicationException.thrift \
		-o lib

thrift-cpp::
	cd lib && thrift1 -I . --gen mstch_cpp2 \
		-o if \
		if/RpcOptions.thrift

fix-includes:
	for i in `find common | egrep '\.(h|cpp|hsc)$$'`; do \
	   sed 's|\(#include *["<]\)common/hs/|\1|' <$$i >$$i.tmp && mv $$i.tmp $$i; \
	done
