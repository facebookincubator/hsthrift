# These are a few rules to help build the open-source hsthrift. This
# file will hopefully go away in due course.

CABAL=cabal

all:: compiler thrift-hs thrift-cpp server

compiler::
	$(CABAL) build thrift-compiler

server::
	$(CABAL) build thrift-server

util::
	$(CABAL) build fb-util

thrift:: thrift-cpp thrift-hs

THRIFT_COMPILE = $(CABAL) new-run thrift-compiler --

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

# tmp until we rewrite #includes automatically during repo sync
fix-includes:
	for i in `find common lib server | egrep '\.(h|cpp|hsc)$$'`; do \
	   sed 's@\(#include *["<]\)common/hs/\(util\|mangle\|thrift/lib\|thrift/server\)/@\1@' <$$i >$$i.tmp && mv $$i.tmp $$i; \
	done
