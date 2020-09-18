# These are a few rules to help build the open-source hsthrift. This
# file will hopefully go away in due course.

CABAL=cabal

all:: compiler thrift-hs thrift-cpp server

compiler::
	$(CABAL) build thrift-compiler

server::
	$(CABAL) build thrift-server --allow-newer=thrift-lib:aeson

util::
	$(CABAL) build fb-util

thrift:: thrift-cpp thrift-hs

THRIFT_COMPILE = $(CABAL) new-run exe:thrift-compiler --

thrift-hs::
	$(THRIFT_COMPILE) --hs \
		lib/if/RpcOptions.thrift \
		-o lib
	$(THRIFT_COMPILE) --hs \
		lib/if/ApplicationException.thrift \
		-o lib
	$(THRIFT_COMPILE) --hs \
		lib/test/if/math.thrift \
		-o lib/test
	$(THRIFT_COMPILE) --hs \
		lib/test/if/echoer.thrift \
		-o lib/test

thrift-cpp::
	cd lib && thrift1 -I . --gen mstch_cpp2 \
		-o if \
		if/RpcOptions.thrift
	cd lib/test/if && thrift1 -I . --gen mstch_cpp2 \
                -o . \
                math.thrift

# tmp until we rewrite #includes automatically during repo sync
fix-includes:
	for i in `find common lib server | egrep '\.(h|cpp|hsc)$$'`; do \
	   sed 's@\(#include *["<]\)common/hs/\(util\|mangle\|thrift/lib\|thrift/server\)/@\1@' <$$i >$$i.tmp && mv $$i.tmp $$i; \
	done
