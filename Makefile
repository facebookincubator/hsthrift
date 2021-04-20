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

THRIFT_COMPILE = $(CABAL) run exe:thrift-compiler --

thrift-hs::
	(cd lib && $(THRIFT_COMPILE) --hs \
		if/RpcOptions.thrift)
	(cd lib && $(THRIFT_COMPILE) --hs \
		if/ApplicationException.thrift)
	(cd lib && $(THRIFT_COMPILE) --hs --use-int \
		test/if/math.thrift \
		-o test)
	mkdir -p cpp-channel/test/if
	(cd lib && $(THRIFT_COMPILE) --hs --use-int \
		test/if/math.thrift \
		-o ../cpp-channel/test/if)
	(cd lib && $(THRIFT_COMPILE) --hs --use-int \
		test/if/math.thrift \
		-o ../server/test)
	(cd lib && $(THRIFT_COMPILE) --hs --use-int \
		test/if/echoer.thrift \
		-o test)
	(cd lib && $(THRIFT_COMPILE) --hs --use-int \
		test/if/echoer.thrift \
		-o ../server/test)
	(cd server && $(THRIFT_COMPILE) --hs \
		test/if/hash_map.thrift \
		-o test)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/hs_prefix.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/foo.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/constants.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		--duplicate-names \
		if/duplicate.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/EnumConst.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/enum.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/exception.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		--use-int --use-hash-map --use-hash-set \
		if/flags.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		--extra-hasfields \
		if/hasfield.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/A.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/B.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/C.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/D.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/E.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/versions.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/monoid.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/hs_test.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/hs_test.thrift -o ../lib/test)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/map.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/messed_up_case.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/namespace.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/namespace_included.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/parens.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		--required-symbols "A,B,C,X,weNeedThis" \
		if/huge.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/scoped_enums.thrift)
	(cd tests && $(THRIFT_COMPILE) --hs \
		if/service.thrift)
	# those files are required for thrift-compiler's tests
	mkdir -p compiler/tests/if
	cp tests/if/*.thrift compiler/tests/if/
	cp tests/if/*.hs compiler/tests/if/

thrift-cpp::
	mkdir -p cpp-channel/if cpp-channel/test/if
	cd lib && thrift1 -I . --gen mstch_cpp2 \
		-o ../cpp-channel/if \
		if/RpcOptions.thrift
	cd lib/test/if && thrift1 -I . --gen mstch_cpp2 \
                -o ../../../cpp-channel/test/if \
                math.thrift
	cd tests/if && thrift1 -I . --gen mstch_cpp2 \
		-o . \
		hs_test.thrift
