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
	(cd lib && $(THRIFT_COMPILE) --hs --use-int \
		test/if/echoer.thrift \
		-o test)
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

thrift-cpp::
	cd lib && thrift1 -I . --gen mstch_cpp2 \
		-o if \
		if/RpcOptions.thrift
	cd lib/test/if && thrift1 -I . --gen mstch_cpp2 \
                -o . \
                math.thrift
	cd tests/if && thrift1 -I . --gen mstch_cpp2 \
		-o . \
		hs_test.thrift

# Copying around some common Haskell modules used
# by many packages in their testsuites. Might
# instead have all these common modules be exposed
# by fb-util?
copy-sources::
	mkdir -p common/util/tests/github/Facebook \
		 server/test/github/Facebook \
		 common/mangle/tests/github \
		 tests/github lib/test/github

	cp common/github/Facebook/Init.hs \
           common/util/tests/github/Facebook/
	cp common/github/Facebook/Init.hs \
           server/test/github/Facebook/

	cp common/github/TestRunner.hs \
	   common/mangle/tests/github/
	cp common/github/TestRunner.hs \
	   common/util/tests/github/
	cp common/github/TestRunner.hs \
	   tests/github/
	cp common/github/TestRunner.hs \
	   compiler/test/github/
	cp common/github/TestRunner.hs \
	   server/test/github/
	cp common/github/TestRunner.hs \
	   lib/test/github/

	cp lib/test/Network.hs server/test/
	cp lib/test/TestChannel.hs server/test/
	cp lib/test/TestChannel.hs tests/

# Copying test .thrift files and their corresponding
# generated (by thrift-compiler) .hs files around to
# get each package directory to store everything the
# corresponding package needs to be built and tested.
# In particular, calling 'cabal sdist' for any
# package after this rule has run should result in a
# self-contained source distribution that can be
# built and tested from the sdist archive alone,
# as done in the Github CI.
prepare-sdists:: copy-sources
	cp -R lib/test/gen-hs2 server/test/
	cp -R compiler/test/fixtures/gen-hs2/HsTest lib/test/gen-hs2/

	mkdir -p compiler/tests/if
	cp tests/if/*.thrift compiler/tests/if/
	cp tests/if/*.hs compiler/tests/if/
