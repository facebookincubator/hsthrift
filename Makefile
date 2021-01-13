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
	$(THRIFT_COMPILE) --hs \
		lib/if/RpcOptions.thrift \
		-o lib
	$(THRIFT_COMPILE) --hs \
		lib/if/ApplicationException.thrift \
		-o lib
	$(THRIFT_COMPILE) --hs --use-int \
		lib/test/if/math.thrift \
		-o lib/test
	$(THRIFT_COMPILE) --hs --use-int \
		lib/test/if/echoer.thrift \
		-o lib/test
	$(THRIFT_COMPILE) --hs \
		server/test/if/hash_map.thrift \
		-o server/test
	$(THRIFT_COMPILE) --hs \
		tests/if/hs_prefix.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/foo.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/constants.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		--duplicate-names \
		tests/if/duplicate.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/EnumConst.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/enum.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/exception.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		--use-int --use-hash-map --use-hash-set \
		tests/if/flags.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs --extra-hasfields \
		tests/if/hasfield.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/A.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/B.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/C.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/D.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/E.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/versions.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/monoid.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/hs_test.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/map.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/messed_up_case.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/namespace.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/namespace_included.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/parens.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		--required-symbols "A,B,C,X,weNeedThis" \
		tests/if/huge.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/scoped_enums.thrift \
		-o tests/
	$(THRIFT_COMPILE) --hs \
		tests/if/service.thrift \
		-o tests/

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
	cp common/github/Network.hs lib/test/
	cp common/github/Network.hs server/test/

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
