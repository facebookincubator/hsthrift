# These are a few rules to help build the open-source hsthrift. This
# file will hopefully go away in due course.
#
# If you are integrating this Makefile into another build system like Nix,
# consider setting the environment/Make variables:
# - THRIFT_COMPILE: location of a built hsthrift compiler binary
# - EXTERNAL_FOLLY_CLIB: when set, assumes folly-clib is already provided in the environment

CABAL_BIN := cabal

THRIFT1 := thrift1
CABAL := $(CABAL_BIN) $(CABAL_CONFIG_FLAGS) $(GETDEPS_CABAL_FLAGS)

# Targets in this file invoke Cabal and hence can't be built in parallel
.NOTPARALLEL:

all:: compiler thrift-hs thrift-cpp server thrift-http

compiler::
	$(CABAL) build exe:thrift-compiler

server::
	$(CABAL) build thrift-server

util::
	$(CABAL) build fb-util

thrift-http::
	$(CABAL) build thrift-http

thrift:: thrift-cpp thrift-hs

.PHONY: thrift-compiler
# Allow injecting a prebuilt thrift compiler by setting THRIFT_COMPILE in
# environment (e.g. with Nix).
ifndef THRIFT_COMPILE
thrift-compiler:: compiler
	$(eval THRIFT_COMPILE := $$(shell $$(CABAL) -v0 list-bin exe:thrift-compiler))
else
thrift-compiler::
	# no-op
endif

thrift-hs:: thrift-compiler
	( \
		(cd lib && $(THRIFT_COMPILE) --hs \
			if/RpcOptions.thrift); \
		(cd lib && $(THRIFT_COMPILE) --hs \
			if/ApplicationException.thrift); \
		(cd lib && $(THRIFT_COMPILE) --hs --use-int \
			test/if/math.thrift \
			-o test); \
		mkdir -p cpp-channel/test/if; \
		(cd lib && $(THRIFT_COMPILE) --hs --use-int \
			test/if/math.thrift \
			-o ../cpp-channel/test/if); \
		(cd lib && $(THRIFT_COMPILE) --hs --use-int \
			test/if/math.thrift \
			-o ../server/test); \
		(cd lib && $(THRIFT_COMPILE) --hs --use-int \
			test/if/math.thrift \
			-o ../http/test); \
		(cd lib && $(THRIFT_COMPILE) --hs --use-int \
			test/if/echoer.thrift \
			-o test); \
		(cd lib && $(THRIFT_COMPILE) --hs --use-int \
			test/if/echoer.thrift \
			-o ../server/test); \
		(cd lib && $(THRIFT_COMPILE) --hs --use-int \
			test/if/echoer.thrift \
			-o ../http/test); \
		(cd server && $(THRIFT_COMPILE) --hs \
			test/if/hash_map.thrift \
			-o test); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/hs_prefix.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/foo.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/constants.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			--duplicate-names \
			if/duplicate.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/EnumConst.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/enum.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/exception.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			--use-int --use-hash-map --use-hash-set \
			if/flags.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			--extra-hasfields \
			if/hasfield.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/A.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/B.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/C.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/D.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/E.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/versions.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/monoid.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/hs_test.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/hs_test.thrift -o ../lib/test); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/map.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/messed_up_case.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/namespace.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/namespace_included.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/parens.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			--required-symbols "A,B,C,X,weNeedThis" \
			if/huge.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/scoped_enums.thrift); \
		(cd tests && $(THRIFT_COMPILE) --hs \
			if/service.thrift); \
	)
	# those files are required for thrift-compiler's tests
	mkdir -p compiler/tests/if
	cp tests/if/*.thrift compiler/tests/if/
	cp tests/if/*.hs compiler/tests/if/

thrift-cpp::
	mkdir -p cpp-channel/if cpp-channel/test/if
	cd lib && $(THRIFT1) -I . --gen mstch_cpp2 \
		-o ../cpp-channel/if \
		if/RpcOptions.thrift
	cd lib/test/if && $(THRIFT1) -I . --gen mstch_cpp2 \
                -o ../../../cpp-channel/test/if \
                math.thrift
	cd tests/if && $(THRIFT1) -I . --gen mstch_cpp2 \
		-o . \
		hs_test.thrift

test::
	$(CABAL) test mangle fb-util thrift-compiler thrift-lib thrift-server thrift-tests --keep-going

.PHONY: cabal-update
cabal-update::
	$(CABAL) update

# Dummy install rule to keep getdeps happy. TODO: actually install things
.PHONY: install
install::
	mkdir -p $(PREFIX)

# Unpack the correct revisions of folly and fast_float under hsthrift/folly,
# and run cmake to generate folly-config.h.
.PHONY: setup-folly
ifndef EXTERNAL_FOLLY_CLIB
setup-folly::
	rm -rf folly-clib/folly folly-clib/fast_float* folly-clib/v*.tar.gz
	(cd folly-clib && \
		mkdir folly && cd folly && \
		git init && \
		git remote add origin https://github.com/facebook/folly.git && \
		git fetch --depth=1 origin $$(sed 's/Subproject commit //' <../../build/deps/github_hashes/facebook/folly-rev.txt) && \
		git checkout FETCH_HEAD \
	)
	(cd folly-clib/folly && \
		echo 'message("FILES_CPP:$${files}")' >>CMakeLists.txt && \
		echo 'message("FILES_H:$${hfiles}")' >>CMakeLists.txt && \
		mkdir _build && cd _build && \
		cmake .. 2>&1 | sed 's/[^m]*m//g' | tee out && \
		grep '^FILES_CPP:' out | \
			sed 's/FILES_CPP://' | \
			sed "s|$$(dirname $$(pwd))/|folly/|g" | \
			sed 's/;/ \\\n            /g' >cppfiles && \
		grep '^FILES_H:' out | \
			sed 's/FILES_H://' | \
			sed "s|$$(dirname $$(pwd))/||g" | \
			sed 's/;/ \\\n            /g' >hfiles && \
		cd ../.. && sed "s|__CPP_FILES__|$$(cat <folly/_build/cppfiles)|;s|__H_FILES__|$$(cat <folly/_build/hfiles)|" <folly-clib.cabal.in >folly-clib.cabal \
	)
	(cd folly-clib && \
	   	wget $$(grep 'url =' ../build/fbcode_builder/manifests/fast_float | sed 's/^.*= *//'); \
		tar xvzf *.tar.gz \
	)

# Use the timestamp of the most recent commit as the version number,
# since there are no upstream releases. Careful to generate something
# that respects Cabal's constraints on version numbers: no leading
# zeroes and fields must be <10 digits.
.PHONY: setup-folly-version setup-folly-0
ver:=$(shell cd folly-clib/folly && date -u "--date=@$$(git log -1 --format=%ct)" +%Y%m%d.%-k%M | sed 's/\.0*/\./')
setup-folly-version::
	sed -i "s/^version:\(\s*\).*$$/version:\1$(ver)/" folly-clib/folly-clib.cabal
	sed -i "s/^\(\s*\)build-depends:\(\s*\)folly-clib\s*$$/\1build-depends: folly-clib==$(ver)/" common/util/fb-util.cabal

# Make version 0.0 of folly-clib, the empty package
setup-folly-0::
	sed "s|__CPP_FILES__||;s|__H_FILES__||" <folly-clib/folly-clib.cabal.in | grep -v '^\s*install-includes' | grep -v '\.h$$' >folly-clib/folly-clib.cabal
	sed -i "s/^version:\(\s*\).*$$/version:\10.0/" folly-clib/folly-clib.cabal

else
setup-folly::
.PHONY: setup-folly-version setup-folly-0
setup-folly-version::
setup-folly-0::
endif

setup-meta::
	ln -s cabal-meta.project cabal.project
