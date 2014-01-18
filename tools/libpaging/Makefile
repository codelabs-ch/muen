LIBRARY = libpaging

include ../../Makeconf
include ../../Makecov

tests: test_$(LIBRARY)
	@obj/tests/test_runner

test_$(LIBRARY):
	@gprbuild $(BUILD_OPTS) -P$@ -XBUILD=tests

build_cov:
	@gprbuild $(BUILD_OPTS) -Ptest_$(LIBRARY) -XBUILD=coverage

clean:
	@rm -rf lib obj
