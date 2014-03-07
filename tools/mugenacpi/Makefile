COMPONENT = mugenacpi

include ../component.mk
include ../../Makecov

build_cov:
	@gprbuild $(BUILD_OPTS) -Ptest_$(COMPONENT) -XBUILD=coverage
