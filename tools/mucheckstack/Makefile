COMPONENT = mucheckstack

TEST_TARGETS = build-testci

include ../component.mk
include ../coverage.mk

include ../../projects/tests.mk

build-testci:
	gprbuild -q -p -Pdata/testci
	gprbuild -q -p -Pdata/testci_main
