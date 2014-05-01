TESTS_DIR = $(CURDIR)/tests-ng
COMPONENT = libmuxml

SCHEMA_FILES = \
   $(GEN_DIR)/muxml-system_a_schema.ads     \
   $(GEN_DIR)/muxml-system_b_schema.ads     \
   $(GEN_DIR)/muxml-system_src_schema.ads   \
   $(GEN_DIR)/muxml-vcpu_profile_schema.ads \
   $(GEN_DIR)/muxml-platform_config_schema.ads
SCHEMA_FILE = schema/`echo $* | sed -e 's/_a//g;s/_b//g;s/_src//g'`.xsd

XSD_FILES   = $(shell find schema/ -name '*.xsd')
RESOLVE_XSL = schema/resolve.xsl

COMPONENT_TARGETS = $(SCHEMA_FILES)
TEST_TARGETS      = $(COMPONENT_TARGETS)
COV_TARGETS       = $(COMPONENT_TARGETS)

ADDITIONAL_CLEAN = $(GEN_DIR)

include ../library.mk
include ../../Makecov

DUMMY := $(shell mkdir -p $(GEN_DIR))

$(GEN_DIR)/muxml-%_schema.ads: $(XSD_FILES) $(RESOLVE_XSL)
	@xsltproc --stringparam format $* $(RESOLVE_XSL) $(SCHEMA_FILE) > $@.tmp
	@../scripts/xml2ada Muxml.$*_schema $@.tmp $@
	@rm $@.tmp

$(OBJ_DIR)/.harness_stamp: $(SCHEMA_FILES)
	mkdir -p $(OBJ_DIR)/tests ../libtest/obj/tests ../libtest/lib
	gnattest --tests-dir=$(TESTS_DIR) -Pgnattest_$(COMPONENT)
	@touch $@

build_tests: $(OBJ_DIR)/.harness_stamp
	gprbuild -p -P obj/tests/gnattest/harness/test_driver

gnattests: build_tests
	obj/tests/gnattest/harness/test_runner
