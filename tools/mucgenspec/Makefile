COMPONENT = mucgenspec

TEMPLATES = $(GEN_DIR)/string_templates.ads

COMPONENT_TARGETS = $(TEMPLATES)
TEST_TARGETS      = $(COMPONENT_TARGETS)
COV_TARGETS       = $(COMPONENT_TARGETS)

ADDITIONAL_CLEAN = $(GEN_DIR)

include ../component.mk
include ../coverage.mk
include ../templates.mk

include ../../build-cfg/mk/tests.mk
