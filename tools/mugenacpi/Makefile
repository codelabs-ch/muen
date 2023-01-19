COMPONENT = mugenacpi

TEMPLATES = src/string_templates.ads

COMPONENT_TARGETS = $(TEMPLATES)
TEST_TARGETS      = $(COMPONENT_TARGETS)
COV_TARGETS       = $(COMPONENT_TARGETS)

include ../component.mk
include ../coverage.mk
include ../profiling.mk
include ../templates.mk

include ../../build-cfg/mk/tests.mk

PROFILE_ARGS = -o $(OBJ_DIR) data/test_policy.xml
