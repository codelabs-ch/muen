COMPONENT = mugenspec

DEPENDS = libmutools

TEMPLATES = src/string_templates.ads

COMPONENT_TARGETS = $(TEMPLATES)
TEST_TARGETS      = $(COMPONENT_TARGETS) prove-specs
COV_TARGETS       = $(COMPONENT_TARGETS)

include ../component.mk
include ../coverage.mk
include ../templates.mk

prove-specs: $(OBJ_DIR)/prove
	gnatprove --report=all -Pgnatprove_mugenspec

$(OBJ_DIR)/prove: data/test_policy.xml $(COMPONENT)
	@mkdir -p $(OBJ_DIR)/prove
	bin/mugenspec -o $@ $<
