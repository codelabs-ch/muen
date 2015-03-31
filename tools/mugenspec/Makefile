COMPONENT = mugenspec

TEMPLATES = src/string_templates.ads
SPECS     = $(wildcard data/*.ad*)

COMPONENT_TARGETS = $(TEMPLATES)
TEST_TARGETS      = $(COMPONENT_TARGETS) prove-specs
COV_TARGETS       = $(COMPONENT_TARGETS)

include ../component.mk
include ../coverage.mk
include ../templates.mk

prove-specs: $(SPECS) $(OBJ_DIR)/prove/skp-subjects.ads
	gnatprove --report=all -Pgnatprove_mugenspec

$(OBJ_DIR)/prove/%: templates/%
	@mkdir -p $(OBJ_DIR)/prove
	cp $< $@
