COMPONENT = mugenspec

TEMPLATES    = $(GEN_DIR)/string_templates.ads
SPECS        = $(wildcard data/*.ad*)
STATIC_SPECS = $(OBJ_DIR)/prove/skp-subjects.ads

COMPONENT_TARGETS = $(TEMPLATES)
TEST_TARGETS      = $(COMPONENT_TARGETS) $(OBJ_DIR)/gnatprove/gnatprove.out
COV_TARGETS       = $(COMPONENT_TARGETS)

ADDITIONAL_CLEAN = $(GEN_DIR)

include ../component.mk
include ../coverage.mk
include ../templates.mk

include ../../projects/tests.mk

$(OBJ_DIR)/gnatprove/gnatprove.out: $(SPECS) $(STATIC_SPECS)
	gnatprove $(SPARK_OPTS) -q --report=fail -Pgnatprove_mugenspec

$(OBJ_DIR)/prove/%: templates/% | $(OBJ_DIR)/prove
	cp $< $@

$(OBJ_DIR)/prove:
	mkdir -p $@
