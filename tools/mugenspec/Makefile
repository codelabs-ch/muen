COMPONENT = mugenspec

TEMPLATES = $(GEN_DIR)/string_templates.ads
SPECS     = $(wildcard data/*.ad*)

COMPONENT_TARGETS = $(TEMPLATES)
TEST_TARGETS      = $(COMPONENT_TARGETS) prove-specs
COV_TARGETS       = $(COMPONENT_TARGETS)

ADDITIONAL_CLEAN = $(GEN_DIR)

include ../component.mk
include ../coverage.mk
include ../templates.mk

include ../../Makeconf

include ../../projects/tests.mk

prove-specs: $(SPECS) $(OBJ_DIR)/prove/skp-subjects.ads
	gnatprove -q --prover=$(SPARK_PROVERS) -Pgnatprove_mugenspec

$(OBJ_DIR)/prove/%: templates/%
	@mkdir -p $(OBJ_DIR)/prove
	cp $< $@
