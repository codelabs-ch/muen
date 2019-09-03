COMPONENT = libmuxml

SCHEMA_FILES = \
	$(GEN_DIR)/muxml-system_a_schema.ads        \
	$(GEN_DIR)/muxml-system_b_schema.ads        \
	$(GEN_DIR)/muxml-system_src_schema.ads      \
	$(GEN_DIR)/muxml-vcpu_profile_schema.ads    \
	$(GEN_DIR)/muxml-system_config_schema.ads   \
	$(GEN_DIR)/muxml-hardware_config_schema.ads \
	$(GEN_DIR)/muxml-mugenschedcfg_schema.ads   \
	$(GEN_DIR)/muxml-component_schema.ads
SCHEMA_FILE = schema/`echo $* | sed -e 's/_a//g;s/_b//g;s/_src//g'`.xsd

XSD_FILES = $(shell find schema/ -name '*.xsd')

COMPONENT_TARGETS = $(SCHEMA_FILES)
TEST_TARGETS      = $(COMPONENT_TARGETS) xmllint
COV_TARGETS       = $(COMPONENT_TARGETS)

LINT_XML   = $(wildcard data/format_*.xml)
LINT_FILES = $(notdir $(LINT_XML))
LINT_OK    = $(LINT_FILES:%=$(OBJ_DIR)/%.lintok)

ADDITIONAL_CLEAN = $(GEN_DIR)

include ../library.mk
include ../coverage.mk

include ../../projects/tests.mk

$(OBJ_DIR) $(GEN_DIR):
	@mkdir $@

$(GEN_DIR)/muxml-%_schema.ads: $(XSD_FILES) $(RESOLVE_XSL) | $(GEN_DIR)
	@$(E) $(COMPONENT) "XSLT $*" \
		"xsltproc --stringparam format $* $(RESOLVE_XSL) $(SCHEMA_FILE) > $(GEN_DIR)/$*.xsd"
	@$(E) $(COMPONENT) "Xml2Ada $*" \
		"$(XML2ADA) Muxml.$*_schema $(GEN_DIR)/$*.xsd $@"

xmllint: $(LINT_OK)
$(OBJ_DIR)/%.lintok: $(COMPONENT_TARGETS) $(LINT_XML) | $(OBJ_DIR)
	@$(E) $(COMPONENT) "Xmllint $*" \
		"xmllint --noout --schema generated/system_$(*:format_%.xml=%).xsd data/$*"
	@touch $@
