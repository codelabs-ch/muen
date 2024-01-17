COMPONENT = libmuxml

SCHEMA_ADS_FILES = \
	$(GEN_DIR)/muxml-system_a_schema.ads        \
	$(GEN_DIR)/muxml-system_a_extended_schema.ads        \
	$(GEN_DIR)/muxml-system_b_schema.ads        \
	$(GEN_DIR)/muxml-system_b_extended_schema.ads        \
	$(GEN_DIR)/muxml-system_src_schema.ads      \
	$(GEN_DIR)/muxml-system_src_extended_schema.ads      \
	$(GEN_DIR)/muxml-vcpu_profile_schema.ads    \
	$(GEN_DIR)/muxml-vcpu_profile_extended_schema.ads    \
	$(GEN_DIR)/muxml-system_config_schema.ads   \
	$(GEN_DIR)/muxml-system_config_extended_schema.ads   \
	$(GEN_DIR)/muxml-hardware_config_schema.ads \
	$(GEN_DIR)/muxml-hardware_config_extended_schema.ads \
	$(GEN_DIR)/muxml-component_schema.ads       \
	$(GEN_DIR)/muxml-component_extended_schema.ads

SYSTEM_SCHEMA_FILES = \
	$(GEN_DIR)/system_a.xsd        \
	$(GEN_DIR)/system_b.xsd        \
	$(GEN_DIR)/system_src.xsd

SCHEMA_FILE = schema/`echo $* | sed -e 's/_a//g;s/_b//g;s/_src//g'`.xsd

XSD_FILES = $(shell find schema/ -name '*.xsd')
SCHEMA_PLUGINS = $(shell find schema_plugins/ -name '*.py')

COMPONENT_TARGETS = $(SCHEMA_ADS_FILES)
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

$(GEN_DIR)/muxml-%_extended_schema.ads: $(GEN_DIR)/%_extended.xsd | $(GEN_DIR)
	@$(E) $(COMPONENT) "Xml2Ada $*_extended" \
		"$(XML2ADA) Muxml.$*_extended_schema $(GEN_DIR)/$*_extended.xsd $@"

$(GEN_DIR)/%_extended.xsd: $(GEN_DIR)/%.xsd $(SCHEMA_PLUGINS) | $(GEN_DIR)
	@$(E) $(COMPONENT) "Doc plugin $*_extended" \
		"schema_plugins/xsd_plugin_doc.py $(GEN_DIR)/$*.xsd $@"

$(GEN_DIR)/muxml-%_schema.ads: $(GEN_DIR)/%.xsd | $(GEN_DIR)
	@$(E) $(COMPONENT) "Xml2Ada $*" \
		"$(XML2ADA) Muxml.$*_schema $(GEN_DIR)/$*.xsd $@"

$(GEN_DIR)/%.xsd: $(XSD_FILES) $(RESOLVE_XSL) | $(GEN_DIR)
	@$(E) $(COMPONENT) "XSLT $*" \
		"xsltproc --stringparam format $* $(RESOLVE_XSL) $(SCHEMA_FILE) > $@"

xmllint: $(LINT_OK)
$(OBJ_DIR)/%.lintok: $(COMPONENT_TARGETS) $(LINT_XML) $(SYSTEM_SCHEMA_FILES) | $(OBJ_DIR)
	@$(E) $(COMPONENT) "Xmllint $*" \
		"xmllint --noout --schema generated/system_$(*:format_%.xml=%).xsd data/$*"
	@touch $@

doc/graph_integration_overview.pdf: doc/graph_integration_overview.tex
	cd doc && pdflatex graph_integration_overview.tex

.PRECIOUS: $(GEN_DIR)/%.xsd $(GEN_DIR)/%_extended.xsd
