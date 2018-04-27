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
TEST_TARGETS      = $(COMPONENT_TARGETS)
COV_TARGETS       = $(COMPONENT_TARGETS)

ADDITIONAL_CLEAN = $(GEN_DIR)

include ../library.mk
include ../coverage.mk

include ../../projects/tests.mk

DUMMY := $(shell mkdir -p $(GEN_DIR))

$(GEN_DIR)/muxml-%_schema.ads: $(XSD_FILES) $(RESOLVE_XSL)
	@$(E) $(COMPONENT) "XSLT $*" \
		"xsltproc --stringparam format $* $(RESOLVE_XSL) $(SCHEMA_FILE) > $(GEN_DIR)/$*.xsd"
	@$(E) $(COMPONENT) "Xml2Ada $*" \
		"$(XML2ADA) Muxml.$*_schema $(GEN_DIR)/$*.xsd $@"
