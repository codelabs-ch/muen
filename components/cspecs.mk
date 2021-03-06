CSPEC_XML     = $(wildcard spec/$(COMPONENT).xml)
CSPEC_INSTALL = $(POLICY_CSPEC_DIR)/$(COMPONENT).xml
COMP_HAS_BIN  = $(wildcard $(COMP_BIN))

DUMMY := $(shell mkdir -p $(POLICY_CSPEC_DIR))

ifneq ($(CSPEC_XML),)
INSTALL_TARGETS += $(CSPEC_INSTALL)
CSPEC_XML_GEN    = $(wildcard $(GEN_DIR)/$(COMPONENT).xml)
ifneq ($(CSPEC_XML_GEN),)
CSPEC_XML = $(CSPEC_XML_GEN)
endif
CSPEC_INST_CMD  = cp $(CSPEC_XML) $(CSPEC_INSTALL)
CSPEC_INST_DEPS = $(CSPEC_XML)
ifneq ($(COMP_HAS_BIN),)
CSPEC_INST_CMD   = $(MUCBINSPLIT) -i $(CSPEC_XML) -b $(COMP_BIN) -o $(CSPEC_INSTALL) $(POLICY_OBJ_DIR)
CSPEC_INST_DEPS += $(MUCBINSPLIT) $(COMP_BIN)
endif
endif

cspecs: $(GEN_DIR)/$(COMPONENT).xml
$(GEN_DIR)/$(COMPONENT).xml: spec/$(COMPONENT).xml $(GEN_DIR) $(MUCGENSPEC) $(CSPEC_TARGETS)
	@$(E) $(COMPONENT) "Generate cspecs" \
		"$(MUCGENSPEC) -i $< -o $@ -I $(GEN_DIR) $(GEN_DIR)"

$(CSPEC_INSTALL): $(CSPEC_INST_DEPS)
	@$(E) $(COMPONENT) "Install cspecs" "$(CSPEC_INST_CMD)"

$(GEN_DIR):
	mkdir -p $@
