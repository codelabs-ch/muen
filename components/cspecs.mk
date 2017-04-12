CSPEC_XML     = $(wildcard spec/$(COMPONENT).xml)
CSPEC_INSTALL = $(POLICY_CSPEC_DIR)/$(COMPONENT).xml

DUMMY := $(shell mkdir -p $(POLICY_CSPEC_DIR))

ifneq ($(CSPEC_XML),)
INSTALL_TARGETS += $(CSPEC_INSTALL)
CSPEC_XML_GEN    = $(wildcard $(GEN_DIR)/$(COMPONENT).xml)
ifneq ($(CSPEC_XML_GEN),)
CSPEC_XML = $(CSPEC_XML_GEN)
endif
endif

cspecs: $(GEN_DIR)/.cspecs
$(GEN_DIR)/.cspecs: $(MUCGENSPEC) spec/$(COMPONENT).xml $(CSPEC_TARGETS)
	$(MUCGENSPEC) -i spec/$(COMPONENT).xml -o $(GEN_DIR)/$(COMPONENT).xml -I $(GEN_DIR) $(GEN_DIR)
	@touch $@

$(CSPEC_INSTALL): $(CSPEC_XML)
	cp $< $@
