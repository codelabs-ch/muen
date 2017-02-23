CSPEC_XML = $(wildcard spec/$(COMPONENT).xml)
ifneq ($(CSPEC_XML),)
INSTALL_TARGETS += install_spec
CSPEC_XML_GEN    = $(wildcard $(GEN_DIR)/$(COMPONENT).xml)
ifneq ($(CSPEC_XML_GEN),)
CSPEC_XML = $(CSPEC_XML_GEN)
endif
endif

cspecs: $(GEN_DIR)/.cspecs
$(GEN_DIR)/.cspecs: $(MUCGENSPEC) spec/$(COMPONENT).xml $(CSPEC_TARGETS)
	$(MUCGENSPEC) -i spec/$(COMPONENT).xml -o $(GEN_DIR)/$(COMPONENT).xml -I $(GEN_DIR) $(GEN_DIR)
	@touch $@

install_spec: $(POLICY_CSPEC_DIR)
	cp $(CSPEC_XML) $(POLICY_CSPEC_DIR)/

$(POLICY_CSPEC_DIR):
	mkdir -p $@
