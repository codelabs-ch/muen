SYSTEM_DEPS  = $(SYSTEM_DIR)/mirageos/mirage-solo5.xml
SYSTEM_DEPS += $(SYSTEM_DIR)/mirageos/component_unikernel.xml

$(SYSTEM): $(SYSTEM_DEPS)
	@$(E) policy "Create $(SYSTEM)" \
		"cd $(SYSTEM_DIR)/mirageos && ./compose.py"
