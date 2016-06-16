SYSTEM_CONFIG     ?= $(POLICY_OBJ_DIR)/system_config.xml
SYSTEM_POLICY_SRC ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME).xml
SYSTEM_POLICY_A   ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_a.xml
SYSTEM_POLICY_B   ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_b.xml

OUTPUT   = $(OBJ_DIR)/skp.ads
LAST_CFG = $(shell cat $(OBJ_DIR)/.cfg 2>/dev/null)
CUR_CFG  = $(HARDWARE):$(ADDITIONAL_HW):$(PLATFORM):$(SYSTEM)

SOURCES  = $(wildcard xml/*.xml)
SOURCES += $(PLATFORM)
SOURCES += $(HARDWARE)
SOURCES += $(ADDITIONAL_HW)

GENERATORS =         \
	$(MUGENACPI)     \
	$(MUGENIOBM)     \
	$(MUGENMSRBM)    \
	$(MUGENMSRSTORE) \
	$(MUGENPT)       \
	$(MUGENSINFO)    \
	$(MUGENSPEC)     \
	$(MUGENVTD)      \
	$(MUGENZP)

GENERATOR_OPTS = -o $(OBJ_DIR) $(SYSTEM_POLICY_B)

include ../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: cfgchange .validated $(OUTPUT)

cfgchange:
	$(if $(findstring $(CUR_CFG),$(LAST_CFG)),,rm -f $(OBJ_DIR)/*; \
		echo $(CUR_CFG) > $(OBJ_DIR)/.cfg)

$(SYSTEM_CONFIG): $(OBJ_DIR)/.cfg
	@echo '<system>'                                                         > $(SYSTEM_CONFIG);
	@echo ' <config>'                                                       >> $(SYSTEM_CONFIG);
	@echo '  <string name="system"              value="$(SYSTEM)"/>'        >> $(SYSTEM_CONFIG);
	@echo '  <string name="hardware"            value="$(HARDWARE)"/>'      >> $(SYSTEM_CONFIG);
	@echo '  <string name="additional_hardware" value="$(ADDITIONAL_HW)"/>' >> $(SYSTEM_CONFIG);
	@echo '  <string name="platform"            value="$(PLATFORM)"/>'      >> $(SYSTEM_CONFIG);
	@echo ' </config>'                                                      >> $(SYSTEM_CONFIG);
	@echo '</system>'                                                       >> $(SYSTEM_CONFIG);

$(SYSTEM_POLICY_SRC): $(SYSTEM_CONFIG) $(MUCFGMERGE) $(SOURCES)
	@$(MUCFGMERGE) -a$(ADDITIONAL_HW) -w$(HARDWARE) -p$(PLATFORM) $< $@

$(SYSTEM_POLICY_A): $(SYSTEM_POLICY_SRC) $(MUCFGEXPAND)
	@$(MUCFGEXPAND) $< $@

$(SYSTEM_POLICY_B): $(SYSTEM_POLICY_A) $(MUCFGALLOC)
	@$(MUCFGALLOC) $< $@

.validated: $(SYSTEM_POLICY_B) $(MUCFGVALIDATE)
	@$(MUCFGVALIDATE) $<
	@touch $@

$(OUTPUT): $(GENERATORS) $(SYSTEM_POLICY_B)
	@for gen in $(GENERATORS); do $$gen $(GENERATOR_OPTS) || exit 1; done
	ln -sf $(SYSTEM_POLICY_SRC) $(POLICY_SRC)
	ln -sf $(SYSTEM_POLICY_A) $(POLICY_A)
	ln -sf $(SYSTEM_POLICY_B) $(POLICY_B)

clean:
	@rm -rf $(OBJ_DIR) .validated
