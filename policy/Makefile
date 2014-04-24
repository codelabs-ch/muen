OUTPUT   = $(OBJ_DIR)/skp.ads
LAST_HW  = $(shell cat $(OBJ_DIR)/.hw 2>/dev/null)
DSDT_SRC = acpi/linux_dsdt.dsl
DSDT_OBJ = $(OBJ_DIR)/linux_dsdt.dsl
DSDT     = $(OBJ_DIR)/linux_dsdt.aml
PATCH    = hardware/$(HARDWARE).patch

POLICY     = xml/demo_system.xml
POLICY_SRC = $(OBJ_DIR)/demo_system.xml
POLICY_A   = $(OBJ_DIR)/demo_system_a.xml
POLICY_B   = $(OBJ_DIR)/demo_system_b.xml
PLATFORM   = platform/$(HARDWARE).xml

GENERATORS =      \
	$(MUGENACPI)  \
	$(MUGENIOBM)  \
	$(MUGENMSRBM) \
	$(MUGENPT)    \
	$(MUGENSPEC)  \
	$(MUGENZP)

GENERATOR_OPTS = -o $(OBJ_DIR) $(POLICY_B)

include ../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: hwchange .validated $(OUTPUT) $(DSDT)

hwchange:
	$(if $(findstring $(HARDWARE),$(LAST_HW)),,rm -f $(OBJ_DIR)/*; \
		echo $(HARDWARE) > $(OBJ_DIR)/.hw)

ifeq ($(HARDWARE), bochs)
$(DSDT_OBJ) $(POLICY_SRC): $(MUCFGMERGE) $(POLICY) $(DSDT_SRC) $(PLATFORM)
	@$(MUCFGMERGE) -p$(PLATFORM) $(POLICY) $(POLICY_SRC)
	cp $(DSDT_SRC) $(OBJ_DIR)
else
$(DSDT_OBJ) $(POLICY_SRC): $(MUCFGMERGE) $(POLICY) $(DSDT_SRC) $(PLATFORM) $(PATCH)
	@$(MUCFGMERGE) -p$(PLATFORM) $(POLICY) $(POLICY_SRC)
	cp $(DSDT_SRC) $(OBJ_DIR)
	cd $(OBJ_DIR) && patch -p1 < ../$(PATCH)
endif

$(POLICY_A): $(POLICY_SRC) $(MUCFGEXPAND)
	@$(MUCFGEXPAND) $< $@

$(POLICY_B): $(POLICY_A) $(MUCFGALLOC)
	@$(MUCFGALLOC) $< $@

.validated: $(POLICY_B) $(MUCFGVALIDATE)
	@$(MUCFGVALIDATE) $<
	@touch $@

$(DSDT): $(DSDT_OBJ)
	@iasl -p$(OBJ_DIR)/linux_dsdt -ta $<

$(OUTPUT): $(GENERATORS) $(POLICY_B)
	@for gen in $(GENERATORS); do $$gen $(GENERATOR_OPTS) || exit 1; done

clean:
	@rm -rf $(OBJ_DIR) .validated
