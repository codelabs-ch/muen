OUTPUT   = $(OBJ_DIR)/skp.ads
LAST_CFG = $(shell cat $(OBJ_DIR)/.cfg 2>/dev/null)

DSDT_FILES = $(patsubst acpi/%.dsl,$(OBJ_DIR)/%.aml,$(wildcard acpi/*.dsl))

POLICY     = xml/$(SYSTEM).xml
POLICY_SRC = $(OBJ_DIR)/$(SYSTEM).xml
POLICY_A   = $(OBJ_DIR)/$(SYSTEM)_a.xml
POLICY_B   = $(OBJ_DIR)/$(SYSTEM)_b.xml
PLATFORM   = platform/$(HARDWARE).xml

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

GENERATOR_OPTS = -o $(OBJ_DIR) $(POLICY_B)

include ../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: cfgchange .validated $(OUTPUT) $(DSDT_FILES)

cfgchange:
	$(if $(findstring $(HARDWARE)/$(SYSTEM),$(LAST_CFG)),,rm -f $(OBJ_DIR)/*; \
		echo $(HARDWARE)/$(SYSTEM) > $(OBJ_DIR)/.cfg)

$(POLICY_SRC): $(MUCFGMERGE) $(POLICY) $(PLATFORM)
	@$(MUCFGMERGE) -p$(PLATFORM) $(POLICY) $(POLICY_SRC)

$(POLICY_A): $(POLICY_SRC) $(MUCFGEXPAND)
	@$(MUCFGEXPAND) $< $@

$(POLICY_B): $(POLICY_A) $(MUCFGALLOC)
	@$(MUCFGALLOC) $< $@

.validated: $(POLICY_B) $(MUCFGVALIDATE)
	@$(MUCFGVALIDATE) $<
	@touch $@

$(OBJ_DIR)/%.aml: acpi/%.dsl
	@iasl -p$@ -ta $<

$(OUTPUT): $(GENERATORS) $(POLICY_B)
	@for gen in $(GENERATORS); do $$gen $(GENERATOR_OPTS) || exit 1; done

clean:
	@rm -rf $(OBJ_DIR) .validated
