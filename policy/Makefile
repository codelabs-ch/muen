OUTPUT   = $(OBJ_DIR)/skp.ads
LAST_CFG = $(shell cat $(OBJ_DIR)/.cfg 2>/dev/null)
DSDT_SRC = platform/$(HARDWARE).dsl
DSDT_OBJ = $(DSDT).dsl
DSDT     = $(OBJ_DIR)/nic_linux_dsdt

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

all: cfgchange .validated $(OUTPUT) $(DSDT).aml

cfgchange:
	$(if $(findstring $(HARDWARE)/$(SYSTEM),$(LAST_CFG)),,rm -f $(OBJ_DIR)/*; \
		echo $(HARDWARE)/$(SYSTEM) > $(OBJ_DIR)/.cfg)

$(DSDT_OBJ) $(POLICY_SRC): $(MUCFGMERGE) $(POLICY) $(DSDT_SRC) $(PLATFORM)
	@$(MUCFGMERGE) -p$(PLATFORM) $(POLICY) $(POLICY_SRC)
	cp $(DSDT_SRC) $(DSDT_OBJ)

$(POLICY_A): $(POLICY_SRC) $(MUCFGEXPAND)
	@$(MUCFGEXPAND) $< $@

$(POLICY_B): $(POLICY_A) $(MUCFGALLOC)
	@$(MUCFGALLOC) $< $@

.validated: $(POLICY_B) $(MUCFGVALIDATE)
	@$(MUCFGVALIDATE) $<
	@touch $@

%.aml: %.dsl
	@iasl -p$@ -ta $<

$(OUTPUT): $(GENERATORS) $(POLICY_B)
	@for gen in $(GENERATORS); do $$gen $(GENERATOR_OPTS) || exit 1; done

clean:
	@rm -rf $(OBJ_DIR) .validated
