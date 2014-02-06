OUTPUT     = $(POLICY_INC)/skp.ads
POLICY_OUT = $(OBJ_DIR)/$(notdir $(POLICY))
LAST_HW    = $(shell cat $(OBJ_DIR)/.hw 2>/dev/null)
DSDT_SRC   = acpi/dsdt.dsl
DSDT_OBJ   = $(OBJ_DIR)/dsdt.dsl
DSDT       = $(OBJ_DIR)/DSDT.aml
PATCH      = hardware/$(HARDWARE).patch

GENERATORS =      \
	$(MUGENACPI)  \
	$(MUGENIOBM)  \
	$(MUGENMSRBM) \
	$(MUGENPT)    \
	$(MUGENSPEC)  \
	$(MUGENZP)

GENERATOR_OPTS = -o $(OBJ_DIR) $(POLICY_OUT)

include ../Makeconf

DUMMY := $(shell mkdir -p $(POLICY_INC))

all: hwchange .validated $(OUTPUT) $(DSDT)

hwchange:
	@$(if $(findstring $(HARDWARE),$(LAST_HW)),,rm -f $(POLICY_OUT) $(DSDT_OBJ); \
		echo $(HARDWARE) > $(OBJ_DIR)/.hw)

ifeq ($(HARDWARE), bochs)
$(DSDT_OBJ) $(POLICY_OUT): $(POLICY) $(DSDT_SRC)
	@cp $(POLICY) $(POLICY_OUT)
	@cp $(DSDT_SRC) $(DSDT_OBJ)
else
$(DSDT_OBJ) $(POLICY_OUT): $(POLICY) $(DSDT_SRC) $(PATCH)
	@cp $(POLICY) $(POLICY_OUT)
	@cp $(DSDT_SRC) $(DSDT_OBJ)
	@cd $(OBJ_DIR) && patch -p3 < ../$(PATCH) >/dev/null
endif

.validated: $(POLICY_OUT) $(MUCFGVALIDATE)
	@$(MUCFGVALIDATE) $(POLICY_OUT)
	@touch $@

$(DSDT): $(DSDT_OBJ)
	@iasl -ta $(OBJ_DIR)/dsdt.dsl

$(OUTPUT): $(GENERATORS) $(POLICY_OUT)
	@for gen in $(GENERATORS); do $$gen $(GENERATOR_OPTS) || exit 1; done

clean:
	@rm -rf $(POLICY_INC) .validated
