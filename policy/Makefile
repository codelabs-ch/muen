DSDT       = $(OBJ_DIR)/DSDT.aml
OUTPUT     = $(POLICY_INC)/skp.ads
POLICY_OUT = $(OBJ_DIR)/$(notdir $(POLICY))
LAST_HW    = $(shell cat $(OBJ_DIR)/.hw 2>/dev/null)

GENERATORS =      \
	$(MUACPIGEN)  \
	$(MUIOBMGEN)  \
	$(MUMSRBMGEN) \
	$(MUPTGEN)    \
	$(MUSPECGEN)  \
	$(MUZPGEN)

GENERATOR_OPTS = -o $(OBJ_DIR) $(POLICY_OUT)

include ../Makeconf

DUMMY := $(shell mkdir -p $(POLICY_INC))

all: hwchange .validated $(OUTPUT) $(DSDT)

hwchange:
	@$(if $(findstring $(HARDWARE),$(LAST_HW)),,rm -f $(POLICY_OUT); \
		echo $(HARDWARE) > $(OBJ_DIR)/.hw)

ifeq ($(HARDWARE), bochs)
$(POLICY_OUT): $(POLICY)
	@cp $< $@
else
$(POLICY_OUT): $(POLICY) hardware/$(HARDWARE).patch
	@cp $< $@
	@cd $(OBJ_DIR) && patch -p3 < ../hardware/$(HARDWARE).patch >/dev/null
endif

.validated: $(POLICY_OUT)
	@$(MUVALIDATE) $(POLICY_OUT)
	@touch $@

$(DSDT): acpi/dsdt.dsl
	@cp $^ $(OBJ_DIR)
	@iasl -ta $(OBJ_DIR)/dsdt.dsl

$(OUTPUT): $(GENERATORS) $(POLICY_OUT)
	@for gen in $(GENERATORS); do $$gen $(GENERATOR_OPTS) || exit 1; done

clean:
	@rm -rf $(POLICY_INC) $(DSDT) .validated

.PHONY: prepare
