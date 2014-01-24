DSDT   = $(OBJ_DIR)/DSDT.aml
OUTPUT = $(POLICY_INC)/skp.ads

GENERATORS =      \
	$(MUACPIGEN)  \
	$(MUIOBMGEN)  \
	$(MUMSRBMGEN) \
	$(MUPTGEN)    \
	$(MUSPECGEN)  \
	$(MUZPGEN)

GENERATOR_OPTS = -o $(OBJ_DIR) $(POLICY)

include ../Makeconf

DUMMY := $(shell mkdir -p $(POLICY_INC))

all: .validated $(OUTPUT) $(DSDT)

.validated: $(POLICY)
	@$(MUVALIDATE) $(POLICY)
	@touch $@

$(DSDT): acpi/$(HARDWARE).dsdt.dsl
	@cp $^ $(OBJ_DIR)
	@iasl -ta $(OBJ_DIR)/$(HARDWARE).dsdt.dsl

$(OUTPUT): $(GENERATORS) $(POLICY)
	@for gen in $(GENERATORS); do $$gen $(GENERATOR_OPTS) || exit 1; done

clean:
	@rm -rf $(POLICY_INC) $(DSDT) .validated

.PHONY: clean
