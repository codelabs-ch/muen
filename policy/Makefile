DSDT       = $(OBJ_DIR)/DSDT.aml
OUTPUT     = $(POLICY_INC)/skp.ads
POLICY_OUT = $(POLICY_INC)/system.xml

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

all: .validated $(OUTPUT) $(DSDT)

$(POLICY_OUT): $(POLICY)
	@cp $^ $@

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

.PHONY: clean
