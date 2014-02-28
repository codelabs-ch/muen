OUTPUT   = $(OBJ_DIR)/skp.ads
LAST_HW  = $(shell cat $(OBJ_DIR)/.hw 2>/dev/null)
DSDT_SRC = acpi/dsdt.dsl
DSDT_OBJ = $(OBJ_DIR)/dsdt.dsl
DSDT     = $(OBJ_DIR)/DSDT.aml
PATCH    = hardware/$(HARDWARE).patch

POLICY     = xml/demo_system.xml
POLICY_SRC = $(OBJ_DIR)/demo_system.xml
POLICY_B   = $(OBJ_DIR)/system.xml

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
$(DSDT_OBJ) $(POLICY_SRC): $(POLICY) $(DSDT_SRC)
	cp $(POLICY) $(DSDT_SRC) $(OBJ_DIR)
else
$(DSDT_OBJ) $(POLICY_SRC): $(POLICY) $(DSDT_SRC) $(PATCH)
	cp $(POLICY) $(DSDT_SRC) $(OBJ_DIR)
	cd $(OBJ_DIR) && patch -p3 < ../$(PATCH)
endif

$(POLICY_B): $(POLICY_SRC)
	@$(MUCFGALLOC) -o $(OBJ_DIR) $(POLICY_SRC)

.validated: $(POLICY_B) $(MUCFGVALIDATE)
	@$(MUCFGVALIDATE) $(POLICY_B)
	@touch $@

$(DSDT): $(DSDT_OBJ)
	@iasl -ta $(OBJ_DIR)/dsdt.dsl

$(OUTPUT): $(GENERATORS) $(POLICY_B)
	@for gen in $(GENERATORS); do $$gen $(GENERATOR_OPTS) || exit 1; done

clean:
	@rm -rf $(OBJ_DIR) .validated
