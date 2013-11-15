POLICY   = $(wildcard xml/*.xml)
SYSTEM   = xml/example_system.xml
DSDT     = acpi/DSDT.aml
SCHEMAS  = $(SCHEMA_DIR)/*.xsd
SCHEMA   = $(SCHEMA_DIR)/system.xsd
TMPL_DIR = $(TOP_DIR)/tools/policy/templates
OUTPUT   = $(POLICY_INC)/skp.ads
HWSTAMP  = $(POLICY_INC)/.$(HARDWARE).stamp

TEMPLATES = $(wildcard $(TMPL_DIR)/*)

include ../Makeconf

DUMMY := $(shell mkdir -p $(POLICY_INC) $(POLICY_PAC))

all: $(OUTPUT) $(DSDT)

$(SYSTEM): $(SYSTEM).tmpl $(HWSTAMP)
	@sed 's/__HARDWARE__/$(HARDWARE)/g' $< > $@

$(DSDT): acpi/$(HARDWARE).dsdt.dsl
	@iasl -ta $<

$(HWSTAMP): xml/hardware/$(HARDWARE).xml
	@rm -f $(POLICY_INC)/.*.stamp
	@touch $@

$(OUTPUT): $(SKPOLICY) $(POLICY) $(SYSTEM) $(SCHEMAS) $(TEMPLATES)
	$(SKPOLICY) $(SCHEMA) $(TMPL_DIR) $(SYSTEM)

clean:
	@rm -rf $(POLICY_INC) $(POLICY_PAC) $(DSDT) $(SYSTEM)

.PHONY: clean
