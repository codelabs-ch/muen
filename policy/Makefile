POLICY  = $(wildcard xml/*.xml)
SYSTEM  = xml/example_system.xml
SCHEMA  = $(TOP_DIR)/tools/policy/schema/system.xsd
OUTPUT  = $(POLICY_INC)/skp.ads
HWSTAMP = $(POLICY_INC)/.$(HARDWARE).stamp

include ../Makeconf

DUMMY := $(shell mkdir -p $(POLICY_INC) $(POLICY_PAC))

all: $(OUTPUT)

$(SYSTEM): $(SYSTEM).tmpl $(HWSTAMP)
	@sed 's/__HARDWARE__/$(HARDWARE)/g' $< > $@

$(HWSTAMP): xml/hardware/$(HARDWARE).xml
	@rm -f $(POLICY_INC)/.*.stamp
	@touch $@

$(OUTPUT): $(POLICY) $(SYSTEM) $(SCHEMA)
	$(SKPOLICY) $(SCHEMA) $(SYSTEM)

clean:
	@rm -rf $(POLICY_INC) $(POLICY_PAC) $(SYSTEM)
