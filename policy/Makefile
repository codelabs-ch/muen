POLICY   = $(wildcard xml/*.xml)
SYSTEM   = xml/example_system.xml
TOOL_DIR = $(TOP_DIR)/tools/policy
SCHEMA   = $(TOOL_DIR)/schema/system.xsd
TMPL_DIR = $(TOOL_DIR)/templates
OUTPUT   = $(POLICY_INC)/skp.ads
HWSTAMP  = $(POLICY_INC)/.$(HARDWARE).stamp

TEMPLATES = $(wildcard $(TMPL_DIR)/*)

include ../Makeconf

DUMMY := $(shell mkdir -p $(POLICY_INC) $(POLICY_PAC))

all: $(OUTPUT)

$(SYSTEM): $(SYSTEM).tmpl $(HWSTAMP)
	@sed 's/__HARDWARE__/$(HARDWARE)/g' $< > $@

$(HWSTAMP): xml/hardware/$(HARDWARE).xml
	@rm -f $(POLICY_INC)/.*.stamp
	@touch $@

$(OUTPUT): $(SKPOLICY) $(POLICY) $(SYSTEM) $(SCHEMA) $(TEMPLATES)
	$(SKPOLICY) $(SCHEMA) $(TMPL_DIR) $(SYSTEM)

clean:
	@rm -rf $(POLICY_INC) $(POLICY_PAC) $(SYSTEM)
