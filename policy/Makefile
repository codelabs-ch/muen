POLICY  = $(wildcard xml/*.xml)
SYSTEM  = xml/example_system.xml
SCHEMA  = $(TOP_DIR)/tools/policy/schema/system.xsd
OUTPUT  = $(POLICY_INC)/skp.ads

include ../Makeconf

DUMMY := $(shell mkdir -p $(POLICY_INC) $(POLICY_PAC))

all: $(OUTPUT)

$(OUTPUT): $(POLICY)
	$(SKPOLICY) $(SCHEMA) $(SYSTEM)

clean:
	@rm -rf $(POLICY_INC) $(POLICY_PAC)
