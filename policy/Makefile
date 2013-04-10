POLICY  = example_system.xml
SCHEMA  = $(TOP_DIR)/tools/policy/schema/system.xsd
INC_DIR = include
PAC_DIR = pack

DUMMY := $(shell mkdir -p $(INC_DIR) $(PAC_DIR))

all: process_policy

include ../Makeconf

process_policy: $(POLICY) $(INC_DIR) $(PAC_DIR)
	$(SKPOLICY) $(SCHEMA) $(POLICY)

clean:
	@rm -rf $(INC_DIR) $(PAC_DIR)
