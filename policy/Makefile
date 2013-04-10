POLICY  = $(wildcard xml/*.xml)
SYSTEM  = xml/example_system.xml
SCHEMA  = $(TOP_DIR)/tools/policy/schema/system.xsd
INC_DIR = include
PAC_DIR = pack
OUTPUT  = $(INC_DIR)/skp.ads

DUMMY := $(shell mkdir -p $(INC_DIR) $(PAC_DIR))

all: $(OUTPUT)

include ../Makeconf

$(OUTPUT): $(POLICY) $(INC_DIR) $(PAC_DIR)
	$(SKPOLICY) $(SCHEMA) $(SYSTEM)

clean:
	@rm -rf $(INC_DIR) $(PAC_DIR)
