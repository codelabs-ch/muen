POLICY  = example_system.xml
INC_DIR = include
PAC_DIR = pack

DUMMY := $(shell mkdir -p $(INC_DIR) $(PAC_DIR))

all: process_policy

include ../Makeconf

process_policy: $(POLICY) $(INC_DIR) $(PAC_DIR)
	$(SKPOLICY) $(POLICY)

clean:
	@rm -rf $(INC_DIR) $(PAC_DIR)
