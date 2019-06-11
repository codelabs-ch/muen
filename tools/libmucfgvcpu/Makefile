COMPONENT = libmucfgvcpu

PROFILE_FILES = \
	$(GEN_DIR)/mucfgvcpu-profile_native.ads \
	$(GEN_DIR)/mucfgvcpu-profile_vm.ads

COMPONENT_TARGETS = $(PROFILE_FILES)
TEST_TARGETS      = $(COMPONENT_TARGETS)
COV_TARGETS       = $(COMPONENT_TARGETS)

ADDITIONAL_CLEAN = $(GEN_DIR)

include ../library.mk
include ../coverage.mk

include ../../projects/tests.mk

DUMMY := $(shell mkdir -p $(GEN_DIR))

$(GEN_DIR)/mucfgvcpu-profile_%.ads: profiles/%.xml
	@$(E) $(COMPONENT) "Create $* profile" "$(XML2ADA) Mucfgvcpu.Profile_$* $< $@"
