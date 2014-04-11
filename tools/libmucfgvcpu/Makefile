COMPONENT = libmucfgvcpu

PROFILE_FILES = \
   $(GEN_DIR)/mucfgvcpu-profile_native.ads \
   $(GEN_DIR)/mucfgvcpu-profile_linux.ads

COMPONENT_DEPS = $(PROFILE_FILES)
TEST_DEPS      = $(COMPONENT_DEPS)
COV_DEPS       = $(COMPONENT_DEPS)

ADDITIONAL_CLEAN = $(GEN_DIR)

include ../library.mk
include ../../Makecov

DUMMY := $(shell mkdir -p $(GEN_DIR))

$(GEN_DIR)/mucfgvcpu-profile_%.ads: profiles/%.xml
	@../scripts/xml2ada Mucfgvcpu.Profile_$* $< $@
