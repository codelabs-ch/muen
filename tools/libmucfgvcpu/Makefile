COMPONENT = libmucfgvcpu

PROFILE_FILES = \
   obj/mucfgvcpu-profile_native.ads \
   obj/mucfgvcpu-profile_linux.ads

COMPONENT_DEPS = $(PROFILE_FILES)
TEST_DEPS      = $(COMPONENT_DEPS)
COV_DEPS       = $(COMPONENT_DEPS)

include ../library.mk
include ../../Makecov

DUMMY := $(shell mkdir -p $(OBJ_DIR))

obj/mucfgvcpu-profile_%.ads: profiles/%.xml
	@../scripts/xml2ada Mucfgvcpu.Profile_$* $< $@
