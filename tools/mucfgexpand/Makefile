COMPONENT = mucfgexpand

include ../component.mk
include ../coverage.mk
include ../profiling.mk

include ../../projects/tests.mk

XML_OBJ := $(wildcard obj/*.xml.diff)
REFS    := $(subst obj/,data/,$(XML_OBJ))

PROFILE_ARGS = data/test_policy.xml $(OBJ_DIR)/profiling.xml

update-refs: $(REFS)
data/%.xml.diff: obj/%.xml.diff
	cp $< $@
