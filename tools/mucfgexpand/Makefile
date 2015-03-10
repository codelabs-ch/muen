COMPONENT = mucfgexpand

include ../component.mk
include ../coverage.mk
include ../profiling.mk

XML_OBJ := $(wildcard obj/*.xml)
REFS    := $(subst .xml,.ref.xml,$(subst obj/,data/,$(XML_OBJ)))

PROFILE_ARGS = data/test_policy.xml $(OBJ_DIR)/profiling.xml

update-refs: $(REFS)
data/%.ref.xml: obj/%.xml
	cp $< $@
