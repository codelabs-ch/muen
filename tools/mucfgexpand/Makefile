COMPONENT = mucfgexpand

include ../component.mk
include ../coverage.mk
include ../profiling.mk

XML_OBJ := $(wildcard obj/*.xml.diff)
XML_OBJ += obj/execute_run.xml

include ../../projects/tests.mk

PROFILE_ARGS = data/test_policy.xml $(OBJ_DIR)/profiling.xml
