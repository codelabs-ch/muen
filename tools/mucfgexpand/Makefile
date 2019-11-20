COMPONENT = mucfgexpand

include ../component.mk
include ../coverage.mk
include ../profiling.mk
include ../fuzzing.mk

XML_OBJ := $(wildcard obj/*.xml.diff)
XML_OBJ += obj/execute_run.xml

include ../../projects/tests.mk

PROFILE_ARGS = data/test_policy.xml $(OBJ_DIR)/profiling.xml
FUZZ_ARGS = @@ $(OBJ_DIR)/fuzzing.xml
