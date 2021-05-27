GNATTEST_EXIT   ?= on
GNATTEST_PASSED ?= show
GNATTEST_OPTS   ?= -q --test-duration --omit-sloc
GNATTEST_RUNNER  = $(OBJ_DIR)/tests/gnattest/harness/test_runner
GNATTEST_DRIVER  = $(OBJ_DIR)/tests/gnattest/harness/test_driver
TESTS_DIR        = $(CURDIR)/tests

SRC_FILES  = $(wildcard $(SRC_DIR)/*)
SRC_FILES += $(wildcard $(TESTS_DIR)/additional/*)

REFS := $(subst obj/,data/,$(XML_OBJ))

$(OBJ_DIR)/.harness_stamp: $(SRC_FILES)
	@mkdir -p $(OBJ_DIR)/tests
	gnattest $(GNATTEST_OPTS) -Pgnattest_$(COMPONENT)
	@touch $@

build_tests: $(TEST_TARGETS) $(OBJ_DIR)/.harness_stamp
	gprbuild $(filter-out --RTS=%,$(BUILD_OPTS)) -P$(GNATTEST_DRIVER) \
		-largs -fprofile-generate

tests: build_tests
	$(GNATTEST_RUNNER) --exit-status=$(GNATTEST_EXIT) --passed-tests=$(GNATTEST_PASSED)

update-refs: $(REFS)
data/%.xml.diff: obj/%.xml.diff
	cp $< $@
data/%.xml: obj/%.xml
	cp $< $@
