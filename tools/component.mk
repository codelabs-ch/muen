include ../../Makeconf

TEST_RUNNER     = $(OBJ_DIR)/tests/test_runner $(TEST_OPTS)
GNATTEST_RUNNER = $(OBJ_DIR)/tests/gnattest/harness/test_runner
GNATTEST_DRIVER = $(OBJ_DIR)/tests/gnattest/harness/test_driver
TESTS_DIR       = $(CURDIR)/tests-ng
TOOLS_DIR       = $(TOP_DIR)/tools

SRC_FILES = $(wildcard $(SRC_DIR)/*)

all: $(COMPONENT)

$(DEPENDS) $(TDEPENDS):
	@$(MAKE) -s -C $(TOOLS_DIR)/$@

tests: test_$(COMPONENT)
	@$(TEST_RUNNER)

$(COMPONENT): $(DEPENDS) $(COMPONENT_TARGETS)
	@gprbuild $(BUILD_OPTS) -P$@

test_$(COMPONENT): $(DEPENDS) $(TDEPENDS) $(TEST_TARGETS)
	@gprbuild $(BUILD_OPTS) -P$@ -XBUILD=tests

build_cov: $(DEPENDS) $(TDEPENDS) $(COV_TARGETS)
	@gprbuild $(BUILD_OPTS) -Ptest_$(COMPONENT) -XBUILD=coverage

$(OBJ_DIR)/.harness_stamp: $(SRC_FILES)
	@mkdir -p $(OBJ_DIR)/tests
	gnattest --tests-dir=$(TESTS_DIR) -Pgnattest_$(COMPONENT)
	@touch $@

build_gnattests: $(DEPENDS) $(TDEPENDS) $(TEST_TARGETS) $(OBJ_DIR)/.harness_stamp
	gprbuild $(BUILD_OPTS) -P$(GNATTEST_DRIVER) -XBUILD=tests

gnattests: build_gnattests
	$(GNATTEST_RUNNER)

clean:
	@rm -rf bin obj $(ADDITIONAL_CLEAN)
