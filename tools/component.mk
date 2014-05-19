include ../../Makeconf

GNATTEST_RUNNER = $(OBJ_DIR)/tests/gnattest/harness/test_runner
GNATTEST_DRIVER = $(OBJ_DIR)/tests/gnattest/harness/test_driver
TESTS_DIR       = $(CURDIR)/tests-ng
TOOLS_DIR       = $(TOP_DIR)/tools

SRC_FILES  = $(wildcard $(SRC_DIR)/*)
SRC_FILES += $(wildcard $(TESTS_DIR)/additional/*)

all: $(COMPONENT)

$(DEPENDS) $(TDEPENDS):
	@$(MAKE) -s -C $(TOOLS_DIR)/$@

$(COMPONENT): $(DEPENDS) $(COMPONENT_TARGETS)
	@gprbuild $(BUILD_OPTS) -P$@

build_cov: $(DEPENDS) $(TDEPENDS) $(COV_TARGETS)
	@gprbuild $(BUILD_OPTS) -Ptest_$(COMPONENT) -XBUILD=coverage

$(OBJ_DIR)/.harness_stamp: $(SRC_FILES)
	@mkdir -p $(OBJ_DIR)/tests
	gnattest --tests-dir=$(TESTS_DIR) -Pgnattest_$(COMPONENT)
	@touch $@

build_tests: $(DEPENDS) $(TDEPENDS) $(TEST_TARGETS) $(OBJ_DIR)/.harness_stamp
	gprbuild $(BUILD_OPTS) -P$(GNATTEST_DRIVER) -XBUILD=tests

tests: build_tests
	$(GNATTEST_RUNNER)

build_gnatcov: $(DEPENDS) $(TDEPENDS) $(COV_TARGETS) $(OBJ_DIR)/.harness_stamp
	gprbuild $(BUILD_OPTS) -P$(GNATTEST_DRIVER) -XBUILD=tests \
		-cargs -ftest-coverage -fprofile-arcs \
		-largs -fprofile-generate

clean:
	@rm -rf bin obj $(ADDITIONAL_CLEAN)
