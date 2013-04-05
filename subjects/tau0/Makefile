PACKAGE = tau0

include ../../Makeconf
include ../../Makespark

all: $(ALL) specs

$(OBJ_DIR)/$(PACKAGE):
	gprbuild $(BUILD_OPTS) -P$(PACKAGE)

specs: $(OBJ_DIR)/$(PACKAGE)
	$(SKCONFIG) $< 16#216000#

clean:
	rm -rf $(OBJ_DIR)
