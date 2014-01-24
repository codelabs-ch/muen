PACKAGE = tau0

include ../../Makeconf
include ../../Makespark

all: $(ALL)

$(OBJ_DIR)/$(PACKAGE):
	gprbuild $(BUILD_OPTS) -P$(PACKAGE)

clean:
	rm -rf $(OBJ_DIR)
