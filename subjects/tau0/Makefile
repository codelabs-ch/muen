PACKAGE = tau0

include ../../Makeconf
include ../../Makespark

all: $(ALL) $(OBJ_DIR)/$(PACKAGE).xml

$(OBJ_DIR)/$(PACKAGE):
	gprbuild $(BUILD_OPTS) -P$(PACKAGE)

$(OBJ_DIR)/$(PACKAGE).xml: $(OBJ_DIR)/$(PACKAGE)
	$(SKCONFIG) $<

clean:
	rm -rf $(OBJ_DIR)
