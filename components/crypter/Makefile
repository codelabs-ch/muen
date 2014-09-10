PACKAGE = crypter

include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: $(OBJ_DIR)/$(PACKAGE)

$(OBJ_DIR)/$(PACKAGE):
	gprbuild $(BUILD_OPTS) -P$(PACKAGE)

clean:
	rm -rf $(OBJ_DIR)
