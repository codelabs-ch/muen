COMPONENT = rts

include ../Makeconf

$(OBJ_DIR)/adalib/libgnat.a: $(OBJ_DIR)/adainclude/*.ad[sb]
	gprbuild --RTS=./obj $(BUILD_OPTS) -P$(COMPONENT)

$(OBJ_DIR)/adainclude/*.ad[sb]: src/*.ad[sb]
	@mkdir -p $(OBJ_DIR)/adainclude $(OBJ_DIR)/adalib
	cp src/* $(OBJ_DIR)/adainclude

clean:
	@rm -rf $(OBJ_DIR)
