include ../common_ada.mk

install:
	gprinstall -p -f --RTS=$(TOP_DIR)/rts/obj -P$(COMPONENT) --prefix=$(TOP_DIR)/components/libraries/

clean:
	@rm -rf $(OBJ_DIR) lib
