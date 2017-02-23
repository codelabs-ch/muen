include ../common_ada.mk
include ../cspecs.mk

install: $(INSTALL_TARGETS)

clean:
	@rm -rf $(OBJ_DIR) $(GEN_DIR) lib
