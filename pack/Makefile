KERNEL = $(TOP_DIR)/kernel/obj/kernel

include ../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: $(OBJ_DIR)/kernel

$(OBJ_DIR)/kernel: $(KERNEL)
	$(SKPACKER) $(KERNEL) $@

clean:
	@rm -rf $(OBJ_DIR)
