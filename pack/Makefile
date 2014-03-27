KNL_NAME = kernel
KNL_FILE = $(TOP_DIR)/kernel/obj/$(KNL_NAME)
IMAGE    = $(OBJ_DIR)/muen.img

include ../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: $(IMAGE)

$(IMAGE): $(KNL_FILE)
	$(MUPACK) -i $(POLICY_DIR) -o $(OBJ_DIR) $(POLICY_DIR)/demo_system_b.xml

clean:
	@rm -rf $(OBJ_DIR)

.PHONY: $(IMAGE)
