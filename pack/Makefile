KNL_NAME = kernel
KNL_FILE = $(TOP_DIR)/kernel/obj/$(KNL_NAME)
IMAGE    = $(OBJ_DIR)/muen.img

include ../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: $(IMAGE)

$(POLICY_B_HASH): $(POLICY_B) $(MUCFGMEMHASHES) $(MUGENSINFO)
	$(MUCFGMEMHASHES) -i $(POLICY_OBJ_DIR) $(POLICY_B) $(POLICY_B_HASH)
	$(MUGENSINFO) -o $(POLICY_OBJ_DIR) $(POLICY_B_HASH)

$(IMAGE): $(KNL_FILE) $(POLICY_B_HASH)
	$(MUPACK) -i $(POLICY_OBJ_DIR) -o $(OBJ_DIR) $(POLICY_B_HASH)

clean:
	@rm -rf $(OBJ_DIR)

.PHONY: $(IMAGE)
