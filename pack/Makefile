KNL_NAME = kernel
KNL_FILE = $(TOP_DIR)/kernel/obj/$(KNL_NAME)
IMAGE    = $(OBJ_DIR)/muen.img

include ../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: $(IMAGE)

$(IMAGE): $(KNL_FILE)
	cp $^ $@
	$(MUPACK) -i $(POLICY_DIR) -o $(OBJ_DIR) $(POLICY_DIR)/system.xml

clean:
	@rm -rf $(OBJ_DIR)

.PHONY: $(IMAGE)
