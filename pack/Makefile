KNL_NAME = kernel
KNL_FILE = $(TOP_DIR)/kernel/obj/$(KNL_NAME)
IMAGE    = $(OBJ_DIR)/kernel

include ../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: $(IMAGE)

$(IMAGE): $(KNL_FILE)
	cp $^ $@
	$(MUPACK) -k $(KNL_NAME) -i $(POLICY_INC) -o $(OBJ_DIR) $(POLICY_INC)/system.xml

clean:
	@rm -rf $(OBJ_DIR)

.PHONY: $(IMAGE)
