IMG_NAME = muen.img
IMG_TAU0 = $(OBJ_DIR)/$(IMG_NAME).cmds

include ../Makeconf

ifneq (,$(wildcard $(POLICY_B)))
FILES := $(shell xmllint --xpath '//file/@filename' $(POLICY_B) | sed -rn 's/filename="([^"]+)"/\1/pg')
FILES := $(filter-out %_sinfo, $(FILES))
FILES := $(addprefix $(POLICY_OBJ_DIR)/, $(FILES))
endif

all: $(IMG_TAU0) $(POLICY_B_MANI)

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

$(POLICY_B_HASH): $(POLICY_B) $(MUCFGMEMHASHES) $(MUGENSINFO) $(FILES) | $(OBJ_DIR)
	@$(E) pack "Generate hashes" \
		"$(MUCFGMEMHASHES) -i $(POLICY_OBJ_DIR) $(POLICY_B) $(POLICY_B_HASH)"
	@$(E) pack "Generate sinfo" \
		"$(MUGENSINFO) -o $(POLICY_OBJ_DIR) $(POLICY_B_HASH)"

$(POLICY_B_CMDS): $(POLICY_B_HASH) $(MUGENTAU0CMDS)
	@$(E) pack "Generate tau0 command stream" "$(MUGENTAU0CMDS) $< $@"

$(IMG_TAU0): $(POLICY_B_CMDS) $(TAU0_STATIC)
	@$(E) pack "Generate image" \
		"$(TAU0_STATIC) -i $(POLICY_OBJ_DIR) -o $(OBJ_DIR) -n $(IMG_NAME) $(POLICY_B_CMDS)"

$(POLICY_B_MANI): $(POLICY_B_HASH) $(MUGENMANIFEST)
	@$(E) pack "Generate policy manifest" "$(MUGENMANIFEST) $< $@"

clean:
	rm -rf $(OBJ_DIR)
