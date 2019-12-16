IMG_NAME_PACK = muen.img.pack
IMG_PACK      = $(OBJ_DIR)/$(IMG_NAME_PACK)
IMG_NAME_TAU0 = muen.img
IMG_TAU0      = $(OBJ_DIR)/$(IMG_NAME_TAU0)

include ../Makeconf

ifneq (,$(wildcard $(POLICY_B)))
FILES := $(shell xmllint --xpath '//file/@filename' $(POLICY_B) | sed -rn 's/filename="([^"]+)"/\1/pg')
FILES := $(filter-out %_sinfo, $(FILES))
FILES := $(addprefix $(POLICY_OBJ_DIR)/, $(FILES))
endif

all: $(OBJ_DIR)/.diffok

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

$(POLICY_B_HASH): $(POLICY_B) $(MUCFGMEMHASHES) $(MUGENSINFO) $(FILES) | $(OBJ_DIR)
	@$(E) pack "Generate hashes" \
		"$(MUCFGMEMHASHES) -i $(POLICY_OBJ_DIR) $(POLICY_B) $(POLICY_B_HASH)"
	@$(E) pack "Generate sinfo" \
		"$(MUGENSINFO) -o $(POLICY_OBJ_DIR) $(POLICY_B_HASH)"

$(POLICY_B_CMDS): $(POLICY_B_HASH) $(MUGENTAU0CMDS)
	@$(E) pack "Generate tau0 command stream" "$(MUGENTAU0CMDS) $< $@"

$(OBJ_DIR)/.diffok: $(IMG_PACK) $(IMG_TAU0)
	@$(E) diff "Diffing images" "diff $(IMG_PACK) $(IMG_TAU0)"
	@touch $@

$(IMG_PACK): $(POLICY_B_HASH) $(MUPACK)
	@$(E) pack "Generate image (packer)" \
		"$(MUPACK) -i $(POLICY_OBJ_DIR) -o $(OBJ_DIR) -n $(IMG_NAME_PACK) $(POLICY_B_HASH)"

$(IMG_TAU0): $(POLICY_B_CMDS) $(TAU0_STATIC)
	@$(E) pack "Generate image (tau0)" \
		"$(TAU0_STATIC) -i $(POLICY_OBJ_DIR) -o $(OBJ_DIR) -n $(IMG_NAME_TAU0) $(POLICY_B_CMDS)"

clean:
	rm -rf $(OBJ_DIR)
