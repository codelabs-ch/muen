IMAGE    = $(OBJ_DIR)/muen.img

include ../Makeconf

FILES := $(shell xmllint --xpath '//file/@filename' $(POLICY_B) | sed -rn 's/filename="([^"]+)"/\1/pg')
FILES := $(filter-out %_sinfo, $(FILES))
FILES := $(addprefix $(POLICY_OBJ_DIR)/, $(FILES))
DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: $(IMAGE)

$(POLICY_B_HASH): $(POLICY_B) $(MUCFGMEMHASHES) $(MUGENSINFO) $(FILES)
	@$(E) pack "Generate hashes" \
		"$(MUCFGMEMHASHES) -i $(POLICY_OBJ_DIR) $(POLICY_B) $(POLICY_B_HASH)"
	@$(E) pack "Generate sinfo" \
		"$(MUGENSINFO) -o $(POLICY_OBJ_DIR) $(POLICY_B_HASH)"

$(POLICY_B_CMDS): $(POLICY_B_HASH) $(MUGENTAU0CMDS)
	@$(E) pack "Generate tau0 command stream" "$(MUGENTAU0CMDS) $< $@"

$(IMAGE): $(KNL_FILE) $(POLICY_B_CMDS)
	@$(E) pack "Generate image (tau0)" \
		"$(TAU0_STATIC) -i $(POLICY_OBJ_DIR) -o $(OBJ_DIR) -n muen.img.tau0 $(POLICY_B_CMDS)"
	@$(E) pack "Generate image (packer)" \
		"$(MUPACK) -i $(POLICY_OBJ_DIR) -o $(OBJ_DIR) $(POLICY_B_HASH)"

clean:
	rm -rf $(OBJ_DIR)
