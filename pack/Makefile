IMAGE      = $(OBJ_DIR)/muen.img
IMAGE_TAU0 = $(OBJ_DIR)/muen.img.tau0

include ../Makeconf

ifneq (,$(wildcard $(POLICY_B)))
IOMMU := $(shell xmllint --xpath 'string(/system/config/boolean[@name="iommu_enabled"]/@value)' $(POLICY_B))
FILES := $(shell xmllint --xpath '//file/@filename' $(POLICY_B) | sed -rn 's/filename="([^"]+)"/\1/pg')
FILES := $(filter-out %_sinfo, $(FILES))
FILES := $(addprefix $(POLICY_OBJ_DIR)/, $(FILES))
endif

DUMMY := $(shell mkdir -p $(OBJ_DIR))

ifeq ($(IOMMU),true)
all: $(IMAGE).diffok
else
all: $(IMAGE)
endif

$(POLICY_B_HASH): $(POLICY_B) $(MUCFGMEMHASHES) $(MUGENSINFO) $(FILES)
	@$(E) pack "Generate hashes" \
		"$(MUCFGMEMHASHES) -i $(POLICY_OBJ_DIR) $(POLICY_B) $(POLICY_B_HASH)"
	@$(E) pack "Generate sinfo" \
		"$(MUGENSINFO) -o $(POLICY_OBJ_DIR) $(POLICY_B_HASH)"

$(POLICY_B_CMDS): $(POLICY_B_HASH) $(MUGENTAU0CMDS)
	@$(E) pack "Generate tau0 command stream" "$(MUGENTAU0CMDS) $< $@"

$(IMAGE).diffok: $(IMAGE) $(IMAGE_TAU0)
	@$(E) diff "Diffing images" "diff $(IMAGE) $(IMAGE_TAU0)"
	@touch $@

$(IMAGE): $(POLICY_B_HASH)
	@$(E) pack "Generate image (packer)" \
		"$(MUPACK) -i $(POLICY_OBJ_DIR) -o $(OBJ_DIR) $(POLICY_B_HASH)"

$(IMAGE_TAU0): $(POLICY_B_CMDS)
	@$(E) pack "Generate image (tau0)" \
		"$(TAU0_STATIC) -i $(POLICY_OBJ_DIR) -o $(OBJ_DIR) -n muen.img.tau0 $(POLICY_B_CMDS)"

clean:
	rm -rf $(OBJ_DIR)
