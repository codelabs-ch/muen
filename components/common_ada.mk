include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

STACK_DEPS  = $(wildcard $(OBJ_DIR)/*.ci)

all: stackcheck

$(COMPONENT): $(COMPONENT_TARGETS)
	@$(E) $(COMPONENT) Build \
		"gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xstacksize=$(COMPONENT_STACK_SIZE)"

stackcheck: $(COMPONENT) $(STACK_DEPS)
	@$(E) $(COMPONENT) "Check stack" \
		"$(MUCHECKSTACK) -P$(COMPONENT) -l$(COMPONENT_STACK_SIZE)"

prepare: $(COMPONENT_TARGETS)

check: stackcheck
