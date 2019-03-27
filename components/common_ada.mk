include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

STACK_DEPS  = $(wildcard $(OBJ_DIR)/*.ci)
STACK_CHECK = $(OBJ_DIR)/.stackcheck_ok

all: $(COMPONENT) $(STACK_CHECK)

$(COMPONENT): $(COMPONENT_TARGETS)
	gprbuild $(BUILD_OPTS) -P$(COMPONENT) -Xstacksize=$(COMPONENT_STACK_SIZE)

$(STACK_CHECK): $(STACK_DEPS)
	$(MUCHECKSTACK) -P$(COMPONENT) -l$(COMPONENT_STACK_SIZE)
	@touch $@

prepare: $(COMPONENT_TARGETS)

.NOTPARALLEL:
