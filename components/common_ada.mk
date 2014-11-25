include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: $(COMPONENT)

$(COMPONENT):
	gprbuild $(BUILD_OPTS) -P$(COMPONENT)
