include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

ifdef GENERATE_CSPECS
ALL = $(GEN_DIR)/.cspecs
endif

ALL += $(COMPONENT)

all: $(ALL)

$(COMPONENT):
	gprbuild $(BUILD_OPTS) -P$(COMPONENT)
