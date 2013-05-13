include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: subjects

subjects:
	gprbuild $(BUILD_OPTS) -P$@

specs: subjects
	$(SKCONFIG) $(OBJ_DIR)/subject_a 16#25a000#
	$(SKCONFIG) $(OBJ_DIR)/subject_b 16#27a000#

clean:
	rm -rf obj
