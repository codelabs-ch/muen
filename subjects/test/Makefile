include ../../Makeconf

BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj

all: subjects

subjects:
	gprbuild $(BUILD_OPTS) -P$@

specs: subjects
	$(SKCONFIG) $(OBJ_DIR)/subject_a 16#236000#
	$(SKCONFIG) $(OBJ_DIR)/subject_b 16#246000#
	$(SKCONFIG) $(OBJ_DIR)/subject_c 16#256000#

clean:
	rm -rf obj
