PROF_OPTS = --tool=callgrind --callgrind-out-file=$(OBJ_DIR)/callgrind.out

prof: $(COMPONENT)
	valgrind $(PROF_OPTS) bin/$(COMPONENT) $(PROFILE_ARGS)
