PROF_OPTS = --tool=callgrind --callgrind-out-file=$(OBJ_DIR)/callgrind.out
BUILD_OPTS += "--compiler-subst=Ada,afl-gcc"

FUZZ_DEPS  ?= fuzz_prepare
FUZZ_ARGS  ?= @@
FUZZ_INPUT ?= $(wildcard data/*.xml)

fuzz: export AFL_SKIP_CPUFREQ=1
fuzz: $(COMPONENT) $(FUZZ_DEPS)
	afl-fuzz -t 1000 -m 1024 $(FUZZ_OPTS) -i $(OBJ_DIR)/in \
		-o $(OBJ_DIR)/out bin/$(COMPONENT)_fuzz $(FUZZ_ARGS)

fuzz_prepare:
	mkdir -p $(OBJ_DIR)/in
	mkdir -p $(OBJ_DIR)/out
	cp $(FUZZ_INPUT) $(OBJ_DIR)/in/
