PROF_OPTS = --tool=callgrind --callgrind-out-file=$(OBJ_DIR)/callgrind.out
BUILD_OPTS += --compiler-subst=Ada,afl-gcc --subdirs=$(SUB_DIR)

COV_BUILD_OPTS = -cargs -O0 -fprofile-arcs -ftest-coverage
COV_BUILD_OPTS += -largs -fprofile-arcs -ftest-coverage

FUZZ_DEPS  ?= fuzz_prepare
FUZZ_ARGS  ?= @@
FUZZ_INPUT ?= $(wildcard data/*.xml)

LCOV_EXCLUDE_PATTERN = '*adainclude*'

fuzz: SUB_DIR=fuzz
fuzz: export AFL_SKIP_CPUFREQ=1
fuzz: $(COMPONENT) $(FUZZ_DEPS)
	afl-fuzz -t 1000 -m 1024 $(FUZZ_OPTS) -i $(OBJ_DIR)/in \
		-o $(OBJ_DIR)/out bin/fuzz/$(COMPONENT)_fuzz $(FUZZ_ARGS)

fuzz_prepare:
	mkdir -p $(OBJ_DIR)/in
	mkdir -p $(OBJ_DIR)/out
	cp $(FUZZ_INPUT) $(OBJ_DIR)/in/

fuzz_cov: SUB_DIR=fuzz_cov
fuzz_cov: EXTRA_BUILD_OPTS=$(COV_BUILD_OPTS)
fuzz_cov: export AFL_BENCH_JUST_ONE :=1
fuzz_cov: export AFL_FAST_CAL := 1
fuzz_cov: $(COMPONENT)
	test -d $(OBJ_DIR)/out || $(MAKE) fuzz
	afl-cov -d $(OBJ_DIR)/out --overwrite --lcov-exclude-pattern "$(LCOV_EXCLUDE_PATTERN)" \
		--coverage-cmd "bin/$(SUB_DIR)/$(COMPONENT)_fuzz AFL_FILE" \
		--code-dir $(OBJ_DIR)/$(SUB_DIR)/cov/
	@unset AFL_BENCH_JUST_ONE
	@unset AFL_FAST_CAL
