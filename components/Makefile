include ../Makeconf
include proofs.mk

COMPONENTS ?=    \
	libmuchannel \
	libmudm      \
	libmutime    \
	libdebuglog  \
	libmusinfo   \
	libxhcidbg   \
	crypter      \
	dbgserver    \
	dm           \
	idle         \
	linux        \
	ps2_drv      \
	sl           \
	sm           \
	time         \
	vt

TESTS = libmutime libmusinfo

INST_TARGET = $(POLICY_OBJ_DIR)/$(@:install-%=%)

COMPONENTS_PREPARE = $(COMPONENTS:%=prepare-%)
COMPONENTS_CHECK   = $(COMPONENTS:%=check-%)
COMPONENTS_INSTALL = $(COMPONENTS:%=install-%)
COMPONENTS_CLEAN   = $(COMPONENTS:%=clean-%)

SPARK_OPTS += $(PROOF_OPTS)
BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj
STACK_SIZE = $(COMPONENT_STACK_SIZE)

LOG = $(OBJ_DIR)/components.log

all: gnatprove/gnatprove.out install

build_spark: build_ada $(COMPONENTS_PREPARE)
	@$(E) components "Build SPARK (debug)" \
		"gprbuild $(BUILD_OPTS) -Pbuild_spark -Xbuild=debug -Xstacksize=$(STACK_SIZE) $(PROOF_OPTS)" \
		$(LOG)
	@$(E) components "Build SPARK (release)" \
		"gprbuild $(BUILD_OPTS) -Pbuild_spark -Xbuild=release -Xstacksize=$(STACK_SIZE) $(PROOF_OPTS)" \
		$(LOG)

# proofs (only once), use explicit proof target to force
gnatprove/gnatprove.out: $(COMPONENTS_PREPARE)
	@$(E) components Proof "gnatprove $(SPARK_OPTS) -Pbuild_spark" $(LOG)

build_ada: $(COMPONENTS_PREPARE)
	@$(E) components "Build Ada" "gprbuild $(BUILD_OPTS) -Pbuild_ada -Xstacksize=$(STACK_SIZE)" $(LOG)

# build SPARK/Ada targets and then execute all defined checks
# (stackcheck etc).
check: $(COMPONENTS_CHECK)
$(COMPONENTS_CHECK): build_spark
	$(MAKE) -C $(@:check-%=%) check

install: $(COMPONENTS_INSTALL)
$(COMPONENTS_INSTALL): $(COMPONENTS_CHECK)
	$(MAKE) -C $(@:install-%=%) $(INST_TARGET)

install-tau0:
	$(MAKE) -C $(@:install-%=%) $(INST_TARGET)

tests:
	for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

clean: $(COMPONENTS_CLEAN) clean-tau0 proof-clean
	rm -rf $(OBJ_DIR)
$(COMPONENTS_CLEAN) clean-tau0:
	$(MAKE) -C $(@:clean-%=%) clean

# forced re-proof
proof: proof-clean gnatprove/gnatprove.out
proof-clean:
	rm -rf gnatprove

prepare: $(COMPONENTS_PREPARE)
$(COMPONENTS_PREPARE): $(POLICY_SRC)
	$(MAKE) -C $(@:prepare-%=%) prepare

.PHONY: $(COMPONENTS) tau0
