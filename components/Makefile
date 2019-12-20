include ../Makeconf
include proofs.mk

# SPARK components to be built
COMPONENTS_SPARK := \
	libmuchannel \
	libmudm \
	libmutime \
	libdebuglog \
	libmusinfo \
	libxhcidbg \
	dbgserver \
	dm \
	example \
	idle \
	ps2_drv \
	sl \
	sm \
	time \
	vt

# components to proof
COMPONENTS_PROOF := \
	dm \
	example \
	sl \
	sm \
	time

# all components incl. Linux, overridable by user
COMPONENTS ?= \
	$(COMPONENTS_SPARK) \
	linux

COMPS_BUILD_SPARK    := $(filter $(COMPONENTS_SPARK),$(COMPONENTS))
COMPS_BUILD_NONSPARK := $(filter-out $(COMPONENTS_SPARK),$(COMPONENTS))
COMPS_PROOF          := $(filter $(COMPONENTS_PROOF),$(COMPONENTS))

TESTS = libmutime libmusinfo

INST_TARGET = $(POLICY_OBJ_DIR)/$(@:install-%=%)

COMPONENTS_PREPARE = $(COMPONENTS:%=.prepare-%)
COMPONENTS_BUILD   = $(COMPS_BUILD_NONSPARK:%=build-%)
COMPONENTS_CHECK   = $(COMPONENTS:%=check-%)
COMPONENTS_INSTALL = $(COMPONENTS:%=install-%)
COMPONENTS_CLEAN   = $(COMPONENTS:%=clean-%)

SPARK_OPTS += $(PROOF_OPTS)
BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj
STACK_SIZE = $(COMPONENT_STACK_SIZE)

LOG = $(OBJ_DIR)/components.log

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: install
ifneq ($(COMPS_PROOF),)
ifeq ($(NO_PROOF),)
all: $(OBJ_DIR)/gnatprove/gnatprove.out
endif
endif

$(OBJ_DIR)/build.gpr:
	@echo 'aggregate project Build extends "component_spark.gpr" is'       > $@
	@echo '   for Project_Files use ('                                    >> $@
	@$(foreach c,$(COMPS_BUILD_SPARK),echo -n '      "../$(c)/$(c).gpr"'  >> $@; \
		if [ $(c) != $(lastword $(COMPS_BUILD_SPARK)) ]; then echo ','    >> $@; \
			else echo '' >> $@; fi;)
	@echo '   );'                                                         >> $@
	@echo 'end Build;'                                                    >> $@

.PHONY: $(OBJ_DIR)/build.gpr

tau0_static:
	$(MAKE) -C tau0-static

build: build_spark $(COMPONENTS_BUILD)
$(COMPONENTS_BUILD): $(COMPONENTS_PREPARE)
	$(MAKE) -C $(@:build-%=%) build

build_spark: $(OBJ_DIR)/build.gpr $(COMPONENTS_PREPARE)
	@$(E) components "Build SPARK (debug)" \
		"gprbuild $(BUILD_OPTS) -P$< -Xbuild=debug -Xstacksize=$(STACK_SIZE) $(PROOF_OPTS)" \
		$(LOG)
	@$(E) components "Build SPARK (release)" \
		"gprbuild $(BUILD_OPTS) -P$< -Xbuild=release -Xstacksize=$(STACK_SIZE) $(PROOF_OPTS)" \
		$(LOG)

$(OBJ_DIR)/proof.gpr:
	@echo 'aggregate project Proof extends "component_spark.gpr" is' > $@
	@echo '   for Project_Files use ('                              >> $@
	@$(foreach c,$(COMPS_PROOF),echo -n '      "../$(c)/$(c).gpr"'  >> $@; \
		if [ $(c) != $(lastword $(COMPS_PROOF)) ]; then echo ','    >> $@; \
			else echo '' >> $@; fi;)
	@echo '   );'                                                   >> $@
	@echo 'end Proof;'                                              >> $@

$(OBJ_DIR)/gnatprove/gnatprove.out: $(OBJ_DIR)/proof.gpr $(COMPONENTS_PREPARE)
	@$(E) components Proof "gnatprove $(SPARK_OPTS) -P$<" $(LOG)

# build components and then execute all defined checks (stackcheck etc).
check: $(COMPONENTS_CHECK)
$(COMPONENTS_CHECK): build
	$(MAKE) -C $(@:check-%=%) check

install: $(COMPONENTS_INSTALL)
$(COMPONENTS_INSTALL): $(COMPONENTS_CHECK)
	$(MAKE) -C $(@:install-%=%) $(INST_TARGET)

install-tau0:
	$(MAKE) -C $(@:install-%=%) $(INST_TARGET)

tests:
	for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

clean: $(COMPONENTS_CLEAN) clean-tau0 clean-tau0-static
	rm -rf $(OBJ_DIR) .prepare-*
$(COMPONENTS_CLEAN) clean-tau0 clean-tau0-static:
	$(MAKE) -C $(@:clean-%=%) clean

# forced re-proof
proof:
	rm -rf $(OBJ_DIR) .prepare-*
	$(MAKE) $(OBJ_DIR)/gnatprove/gnatprove.out

prepare: $(COMPONENTS_PREPARE)
$(COMPONENTS_PREPARE): $(POLICY_SRC)
	$(MAKE) -C $(@:.prepare-%=%) prepare
	@touch $@

.PHONY: $(COMPONENTS) tau0
