include ../Makeconf
include proofs.mk

# all known Ada components
COMPONENTS_ADA := \
	libmuchannel \
	libmudm \
	libmutime \
	libdebuglog \
	libmusinfo \
	libxhcidbg \
	dbgserver \
	idle \
	ps2_drv \
	vt

# all known SPARK components
COMPONENTS_SPARK := \
	crypter \
	dm \
	sl \
	sm \
	time

# all components, overridable by user
COMPONENTS ?= \
	$(COMPONENTS_ADA) \
	$(COMPONENTS_SPARK) \
	linux

COMPS_ADA   := $(filter $(COMPONENTS_ADA),$(COMPONENTS))
COMPS_SPARK := $(filter $(COMPONENTS_SPARK),$(COMPONENTS))

TESTS = libmutime libmusinfo

INST_TARGET = $(POLICY_OBJ_DIR)/$(@:install-%=%)

COMPONENTS_PREPARE = $(COMPONENTS:%=.prepare-%)
COMPONENTS_CHECK   = $(COMPONENTS:%=check-%)
COMPONENTS_INSTALL = $(COMPONENTS:%=install-%)
COMPONENTS_CLEAN   = $(COMPONENTS:%=clean-%)

SPARK_OPTS += $(PROOF_OPTS)
BUILD_OPTS += --RTS=$(TOP_DIR)/rts/obj
STACK_SIZE = $(COMPONENT_STACK_SIZE)

LOG = $(OBJ_DIR)/components.log

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: install
ifeq ($(NO_PROOF),)
all: $(OBJ_DIR)/gnatprove/gnatprove.out
endif

build_ada: $(OBJ_DIR)/build_ada.gpr $(COMPONENTS_PREPARE)
	@$(E) components "Build Ada" "gprbuild $(BUILD_OPTS) -P$< -Xstacksize=$(STACK_SIZE)" $(LOG)

build_spark: $(OBJ_DIR)/build_spark.gpr $(COMPONENTS_PREPARE) build_ada
	@$(E) components "Build SPARK (debug)" \
		"gprbuild $(BUILD_OPTS) -P$< -Xbuild=debug -Xstacksize=$(STACK_SIZE) $(PROOF_OPTS)" \
		$(LOG)
	@$(E) components "Build SPARK (release)" \
		"gprbuild $(BUILD_OPTS) -P$< -Xbuild=release -Xstacksize=$(STACK_SIZE) $(PROOF_OPTS)" \
		$(LOG)

# proofs (only once), use explicit proof target to force
$(OBJ_DIR)/gnatprove/gnatprove.out: $(COMPONENTS_PREPARE)
	@$(E) components Proof "gnatprove $(SPARK_OPTS) -P$(OBJ_DIR)/build_spark.gpr" $(LOG)

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

clean: $(COMPONENTS_CLEAN) clean-tau0
	rm -rf $(OBJ_DIR) .prepare-*
$(COMPONENTS_CLEAN) clean-tau0:
	$(MAKE) -C $(@:clean-%=%) clean

# forced re-proof
proof:
	rm -rf $(OBJ_DIR) .prepare-*
	$(MAKE) $(OBJ_DIR)/gnatprove/gnatprove.out

prepare: $(COMPONENTS_PREPARE)
$(COMPONENTS_PREPARE): $(POLICY_SRC)
	$(MAKE) -C $(@:.prepare-%=%) prepare
	@touch $@

$(OBJ_DIR)/build_ada.gpr:
	@echo 'aggregate project Build_Ada extends "library_ada.gpr" is'   > $@
	@echo '   for Project_Files use ('                                >> $@
	@$(foreach c,$(COMPS_ADA),echo -n '      "../$(c)/$(c).gpr"' >> $@; \
		if [ $(c) != $(lastword $(COMPS_ADA)) ]; then echo ','   >> $@; \
			else echo '' >> $@; fi;)
	@echo '   );'                                                     >> $@
	@echo 'end Build_Ada;'                                            >> $@

$(OBJ_DIR)/build_spark.gpr:
	@echo 'aggregate project Build_SPARK extends "component_spark.gpr" is' > $@
	@echo '   for Project_Files use ('                                    >> $@
	@$(foreach c,$(COMPS_SPARK),echo -n '      "../$(c)/$(c).gpr"'   >> $@; \
		if [ $(c) != $(lastword $(COMPS_SPARK)) ]; then echo ','     >> $@; \
			else echo '' >> $@; fi;)
	@echo '   );'                                                         >> $@
	@echo 'end Build_SPARK;'                                              >> $@

.PHONY: $(COMPONENTS) $(OBJ_DIR)/build_ada.gpr $(OBJ_DIR)/build_spark.gpr tau0
