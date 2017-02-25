SYSTEM_CONFIG       ?= $(POLICY_OBJ_DIR)/system_config.xml
SYSTEM_POLICY_SRC   ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME).xml
SYSTEM_POLICY_WCOMP ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_with_comp.xml
SYSTEM_POLICY_A     ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_a.xml
SYSTEM_POLICY_B     ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_b.xml

SCHED_CONFIG ?= $(wildcard scheduling/$(SYSTEM_NAME).xml)

SCHED_PLAN_XML ?= $(POLICY_OBJ_DIR)/scheduling_plans.xml
SCHED_PLAN_PDF ?= $(POLICY_OBJ_DIR)/scheduling_plans.pdf
SCHED_PLAN_RAW ?= $(POLICY_OBJ_DIR)/scheduling_plans
SCHED_PLAN_DBG ?= $(POLICY_OBJ_DIR)/mugenschedcfg.log


OUTPUT   = $(OBJ_DIR)/skp.ads
LAST_CFG = $(shell cat $(OBJ_DIR)/.cfg 2>/dev/null)
CUR_CFG  = $(HARDWARE):$(ADDITIONAL_HW):$(PLATFORM):$(SYSTEM)

SOURCES  = $(wildcard xml/*.xml)
SOURCES += $(PLATFORM)
SOURCES += $(HARDWARE)
SOURCES += $(ADDITIONAL_HW)

GENERATORS =         \
	$(MUGENACPI)     \
	$(MUGENIOBM)     \
	$(MUGENMSRBM)    \
	$(MUGENMSRSTORE) \
	$(MUGENPT)       \
	$(MUGENSPEC)     \
	$(MUGENUKVM)     \
	$(MUGENVTD)      \
	$(MUGENZP)

GENERATOR_OPTS = -o $(OBJ_DIR) $(SYSTEM_POLICY_B)

include ../Makeconf

empty :=
comma := ,
space := $(empty) $(empty)

CSPECS_XML = $(wildcard $(POLICY_CSPEC_DIR)/*.xml)
CSPECS_CSV = $(subst $(space),$(comma),$(CSPECS_XML))
ifneq ($(strip $(CSPECS_XML)),)
JOIN = $(MUCFGCJOIN) -i '$<' -o '$@' -c $(CSPECS_CSV)
else
JOIN = cp '$<' '$@'
endif

DUMMY := $(shell mkdir -p $(OBJ_DIR))

ifneq ($(CUR_CFG),$(LAST_CFG))
	CFG_DEPS = cfgchange
endif

ifneq (,$(SCHED_CONFIG))
	SCHED_CPUS = $(shell xmllint --xpath 'string(/hardware/processor/@cpuCores)' $(HARDWARE))
	SCHED_GEN  = $(SCHED_PLAN_PDF)
endif

compile: .validated $(OUTPUT) $(POLICY_A) $(POLICY_B)

merge: $(POLICY_SRC)

cfgchange:
	rm -rf $(OBJ_DIR)/*
	echo $(CUR_CFG) > $(OBJ_DIR)/.cfg

$(SYSTEM_CONFIG): $(CFG_DEPS)
	@echo '<system>'                                                         > $@;
	@echo ' <config>'                                                       >> $@;
	@echo '  <string name="system"              value="$(SYSTEM)"/>'        >> $@;
	@echo '  <string name="hardware"            value="$(HARDWARE)"/>'      >> $@;
	@echo '  <string name="additional_hardware" value="$(ADDITIONAL_HW)"/>' >> $@;
	@echo '  <string name="platform"            value="$(PLATFORM)"/>'      >> $@;
	@echo ' </config>'                                                      >> $@;
	@echo '</system>'                                                       >> $@;

$(POLICY_SRC): $(SYSTEM_POLICY_SRC)
	ln -sf $< $@

$(POLICY_A): $(SYSTEM_POLICY_A)
	ln -sf $< $@

$(POLICY_B): $(SYSTEM_POLICY_B)
	ln -sf $< $@

$(SCHED_PLAN_PDF): $(SCHED_PLAN_XML)
	(cd $(POLICY_OBJ_DIR) && gnuplot -e "outfile='$@'" $(MUGENSCHEDCFG_DIR)/plot.gnuplot)

$(SCHED_PLAN_XML): $(SCHED_CONFIG) $(MUGENSCHEDCFG) $(HARDWARE)
	$(MUGENSCHEDCFG) -c $(SCHED_CPUS) -r $(SCHED_PLAN_RAW) -d $(SCHED_PLAN_DBG) $^

$(SYSTEM_POLICY_SRC): $(SYSTEM_CONFIG) $(SCHED_GEN) $(MUCFGMERGE) $(SOURCES)
	@$(MUCFGMERGE) $< $@ -I $(POLICY_OBJ_DIR)

$(SYSTEM_POLICY_WCOMP): $(SYSTEM_POLICY_SRC) $(MUCFGCJOIN) $(CSPECS_XML)
	$(JOIN)

$(SYSTEM_POLICY_A): $(SYSTEM_POLICY_WCOMP) $(MUCFGEXPAND)
	@$(MUCFGEXPAND) $< $@

$(SYSTEM_POLICY_B): $(SYSTEM_POLICY_A) $(MUCFGALLOC)
	@$(MUCFGALLOC) $< $@

.validated: $(SYSTEM_POLICY_B) $(MUCFGVALIDATE)
	@$(MUCFGVALIDATE) $<
	@touch $@

$(OUTPUT): $(GENERATORS) $(SYSTEM_POLICY_B)
	@for gen in $(GENERATORS); do $$gen $(GENERATOR_OPTS) || exit 1; done

clean:
	@rm -rf $(OBJ_DIR) .validated
