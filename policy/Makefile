SYSTEM_CONFIG       ?= $(POLICY_OBJ_DIR)/system_config.xml
SYSTEM_POLICY_SRC   ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME).xml
SYSTEM_POLICY_WCOMP ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_with_comp.xml
SYSTEM_POLICY_A     ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_a.xml
SYSTEM_POLICY_B     ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_b.xml

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

GEN_NAMES  = $(notdir $(GENERATORS))
GEN_STAMPS = $(GEN_NAMES:%=.gen-%)

GENERATOR_OPTS = -o $(OBJ_DIR) $(SYSTEM_POLICY_B)

include ../Makeconf

# Dot not care about scheduling if policy already contains scheduling node.
SCHED_PRESENT = $(shell xmllint --xpath 'string(/system/scheduling)' $(SYSTEM))

ifeq (,$(SCHED_PRESENT))

# Create this target for static/ and generated/ plans.
SCHED_PLAN_XML ?= $(POLICY_OBJ_DIR)/scheduling_plans.xml

# Per-platform static plans in scheduling/static take precedence over
# per-system static plans in the same directory.  If both static files are not
# found, it is assumed that a plan needs to be generated, for which a
# per-system Mugenschedcfg settings must be present. Fail if no such config is
# found.
SCHED_PLATFORM = $(notdir $(PLATFORM))
SCHED_CONFIG   = $(wildcard scheduling/static/$(SYSTEM_NAME)-$(SCHED_PLATFORM))
ifeq (,$(SCHED_CONFIG))
SCHED_CONFIG = $(wildcard scheduling/static/$(SYSTEM_NAME).xml)
endif
ifeq (,$(SCHED_CONFIG))
SCHED_CONFIG    = $(wildcard scheduling/generate/$(SYSTEM_NAME).xml)
SCHED_CPUS      = $(shell xmllint --xpath 'string(/hardware/processor/@cpuCores)' $(HARDWARE))
SCHED_PLAN_PDF ?= $(POLICY_OBJ_DIR)/scheduling_plans.pdf
SCHED_PLAN_RAW ?= $(POLICY_OBJ_DIR)/scheduling_plans
SCHED_PLAN_DBG ?= $(POLICY_OBJ_DIR)/mugenschedcfg.log
endif
ifeq (,$(SCHED_CONFIG))
$(error No scheduling configuration for $(SYSTEM_NAME) (platform: $(SCHED_PLATFORM)))
endif

endif # SCHED_PRESENT

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

ifneq ($(CUR_CFG),$(LAST_CFG))
	CFG_DEPS = cfgchange
endif


compile: .validated $(OUTPUT) $(POLICY_A) $(POLICY_B)

merge: $(POLICY_SRC)

$(OBJ_DIR):
	@mkdir -p $@

cfgchange: | $(OBJ_DIR)
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


ifneq (,$(SCHED_CPUS))
$(SCHED_PLAN_XML): $(SCHED_CONFIG) $(CFG_DEPS) $(MUGENSCHEDCFG) | $(OBJ_DIR)
	@$(E) policy "Generate scheduling plans" \
		"$(MUGENSCHEDCFG) -c $(SCHED_CPUS) -r $(SCHED_PLAN_RAW) -d $(SCHED_PLAN_DBG) $^"
else
$(SCHED_PLAN_XML): $(SCHED_CONFIG) $(CFG_DEPS) | $(OBJ_DIR)
	@$(E) policy "Copy $(SCHED_CONFIG)" "cp $< $@"
endif

$(SYSTEM_POLICY_SRC): $(SYSTEM_CONFIG) $(SCHED_PLAN_XML) $(MUCFGMERGE) $(SOURCES) | $(OBJ_DIR)
	@$(E) policy Merge "$(MUCFGMERGE) $< $@ -I $(POLICY_OBJ_DIR)"

$(SYSTEM_POLICY_WCOMP): $(SYSTEM_POLICY_SRC) $(MUCFGCJOIN) $(CSPECS_XML)
	@$(E) policy Join "$(JOIN)"

$(SYSTEM_POLICY_A): $(SYSTEM_POLICY_WCOMP) $(MUCFGEXPAND)
	@$(E) policy Expand "$(MUCFGEXPAND) $< $@"

$(SYSTEM_POLICY_B): $(SYSTEM_POLICY_A) $(MUCFGALLOC)
	@$(E) policy Alloc "$(MUCFGALLOC) $< $@"

.validated: $(SYSTEM_POLICY_B) $(MUCFGVALIDATE)
	@$(E) policy Validate "$(MUCFGVALIDATE) $<"
	@touch $@

$(OUTPUT): $(GEN_STAMPS)
$(GEN_STAMPS): $(GENERATORS) $(SYSTEM_POLICY_B) .validated
	@$(E) policy "Execute $(@:.gen-%=%)" \
		"$(TOP_DIR)/tools/$(@:.gen-%=%)/bin/$(@:.gen-%=%) $(GENERATOR_OPTS)"
	@touch $@

clean:
	@rm -rf $(OBJ_DIR) .validated .gen-*
