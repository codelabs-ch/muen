SYSTEM_CONFIG            ?= $(POLICY_OBJ_DIR)/system_config.xml
SYSTEM_POLICY_SRC        ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME).xml
SYSTEM_POLICY_WCOMP      ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_with_comp.xml
SYSTEM_POLICY_VA_ALLOC   ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_with_va.xml
SYSTEM_POLICY_VA_ALLOC_F ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_with_va_filtered.xml
SYSTEM_POLICY_A          ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_a.xml
SYSTEM_POLICY_B          ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_b.xml
SYSTEM_POLICY_B_EXT      ?= $(POLICY_OBJ_DIR)/$(SYSTEM_NAME)_b_ext.xml

OUTPUT   = $(OBJ_DIR)/skp.ads
LAST_CFG = $(shell cat $(OBJ_DIR)/.cfg 2>/dev/null)
CUR_CFG  = $(HARDWARE):$(ADDITIONAL_HW):$(PLATFORM):$(SYSTEM)

SOURCES  = $(wildcard xml/*.xml)
SOURCES += $(SYSTEM)
SOURCES += $(PLATFORM)
SOURCES += $(HARDWARE)
SOURCES += $(ADDITIONAL_HW)

GENERATORS =         \
	$(MUGENACPI)     \
	$(MUGENMSRSTORE) \
	$(MUGENSPEC)     \
	$(MUGENSOLO5)    \
	$(MUGENZP)

GEN_NAMES  = $(notdir $(GENERATORS))
GEN_STAMPS = $(GEN_NAMES:%=.gen-%)

GENERATOR_OPTS = -o $(OBJ_DIR) $(SYSTEM_POLICY_B)

include ../Makeconf

# Include system policy Makefile if it exists to enable creation of system
# policy prior to merge. The snippet is expected to have the same name as the
# system policy but with the file extension 'mk' instead of 'xml'.
SYSTEM_MK = $(patsubst %.xml,%.mk,$(SYSTEM))
ifneq (,$(wildcard $(SYSTEM_MK)))
include $(SYSTEM_MK)
CLEAN_DEPS += clean-$(SYSTEM)
endif

# Do not check scheduling when cleaning.
ifneq ($(MAKECMDGOALS),clean)
# Only check for scheduling section if system policy file is present.
ifneq (,$(wildcard $(SYSTEM)))
# Do not care about scheduling if policy already contains scheduling node.
SCHED_PRESENT = $(shell xmllint --xpath 'string(/system/scheduling)' $(SYSTEM))
else
# The SYSTEM_SCHED_PRESENT variable is set if the system policy does not yet
# exist but the created one already has a plan defined.
SCHED_PRESENT = $(SYSTEM_SCHED_PRESENT)
endif

ifeq (,$(SCHED_PRESENT))

# Create this target for static/ and generated/ plans.
SCHED_PLAN_XML ?= $(POLICY_OBJ_DIR)/scheduling_plans.xml

# Per-platform static plans in scheduling/static take precedence over
# per-system static plans in the same directory. Fail if no such config is
# found.
SCHED_PLATFORM = $(notdir $(PLATFORM))
SCHED_CONFIG   = $(wildcard scheduling/static/$(SYSTEM_NAME)-$(SCHED_PLATFORM))
ifeq (,$(SCHED_CONFIG))
SCHED_CONFIG = $(wildcard scheduling/static/$(SYSTEM_NAME).xml)
endif
ifeq (,$(SCHED_CONFIG))
$(error No scheduling configuration for $(SYSTEM_NAME) (platform: $(SCHED_PLATFORM)))
endif

endif # SCHED_PRESENT
endif # Not 'clean' target

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

ifneq (,$(LAST_CFG))
ifneq ($(CUR_CFG),$(LAST_CFG))
	CFG_DEPS = cfgchange
endif
endif


compile: .validated $(OUTPUT) $(POLICY_A) $(POLICY_B)
compile-ext_b: $(SYSTEM_POLICY_B_EXT)

merge: $(POLICY_SRC)

$(OBJ_DIR):
	@mkdir -p $@

cfgchange:
	@$(E) policy "New config: $(CUR_CFG)" "rm -rf $(OBJ_DIR)/*"

$(OBJ_DIR)/.cfg: $(CFG_DEPS) | $(OBJ_DIR)
	echo $(CUR_CFG) > $(OBJ_DIR)/.cfg

$(SYSTEM_CONFIG): $(OBJ_DIR)/.cfg
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

$(SCHED_PLAN_XML): $(SCHED_CONFIG) $(CFG_DEPS) | $(OBJ_DIR)
	@$(E) policy "Copy $(SCHED_CONFIG)" "cp $< $@"

$(SYSTEM_POLICY_SRC): $(SYSTEM_CONFIG) $(SCHED_PLAN_XML) $(MUCFGMERGE) $(SOURCES) | $(OBJ_DIR)
	@$(E) policy Merge "$(MUCFGMERGE) $< $@ -I $(POLICY_OBJ_DIR)"

$(SYSTEM_POLICY_WCOMP): $(SYSTEM_POLICY_SRC) $(MUCFGCJOIN) $(CSPECS_XML)
	@$(E) policy Join "$(JOIN)"

$(SYSTEM_POLICY_VA_ALLOC): $(SYSTEM_POLICY_WCOMP) $(MUCFGVRESALLOC)
	@$(E) policy "Allocate virtual memory" "$(MUCFGVRESALLOC) $< $@"

$(SYSTEM_POLICY_VA_ALLOC_F): $(SYSTEM_POLICY_VA_ALLOC) $(MUXMLFILTER)
	@$(E) policy "Filter to core schema" "$(MUXMLFILTER) -isn Format_Src_Ext -osn Format_Src $< $@"

ifneq (,$(MUEN_UCODE_DIR))
$(SYSTEM_POLICY_A): $(MUCFGUCODE)
endif
$(SYSTEM_POLICY_A): $(SYSTEM_POLICY_VA_ALLOC_F) $(MUCFGEXPAND)
ifneq (,$(MUEN_UCODE_DIR))
	@$(E) policy "Apply Intel MCU" "PATH=$(PATH):$(MUEN_IUCODE_TOOL_PATH) \
		$(MUCFGUCODE) -i $< -u $(MUEN_UCODE_DIR) -o $(POLICY_OBJ_DIR)"
endif
	@$(E) policy Expand "$(MUCFGEXPAND) $< $@"

$(SYSTEM_POLICY_B): $(SYSTEM_POLICY_A) $(MUCFGALLOC)
	@$(E) policy "Allocate physical memory" "$(MUCFGALLOC) $< $@"

$(SYSTEM_POLICY_B_EXT): $(SYSTEM_POLICY_B) $(DOC_PLUGIN_POLICY_B_EXTENDER) $(SYSTEM_POLICY_VA_ALLOC)
	@$(E) policy "Extend policy b with documentation" \
		"$(DOC_PLUGIN_POLICY_B_EXTENDER) --pj $(SYSTEM_POLICY_VA_ALLOC) --pb $(SYSTEM_POLICY_B) --o $@"

.validated: $(SYSTEM_POLICY_B) $(MUCFGVALIDATE)
	@$(E) policy Validate "$(MUCFGVALIDATE) $<"
	@touch $@

$(OUTPUT): $(GEN_STAMPS)
$(GEN_STAMPS): $(GENERATORS) $(SYSTEM_POLICY_B) .validated
	@$(E) policy "Execute $(@:.gen-%=%)" \
		"$(TOP_DIR)/tools/$(@:.gen-%=%)/bin/$(@:.gen-%=%) $(GENERATOR_OPTS)"
	@touch $@

clean-$(SYSTEM):
	@rm -f $(SYSTEM)

clean: $(CLEAN_DEPS)
	@rm -rf $(OBJ_DIR) .validated .gen-*
