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
	$(MUGENSINFO)    \
	$(MUGENSPEC)     \
	$(MUGENVTD)      \
	$(MUGENZP)

GENERATOR_OPTS = -o $(OBJ_DIR) $(POLICY_B)

include ../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: cfgchange .validated $(OUTPUT)

cfgchange:
	$(if $(findstring $(CUR_CFG),$(LAST_CFG)),,rm -f $(OBJ_DIR)/*; \
		echo $(CUR_CFG) > $(OBJ_DIR)/.cfg)

$(POLICY_SRC): $(MUCFGMERGE) $(SOURCES)
	@$(MUCFGMERGE) -a$(ADDITIONAL_HW) -w$(HARDWARE) -p$(PLATFORM) $(SYSTEM) $@

$(POLICY_A): $(POLICY_SRC) $(MUCFGEXPAND)
	@$(MUCFGEXPAND) $< $@

$(POLICY_B): $(POLICY_A) $(MUCFGALLOC)
	@$(MUCFGALLOC) $< $@

.validated: $(POLICY_B) $(MUCFGVALIDATE)
	@$(MUCFGVALIDATE) $<
	@touch $@

$(OUTPUT): $(GENERATORS) $(POLICY_B)
	@for gen in $(GENERATORS); do $$gen $(GENERATOR_OPTS) || exit 1; done

clean:
	@rm -rf $(OBJ_DIR) .validated
