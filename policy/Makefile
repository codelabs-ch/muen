OUTPUT   = $(OBJ_DIR)/skp.ads
LAST_CFG = $(shell cat $(OBJ_DIR)/.cfg 2>/dev/null)

PLATFORM = platform/$(HARDWARE).xml

SOURCES  = $(wildcard xml/*.xml)
SOURCES += $(PLATFORM)

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
	$(if $(findstring $(HARDWARE)/$(SYSTEM),$(LAST_CFG)),,rm -f $(OBJ_DIR)/*; \
		echo $(HARDWARE)/$(SYSTEM) > $(OBJ_DIR)/.cfg)

$(POLICY_SRC): $(MUCFGMERGE) $(SOURCES)
	@$(MUCFGMERGE) -w$(PLATFORM) $(POLICY) $(POLICY_SRC)

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
