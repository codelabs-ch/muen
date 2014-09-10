include ../Makeconf

COMPONENTS = \
	linux    \
	sm       \
	tau0     \
	time     \
	vt

BINARIES = \
	$(subst linux,,$(COMPONENTS))

all: $(COMPONENTS) install

$(COMPONENTS):
	@$(MAKE) -C $@

install:
	@for bin in $(BINARIES); do \
		$(TO_RAW_CMD) $$bin/obj/$$bin $(POLICY_OBJ_DIR)/$$bin || exit 1; \
	done
	$(MAKE) -C linux $@

clean:
	@for subj in $(COMPONENTS); do make clean -C $$subj; done

.PHONY: $(COMPONENTS)
