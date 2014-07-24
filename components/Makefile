include ../Makeconf

SUBJECTS =  \
	linux   \
	sm      \
	tau0    \
	time    \
	vt

BINARIES = \
	$(subst linux,,$(SUBJECTS))

all: $(SUBJECTS) install

$(SUBJECTS):
	@$(MAKE) -C $@

install:
	@for bin in $(BINARIES); do \
		$(TO_RAW_CMD) $$bin/obj/$$bin $(POLICY_OBJ_DIR)/$$bin || exit 1; \
	done
	$(MAKE) -C linux $@

clean:
	@for subj in $(SUBJECTS); do make clean -C $$subj; done

.PHONY: $(SUBJECTS)
