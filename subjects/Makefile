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
		cp $$bin/obj/$$bin $(POLICY_INC) || exit 1; \
	done
	@cp linux/src/arch/x86/boot/bzImage $(POLICY_INC)

clean:
	@for subj in $(SUBJECTS); do make clean -C $$subj; done

.PHONY: $(SUBJECTS)
