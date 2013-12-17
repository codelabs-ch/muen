include Makeconf

export HARDWARE

ifeq (,$(filter $(MAKECMDGOALS),clean distclean))
CONTRIB := $(shell $(MAKE) -C contrib)
endif

all: pack

skconfig:
	$(MAKE) $@ -C tools

skpacker: policy
	$(MAKE) $@ -C tools

skpolicy:
	$(MAKE) $@ -C tools

rts:
	$(MAKE) -C $@

policy: skpolicy
	$(MAKE) -C $@

subjects: skconfig policy rts
	$(MAKE) -C $@

kernel: policy rts
	$(MAKE) -C $@

pack: skpacker kernel subjects
	$(MAKE) -C $@

deploy: HARDWARE=t430s
deploy: pack
	$(MAKE) -C $@

emulate: pack
	$(MAKE) -C $@

clean:
	$(MAKE) clean -C deploy
	$(MAKE) clean -C tools
	$(MAKE) clean -C kernel
	$(MAKE) clean -C pack
	$(MAKE) clean -C policy
	$(MAKE) clean -C subjects
	$(MAKE) clean -C rts

distclean: clean
	$(MAKE) clean -C contrib

.PHONY: deploy emulate kernel pack policy rts subjects
