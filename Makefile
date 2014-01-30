include Makeconf

export HARDWARE

ifeq (,$(filter $(MAKECMDGOALS),clean distclean))
CONTRIB := $(shell $(MAKE) -C contrib)
endif

all: pack

rts:
	$(MAKE) -C $@

policy: tools
	$(MAKE) -C $@

subjects: policy rts
	$(MAKE) -C $@

kernel: policy rts
	$(MAKE) -C $@

pack: policy kernel subjects
	$(MAKE) -C $@

tools:
	$(MAKE) -C $@

deploy: HARDWARE=lenovo-t430s
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
	rm -rf contrib/obj

distclean: clean
	$(MAKE) clean -C contrib

.PHONY: deploy emulate kernel pack policy rts subjects tools
