include Makeconf

export HARDWARE
export SYSTEM

ifeq (,$(NO_CONTRIB))
CONTRIB = contrib
endif

all: pack

contrib rts:
	$(MAKE) -C $@

policy-merge: tools
	$(MAKE) -C policy merge

policy-compile: tools components
	$(MAKE) -C policy compile

components: policy-merge rts $(CONTRIB)
	$(MAKE) -C $@

kernel: policy-compile rts
	$(MAKE) -C $@

tau0: policy-compile rts
	$(MAKE) -C components install-$@

pack: kernel tau0 components
	$(MAKE) -C $@

tools: $(CONTRIB)
	$(MAKE) -C $@

tools_install:
	$(MAKE) -C tools install PREFIX=$(PREFIX)

deploy: HARDWARE=hardware/lenovo-t430s.xml
deploy: SYSTEM=xml/demo_system_vtd.xml
deploy: pack
	$(MAKE) -C $@

emulate: pack
	$(MAKE) -C $@

iso: HARDWARE=hardware/lenovo-t430s.xml
iso: SYSTEM=xml/demo_system_vtd.xml
iso: pack
	$(MAKE) -C emulate $@

tests: $(CONTRIB)
	$(MAKE) -C tools $@
	$(MAKE) -C components $@

clean:
	$(MAKE) clean -C deploy
	$(MAKE) clean -C tools
	$(MAKE) clean -C kernel
	$(MAKE) clean -C pack
	$(MAKE) clean -C policy
	$(MAKE) clean -C components
	$(MAKE) clean -C rts
	$(MAKE) clean -C emulate
	rm -rf contrib/obj

distclean: clean
	$(MAKE) clean -C contrib

.PHONY: components contrib deploy emulate kernel pack rts tools
