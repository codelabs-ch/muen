include Makeconf

export HARDWARE
export SYSTEM

ifeq (,$(NO_CONTRIB))
CONTRIB = contrib
endif

all: pack

contrib rts:
	$(MAKE) -C $@

policy: tools
	$(MAKE) -C $@

components: policy rts $(CONTRIB)
	$(MAKE) -C $@

kernel: policy rts
	$(MAKE) -C $@

tau0: policy rts
	$(MAKE) -C components install-$@

pack: policy kernel tau0 components
	$(MAKE) -C $@

tools: $(CONTRIB)
	$(MAKE) -C $@

tools_install:
	$(MAKE) -C tools install PREFIX=$(PREFIX)

deploy: HARDWARE=$(POLICY_DIR)/platform/lenovo-t430s.xml
deploy: SYSTEM=$(POLICY_DIR)/xml/demo_system_vtd.xml
deploy: pack
	$(MAKE) -C $@

emulate: pack
	$(MAKE) -C $@

iso: pack
	$(MAKE) -C emulate $@

tests:
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
	rm -rf contrib/obj

distclean: clean
	$(MAKE) clean -C contrib

.PHONY: components contrib deploy emulate kernel pack policy rts tools
