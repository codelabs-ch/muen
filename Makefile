include Makeconf

export HARDWARE
export SYSTEM

ifeq (,$(NO_CONTRIB))
CONTRIB = contrib
endif

all:		## Default target: pack
all: pack

contrib rts:
	$(MAKE) -C $@

policy-merge:	## Merge constituent parts into single policy XML file
policy-merge: tools
	$(MAKE) -C policy merge

policy-compile:	## Compile and validate system policy
policy-compile: tools components
	$(MAKE) -C policy compile

components:	## Build all components
components: policy-merge rts $(CONTRIB)
	$(MAKE) -C $@

kernel:		## Compile policy and build Muen Kernel
kernel: policy-compile rts
	$(MAKE) -C $@

tau0: policy-compile rts
	$(MAKE) -C components install-$@

tau0_static:	## Build system resource manager Tau0
	$(MAKE) -C components $@

pack:		## Build and create Muen system image (Multiboot)
pack: kernel tau0 tau0_static components
	$(MAKE) -C $@

tools:		## Build Muen toolchain
tools: $(CONTRIB)
	$(MAKE) -C $@

tools_install:
	$(MAKE) -C tools install PREFIX=$(PREFIX)

deploy:		## Build and deploy Muen system to hardware target
deploy: HARDWARE=hardware/lenovo-t430s.xml
deploy: iso
	$(MAKE) -C $@

emulate:	## Build and run Muen system in KVM
emulate: pack
	$(MAKE) -C $@

iso:		## Build and create Muen system image (ISO)
iso: HARDWARE=hardware/lenovo-t430s.xml
iso: pack
	$(MAKE) -C emulate $@

tests:		## Run unit tests for tools and components
tests: $(CONTRIB)
	$(MAKE) -C tools $@
	$(MAKE) -C components $@

clean:		## Clean up everything except contrib sources
	$(MAKE) clean -C deploy
	$(MAKE) clean -C tools
	$(MAKE) clean -C kernel
	$(MAKE) clean -C pack
	$(MAKE) clean -C policy
	$(MAKE) clean -C components
	$(MAKE) clean -C rts
	$(MAKE) clean -C emulate
	rm -rf contrib/obj

distclean:	## Clean up everything including contrib sources
distclean: clean
	$(MAKE) clean -C contrib

help:
	@grep -h "##" $(MAKEFILE_LIST) | grep -v grep | sed -e 's/##//'

.PHONY: components contrib deploy emulate help kernel pack rts tools
