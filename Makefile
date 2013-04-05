all: packer

subjects: config
	$(MAKE) -C $@

config:
	$(MAKE) -C $@

policy: subjects
	$(MAKE) -C $@

kernel: policy
	$(MAKE) -C $@

packer: kernel
	$(MAKE) -C $@

deploy: kernel
	$(MAKE) -C packer $@

emulate: packer
	$(MAKE) -C $@

tests:
	$(MAKE) tests -C config
	$(MAKE) tests -C packer
	$(MAKE) tests -C policy

clean:
	$(MAKE) clean -C config
	$(MAKE) clean -C kernel
	$(MAKE) clean -C packer
	$(MAKE) clean -C policy
	$(MAKE) clean -C subjects

.PHONY: config emulate kernel packer policy subjects
