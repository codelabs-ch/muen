all: packer

subjects: config policy
	$(MAKE) -C $@

config:
	$(MAKE) -C $@

policy:
	$(MAKE) -C $@

kernel: policy
	$(MAKE) -C $@

packer: kernel subjects
	$(MAKE) -C $@

deploy: kernel subjects
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
