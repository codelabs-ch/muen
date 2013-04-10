all: packer

tools:
	$(MAKE) -C $@

policy:
	$(MAKE) -C $@

subjects: policy tools
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
	$(MAKE) tests -C packer
	$(MAKE) tests -C policy

clean:
	$(MAKE) clean -C tools
	$(MAKE) clean -C kernel
	$(MAKE) clean -C packer
	$(MAKE) clean -C policy
	$(MAKE) clean -C subjects

.PHONY: emulate kernel packer policy subjects tools
