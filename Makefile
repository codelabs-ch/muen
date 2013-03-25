all: packer

subjects:
	$(MAKE) -C $@

config: subjects
	$(MAKE) -C $@

policy:
	$(MAKE) -C $@

kernel: config policy
	$(MAKE) -C $@

packer: kernel
	$(MAKE) -C $@

deploy: packer
	$(MAKE) -C $< $@

tests:
	$(MAKE) tests -C policy

clean:
	$(MAKE) clean -C config
	$(MAKE) clean -C kernel
	$(MAKE) clean -C packer
	$(MAKE) clean -C policy
	$(MAKE) clean -C subjects

.PHONY: config kernel packer policy subjects
