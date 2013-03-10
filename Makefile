all: packer

subjects:
	$(MAKE) -C $@

config: subjects
	$(MAKE) -C $@

kernel: config
	$(MAKE) -C $@

packer: kernel
	$(MAKE) -C $@

deploy: packer
	$(MAKE) -C $< $@

clean:
	$(MAKE) clean -C config
	$(MAKE) clean -C kernel
	$(MAKE) clean -C packer
	$(MAKE) clean -C subjects

.PHONY: config kernel packer subjects
