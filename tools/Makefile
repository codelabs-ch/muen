all: skconfig skpacker skpolicy

skconfig:
	$(MAKE) -C config

skpacker:
	$(MAKE) -C packer

skpolicy:
	$(MAKE) -C policy

tests:
	$(MAKE) tests -C config
	$(MAKE) tests -C packer
	$(MAKE) tests -C policy

clean:
	$(MAKE) clean -C config
	$(MAKE) clean -C packer
	$(MAKE) clean -C policy
