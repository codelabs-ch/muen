all: skconfig skpacker

skconfig:
	$(MAKE) -C config

skpacker:
	$(MAKE) -C packer

tests:
	$(MAKE) tests -C config
	$(MAKE) tests -C packer

clean:
	$(MAKE) clean -C config
	$(MAKE) clean -C packer
