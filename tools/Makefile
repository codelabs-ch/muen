all: skconfig skpacker skpolicy mutools

skconfig:
	$(MAKE) -C config

skpacker:
	$(MAKE) -C packer

skpolicy:
	$(MAKE) -C policy

mutools:
	@gprbuild -p -Pprojects/$@

tests:
	$(MAKE) $@ -C config
	$(MAKE) $@ -C packer
	$(MAKE) $@ -C policy

clean:
	$(MAKE) $@ -C config
	$(MAKE) $@ -C packer
	$(MAKE) $@ -C policy
	@rm -rf bin lib obj
