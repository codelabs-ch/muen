all: skconfig skpacker skpolicy tools

skconfig:
	$(MAKE) -C config

skpacker:
	$(MAKE) -C packer

skpolicy:
	$(MAKE) -C policy

tools:
	$(MAKE) -C muptgen
	$(MAKE) -C muzpgen

tests:
	$(MAKE) $@ -C config
	$(MAKE) $@ -C packer
	$(MAKE) $@ -C policy
	$(MAKE) $@ -C libmuxml

clean:
	$(MAKE) $@ -C config
	$(MAKE) $@ -C packer
	$(MAKE) $@ -C policy
	$(MAKE) $@ -C libmulog
	$(MAKE) $@ -C muptgen
	$(MAKE) $@ -C muzpgen
