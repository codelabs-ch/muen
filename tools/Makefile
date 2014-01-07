all: skconfig skpacker skpolicy tools

skconfig:
	$(MAKE) -C config

skpacker:
	$(MAKE) -C packer

skpolicy:
	$(MAKE) -C policy

tools:
	$(MAKE) -C libmuxml
	$(MAKE) -C muptgen
	$(MAKE) -C muzpgen
	$(MAKE) -C mumsrbmgen

tests:
	$(MAKE) $@ -C config
	$(MAKE) $@ -C packer
	$(MAKE) $@ -C policy
	$(MAKE) $@ -C libmuxml
	$(MAKE) $@ -C muzpgen
	$(MAKE) $@ -C muptgen
	$(MAKE) $@ -C mumsrbmgen

clean:
	$(MAKE) $@ -C config
	$(MAKE) $@ -C packer
	$(MAKE) $@ -C policy
	$(MAKE) $@ -C libmulog
	$(MAKE) $@ -C libmuxml
	$(MAKE) $@ -C libtest
	$(MAKE) $@ -C muptgen
	$(MAKE) $@ -C muzpgen
