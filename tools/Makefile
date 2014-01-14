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
	$(MAKE) -C muiobmgen
	$(MAKE) -C muacpigen
	$(MAKE) -C mupack
	$(MAKE) -C muspecgen

tests:
	$(MAKE) $@ -C config
	$(MAKE) $@ -C packer
	$(MAKE) $@ -C policy
	$(MAKE) $@ -C libmuxml
	$(MAKE) $@ -C libmutools
	$(MAKE) $@ -C muzpgen
	$(MAKE) $@ -C muptgen
	$(MAKE) $@ -C mumsrbmgen
	$(MAKE) $@ -C muiobmgen
	$(MAKE) $@ -C muacpigen
	$(MAKE) $@ -C mupack
	$(MAKE) $@ -C muspecgen

clean:
	$(MAKE) $@ -C config
	$(MAKE) $@ -C packer
	$(MAKE) $@ -C policy
	$(MAKE) $@ -C libmulog
	$(MAKE) $@ -C libmuxml
	$(MAKE) $@ -C libmutools
	$(MAKE) $@ -C libtest
	$(MAKE) $@ -C muzpgen
	$(MAKE) $@ -C muptgen
	$(MAKE) $@ -C mumsrbmgen
	$(MAKE) $@ -C muiobmgen
	$(MAKE) $@ -C muacpigen
	$(MAKE) $@ -C mupack
	$(MAKE) $@ -C muspecgen
