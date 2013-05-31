all: tau0 idle vt sm xv6 crypter

tau0:
	$(MAKE) -C $@

dumper:
	$(MAKE) -C $@

idle:
	$(MAKE) -C $@

vt:
	$(MAKE) -C $@

sm:
	$(MAKE) -C $@

xv6:
	$(MAKE) -C $@

crypter:
	$(MAKE) -C $@

clean:
	$(MAKE) clean -C tau0
	$(MAKE) clean -C idle
	$(MAKE) clean -C vt
	$(MAKE) clean -C sm
	$(MAKE) clean -C xv6
	$(MAKE) clean -C crypter

.PHONY: crypter dumper idle sm tau0 vt xv6
