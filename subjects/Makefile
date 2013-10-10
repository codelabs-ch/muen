all: tau0 idle vt sm xv6 crypter linux

tau0 idle vt sm xv6 linux crypter:
	$(MAKE) -C $@

clean:
	$(MAKE) clean -C tau0
	$(MAKE) clean -C idle
	$(MAKE) clean -C vt
	$(MAKE) clean -C sm
	$(MAKE) clean -C xv6
	$(MAKE) clean -C linux
	$(MAKE) clean -C crypter

.PHONY: crypter idle sm tau0 vt xv6 linux
