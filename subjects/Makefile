all: tau0 dumper idle test vt sm xv6

tau0:
	$(MAKE) -C $@

dumper:
	$(MAKE) -C $@

idle:
	$(MAKE) -C $@

test:
	$(MAKE) -C $@

vt:
	$(MAKE) -C $@

sm:
	$(MAKE) -C $@

xv6:
	$(MAKE) -C $@

clean:
	$(MAKE) clean -C tau0
	$(MAKE) clean -C dumper
	$(MAKE) clean -C idle
	$(MAKE) clean -C test
	$(MAKE) clean -C vt
	$(MAKE) clean -C sm
	$(MAKE) clean -C xv6

.PHONY: dumper tau0 idle test vt sm xv6
