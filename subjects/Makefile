all: tau0 dumper idle test vt sm

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

clean:
	$(MAKE) clean -C tau0
	$(MAKE) clean -C dumper
	$(MAKE) clean -C idle
	$(MAKE) clean -C test
	$(MAKE) clean -C vt
	$(MAKE) clean -C sm

.PHONY: dumper tau0 idle test vt sm
