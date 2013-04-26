all: tau0 dumper idle test vt

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

clean:
	$(MAKE) clean -C tau0
	$(MAKE) clean -C dumper
	$(MAKE) clean -C idle
	$(MAKE) clean -C test
	$(MAKE) clean -C vt

.PHONY: dumper tau0 idle test vt
