all: tau0 dumper idle test

tau0:
	$(MAKE) -C $@

dumper:
	$(MAKE) -C $@

idle:
	$(MAKE) -C $@

test:
	$(MAKE) -C $@

clean:
	$(MAKE) clean -C tau0
	$(MAKE) clean -C dumper
	$(MAKE) clean -C idle
	$(MAKE) clean -C test

.PHONY: dumper tau0 idle test
