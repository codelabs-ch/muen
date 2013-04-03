all: tau0 dumper test

tau0:
	$(MAKE) -C $@

dumper:
	$(MAKE) -C $@

test:
	$(MAKE) -C $@

clean:
	$(MAKE) clean -C tau0
	$(MAKE) clean -C dumper
	$(MAKE) clean -C test

.PHONY: dumper tau0 test
