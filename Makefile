all: kernel

subjects:
	$(MAKE) -C $@

config: subjects
	$(MAKE) -C $@

kernel: config
	$(MAKE) -C $@

deploy: kernel
	$(MAKE) -C $< $@

clean:
	$(MAKE) clean -C config
	$(MAKE) clean -C kernel
	$(MAKE) clean -C subjects

.PHONY: config kernel subjects
