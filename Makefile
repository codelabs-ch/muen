all: kernel

kernel:
	$(MAKE) -C $@

.PHONY: kernel
