all: kernel

subjects:
	$(MAKE) -C $@

kernel: subjects
	$(MAKE) -C $@

clean:
	$(MAKE) clean -C kernel
	$(MAKE) clean -C subjects

.PHONY: kernel subjects
