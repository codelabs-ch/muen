include ../../Makeconf

CONFIG = 3.11
CC     = /usr/bin/gcc
HOSTCC = $(CC)

all: .built

.built: src/.config
	@make CC=$(CC) HOSTCC=$(HOSTCC) -C initramfs
	@make CC=$(CC) HOSTCC=$(HOSTCC) oldconfig -C src
	@make CC=$(CC) HOSTCC=$(HOSTCC) -j$(NUM_CPUS) -C src
	@make -C acpi
	@touch $@

src/.config: config/linux-$(CONFIG)
	@cp $< $@

clean:
	@make clean -C initramfs
	@make clean -C src
	@make clean -C acpi
	@rm -f .built
