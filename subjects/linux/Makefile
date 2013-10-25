include ../../Makeconf

CONFIG = 3.11
CC     = /usr/bin/gcc
HOSTCC = $(CC)

all: .built

.built: src/.config
	@make CC=$(CC) HOSTCC=$(HOSTCC) -C initramfs
	@make CC=$(CC) HOSTCC=$(HOSTCC) oldconfig -C src
	@make CC=$(CC) HOSTCC=$(HOSTCC) -j$(NUM_CPUS) -C src
	@touch $@

src/.config: config/linux-$(CONFIG)
	@cp $< $@

clean:
	@make clean -C initramfs
	@make clean -C src
	@rm -f .built
