all: .emulate

IMAGE ?= ../pack/obj/kernel

bootable.iso: $(IMAGE) grub.cfg Makefile
	grub-mkrescue -o $@ -graft-points \
		boot/grub/grub.cfg=grub.cfg \
		boot/kernel.bin=$(IMAGE)

.emulate: stop bootable.iso
	@echo cont | bochs -q >/dev/null 2>&1 & echo "$$!" > $@

stop:
	@kill `cat .emulate 2>/dev/null` 2>/dev/null || true

clean: stop
	@rm -f bootable.iso bochsout.txt serial.out debugger.out
	@rm -f .emulate

.PHONY: .emulate
