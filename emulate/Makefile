all: .emulate

bootable.iso: ../pack/obj/kernel grub.cfg Makefile
	grub-mkrescue -o $@ -graft-points \
		boot/grub/grub.cfg=grub.cfg \
		boot/kernel.bin=../pack/obj/kernel

.emulate: stop bootable.iso
	@bochs -q >/dev/null 2>&1 & echo "$$!" > $@

stop:
	@kill `cat .emulate 2>/dev/null` 2>/dev/null || true

clean: stop
	@rm -f bootable.iso bochsout.txt serial.out debugger.out
	@rm -f .emulate

.PHONY: .emulate
