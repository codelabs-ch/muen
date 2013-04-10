all: .emulate

bootable.iso: ../pack/obj/kernel
	cp $< iso/boot/kernel.bin
	grub-mkrescue -o $@ iso/

.emulate: stop bootable.iso
	@bochs -q >/dev/null 2>&1 & echo "$$!" > $@

stop:
	@kill `cat .emulate 2>/dev/null` 2>/dev/null || true

clean: stop
	@rm -f bootable.iso bochsout.txt serial.out debugger.out
	@rm -f iso/boot/kernel.bin .emulate

.PHONY: .emulate
