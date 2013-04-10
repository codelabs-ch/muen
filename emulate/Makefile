all: start_bochs

bootable.iso: ../pack/obj/kernel
	cp $< iso/boot/kernel.bin
	grub-mkrescue -o $@ iso/

start_bochs: bootable.iso
	@bochs -q >/dev/null 2>&1 &

clean:
	@rm -f bootable.iso bochsout.txt serial.out debugger.out
	@rm -f iso/boot/kernel.bin
