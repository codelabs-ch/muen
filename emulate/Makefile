all: .emulate

IMAGE ?= ../pack/obj/kernel
ISOFILE = bootable.iso
ISOPATH = $(CURDIR)/$(ISOFILE)

iso: $(ISOFILE)
	@echo "* Successfully created bootable Muen ISO '$(ISOPATH)'"
	@echo "  Either burn it on a CD-ROM or dump it on an USB stick:"
	@echo "    \$$ sudo cat $(ISOPATH) > /dev/sdX"
	@echo

$(ISOFILE): $(IMAGE) grub.cfg Makefile
	@grub-mkrescue -o $@ -graft-points \
		boot/grub/grub.cfg=grub.cfg \
		boot/kernel.bin=$(IMAGE)

.emulate: clean $(ISOFILE)
	@echo cont | bochs -q >emulate.out 2>&1 & echo "$$!" > $@

stop:
	@kill `cat .emulate 2>/dev/null` 2>/dev/null || true

clean: stop
	@rm -f $(ISOFILE) bochsout.txt *.out
	@rm -f .emulate

.PHONY: .emulate
