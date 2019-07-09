include ../projects/exec.mk

all: .emulate

IMAGE        ?= ../pack/obj/muen.img
BOCHS_CONFIG ?= bochsrc

ISOFILE = muen.iso
ISOPATH = $(CURDIR)/$(ISOFILE)

iso: $(ISOFILE)
	@echo
	@echo "* Successfully created bootable Muen ISO '$(ISOPATH)'"
	@echo "  Either burn it on a CD-ROM or dump it on an USB stick:"
	@echo "    \$$ sudo dd bs=1M if=$(ISOPATH) of=/dev/sdX"
	@echo

$(ISOFILE): $(IMAGE) grub.cfg Makefile
	@$(E) emulate "Generate ISO" \
		"grub-mkrescue -o $@ boot/grub/grub.cfg=grub.cfg boot/kernel.bin=$(IMAGE)"

.emulate: clean $(ISOFILE)
	@echo cont | bochs -q -f $(BOCHS_CONFIG) >emulate.out 2>&1 & echo "$$!" > $@

stop:
	kill `cat .emulate 2>/dev/null` 2>/dev/null || true

clean: stop
	rm -f $(ISOFILE) bochsout.txt *.out
	rm -rf .emulate obj

.PHONY: .emulate
