include ../Makeconf

all: .emulate

ISOFILE = muen.iso
ISOPATH = $(CURDIR)/$(ISOFILE)

IMAGE     ?= ../pack/obj/muen.img
IMAGE_CSL ?= ../pack/obj/muen.img.cmds

GRUB_MKRESCUE ?= $(CONTRIB_BUILD_DIR)/bin/grub-mkrescue
GRUB_MODS     ?= $(CONTRIB_BUILD_DIR)/lib/grub/i386-pc

QEMU             ?= qemu-system-x86_64
QEMU_MEM         ?= 5120
QEMU_SSH_PORT    ?= 5555
QEMU_SERIAL_WAIT ?= 8

QEMU_NETDEV_OPTS := "user,id=net0,net=192.168.254.0/24,"
QEMU_NETDEV_OPTS := "$(QEMU_NETDEV_OPTS)dhcpstart=192.168.254.100,"
QEMU_NETDEV_OPTS := "$(QEMU_NETDEV_OPTS)hostfwd=tcp::$(QEMU_SSH_PORT)-:22"

ifneq ($(QEMU_NETDEV_EXTRA_OPTS),)
	QEMU_NETDEV_OPTS := "$(QEMU_NETDEV_OPTS),$(QEMU_NETDEV_EXTRA_OPTS)"
endif

QEMU_OPTS = \
	-drive file=$(ISOFILE),index=0,media=disk,format=raw \
	-serial file:serial.out \
	-machine q35,accel=kvm,kernel-irqchip=split \
	-cpu host,+invtsc \
	-m $(QEMU_MEM) \
	-smp cores=2,threads=2,sockets=1 \
	-device intel-iommu,intremap=on,device-iotlb=on \
	-device ioh3420,id=pcie.1,chassis=1 \
	-device virtio-net-pci,bus=pcie.1,netdev=net0,disable-legacy=on,disable-modern=off,iommu_platform=on,ats=on \
	-netdev $(QEMU_NETDEV_OPTS) \
	-device qemu-xhci,id=xhci \
	-device usb-tablet,bus=xhci.0 \
	-device rtl8139,netdev=net1 \
	-netdev user,id=net1,net=192.168.253.0/24,dhcpstart=192.168.253.100 \
	-curses

iso: $(ISOFILE)
	@echo
	@echo "* Successfully created bootable Muen ISO '$(ISOPATH)'"
	@echo "  Either burn it on a CD-ROM or dump it on an USB stick:"
	@echo "    \$$ sudo dd bs=1M if=$(ISOPATH) of=/dev/sdX"
	@echo

$(ISOFILE): clean $(IMAGE) $(IMAGE_CSL) grub.cfg Makefile
	@$(E) emulate "Generate ISO" \
		"$(GRUB_MKRESCUE) -d $(GRUB_MODS) -o $@ boot/grub/grub.cfg=grub.cfg boot/kernel.bin=$(IMAGE_CSL) boot/kernel.bin.mb=$(IMAGE)"

.emulate: $(ISOFILE)
	@echo "Using command '$(QEMU) $(QEMU_OPTS) $(QEMU_EXTRA_OPTS)'" > emulate.out
	screen -c screenrc -L -dmS kvm-muen $(QEMU) $(QEMU_OPTS) $(QEMU_EXTRA_OPTS) -pidfile $@
	@echo
	@echo -n "* QEMU/KVM started for '$(ISOFILE)', waiting for boot: "
	@for i in $$(seq 1 $(QEMU_SERIAL_WAIT)); do \
		sleep 1; \
		echo -n .; \
		boot=$$(cat serial.out 2>/dev/null | grep 'Booting Muen'); \
		if [ -n "$$boot" ]; then \
			echo " OK"; echo; \
			echo "    SSH: ssh -p $(QEMU_SSH_PORT) root@localhost"; \
			echo "         (password: muen)"; \
			echo "Console: screen -r kvm-muen"; \
			echo "         (C-A k to quit, C-A d to detach)"; \
			break; \
		fi; \
	done; \
	if [ -z "$$boot" ]; then echo "\nERROR:" && cat emulate.out && exit 1; fi

stop:
	kill `cat .emulate 2>/dev/null` 2>/dev/null || true

clean: stop
	rm -f $(ISOFILE) *.out
	rm -rf .emulate obj

.PHONY: .emulate
