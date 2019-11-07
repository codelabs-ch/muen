include ../projects/exec.mk

all: .emulate

ISOFILE = muen.iso
ISOPATH = $(CURDIR)/$(ISOFILE)

IMAGE ?= ../pack/obj/muen.img

QEMU          ?= qemu-system-x86_64
QEMU_MEM      ?= 5120
QEMU_SSH_PORT ?= 5555

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
	-netdev user,id=net0,net=192.168.254.0/24,dhcpstart=192.168.254.100,hostfwd=tcp::$(QEMU_SSH_PORT)-:22 \
	-device qemu-xhci,id=xhci \
	-device usb-tablet,bus=xhci.0 \
	-curses

iso: $(ISOFILE)
	@echo
	@echo "* Successfully created bootable Muen ISO '$(ISOPATH)'"
	@echo "  Either burn it on a CD-ROM or dump it on an USB stick:"
	@echo "    \$$ sudo dd bs=1M if=$(ISOPATH) of=/dev/sdX"
	@echo

$(ISOFILE): clean $(IMAGE) grub.cfg Makefile
	@$(E) emulate "Generate ISO" \
		"grub-mkrescue -o $@ boot/grub/grub.cfg=grub.cfg boot/kernel.bin=$(IMAGE)"

.emulate: $(ISOFILE)
	screen -c screenrc -L -dmS kvm-muen $(QEMU) $(QEMU_OPTS) $(QEMU_EXTRA_OPTS) -pidfile $@
	@echo
	@echo -n "* QEMU/KVM started for '$(ISOFILE)', waiting for boot: "
	@for i in $$(seq 1 5); do \
		sleep 1; \
		echo -n .; \
		boot=$$(cat serial.out 2>/dev/null | grep 'Booting Muen'); \
		if [ -n "$$boot" ]; then \
			echo " OK"; echo; \
			echo "    SSH: ssh -p $(QEMU_SSH_PORT) root@localhost"; \
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
