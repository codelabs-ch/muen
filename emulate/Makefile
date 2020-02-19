include ../Makeconf

all: .emulate

ISOFILE = muen.iso
ISOPATH = $(CURDIR)/$(ISOFILE)

IMAGE_BASE   ?= muen.img
IMAGE_MB     ?= ../pack/obj/$(IMAGE_BASE)
IMAGE_CSL    ?= ../pack/obj/$(IMAGE_BASE).cmds
IMAGE_MB_SIG ?= $(OBJ_DIR)/$(IMAGE_BASE).sig
IMAGE_SBS    ?= $(OBJ_DIR)/$(IMAGE_BASE).sbs

QEMU_BIN         ?= qemu-system-x86_64
QEMU_MEM         ?= 5120
QEMU_SSH_PORT    ?= 5555
QEMU_SERIAL_WAIT ?= 8

QEMU_NETDEV_OPTS := "user,id=net0,net=192.168.254.0/24,"
QEMU_NETDEV_OPTS := "$(QEMU_NETDEV_OPTS)dhcpstart=192.168.254.100,"
QEMU_NETDEV_OPTS := "$(QEMU_NETDEV_OPTS)hostfwd=tcp::$(QEMU_SSH_PORT)-:22"

ifneq ($(QEMU_NETDEV_EXTRA_OPTS),)
QEMU_NETDEV_OPTS := "$(QEMU_NETDEV_OPTS),$(QEMU_NETDEV_EXTRA_OPTS)"
endif

ifeq ($(MUEN_EFI),)
GRUB_MODS_PATH ?= $(CONTRIB_BUILD_DIR)/lib/grub/i386-pc
QEMU_OPTS = -curses
GRUB_CFG = grub.cfg
else
GRUB_MODS_PATH ?= $(CONTRIB_BUILD_DIR)/lib/grub/x86_64-efi
QEMU_OPTS = -bios OVMF.fd
GRUB_CFG = grub.cfg.efi
endif

ISO_FILES_SRC := \
	$(GRUB_CFG) \
	$(OBJ_DIR)/$(GRUB_CFG).sig \
	$(IMAGE_CSL) \
	$(IMAGE_SBS) \
	$(IMAGE_MB) \
	$(IMAGE_MB_SIG)
ISO_FILES := \
	boot/grub/grub.cfg=$(GRUB_CFG) \
	boot/grub/grub.cfg.sig=$(OBJ_DIR)/$(GRUB_CFG).sig \
	boot/kernel.csl=$(IMAGE_CSL) \
	boot/kernel.sbs=$(IMAGE_SBS) \
	boot/kernel.mb=$(IMAGE_MB) \
	boot/kernel.mb.sig=$(IMAGE_MB_SIG)

GRUB_MKRESCUE ?= $(CONTRIB_BUILD_DIR)/bin/grub-mkrescue
GRUB_MKRESCUE_PRELOAD_MODS ?= multiboot gcry_rsa gcry_sha512 configfile csl pgp sbs normal

QEMU_OPTS += \
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
	-netdev user,id=net1,net=192.168.253.0/24,dhcpstart=192.168.253.100

QEMU_CMD ?= $(QEMU_BIN) $(QEMU_OPTS) $(QEMU_EXTRA_OPTS)

iso: $(ISOFILE)
	@echo
	@echo "* Successfully created bootable Muen ISO '$(ISOPATH)'"
	@echo "  Either burn it on a CD-ROM or dump it on an USB stick:"
	@echo "    \$$ sudo dd bs=1M if=$(ISOPATH) of=/dev/sdX"
	@echo

$(OBJ_DIR):
	mkdir $@

$(IMAGE_SBS): $(IMAGE_CSL) | $(OBJ_DIR)
	@$(E) emulate "SBS create `basename $@`" \
		"$(SBS_CREATE) -k $(SBS_PRIVKEY) -i $< -o $@"
	@$(E) emulate "SBS inspect `basename $@`" \
		"$(SBS_INSPECT) -s $@"

$(IMAGE_MB_SIG): $(IMAGE_MB) | $(OBJ_DIR)
$(OBJ_DIR)/$(GRUB_CFG).sig: $(GRUB_CFG) | $(OBJ_DIR)
$(OBJ_DIR)/$(GRUB_CFG).sig $(IMAGE_MB_SIG):
	@rm -f $@
	@$(E) emulate "Creating signature `basename $@`" \
		"gpg --batch --digest-algo SHA512 -b --homedir $(SBS_PRIVKEY) -o $@ $<"

$(ISOFILE): $(ISO_FILES_SRC)
	@$(E) emulate "Generate ISO" \
		"$(GRUB_MKRESCUE) -d $(GRUB_MODS_PATH) --modules='$(GRUB_MKRESCUE_PRELOAD_MODS)' --pubkey keys/testkey.pub -o $@ $(ISO_FILES)"

.emulate: stop $(ISOFILE)
	@echo "Using command '$(QEMU_CMD)'" > emulate.out
	screen -c screenrc -L -dmS kvm-muen $(QEMU_CMD) -pidfile $@
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
	if [ -z "$$boot" ]; then echo "\nERROR executing '$(QEMU_CMD)', see emulate.out for details" && exit 1; fi

stop:
	kill `cat .emulate 2>/dev/null` 2>/dev/null || true
	rm -f .emulate

clean: stop
	rm -f $(ISOFILE) *.out
	rm -rf obj

.PHONY: .emulate
