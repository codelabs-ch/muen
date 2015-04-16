include ../../Makeconf

CONFIG ?= 3.18
CC     ?= /usr/bin/gcc
HOSTCC ?= $(CC)

all: .built

.built: src/.config
	make -C initramfs
	make CC="$(CC)" LOCALVERSION= HOSTCC="$(HOSTCC)" oldconfig -C src
	make CC="$(CC)" LOCALVERSION= HOSTCC="$(HOSTCC)" -j$(NUM_CPUS) -C src
	@touch $@

src/.config: config/linux64-$(CONFIG)
	@cp $< $@

install: .built
	@$(MULNXBZPATCH) src/arch/x86/boot/bzImage $(POLICY_OBJ_DIR)/bzImage
	@cp initramfs/obj/initramfs.cpio.gz $(POLICY_OBJ_DIR)
	@cp initramfs/obj/initramfs_integ.cpio.gz $(POLICY_OBJ_DIR)
	@cp initramfs/obj/initramfs_integ_kt.cpio.gz $(POLICY_OBJ_DIR)

clean:
	@make clean -C initramfs
	@make clean -C src
	@rm -f .built
