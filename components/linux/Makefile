include ../../Makeconf

CONFIG ?= 4.10
CC     ?= /usr/bin/gcc
HOSTCC ?= $(CC)

INITRAMFS_FN := initramfs.cpio.xz
INITRAMFS    ?= $(OBJ_DIR)/$(INITRAMFS_FN)
SITE         ?= https://codelabs-ch.github.io/buildroot-muen

OBJDIR := $(shell mkdir -p $(OBJ_DIR))

all: $(OBJ_DIR)/.built

$(OBJ_DIR)/.built: src/.config $(INITRAMFS)
	make CC="$(CC)" LOCALVERSION= HOSTCC="$(HOSTCC)" oldconfig -C src
	make CC="$(CC)" LOCALVERSION= HOSTCC="$(HOSTCC)" -j$(NUM_CPUS) -C src
	@touch $@

src/.config: config/linux64-$(CONFIG)
	cp $< $@

$(INITRAMFS):
	@wget -c $(SITE)/$(INITRAMFS_FN) -O $@.tmp
	@mv $@.tmp $@

install: $(OBJ_DIR)/.built spec/linux.xml
	$(MULNXBZPATCH) src/arch/x86/boot/bzImage $(POLICY_OBJ_DIR)/bzImage
	cp $(INITRAMFS) $(POLICY_OBJ_DIR)/$(INITRAMFS_FN)
	cp spec/linux.xml $(POLICY_CSPEC_DIR)/

clean:
	@rm -rf $(OBJ_DIR)
	@make clean -C src
