include ../../Makeconf

CONFIG ?= 4.14
CC     ?= /usr/bin/gcc
HOSTCC ?= $(CC)

INITRAMFS_FN := initramfs.cpio.xz
INITRAMFS    ?= $(OBJ_DIR)/$(INITRAMFS_FN)
SITE         ?= https://codelabs-ch.github.io/buildroot-muen

CSPEC_XML     = spec/linux.xml
CSPEC_INSTALL = $(POLICY_CSPEC_DIR)/linux.xml

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

$(POLICY_OBJ_DIR)/linux: $(POLICY_OBJ_DIR)/bzImage
$(POLICY_OBJ_DIR)/bzImage: $(OBJ_DIR)/.built $(CSPEC_INSTALL)
	$(MULNXBZPATCH) src/arch/x86/boot/bzImage $@
	cp $(INITRAMFS) $(POLICY_OBJ_DIR)/$(INITRAMFS_FN)

$(CSPEC_INSTALL): $(CSPEC_XML)
	cp $< $@

clean:
	@rm -rf $(OBJ_DIR)
	@make clean -C src
