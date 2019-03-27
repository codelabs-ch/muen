include ../../Makeconf

CONFIG ?= 4.18
CC     ?= /usr/bin/gcc
HOSTCC ?= $(CC)

INITRAMFS_BASE := initramfs.cpio.gz
INITRAMFS_URL  := $(INITRAMFS_BASE)-smp
INITRAMFS      ?= $(OBJ_DIR)/$(INITRAMFS_BASE)
SITE           ?= https://codelabs-ch.github.io/buildroot-muen

CSPEC_XML     = spec/linux.xml
CSPEC_INSTALL = $(POLICY_CSPEC_DIR)/linux.xml

DUMMY := $(shell mkdir -p $(OBJ_DIR) $(POLICY_OBJ_DIR))

all: $(OBJ_DIR)/.built $(INITRAMFS)

$(OBJ_DIR)/.built: src/.config
	$(MAKE) CC="$(CC)" LOCALVERSION= HOSTCC="$(HOSTCC)" oldconfig -C src
	$(MAKE) CC="$(CC)" LOCALVERSION= HOSTCC="$(HOSTCC)" -C src
	@touch $@

src/.config: config/linux64-$(CONFIG)
	cp $< $@

$(INITRAMFS):
	cd $(OBJ_DIR)
	wget -c $(SITE)/$(INITRAMFS_URL)-$(CONFIG)
	wget -c $(SITE)/$(INITRAMFS_URL)-$(CONFIG).sha256
	sha256sum -c $(INITRAMFS_URL)-$(CONFIG).sha256
	mv $(INITRAMFS_URL)-$(CONFIG) $@
	rm $(INITRAMFS_URL)-$(CONFIG).sha256

$(POLICY_OBJ_DIR)/linux: $(POLICY_OBJ_DIR)/bzImage install_initramfs
$(POLICY_OBJ_DIR)/bzImage: $(OBJ_DIR)/.built $(CSPEC_INSTALL)
	$(MULNXBZPATCH) src/arch/x86/boot/bzImage $@

install_initramfs: $(POLICY_OBJ_DIR)/$(INITRAMFS_BASE) $(INITRAMFS)
$(POLICY_OBJ_DIR)/$(INITRAMFS_BASE): $(INITRAMFS)
	cp $< $@

$(CSPEC_INSTALL): $(CSPEC_XML)
	cp $< $@

prepare:

check:

clean:
	@rm -rf $(OBJ_DIR)
	@$(MAKE) clean -C src
