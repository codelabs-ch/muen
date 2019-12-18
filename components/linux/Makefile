include ../../Makeconf

CONFIG ?= 4.18

INITRAMFS_BASE := initramfs.cpio.gz
INITRAMFS_URL  := $(INITRAMFS_BASE)-smp
INITRAMFS_REV  := d8c9f20b2a25b99980903ffab2412ebdeac590d9
INITRAMFS      ?= $(OBJ_DIR)/$(INITRAMFS_BASE)
SITE           ?= https://github.com/codelabs-ch/buildroot-muen/raw/$(INITRAMFS_REV)/

LOG = $(OBJ_DIR)/log.obj

all: $(OBJ_DIR)/.built $(INITRAMFS)

$(OBJ_DIR)/.built: src/.config | $(OBJ_DIR)
	@$(E) linux Oldconfig 'MAKEFLAGS="$(filter-out s,$(MAKEFLAGS))" $(MAKE) LOCALVERSION= oldconfig -C src' $(LOG)
	@$(E) linux Build 'MAKEFLAGS="$(filter-out s,$(MAKEFLAGS))" $(MAKE) LOCALVERSION= -C src' $(LOG)
	@touch $@

install_mods:
	make LOCALVERSION= -j$(NUM_CPUS) -C src modules
	make LOCALVERSION= -j$(NUM_CPUS) INSTALL_MOD_STRIP=1 INSTALL_MOD_PATH=$(OBJ_DIR) -C src modules_install

src/.config: config/linux64-$(CONFIG)
	cp $< $@

$(INITRAMFS): | $(OBJ_DIR)
	cd $(OBJ_DIR)
	@$(E) linux "Download initramfs" "wget -c $(SITE)/$(INITRAMFS_URL)-$(CONFIG)" $(LOG)
	@$(E) linux "Download initramfs.sha256" "wget -c $(SITE)/$(INITRAMFS_URL)-$(CONFIG).sha256" $(LOG)
	@$(E) linux "Check initramfs.sha256" "sha256sum -c $(INITRAMFS_URL)-$(CONFIG).sha256" $(LOG)
	mv $(INITRAMFS_URL)-$(CONFIG) $@
	rm $(INITRAMFS_URL)-$(CONFIG).sha256

$(POLICY_OBJ_DIR) $(OBJ_DIR):
	mkdir -p $@

# use cspecs install mechanism
COMPONENT = linux
include ../cspecs.mk

$(POLICY_OBJ_DIR)/linux: $(POLICY_OBJ_DIR)/bzImage install_initramfs
$(POLICY_OBJ_DIR)/bzImage: $(OBJ_DIR)/.built $(CSPEC_INSTALL) | $(POLICY_OBJ_DIR)
	@$(E) linux "Patch and install" "$(MULNXBZPATCH) src/arch/x86/boot/bzImage $@" $(LOG)

install_initramfs: $(POLICY_OBJ_DIR)/$(INITRAMFS_BASE) $(INITRAMFS)
$(POLICY_OBJ_DIR)/$(INITRAMFS_BASE): $(INITRAMFS) | $(POLICY_OBJ_DIR)
	@$(E) linux "Install initramfs" "cp $< $@" $(LOG)

prepare:

check:

clean:
	rm -rf $(OBJ_DIR)
	$(MAKE) clean -C src
