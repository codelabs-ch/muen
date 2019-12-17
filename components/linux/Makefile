include ../../Makeconf

KERNEL_VERSION ?= 4.18
LINUX_CONFIG   ?= config/linux64-$(KERNEL_VERSION)

INITRAMFS_BASE := initramfs.cpio.gz
INITRAMFS_URL  := $(INITRAMFS_BASE)-smp
INITRAMFS_REV  := d8c9f20b2a25b99980903ffab2412ebdeac590d9
INITRAMFS      ?= $(OBJ_DIR)/$(INITRAMFS_BASE)
SITE           ?= https://github.com/codelabs-ch/buildroot-muen/raw/$(INITRAMFS_REV)/

SRC_BZIMAGE = src/arch/x86/boot/bzImage
GENCSPEC    = misc/linux-muen-gencspec.py

LINUX_FLAGS  = LOCALVERSION=
LINUX_FLAGS += MAKEFLAGS="$(filter-out s,$(MAKEFLAGS))"

LOG = $(OBJ_DIR)/log.obj

all: build $(INITRAMFS)

$(SRC_BZIMAGE): src/.config
	@$(E) linux Oldconfig '$(LINUX_FLAGS) $(MAKE) oldconfig -C src' $(LOG)
	@$(E) linux Build '$(LINUX_FLAGS) $(MAKE) -C src' $(LOG)

$(OBJ_DIR)/bzImage: $(SRC_BZIMAGE) | $(OBJ_DIR)
	@$(E) linux "Patch" "$(MULNXBZPATCH) $< $@" $(LOG)

build_mods: src/.config
	@$(E) linux "Build kernel modules" \
		'$(LINUX_FLAGS) $(MAKE) -C src modules' $(LOG)

install_mods: build_mods | $(OBJ_DIR)
	@$(E) linux "Install kernel modules" \
		'$(LINUX_FLAGS) $(MAKE) INSTALL_MOD_STRIP=1 INSTALL_MOD_PATH=$(OBJ_DIR) -C src modules_install' $(LOG)

src/.config: $(LINUX_CONFIG)
	cp $< $@

$(INITRAMFS): | $(OBJ_DIR)
	cd $(OBJ_DIR)
	@$(E) linux "Download initramfs" "wget -c $(SITE)/$(INITRAMFS_URL)-$(KERNEL_VERSION)" $(LOG)
	@$(E) linux "Download initramfs.sha256" "wget -c $(SITE)/$(INITRAMFS_URL)-$(KERNEL_VERSION).sha256" $(LOG)
	@$(E) linux "Check initramfs.sha256" "sha256sum -c $(INITRAMFS_URL)-$(KERNEL_VERSION).sha256" $(LOG)
	mv $(INITRAMFS_URL)-$(KERNEL_VERSION) $@
	rm $(INITRAMFS_URL)-$(KERNEL_VERSION).sha256

$(POLICY_OBJ_DIR) $(OBJ_DIR):
	mkdir -p $@

# use cspecs install mechanism
COMPONENT = linux
include ../cspecs.mk

$(GEN_DIR)/$(COMPONENT).xml: spec/$(COMPONENT).xml $(OBJ_DIR)/bzImage | $(GEN_DIR)
	@$(E) linux "Generate XML spec" "$(GENCSPEC) $(OBJ_DIR)/bzImage $< --out_spec $@" $(LOG)

$(POLICY_OBJ_DIR)/linux: $(POLICY_OBJ_DIR)/bzImage $(POLICY_OBJ_DIR)/$(INITRAMFS_BASE)
$(POLICY_OBJ_DIR)/bzImage: $(OBJ_DIR)/bzImage $(CSPEC_INSTALL) | $(POLICY_OBJ_DIR)
	@$(E) linux "Install bzImage" "cp $< $@" $(LOG)
$(POLICY_OBJ_DIR)/$(INITRAMFS_BASE): $(INITRAMFS) | $(POLICY_OBJ_DIR)
	@$(E) linux "Install initramfs" "cp $< $@" $(LOG)

install_initramfs: $(POLICY_OBJ_DIR)/$(INITRAMFS_BASE)

prepare:

build: $(GEN_DIR)/$(COMPONENT).xml

check:

clean:
	rm -rf $(OBJ_DIR) $(GEN_DIR)
	$(MAKE) clean -C src
