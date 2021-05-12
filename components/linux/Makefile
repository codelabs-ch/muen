include ../../Makeconf

KERNEL_VERSION ?= 5.4
LINUX_CONFIG   ?= config/linux64-$(KERNEL_VERSION)

INITRAMFS_BASE := initramfs.cpio.gz
INITRAMFS_REV  := 61fa58c582e58fd776cb45f9cdf01d22f5b7f91f
INITRAMFS      ?= $(OBJ_DIR)/$(INITRAMFS_BASE)
SITE           ?= https://github.com/codelabs-ch/buildroot-muen/raw/$(INITRAMFS_REV)/

SRC_BZIMAGE = src/arch/x86/boot/bzImage
GENCSPEC    = misc/linux-muen-gencspec.py
EXTRA_MODS  = $(notdir $(wildcard modules/*))

MODS_SRC     = $(wildcard modules/*/*.[ch])
MODS_BUILD   = $(EXTRA_MODS:%=$(OBJ_DIR)/.%-build)
MODS_CLEAN   = $(EXTRA_MODS:%=clean-%)
MODS_OBJ_DIR = $(OBJ_DIR)/modules
MODS_PATH    = $(realpath modules)

LINUX_MODS_DESTDIR ?=

MODS_INITRAMFS_BASE = modules_$(INITRAMFS_BASE)
MODS_INITRAMFS      = $(OBJ_DIR)/$(MODS_INITRAMFS_BASE)

INSTALL_MOD_OPTS  = INSTALL_MOD_STRIP=1
INSTALL_MOD_OPTS += INSTALL_MOD_PATH=$(MODS_OBJ_DIR)/$(LINUX_MODS_DESTDIR)

LINUX_FLAGS  = LOCALVERSION=
LINUX_FLAGS += MAKEFLAGS='$(filter-out s,$(MAKEFLAGS))'

LOG = $(OBJ_DIR)/log.obj

all: build $(INITRAMFS)

$(SRC_BZIMAGE): src/.config
	@$(E) linux Oldconfig "$(LINUX_FLAGS) $(MAKE) oldconfig -C src" $(LOG)
	@$(E) linux Build "$(LINUX_FLAGS) $(MAKE) -C src" $(LOG)

$(OBJ_DIR)/bzImage: $(SRC_BZIMAGE) | $(OBJ_DIR)
	@$(E) linux "Patch" "$(MULNXBZPATCH) $< $@" $(LOG)

$(OBJ_DIR)/.mods-build: $(SRC_BZIMAGE) | $(OBJ_DIR)
	@$(E) linux "Build kernel modules" \
		"$(LINUX_FLAGS) $(MAKE) -C src modules" $(LOG)
	@touch $@

$(MODS_BUILD): MOD_NAME = $(@:$(OBJ_DIR)/.%-build=%)
$(MODS_BUILD): MOD_PATH = $(MODS_PATH)/$(MOD_NAME)
$(MODS_BUILD): $(OBJ_DIR)/.mods-build $(MODS_SRC) | $(OBJ_DIR)
	@$(E) linux "Build module $(MOD_NAME)" \
		"$(LINUX_FLAGS) $(MAKE) M=$(MOD_PATH) -C src modules" $(LOG)
	@touch $@

$(OBJ_DIR)/.mods-install: $(OBJ_DIR)/.mods-build | $(OBJ_DIR)
	@$(E) linux "Install kernel modules" \
		"$(LINUX_FLAGS) $(MAKE) $(INSTALL_MOD_OPTS) -C src modules_install" $(LOG)
	@touch $@

# Serialize module installation since some depmod versions have issues with
# parallel invocation.
$(OBJ_DIR)/.mods-extra-install: $(OBJ_DIR)/.mods-install $(MODS_BUILD)
	$(foreach MOD,$(EXTRA_MODS),$(E) linux "Install module $(MOD)" \
		"$(LINUX_FLAGS) $(MAKE) M=$(MODS_PATH)/$(MOD) $(INSTALL_MOD_OPTS) -C src modules_install" \
		$(LOG);)
	@touch $@

$(MODS_CLEAN): MOD_PATH = $(MODS_PATH)/$(@:clean-%=%)
$(MODS_CLEAN):
	$(MAKE) M=$(MOD_PATH) -C src clean

$(MODS_INITRAMFS): $(OBJ_DIR)/.mods-extra-install
	@$(E) linux "Generate $(MODS_INITRAMFS_BASE)" \
		"cd $(MODS_OBJ_DIR) && find . | cpio -R +0:+0 -o -H newc --quiet | gzip -9 > $@" $(LOG)

src/.config: $(LINUX_CONFIG)
	cp $< $@

$(INITRAMFS): | $(OBJ_DIR)
	@$(E) linux "Download initramfs" "wget -P $(OBJ_DIR) -c $(SITE)/$(INITRAMFS_BASE)" $(LOG)
	@$(E) linux "Download initramfs.sha256" "wget -P $(OBJ_DIR) -c $(SITE)/$(INITRAMFS_BASE).sha256" $(LOG)
	@$(E) linux "Check initramfs.sha256" \
		"cd $(OBJ_DIR) && sha256sum -c $(INITRAMFS_BASE).sha256" $(LOG)

$(POLICY_OBJ_DIR) $(OBJ_DIR):
	mkdir -p $@

# use cspecs install mechanism
COMPONENT = linux
include ../cspecs.mk

$(GEN_DIR)/$(COMPONENT).xml: $(INITRAMFS) $(MODS_INITRAMFS) | $(GEN_DIR)
$(GEN_DIR)/$(COMPONENT).xml: spec/$(COMPONENT).xml $(OBJ_DIR)/bzImage $(POLICY_SRC)
	@$(E) linux "Generate XML spec" \
		"$(GENCSPEC) $(OBJ_DIR)/bzImage $< --out_spec $@ --src_policy $(POLICY_SRC)" $(LOG)

$(POLICY_OBJ_DIR)/linux: $(POLICY_OBJ_DIR)/bzImage $(POLICY_OBJ_DIR)/$(INITRAMFS_BASE)
$(POLICY_OBJ_DIR)/linux: $(POLICY_OBJ_DIR)/$(MODS_INITRAMFS_BASE)

$(POLICY_OBJ_DIR)/bzImage: $(OBJ_DIR)/bzImage $(CSPEC_INSTALL) | $(POLICY_OBJ_DIR)
	@$(E) linux "Install bzImage" "cp $< $@" $(LOG)
$(POLICY_OBJ_DIR)/$(INITRAMFS_BASE): $(INITRAMFS) | $(POLICY_OBJ_DIR)
	@$(E) linux "Install initramfs" "cp $< $@" $(LOG)
$(POLICY_OBJ_DIR)/$(MODS_INITRAMFS_BASE): $(MODS_INITRAMFS) | $(POLICY_OBJ_DIR)
	@$(E) linux "Install $(MODS_INITRAMFS_BASE)" "cp $< $@" $(LOG)

install_initramfs: $(POLICY_OBJ_DIR)/$(INITRAMFS_BASE)

prepare:

build: $(GEN_DIR)/$(COMPONENT).xml

check:

clean: $(MODS_CLEAN)
	rm -rf $(OBJ_DIR) $(GEN_DIR)
	$(MAKE) clean -C src
