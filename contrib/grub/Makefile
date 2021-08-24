PV  = 2.06
PKG = grub-$(PV)
DLA = $(PKG).tar.xz
SRC = src/$(DLA)

PATCHES = \
	patches/0001-Implement-Command-Stream-Loader-CSL-module.patch \
	patches/0002-pgp-Provide-grub_verify_signature2-function.patch \
	patches/0003-include-grub-file.h-Add-GRUB_FILE_TYPE_SBS.patch \
	patches/0004-pgp-Do-not-verify-GRUB_FILE_TYPE_SBS-files.patch \
	patches/0005-grub-shell-Improve-mkrescue_args-handling.patch \
	patches/0006-Implement-Signed-Block-Stream-SBS-loader.patch

define CMD_DL
	cp $(SRC) $(TMP)
endef

ifeq (,$(MUEN_EFI))
GRUB_PLATFORM := pc
GRUB_TARGET   := i386
else
GRUB_PLATFORM := efi
GRUB_TARGET   := x86_64
endif

define CMD_CONFIGURE
	cd $(WRK) && ./autogen.sh && ./configure \
		--prefix=$(BUILDDIR)/$(GRUB_TARGET)-$(GRUB_PLATFORM) \
		--disable-device-mapper \
		--disable-grub-mount \
		--disable-grub-themes \
		--disable-grub-mkfont \
		--disable-grub-emu-pci \
		--disable-grub-emu-sdl \
		--disable-efiemu \
		--disable-nls \
		--disable-libzfs \
		--disable-liblzma \
		--disable-cache-stats \
		--with-platform=$(GRUB_PLATFORM) \
		--target=$(GRUB_TARGET)
endef

all: install_grub_platform

include ../contrib.archive.mk

install_grub_platform: $(TMP)/.$(GRUB_TARGET)-$(GRUB_PLATFORM)
$(TMP)/.$(GRUB_TARGET)-$(GRUB_PLATFORM):
	@if [ -d $(WRK) ]; then \
		$(E) $(PKG) Cleaning "$(MAKE) -C $(WRK) clean"; \
		touch $(STAMP_PATCH); \
	fi
	$(MAKE) install_default
	@touch $@
