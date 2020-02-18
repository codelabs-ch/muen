PV  = 2.04
PKG = grub-$(PV)
DLA = $(PKG).tar.xz
SRC = src/$(DLA)

PATCHES = \
	patches/0001-Implement-Command-Stream-Loader-CSL-module.patch \
	patches/0002-Use-multiboot-module-to-compare-in-memory-images.patch \
	patches/0003-pgp-Provide-grub_verify_signature2-function.patch \
	patches/0004-include-grub-file.h-Add-GRUB_FILE_TYPE_SBS.patch \
	patches/0005-pgp-Do-not-verify-GRUB_FILE_TYPE_SBS-files.patch \
	patches/0006-grub-shell-Improve-mkrescue_args-handling.patch \
	patches/0007-Implement-Signed-Block-Stream-SBS-loader.patch

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
		--prefix=$(BUILDDIR) \
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

all: install_default

include ../contrib.archive.mk
