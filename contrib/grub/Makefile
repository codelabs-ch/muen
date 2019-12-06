PV  = 2.04
PKG = grub-$(PV)
DLA = $(PKG).tar.xz
SRC = src/$(DLA)

PATCHES = \
	patches/0001-Implement-Command-Stream-Loader-CSL-module.patch \
	patches/0002-Use-multiboot-module-to-compare-in-memory-images.patch

define CMD_DL
	cp $(SRC) $(TMP)
endef

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
		--disable-cache-stats
endef

all: install_default

include ../contrib.archive.mk
