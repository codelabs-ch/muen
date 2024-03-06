REV = 6091259ac0ef
PKG = grub
SRC = https://github.com/codelabs-ch/grub2.git

ifeq (,$(MUEN_EFI))
GRUB_PLATFORM := pc
GRUB_TARGET   := i386
else
GRUB_PLATFORM := efi
GRUB_TARGET   := x86_64
endif

define CMD_CONFIGURE
	cd $(WRK) && ./bootstrap && ./configure \
		--prefix=$(BUILDDIR)/$(GRUB_TARGET)-$(GRUB_PLATFORM) \
		--disable-werror \
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

include ../contrib.git.mk

install_grub_platform: $(TMP)/.$(GRUB_TARGET)-$(GRUB_PLATFORM)
$(TMP)/.$(GRUB_TARGET)-$(GRUB_PLATFORM):
	@if [ -d $(WRK) ]; then \
		$(E) $(PKG) Cleaning "$(MAKE) -C $(WRK) clean"; \
		touch $(STAMP_PATCH); \
	fi
	$(MAKE) install_default
	@touch $@
