REV = 9a2d16944c02030bfd72805543cb48a38f3e8de2
PKG = ada-bfd
SRC = https://github.com/stcarrez/ada-bfd.git

BUILD_OPTS = -j1

PATCHES = \
	patches/0001-Add-liberty-to-LINKER_OPTIONS.patch

define CMD_CONFIGURE
	cd $(WRK) && ./configure --prefix=$(BUILDDIR)
endef

all: install_default

include ../contrib.git.mk
