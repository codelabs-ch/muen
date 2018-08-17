REV = 227fc89c8827874a0ed76cb22b55a0977254d5bb
PKG = ada-bfd
SRC = https://github.com/stcarrez/ada-bfd.git

PATCHES = 0001-Adapt-to-2.29-API-change-of-disassembler-function.patch

define CMD_CONFIGURE
	cd $(WRK) && ./configure --prefix=$(BUILDDIR)
endef

all: install_default

include ../contrib.git.mk
