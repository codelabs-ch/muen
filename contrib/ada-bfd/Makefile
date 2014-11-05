PV  = 1.0.0
PKG = ada-bfd-$(PV)
ARC = $(PKG).tar.gz
SRC = http://ada-bfd.googlecode.com/files/$(ARC)
DLA = $(TMP)/$(ARC)

PATCHES = handle-missing-HAS_LOAD_PAGE.patch

define CMD_CONFIGURE
	cd $(WRK) && ./configure --prefix=$(BUILDDIR)
endef

all: install_default

include ../contrib.archive.mk
