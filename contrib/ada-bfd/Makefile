PV  = 1.0.1
PKG = ada-bfd-$(PV)
ARC = $(PKG).tar.gz
SRC = http://download.vacs.fr/ada-bfd/$(ARC)
DLA = $(TMP)/$(ARC)

define CMD_CONFIGURE
	cd $(WRK) && ./configure --prefix=$(BUILDDIR)
endef

all: install_default

include ../contrib.archive.mk
