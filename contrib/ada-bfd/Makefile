PV  = 1.0.0
PKG = ada-bfd-$(PV)
ARC = $(PKG).tar.gz
SRC = http://ada-bfd.googlecode.com/files/$(ARC)
DLA = $(WRK).tar.gz

CONFIG_OPTS = \
	--prefix=$(BUILDDIR)

define CMD_CONFIGURE
	@cd $(WRK) && ./configure $(CONFIG_OPTS)
endef

include ../Makeconf
