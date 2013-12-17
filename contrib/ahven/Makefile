PV  = 2.3
PKG = ahven-$(PV)
ARC = $(PKG).tar.gz
SRC = http://sf.net/projects/ahven/files/ahven/Ahven%20$(PV)/$(ARC)/download
DLA = $(WRK).tar.gz

define CMD_DL
	@wget $(SRC) -O $@
endef

define CMD_INSTALL
	@$(MAKE) -C $(WRK) PREFIX=$(BUILDDIR) install_lib
endef

include ../Makeconf
