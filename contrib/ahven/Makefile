PV  = 2.4
PKG = ahven-$(PV)
ARC = $(PKG).tar.gz
SRC = http://sf.net/projects/ahven/files/ahven/Ahven%20$(PV)/$(ARC)/download
DLA = $(WRK).tar.gz

PATCHES = fix-warnings.patch

define CMD_DL
	@wget $(SRC) -O $@
endef

define CMD_INSTALL
	@$(MAKE) -C $(WRK) PREFIX=$(BUILDDIR) install_lib
endef

all: install_default

include ../contrib.archive.mk
