PV  = 2.3
PKG = ahven-$(PV)
ARC = $(PKG).tar.gz
SRC = http://sf.net/projects/ahven/files/ahven/Ahven%20$(PV)/$(ARC)/download
DLA = $(WRK).tar.gz

include ../Makeconf

$(DLA):
	@wget $(SRC) -O $@

$(STAMP_UNPACK): $(DLA)
	@tar xfz $^ -C $(TMP)
	@touch $@

$(STAMP_BUILD): $(STAMP_UNPACK)
	@$(MAKE) -C $(WRK)
	@touch $@

$(STAMP_INSTALL): $(STAMP_BUILD)
	@$(MAKE) -C $(WRK) PREFIX=$(BUILDDIR) install_lib
	@touch $@
