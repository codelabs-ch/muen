PV  = 1.0.0
PKG = ada-bfd-$(PV)
ARC = $(PKG).tar.gz
SRC = http://ada-bfd.googlecode.com/files/$(ARC)
DLA = $(WRK).tar.gz

CONFIG_OPTS = \
	--prefix=$(BUILDDIR)

include ../Makeconf

$(DLA):
	@cd $(TMP) && wget $(SRC)

$(STAMP_UNPACK): $(DLA)
	@tar xfz $^ -C $(TMP)
	@touch $@

$(STAMP_CONFIGURE): $(STAMP_UNPACK)
	@cd $(WRK) && ./configure $(CONFIG_OPTS)
	@touch $@

$(STAMP_BUILD): $(STAMP_CONFIGURE)
	@$(MAKE) -C $(WRK)
	@touch $@

$(STAMP_INSTALL): $(STAMP_BUILD)
	@$(MAKE) -C $(WRK) install_static
	@touch $@
