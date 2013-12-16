PV  = 1.0.0
PKG = ada-bfd-$(PV)
ARC = $(PKG).tar.gz
SRC = http://ada-bfd.googlecode.com/files/$(ARC)
TMP = $(CURDIR)/tmp
WRK = $(TMP)/$(PKG)
DLA = $(WRK).tar.gz

DUMMY := $(shell mkdir -p $(TMP))

STAMP_COMMON    = $(TMP)/.$(PKG)
STAMP_BUILD     = $(STAMP_COMMON)-build
STAMP_INSTALL   = $(STAMP_COMMON)-install
STAMP_CONFIGURE = $(STAMP_COMMON)-configure
STAMP_UNPACK    = $(STAMP_COMMON)-unpack

CONFIG_OPTS = \
	--prefix=$(BUILDDIR)

include ../Makeconf

all: $(STAMP_INSTALL)

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

clean:
	@rm -rf $(TMP)
