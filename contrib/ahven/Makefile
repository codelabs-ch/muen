PV  = 2.3
PKG = ahven-$(PV)
ARC = $(PKG).tar.gz
SRC = http://sf.net/projects/ahven/files/ahven/Ahven%20$(PV)/$(ARC)/download
TMP = $(CURDIR)/tmp
WRK = $(TMP)/$(PKG)
DLA = $(WRK).tar.gz

DUMMY := $(shell mkdir -p $(TMP))

STAMP_COMMON  = $(TMP)/.$(PKG)
STAMP_BUILD   = $(STAMP_COMMON)-build
STAMP_INSTALL = $(STAMP_COMMON)-install
STAMP_UNPACK  = $(STAMP_COMMON)-unpack

include ../Makeconf

all: $(STAMP_INSTALL)

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

clean:
	@rm -rf $(TMP)
