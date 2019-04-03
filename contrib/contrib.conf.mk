BUILDDIR = $(CURDIR)/../build

TMP = $(CURDIR)/tmp
WRK = $(TMP)/$(PKG)

DUMMY := $(shell mkdir -p $(TMP))

STAMP_COMMON    = $(TMP)/.$(PKG)
STAMP_DOWNLOAD  = $(STAMP_COMMON)-download
STAMP_UNPACK    = $(STAMP_COMMON)-unpack
STAMP_PATCH     = $(STAMP_COMMON)-patch
STAMP_CONFIGURE = $(STAMP_COMMON)-configure
STAMP_BUILD     = $(STAMP_COMMON)-build
STAMP_INSTALL   = $(STAMP_COMMON)-install
