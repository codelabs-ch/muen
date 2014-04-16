PV  = 1.00
PKG = xia-$(PV)
ARC = $(PKG).tgz
SRC = http://www.mckae.com/xia/xia-1.00.tgz
DLA = $(WRK).tgz

PATCHES = fix-warnings.patch

define CMD_CLEAN_EXTRA
	@rm -f src
endef

all: patch src

src:
	@ln -sf $(WRK) src

include ../contrib.archive.mk
