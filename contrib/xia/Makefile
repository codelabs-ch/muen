PV  = 1.00
PKG = xia-$(PV)
ARC = $(PKG).tgz
SRC = http://www.mckae.com/xia/xia-1.00.tgz
DLA = $(WRK).tgz

define CMD_CLEAN_EXTRA
	@rm -f src
endef

all: download src

src:
	@ln -sf $(WRK) src
	@patch -d $(WRK) -p1 < fix-warnings.patch

include ../contrib.archive.mk
