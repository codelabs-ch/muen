PKG = gnatcoll
SRC = https://github.com/AdaCore/gnatcoll-core.git
# Current head of branch 20.2
REV = d71d4fce1438dd645c546892a8c8a6a5e65a856d

define CMD_CONFIGURE
$(MAKE) -C $(WRK) ENABLE_SHARED=no prefix=$(BUILDDIR) setup
endef

define CMD_INSTALL
$(MAKE) -C $(WRK) install
endef

all: install_default

include ../contrib.git.mk
