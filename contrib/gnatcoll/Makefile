PKG = gnatcoll
SRC = https://github.com/AdaCore/gnatcoll-core.git
REV = 70c851f7a25b1984ecd539fa152bd00f9eaf76f9

define CMD_CONFIGURE
$(MAKE) -C $(WRK) ENABLE_SHARED=no prefix=$(BUILDDIR) setup
endef

define CMD_INSTALL
$(MAKE) -C $(WRK) install
endef

all: install_default

include ../contrib.git.mk
