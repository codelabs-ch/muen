GNAT_RELEASE := $(shell gnat --version 2>/dev/null | head -n1 | cut -d " " -f2 | tr A-Z a-z)
GNAT_NUMBER  := $(shell gnat --version 2>/dev/null | head -n1 | cut -d " " -f3 | tr A-Z a-z)

ifeq ($(GNAT_RELEASE), gpl)
	BUILD_OPTS = static
	REV = gpl-$(GNAT_NUMBER)
	DEF_CMD_CONFIGURE = cd $(WRK) && ./configure_minimal --prefix=$(BUILDDIR)
	DEF_CMD_INSTALL = @+$(MAKE) -C $(WRK) install_static
else
	REV = master
	DEF_CMD_CONFIGURE = @+$(MAKE) -C $(WRK) ENABLE_SHARED=no prefix=$(BUILDDIR) setup
	DEF_CMD_INSTALL = @+$(MAKE) -C $(WRK) install
endif

PKG = gnatcoll
SRC = https://github.com/codelabs-ch/gnatcoll.git

define CMD_CONFIGURE
	$(DEF_CMD_CONFIGURE)
endef

define CMD_INSTALL
	$(DEF_CMD_INSTALL)
endef

all: install_default

include ../contrib.git.mk
