GNAT_RELEASE := $(shell gnat --version 2>/dev/null | head -n1 | cut -d " " -f2 | tr A-Z a-z)
GNAT_NUMBER  := $(shell gnat --version 2>/dev/null | head -n1 | cut -d " " -f3 | tr A-Z a-z)

ifeq ($(GNAT_RELEASE), gpl)
	REV = gpl-$(GNAT_NUMBER)
else ifeq ($(GNAT_RELEASE), pro)
	REV = 17
else
define CMD_UNPACK
	$(error Unsupported GNAT compiler)
endef
endif

PKG = gnatcoll
SRC = https://github.com/codelabs-ch/gnatcoll.git

BUILD_OPTS   = static
INSTALL_OPTS = install_static

define CMD_CONFIGURE
	cd $(WRK) && ./configure_minimal --prefix=$(BUILDDIR)
endef

define CMD_INSTALL
	@+$(MAKE) -C $(WRK) install_static
endef

all: install_default

include ../contrib.git.mk
