GNAT_RELEASE := $(shell gnat --version | head -n1 | cut -d " " -f2 | tr A-Z a-z)
GNAT_NUMBER  := $(shell gnat --version | head -n1 | cut -d " " -f3 | tr A-Z a-z)

ifeq ($(GNAT_RELEASE), gpl)
	REV = gpl-$(GNAT_NUMBER)
else
	REV = 17
endif

PKG = gnatcoll
SRC = https://github.com/AdaCore/gnatcoll.git

define CMD_CONFIGURE
	cd $(WRK) && ./configure_minimal --prefix=$(BUILDDIR)
endef

all: install_default

include ../contrib.git.mk
