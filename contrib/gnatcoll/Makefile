GNAT_RELEASE := $(shell gnat --version | head -n1 | cut -d " " -f2)

ifeq ($(GNAT_RELEASE), GPL)
	REV = gpl-2016
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
