REV = gpl-2016
PKG = gnatcoll
SRC = https://github.com/AdaCore/gnatcoll.git

define CMD_CONFIGURE
	cd $(WRK) && ./configure_minimal --prefix=$(BUILDDIR)
endef

all: install_default

include ../contrib.git.mk
