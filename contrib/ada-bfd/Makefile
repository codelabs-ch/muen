REV = bf2e3c1515ff50293ee40678304cabef7df919cb
PKG = ada-bfd
SRC = https://github.com/stcarrez/ada-bfd.git

BUILD_OPTS = -j1

define CMD_CONFIGURE
	cd $(WRK) && ./configure --prefix=$(BUILDDIR)
endef

all: install_default

include ../contrib.git.mk
