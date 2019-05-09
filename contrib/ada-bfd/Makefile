REV = 235a23452f7c5104ce6270a84f9794f497d496aa
PKG = ada-bfd
SRC = https://github.com/stcarrez/ada-bfd.git

define CMD_CONFIGURE
	cd $(WRK) && ./configure --prefix=$(BUILDDIR)
endef

all: install_default

include ../contrib.git.mk
