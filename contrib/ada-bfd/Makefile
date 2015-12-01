REV = d49e58181d2b1940332af211c93e91eca78cc8bb
PKG = ada-bfd
SRC = https://github.com/stcarrez/ada-bfd.git

define CMD_CONFIGURE
	cd $(WRK) && ./configure --prefix=$(BUILDDIR)
endef

all: install_default

include ../contrib.git.mk
