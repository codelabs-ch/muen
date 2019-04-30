REV = afcd3c0e6f25c7bec7fe8f240c8376eaa2125b63
PKG = ada-bfd
SRC = https://github.com/stcarrez/ada-bfd.git

define CMD_CONFIGURE
	cd $(WRK) && ./configure --prefix=$(BUILDDIR)
endef

all: install_default

include ../contrib.git.mk
