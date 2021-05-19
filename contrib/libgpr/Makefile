PKG = libgpr
SRC = https://github.com/AdaCore/gprbuild.git
REV = a902fba777393e36f718f55e179f4c2322baee01

BUILD_OPTS = prefix=$(BUILDDIR) libgpr.build.static

define CMD_INSTALL
$(MAKE) -C $(WRK) prefix=$(BUILDDIR) libgpr.install.static
endef

all: install_default

include ../contrib.git.mk
