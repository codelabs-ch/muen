PKG = libgpr
SRC = https://github.com/AdaCore/gprbuild.git
# Current head of branch 20.2
REV = de1c2905801bbb086ea572b1dc0b2270bf3f388a

BUILD_OPTS = prefix=$(BUILDDIR) libgpr.build.static

define CMD_INSTALL
$(MAKE) -C $(WRK) prefix=$(BUILDDIR) libgpr.install.static
endef

all: install_default

include ../contrib.git.mk
