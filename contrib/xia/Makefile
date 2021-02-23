REV = 31a4d159f296109d1bd1f605b347b0509b8ef2cc
PKG = xia
SRC = https://github.com/codelabs-ch/$(PKG).git

NUM_CPUS = $(shell getconf _NPROCESSORS_ONLN)

BUILD_OPTS = prefix=$(BUILDDIR) GPR_INSTALL_SUBDIR=share/gpr NUM_CPUS=$(NUM_CPUS)
INSTALL_OPTS = $(BUILD_OPTS)

all: install

include ../contrib.git.mk

install: $(BUILDDIR)/share/gpr/xia.gpr

$(BUILDDIR)/share/gpr/xia.gpr: install_default
	cp $(BUILDDIR)/share/gpr/XIA.gpr $@
