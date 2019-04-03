REV = f06f366
PKG = alog
SRC = https://git.codelabs.ch/git/alog.git

BUILD_OPTS = LIBRARY_KIND=static

define CMD_INSTALL
    gprinstall -p $(WRK)/alog.gpr -XLIBRARY_KIND=static -XALOG_VERSION= --prefix=$(BUILDDIR) --project-subdir=lib/gnat
endef

all: install_default

include ../contrib.git.mk
