REV = d1a98fe
PKG = alog
SRC = https://git.codelabs.ch/git/alog.git

BUILD_OPTS = LIBRARY_KIND=static

define CMD_INSTALL
    @gprinstall -p $(WRK)/alog.gpr -XLIBRARY_KIND=static --prefix=$(BUILDDIR) --project-subdir=lib/gnat
endef

all: install_default

include ../contrib.git.mk
