REV = e666302
PKG = alog
SRC = https://git.codelabs.ch/git/alog.git

BUILD_OPTS = LIBRARY_KIND=static

define CMD_INSTALL
gprinstall -p $(WRK)/alog.gpr -XLIBRARY_KIND=static -XALOG_VERSION= --prefix=$(BUILDDIR) --project-subdir=share/gpr
endef

all: install_default

include ../contrib.git.mk
