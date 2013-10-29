include ../../../Makeconf

all: $(OBJ_DIR)/initramfs.cpio

##### Busybox

BB_VERSION = 1.21.1
BB_BINARY  = $(SRC_DIR)/busybox-i686
#BB_BINARY  = $(OBJ_DIR)/busybox/busybox

.PHONY: $(OBJ_DIR)/busybox/busybox src-clean

$(SRC_DIR)/busybox-i686: $(SRC_DIR)/busybox-i686.sha1
	@wget \
	    http://busybox.net/downloads/binaries/$(BB_VERSION)/busybox-i686 \
	    -O $@
	@cd $(SRC_DIR) && sha1sum -c $< && touch $@ || (rm -f $@ && false)

$(SRC_DIR)/busybox-$(BB_VERSION).tar.bz2:
	@wget http://busybox.net/downloads/busybox-$(BB_VERSION).tar.bz2 -O $@

$(SRC_DIR)/busybox-$(BB_VERSION): $(SRC_DIR)/busybox-$(BB_VERSION).tar.bz2
	@cd $(SRC_DIR) &&               \
	  sha1sum -c $<.sha1 &&         \
	  bzcat $< | tar x

$(OBJ_DIR)/busybox/.configured: $(SRC_DIR)/busybox-$(BB_VERSION)
	@mkdir -p $(dir $@)
	@cd $(dir $@) &&                \
	  $(MAKE) CC=$(CC)              \
	          HOSTCC=$(HOSTCC)      \
	          KBUILD_SRC=$<         \
	          -f $</Makefile        \
	          defconfig
	# Enable static build,
	# Disable some not statically linkable stuff (to work with glibc)
	@sed -e 's,# CONFIG_STATIC is not set,CONFIG_STATIC=y,' \
	     -e 's,CONFIG_NSLOOKUP=y,# CONFIG_NSLOOKUP is not set,' \
	     -e 's,CONFIG_NETSTAT=y,# CONFIG_NETSTAT is not set,' \
	     -e 's,CONFIG_INETD=y,# CONFIG_INETD is not set,' \
	     -i $(dir $@)/.config
	@touch $@

$(OBJ_DIR)/busybox/busybox: $(OBJ_DIR)/busybox/.configured
	@cd $(dir $@) &&                        \
	  $(MAKE) CC=$(CC)                      \
	          CFLAGS="-m32 -march=i486"     \
	          LDFLAGS="-m32"                \
	          HOSTCC=$(HOSTCC)

src-clean:
	@rm -rf $(SRC_DIR)/busybox-$(BB_VERSION) \
	        $(SRC_DIR)/busybox-$(BB_VERSION).tar.bz2

##### Initramfs structure

DIRS		= bin dev
FILES		= bin/busybox bin/sh init
INITRAMFS_ROOT	= $(OBJ_DIR)/root
INITRAMFS_DIRS	= $(addprefix $(INITRAMFS_ROOT)/,$(DIRS))
INITRAMFS_FILES	= $(addprefix $(INITRAMFS_ROOT)/,$(FILES))

.PHONY: clean

$(INITRAMFS_DIRS):
	@install -d $@

$(INITRAMFS_FILES): $(INITRAMFS_DIRS)

$(INITRAMFS_ROOT)/bin/busybox: $(BB_BINARY)
	@install -m0755 $< $@

$(INITRAMFS_ROOT)/bin/sh:
	@ln -s busybox $@

$(INITRAMFS_ROOT)/init: $(SRC_DIR)/init
	@install -m0755 $< $@

$(OBJ_DIR)/initramfs.cpio: $(SRC_DIR)/initramfs.tmpl $(INITRAMFS_FILES)
	@cp $< $@.tmp
	@cd $(INITRAMFS_ROOT) && \
	  find . | cpio -o -A -H newc -O $@.tmp -R root.root
	@mv $@.tmp $@

clean:
	@rm -rf $(OBJ_DIR)
