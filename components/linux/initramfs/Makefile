include ../../../Makeconf

SITE = http://codelabs-ch.github.io/buildroot-muen

FILES = \
	initramfs.cpio.gz \
	initramfs_integ.cpio.gz \
	initramfs_integ_kt.cpio.gz
WGETS = $(patsubst %.cpio.gz,$(OBJ_DIR)/%.cpio.gz,$(FILES))

OBJDIR := $(shell mkdir -p $(OBJ_DIR))

all: $(WGETS)

$(OBJ_DIR)/%.cpio.gz:
	@wget $(SITE)/$*.cpio.gz -O $@.tmp
	@mv $@.tmp $@

clean:
	@rm -rf $(OBJ_DIR)
