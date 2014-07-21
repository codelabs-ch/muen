include ../../../Makeconf

FILE = initramfs.cpio.gz
SITE = http://codelabs-ch.github.io/buildroot-muen
URL  = $(SITE)/$(FILE)

all: $(OBJ_DIR)/$(FILE)

$(OBJ_DIR)/$(FILE):
	@mkdir -p $(OBJ_DIR)
	@wget $(URL) -O $@.tmp
	@mv $@.tmp $@

clean:
	@rm -rf $(OBJ_DIR)
