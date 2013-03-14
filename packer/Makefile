OBJ_DIR  = obj
KERN_DIR = $(CURDIR)/../kernel
SUBJ_DIR = $(CURDIR)/../subjects

DUMMY := $(shell mkdir -p $(OBJ_DIR))

TARGET_IP = 192.168.254.2

SUBJ_BINS = \
	$(OBJ_DIR)/subject_0 \
	$(OBJ_DIR)/subject_1

all: $(OBJ_DIR)/kernel

$(OBJ_DIR)/subject_0: $(SUBJ_DIR)/test/obj/subject_0
	objcopy -O binary $< $@

$(OBJ_DIR)/subject_1: $(SUBJ_DIR)/test/obj/subject_1
	objcopy -O binary $< $@

$(OBJ_DIR)/kernel: $(KERN_DIR)/obj/kernel $(SUBJ_BINS)
	objcopy --add-section .subject0=$(OBJ_DIR)/subject_0 $< $@ \
		--change-section-address .subject0=0x00240000          \
		--set-section-flags .subject0=alloc 2>/dev/null
	objcopy --add-section .subject1=$(OBJ_DIR)/subject_1 $@ \
		--change-section-address .subject1=0x00250000       \
		--set-section-flags .subject1=alloc 2>/dev/null
	objcopy -O binary $@ $@

deploy: $(OBJ_DIR)/kernel .server
	echo y | amttool $(TARGET_IP) reset

.server:
	(cd obj && python -m SimpleHTTPServer >/dev/null 2>&1 & echo "$$!" > $@)

clean:
	rm -rf obj/*
	kill `cat .server 2>/dev/null` 2>/dev/null || true
	rm -f .server
