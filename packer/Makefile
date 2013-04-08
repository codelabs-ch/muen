KERN_DIR = $(CURDIR)/../kernel
SUBJ_DIR = $(CURDIR)/../subjects
PT_DIR   = $(CURDIR)/../policy/include

TARGET_IP = 192.168.254.2

include ../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

all: $(OBJ_DIR)/kernel

$(OBJ_DIR)/tau0: $(SUBJ_DIR)/tau0/obj/tau0
	objcopy -O binary $< $@

$(OBJ_DIR)/subject1: $(SUBJ_DIR)/test/obj/subject1
	objcopy -O binary $< $@

$(OBJ_DIR)/subject2: $(SUBJ_DIR)/test/obj/subject2
	objcopy -O binary $< $@

packer:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

$(OBJ_DIR)/kernel: packer
	$(OBJ_DIR)/packer $(KERN_DIR)/obj/kernel obj/kernel

deploy: $(OBJ_DIR)/kernel .server
	echo y | amttool $(TARGET_IP) powerup
	echo y | amttool $(TARGET_IP) reset

.server:
	(cd obj && python -m SimpleHTTPServer >/dev/null 2>&1 & echo "$$!" > $@)

tests: build_tests
	@$(OBJ_DIR)/test_runner

build_tests:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -Ppacker_tests

clean:
	rm -rf obj/*
	kill `cat .server 2>/dev/null` 2>/dev/null || true
	rm -f .server
