KERN_DIR = $(CURDIR)/../kernel
SUBJ_DIR = $(CURDIR)/../subjects
PT_DIR   = $(CURDIR)/../policy/include

TARGET_IP = 192.168.254.2

include ../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

SUBJ_BINS = \
	$(OBJ_DIR)/tau0 \
	$(OBJ_DIR)/subject_1 \
	$(OBJ_DIR)/subject_2

all: $(OBJ_DIR)/kernel

$(OBJ_DIR)/tau0: $(SUBJ_DIR)/tau0/obj/tau0
	objcopy -O binary $< $@

$(OBJ_DIR)/subject_1: $(SUBJ_DIR)/test/obj/subject_1
	objcopy -O binary $< $@

$(OBJ_DIR)/subject_2: $(SUBJ_DIR)/test/obj/subject_2
	objcopy -O binary $< $@

$(OBJ_DIR)/kernel: $(KERN_DIR)/obj/kernel $(SUBJ_BINS)
	objcopy --add-section .tau0=$(OBJ_DIR)/tau0 $< $@          \
		--change-section-address .tau0=0x00240000              \
		--set-section-flags .tau0=alloc 2>/dev/null
	objcopy --add-section .tau0pt=$(PT_DIR)/tau0.pt $@         \
		--change-section-address .tau0pt=0x001f0000            \
		--set-section-flags .tau0pt=alloc 2>/dev/null
	objcopy --add-section .tau0iobm=$(PT_DIR)/tau0.iobm $@     \
		--change-section-address .tau0iobm=0x00244000          \
		--set-section-flags .tau0iobm=alloc 2>/dev/null
	objcopy --add-section .subject1=$(OBJ_DIR)/subject_1 $@    \
		--change-section-address .subject1=0x00250000          \
		--set-section-flags .subject1=alloc 2>/dev/null
	objcopy --add-section .subject1pt=$(PT_DIR)/subject1.pt $@ \
		--change-section-address .subject1pt=0x001f4000        \
		--set-section-flags .subject1pt=alloc 2>/dev/null
	objcopy --add-section .subject2=$(OBJ_DIR)/subject_2 $@    \
		--change-section-address .subject2=0x00260000          \
		--set-section-flags .subject2=alloc 2>/dev/null
	objcopy --add-section .subject2pt=$(PT_DIR)/subject2.pt $@ \
		--change-section-address .subject2pt=0x001f8000        \
		--set-section-flags .subject2pt=alloc 2>/dev/null
	objcopy -O binary $@ $@

deploy: $(OBJ_DIR)/kernel .server
	echo y | amttool $(TARGET_IP) reset

.server:
	(cd obj && python -m SimpleHTTPServer >/dev/null 2>&1 & echo "$$!" > $@)

clean:
	rm -rf obj/*
	kill `cat .server 2>/dev/null` 2>/dev/null || true
	rm -f .server
