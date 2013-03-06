OBJ_DIR = $(CURDIR)/obj

BUILD_OPTS = -p -j0 --RTS=zfp

all: subjects

subjects:
	gprbuild $(BUILD_OPTS) -Psubject0
	gprbuild $(BUILD_OPTS) -Psubject1
	objcopy -O binary $(OBJ_DIR)/subject_0.elf $(OBJ_DIR)/subject_0
	objcopy -O binary $(OBJ_DIR)/subject_1.elf $(OBJ_DIR)/subject_1

clean:
	rm -rf obj
