OBJ_DIR = $(CURDIR)/obj

BUILD_OPTS = -p -j0 --RTS=zfp

all: subjects

subjects:
	gprbuild $(BUILD_OPTS) -Psubject0
	gprbuild $(BUILD_OPTS) -Psubject1

clean:
	rm -rf obj
