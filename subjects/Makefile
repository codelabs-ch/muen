SRC_DIR = $(CURDIR)/src
OBJ_DIR = $(CURDIR)/obj

BUILD_OPTS = -p -j0 --RTS=zfp

all: subjects

subjects:
	gprbuild $(BUILD_OPTS) -Psubjects

clean:
	rm -rf obj
