SRC_DIR = $(CURDIR)/src
OBJ_DIR = $(CURDIR)/obj
VC_DIR  = $(OBJ_DIR)/vc

VERSION_SPEC = src/sk-version.ads
VERSION      = $(GIT_REV)
GIT_REV      = $(shell git describe --always)

NUM_CPUS  := $(shell getconf _NPROCESSORS_ONLN)
BUILD_OPTS = -p -j$(NUM_CPUS) --RTS=zfp

TARGET_IP = 192.168.254.2

SPARK_OPTS = \
	-vcg                                 \
	-nosli                               \
	-nolistings                          \
	-warning_file=warnings.conf          \
	-error_explanations=first_occurrence \
	-brief                               \
	-rules=all

ALL = kernel

ifeq ($(NO_PROOF),)
ALL   += $(OBJ_DIR)/kernel.sum
DUMMY := $(shell mkdir -p $(VC_DIR))
ifeq ($(SPARK_DIR),)
$(error SPARK_DIR not set!)
endif
endif

all: $(ALL)

git-rev: FORCE
	@if [ -d .git ]; then \
		if [ -r $@ ]; then \
			if [ "$$(cat $@)" != "$(GIT_REV)" ]; then \
				echo $(GIT_REV) > $@; \
			fi; \
		else \
			echo $(GIT_REV) > $@; \
		fi \
	fi

$(VERSION_SPEC): git-rev
	@echo "package SK.Version is"                   > $@
	@echo "   Version_String : constant String :=" >> $@
	@echo "     \"$(VERSION)\";"                   >> $@
	@echo "end SK.Version;"                        >> $@

kernel: $(VERSION_SPEC)
	gprbuild $(BUILD_OPTS) -Pkernel

deploy: kernel .server
	echo y | amttool $(TARGET_IP) reset

.server:
	(cd obj && python -m SimpleHTTPServer >/dev/null 2>&1 & echo "$$!" > $@)

$(OBJ_DIR)/kernel.sum: $(OBJ_DIR)/kernel.rep
	pogs -d=$(VC_DIR) -o=$@
	tail -n13 $@ | head -n12

$(OBJ_DIR)/kernel.rep: $(OBJ_DIR)/kernel.idx $(OBJ_DIR)/target.cfg
	@spark $(SPARK_OPTS)                   \
		-report_file=$@                    \
		-index_file=$(OBJ_DIR)/kernel.idx  \
		-config_file=$(OBJ_DIR)/target.cfg \
		-output_directory=$(VC_DIR)        \
		src/*.adb
	(cd $(VC_DIR) && sparksimp -l)

$(OBJ_DIR)/kernel.idx: src/*.ad*
	@sparkmake $(SPARKMAKE_OPTS)     \
		-index=$(OBJ_DIR)/kernel.idx \
		-meta=$(OBJ_DIR)/kernel.smf  \
		-exclude=$(SRC_DIR)/debug/*  \
		-exclude=$(OBJ_DIR)/*

$(OBJ_DIR)/target.cfg: $(OBJ_DIR)/confgen
	$< > $@

$(OBJ_DIR)/confgen: $(SPARK_DIR)/lib/spark/confgen.adb
	LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gnatmake -D $(OBJ_DIR) -o $@ $<

clean:
	rm -rf obj/*
	kill `cat .server 2>/dev/null` 2>/dev/null || true
	rm -f .server

FORCE:
