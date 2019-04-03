DIR := $(patsubst %/,%,$(dir $(realpath $(filter %exec.mk, $(MAKEFILE_LIST)))))

E := $(DIR)/execute

# GNU make output sync setting. Only relevant when running multiple jobs (make
# -j) in verbose output mode. Possible output sync granularity values: none,
#  line, target, recurse.
BUILD_OUTPUT_SYNC ?= target

ifeq (,$(BUILD_OUTPUT_VERBOSE))
	MAKEFLAGS += -s
else
	MAKEFLAGS += -O$(BUILD_OUTPUT_SYNC)
endif
