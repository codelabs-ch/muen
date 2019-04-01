DIR := $(patsubst %/,%,$(dir $(realpath $(filter %exec.mk, $(MAKEFILE_LIST)))))

E := $(DIR)/execute

BUILD_OUTPUT_SYNC ?= target

ifeq (,$(BUILD_OUTPUT_VERBOSE))
	MAKEFLAGS += -s
else
	MAKEFLAGS += -O$(BUILD_OUTPUT_SYNC)
endif
