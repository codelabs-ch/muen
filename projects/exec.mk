DIR := $(patsubst %/,%,$(dir $(realpath $(filter %exec.mk, $(MAKEFILE_LIST)))))

E := $(DIR)/execute

ifeq (,$(BUILD_OUTPUT_VERBOSE))
	MAKEFLAGS += -s
else
	MAKEFLAGS += -Otarget
endif
