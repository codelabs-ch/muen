DIR := $(patsubst %/,%,$(dir $(realpath $(filter %exec.mk, $(MAKEFILE_LIST)))))

ifeq (,$(VERBOSE_OUTPUT))
	MAKEFLAGS += -s -Oline
	E := $(DIR)/execute
endif
