DIR := $(patsubst %/,%,$(dir $(realpath $(filter %exec.mk, $(MAKEFILE_LIST)))))

E := $(DIR)/execute

ifeq (,$(VERBOSE_OUTPUT))
	MAKEFLAGS += -s
else
	MAKEFLAGS += -Otarget
endif
