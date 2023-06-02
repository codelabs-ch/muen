# required for MAKEFLAGS silent option
include ../build-cfg/mk/exec.mk

MUEN_CONTRIB_RECIPES ?= \
	ada-bfd   \
	alog      \
	grub      \
	libhwbase \
	lsc       \
	xia

RECIPES_DOWNLOAD = $(MUEN_CONTRIB_RECIPES:%=download-%)
RECIPES_CLEAN    = $(MUEN_CONTRIB_RECIPES:%=clean-%)

all: build_recipes

build_recipes: $(MUEN_CONTRIB_RECIPES)
$(MUEN_CONTRIB_RECIPES):
	$(MAKE) -C $@

gnatcoll: libgpr

download: $(RECIPES_DOWNLOAD)
$(RECIPES_DOWNLOAD):
	$(MAKE) -C $(@:download-%=%) download

clean: $(RECIPES_CLEAN)
	rm -rf build
$(RECIPES_CLEAN):
	$(MAKE) -C $(@:clean-%=%) clean

.PHONY: $(MUEN_CONTRIB_RECIPES)
