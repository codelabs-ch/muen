# required for MAKEFLAGS silent option
include ../projects/exec.mk

RECIPES =     \
	ada-bfd   \
	alog      \
	gnatcoll  \
	grub      \
	libgpr    \
	libhwbase \
	lsc       \
	xia

RECIPES_DOWNLOAD = $(RECIPES:%=download-%)
RECIPES_CLEAN    = $(RECIPES:%=clean-%)

all: build_recipes

build_recipes: $(RECIPES)
$(RECIPES):
	$(MAKE) -C $@

gnatcoll: libgpr

download: $(RECIPES_DOWNLOAD)
$(RECIPES_DOWNLOAD):
	$(MAKE) -C $(@:download-%=%) download

clean: $(RECIPES_CLEAN)
	rm -rf build
$(RECIPES_CLEAN):
	$(MAKE) -C $(@:clean-%=%) clean

.PHONY: $(RECIPES)
