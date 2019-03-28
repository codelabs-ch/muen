RECIPES =    \
	ada-bfd  \
	alog     \
	gnatcoll \
	libhwbase \
	lsc      \
	xia

MAKEFLAGS = -Otarget

RECIPES_DOWNLOAD = $(RECIPES:%=download-%)
RECIPES_CLEAN    = $(RECIPES:%=clean-%)

ifdef QUIET
VERBOSE = @
endif

all: build_recipes

build_recipes: $(RECIPES)
$(RECIPES):
	$(VERBOSE)$(MAKE) -C $@

download: $(RECIPES_DOWNLOAD)
$(RECIPES_DOWNLOAD):
	$(VERBOSE)$(MAKE) -C $(@:download-%=%) download

clean: $(RECIPES_CLEAN)
	$(VERBOSE)rm -rf build
$(RECIPES_CLEAN):
	$(VERBOSE)$(MAKE) -C $(@:clean-%=%) clean

.PHONY: $(RECIPES)
