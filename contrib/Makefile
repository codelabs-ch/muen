RECIPES =   \
	ada-bfd \
	alog    \
	lsc     \
	xia

all: build_recipes

build_recipes:
	$(foreach recipe,$(RECIPES),$(MAKE) -C $(recipe) || exit 1;)

clean:
	$(foreach recipe,$(RECIPES),$(MAKE) -C $(recipe) clean;)
	@rm -rf build
