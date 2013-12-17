RECIPES =   \
	ada-bfd \
	ahven   \
	alog

all: build_recipes

build_recipes:
	$(foreach recipe,$(RECIPES),$(MAKE) -C $(recipe);)

clean:
	$(foreach recipe,$(RECIPES),$(MAKE) -C $(recipe) clean;)
	@rm -rf build
