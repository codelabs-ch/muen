RECIPES =   \
	ada-bfd \
	ahven

all: build_recipes

build_recipes:
	$(foreach recipe,$(RECIPES),$(MAKE) -C $(recipe);)

clean:
	$(foreach recipe,$(RECIPES),$(MAKE) -C $(recipe) clean;)
	@rm -rf build
