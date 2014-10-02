include ../Makeconf

COMPONENTS = \
	linux    \
	sm       \
	tau0     \
	time     \
	vt

all: $(COMPONENTS) install

$(COMPONENTS):
	@$(MAKE) -C $@

install:
	@for component in $(COMPONENTS); do \
		$(MAKE) $@ -C $$component || exit 1; \
	done

clean:
	@for subj in $(COMPONENTS); do make clean -C $$subj; done

.PHONY: $(COMPONENTS)
