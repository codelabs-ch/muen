include ../Makeconf

COMPONENTS ?=    \
	libmuchannel \
	libdebuglog  \
	libmutime    \
	crypter      \
	dbgserver    \
	idle         \
	linux        \
	sm           \
	tau0         \
	time         \
	vt

TESTS = libmutime

all: $(COMPONENTS)

$(COMPONENTS):
	@$(MAKE) -C $@
	@$(MAKE) -C $@ install

tests:
	@for prj in $(TESTS); do $(MAKE) $@ -C $$prj || exit 1; done

clean:
	@for prj in $(COMPONENTS); do $(MAKE) $@ -C $$prj || exit 1; done
	@rm -rf libraries

.NOTPARALLEL:

.PHONY: $(COMPONENTS)
