COMPONENT = mucfgexpand

DEPENDS =         \
	liballoc      \
	libmucfgcheck \
	libmucfgvcpu  \
	libpaging

include ../component.mk
include ../coverage.mk

XML_OBJ := $(wildcard obj/*.xml)
REFS    := $(subst .xml,.ref.xml,$(subst obj/,data/,$(XML_OBJ)))

update-refs: $(REFS)
data/%.ref.xml: obj/%.xml
	cp $< $@
