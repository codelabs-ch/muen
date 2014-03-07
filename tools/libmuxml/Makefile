COMPONENT = libmuxml

SCHEMA_FILES = \
   obj/muxml-format_a_schema.ads \
   obj/muxml-format_b_schema.ads \
   obj/muxml-format_src_schema.ads

XSD_FILES   = $(wildcard schema/*.xsd)
RESOLVE_XSL = schema/resolve.xsl

COMPONENT_DEPS = $(SCHEMA_FILES)
TEST_DEPS      = $(COMPONENT_DEPS)

ADDITIONAL_CLEAN = $(SCHEMA_FILES)

include ../component.mk
include ../../Makecov

DUMMY := $(shell mkdir -p $(OBJ_DIR))

obj/muxml-format_%_schema.ads: $(XSD_FILES) $(RESOLVE_XSL)
	@xsltproc --stringparam format format_$* $(RESOLVE_XSL) schema/system.xsd | tidy -q -i -xml --hide-comments yes -o $(OBJ_DIR)/schema.pretty
	@echo -n > $@.tmp
	@echo -n '--  Auto-generated, '                             >> $@.tmp
	@date --iso=seconds                                         >> $@.tmp
	@echo 'package Muxml.Format_$*_Schema is'                   >> $@.tmp
	@echo '   XSD_Id : constant String := "schema/system.xsd";' >> $@.tmp
	@echo '   XSD    : constant String := ""'                   >> $@.tmp
	@sed -e 's/\t/ /g;s/"/""/g;s/^\(.*\)$$/\& "\1"/g' $(OBJ_DIR)/schema.pretty >> $@.tmp
	@echo '& "";'                                               >> $@.tmp
	@echo 'end Muxml.Format_$*_Schema;'                         >> $@.tmp
	@mv $@.tmp $@

build_cov: $(SCHEMA_FILES)
	@gprbuild $(BUILD_OPTS) -Ptest_libmuxml -XBUILD=coverage

.PHONY: all
