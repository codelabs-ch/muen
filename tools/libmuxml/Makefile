COMPONENT = libmuxml

SCHEMA_FILES = \
   obj/muxml-format_a_schema.ads   \
   obj/muxml-format_b_schema.ads   \
   obj/muxml-format_src_schema.ads \
   obj/muxml-vcpu_profile_schema.ads

XSD_FILES   = $(wildcard schema/*.xsd)
RESOLVE_XSL = schema/resolve.xsl

COMPONENT_DEPS = $(SCHEMA_FILES)
TEST_DEPS      = $(COMPONENT_DEPS)
COV_DEPS       = $(COMPONENT_DEPS)

include ../library.mk
include ../../Makecov

DUMMY := $(shell mkdir -p $(OBJ_DIR))

obj/muxml-format_%_schema.ads: $(XSD_FILES) $(RESOLVE_XSL)
	@xsltproc --stringparam format format_$* $(RESOLVE_XSL) schema/system.xsd > $@.tmp
	@scripts/xml2ada Muxml.Format_$*_Schema $@.tmp $@

obj/muxml-vcpu_profile_schema.ads: $(XSD_FILES) $(RESOLVE_XSL)
	@xsltproc --stringparam format format_$* $(RESOLVE_XSL) schema/vcpu_profile.xsd > $@.tmp
	@scripts/xml2ada Muxml.VCPU_Profile_Schema $@.tmp $@

.PHONY: all
