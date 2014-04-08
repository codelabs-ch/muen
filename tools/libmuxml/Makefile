COMPONENT = libmuxml

SCHEMA_FILES = \
   obj/muxml-system_a_schema.ads   \
   obj/muxml-system_b_schema.ads   \
   obj/muxml-system_src_schema.ads \
   obj/muxml-vcpu_profile_schema.ads
SCHEMA_FILE = schema/`echo $* | sed -e 's/_a//g;s/_b//g;s/_src//g'`.xsd

XSD_FILES   = $(wildcard schema/*.xsd)
RESOLVE_XSL = schema/resolve.xsl

COMPONENT_DEPS = $(SCHEMA_FILES)
TEST_DEPS      = $(COMPONENT_DEPS)
COV_DEPS       = $(COMPONENT_DEPS)

include ../library.mk
include ../../Makecov

DUMMY := $(shell mkdir -p $(OBJ_DIR))

obj/muxml-%_schema.ads: $(XSD_FILES) $(RESOLVE_XSL)
	@xsltproc --stringparam format $* $(RESOLVE_XSL) $(SCHEMA_FILE) > $@.tmp
	@scripts/xml2ada Muxml.$*_schema $@.tmp $@
