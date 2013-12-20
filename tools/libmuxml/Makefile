SCHEMA_FILE = muxml-schemas.ads
XSD_FILES   = $(wildcard schema/*.xsd)
RESOLVE_XSL = schema/resolve.xsl

all: schema-spec

include ../../Makeconf

DUMMY := $(shell mkdir -p $(OBJ_DIR))

schema-spec: src/$(SCHEMA_FILE)

src/$(SCHEMA_FILE): $(XSD_FILES) $(RESOLVE_XSL)
	@xsltproc $(RESOLVE_XSL) schema/system.xsd | tidy -q -i -xml --hide-comments yes -o $(OBJ_DIR)/schema.pretty
	@echo -n > $@
	@echo -n '--  Auto-generated, '                             >> $@
	@date --iso=seconds                                         >> $@
	@echo 'package Muxml.Schemas is'                            >> $@
	@echo '   XSD_Id : constant String := "schema/system.xsd";' >> $@
	@echo '   XSD    : constant String := ""'                   >> $@
	@sed -e 's/\t/ /g;s/"/""/g;s/^\(.*\)$$/\& "\1"/g' $(OBJ_DIR)/schema.pretty >> $@
	@echo '& "";'                                               >> $@
	@echo 'end Muxml.Schemas;'                                  >> $@

tests: test_libmuxml
	@obj/tests/test_runner

test_libmuxml: schema-spec
	@gprbuild $(BUILD_OPTS) -P$@

clean:
	@rm -rf lib obj
	@rm -f src/$(SCHEMA_FILE)
