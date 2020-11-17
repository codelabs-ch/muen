COMPONENT = sl

COMPONENT_TARGETS = cspecs
INSTALL_TARGETS = $(OBJ_DIR)/.adjust-cspec

CSPEC_SCRIPT = misc/adjust-xml-spec.py

include ../component_spark.mk

$(OBJ_DIR)/.adjust-cspec: $(CSPEC_SCRIPT) $(CSPEC_INSTALL)
	@$(E) $(COMPONENT) "Adjusting component XML spec" "$< $(CSPEC_INSTALL) \
		$(CSPEC_INSTALL)"
	@touch $@
