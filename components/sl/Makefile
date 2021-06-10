COMPONENT = sl

COMPONENT_TARGETS = $(CSPECS)
INSTALL_TARGETS = $(OBJ_DIR)/.adjust-cspec

CSPEC_SCRIPT = misc/adjust-xml-spec.py

# Required because unused subprogram LSC.AES.Print.Print_Round has dynamic
# stack, see also T578.
STACK_OPTS = --allow-dynamic

include ../component_spark.mk

$(OBJ_DIR)/.adjust-cspec: $(CSPEC_SCRIPT) $(CSPEC_INSTALL)
	@$(E) $(COMPONENT) "Adjusting component XML spec" "$< $(CSPEC_INSTALL) \
		$(CSPEC_INSTALL)"
	@touch $@
