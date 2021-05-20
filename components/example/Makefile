COMPONENT = example

CSPEC_TARGETS     = $(GEN_DIR)/example_config.gpr
COMPONENT_TARGETS = cspecs

XSL_TFM_GPR = misc/gpr.xsl

include ../component_spark.mk

$(GEN_DIR)/example_config.gpr: $(XSL_TFM_GPR) $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate example_config.gpr" "xsltproc -o $@ --stringparam \
		GPRNAME example_config $< $(POLICY_SRC)"
