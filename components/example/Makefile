COMPONENT = example

CSPEC_TARGETS     = spec/example.xml $(GEN_DIR)/example_config.gpr
COMPONENT_TARGETS = cspecs

XSL_TFM_GPR = misc/gpr.xsl

include ../component_spark.mk

spec/example.xml: spec/example.xsl $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate $@" "xsltproc -o $@ $< $(POLICY_SRC)"

$(GEN_DIR)/example_config.gpr: $(XSL_TFM_GPR) $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate example_config.gpr" "xsltproc -o $@ --stringparam \
		GPRNAME example_config $< $(POLICY_SRC)"
