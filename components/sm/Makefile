COMPONENT = sm

CSPEC_TARGETS     = $(GEN_DIR)/config.xml $(GEN_DIR)/config.gpr
COMPONENT_TARGETS = cspecs

XSL_TFM_COMMON_CFG = ../xslt/config.xsl
XSL_TFM_COMMON_GPR = ../xslt/gpr_config.xsl
XSL_TFM_CFG        = misc/config.xsl
XSL_TFM_GPR        = misc/gpr.xsl

include ../component_spark.mk

$(GEN_DIR)/config.xml: $(XSL_TFM_CFG) $(XSL_TFM_COMMON_CFG) $(POLICY_SRC)
	xsltproc -o $@ --stringparam COMPONENTNAME $(COMPONENT) $< $(POLICY_SRC)
$(GEN_DIR)/config.gpr: $(XSL_TFM_GPR) $(XSL_TFM_COMMON_GPR) $(POLICY_SRC)
	xsltproc -o $@ --stringparam GPRNAME config $< $(POLICY_SRC)
