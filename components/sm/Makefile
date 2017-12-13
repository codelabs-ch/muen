COMPONENT = sm

CSPEC_TARGETS     = $(GEN_DIR)/config.xml
COMPONENT_TARGETS = cspecs

XSL_TFM_CFG = misc/config.xsl

include ../component_spark.mk

$(GEN_DIR)/config.xml: $(XSL_TFM_CFG) $(POLICY_SRC)
	xsltproc -o $@ --stringparam COMPONENTNAME $(COMPONENT) $< $(POLICY_SRC)
