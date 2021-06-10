COMPONENT = libmudebuglog

CSPEC_TARGETS     = $(GEN_DIR)/config.xml
COMPONENT_TARGETS = $(CSPECS)

XSL_TFM_COMMON_CFG = ../xslt/config.xsl
XSL_TFM_CFG        = misc/config.xsl

NO_PROOF = true

include ../library_spark.mk

$(GEN_DIR)/config.xml: $(XSL_TFM_CFG) $(XSL_TFM_COMMON_CFG) $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate config.xml" "xsltproc -o $@ $< $(POLICY_SRC)"
