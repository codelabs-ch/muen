COMPONENT = dbgserver

CSPEC_TARGETS     = $(GEN_DIR)/config.xml $(GEN_DIR)/logchannels.xml
COMPONENT_TARGETS = cspecs

XSL_TFM_CFG = misc/config.xsl
XSL_TFM_LOG = misc/logchannels.xsl

include ../component_ada.mk

$(GEN_DIR)/config.xml: $(XSL_TFM_CFG) $(POLICY_SRC)
	xsltproc -o $@ --stringparam COMPONENTNAME $(COMPONENT) $< $(POLICY_SRC)
$(GEN_DIR)/logchannels.xml: $(XSL_TFM_LOG) $(POLICY_SRC)
	xsltproc -o $@ --stringparam COMPONENTNAME $(COMPONENT) $< $(POLICY_SRC)
