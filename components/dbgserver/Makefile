COMPONENT = dbgserver

GENERATE_CSPECS_FROM = spec/$(COMPONENT).xml
CSPEC_TARGETS = $(GEN_DIR)/config.xml

XSL_TFM_CFG = misc/config.xsl

include ../component_ada.mk

$(GEN_DIR)/config.xml: $(XSL_TFM_CFG) $(POLICY_SRC)
	xsltproc -o $@ --stringparam COMPONENTNAME $(COMPONENT) $< $(POLICY_SRC)
