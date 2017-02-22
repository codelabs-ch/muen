COMPONENT = dbgserver

GENERATE_CSPECS_FROM = $(CSPECS_DIR)/component_$(COMPONENT).xml
COMPONENT_TARGETS = $(GEN_DIR)/dbg-hardware.ads $(GEN_DIR)/config.xml

XSL_TFM_HW  = misc/hardware.xsl
XSL_TFM_CFG = misc/config.xsl

include ../component_ada.mk

$(GEN_DIR)/dbg-hardware.ads: $(XSL_TFM_HW) $(POLICY_B)
	xsltproc -o $@ --stringparam PACKAGENAME Dbg.Hardware $< $(POLICY_B)
$(GEN_DIR)/config.xml: $(XSL_TFM_CFG) $(POLICY_SRC)
	xsltproc -o $@ --stringparam COMPONENTNAME $(COMPONENT) $< $(POLICY_SRC)
