COMPONENT = dbgserver

GENERATE_CSPECS_FROM = $(CSPECS_DIR)/component_$(COMPONENT).xml
COMPONENT_TARGETS = $(GEN_DIR)/dbg-hardware.ads

XSL_TFM = misc/hardware.xsl

include ../component_ada.mk

$(GEN_DIR)/dbg-hardware.ads: $(XSL_TFM) $(POLICY_B)
	xsltproc -o $@ --stringparam PACKAGENAME Dbg.Hardware $< $(POLICY_B)
