COMPONENT = dbgserver

GENERATE_CSPECS   = 1
COMPONENT_TARGETS = $(GEN_DIR)/dbg-hardware.ads

XSL_TFM = misc/hardware.xsl

include ../component_ada.mk

$(GEN_DIR)/dbg-hardware.ads: $(XSL_TFM) $(POLICY_B)
	xsltproc -o $@ --stringparam PACKAGENAME Dbg.Hardware $< $(POLICY_B)
