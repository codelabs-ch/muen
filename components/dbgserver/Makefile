COMPONENT = dbgserver

CSPEC_TARGETS     = $(GEN_DIR)/config.xml $(GEN_DIR)/dbg_config.gpr $(GEN_DIR)/logchannels.xml
CSPEC_TARGETS    += $(GEN_DIR)/dbg-subject_list.ads
COMPONENT_TARGETS = cspecs
NO_PROOF          = true

XSL_TFM_COMMON_CFG = ../xslt/config.xsl
XSL_TFM_CFG        = misc/config.xsl
XSL_TFM_LOG        = misc/logchannels.xsl
XSL_TFM_GPR        = misc/gpr.xsl
TFM_NAMES          = misc/extract-subject-names.py

include ../component_spark.mk

$(GEN_DIR)/config.xml: $(XSL_TFM_CFG) $(XSL_TFM_COMMON_CFG) $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate config.xml" "xsltproc -o $@ --stringparam \
		COMPONENTNAME $(COMPONENT) $< $(POLICY_SRC)"
$(GEN_DIR)/logchannels.xml: $(XSL_TFM_LOG) $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate logchannels.xml" "xsltproc -o $@ \
		--stringparam COMPONENTNAME $(COMPONENT) $< $(POLICY_SRC)"
$(GEN_DIR)/dbg-subject_list.ads: $(TFM_NAMES) $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate `basename $@`" "$< --out $@ \
		--src_policy $(POLICY_SRC)"
$(GEN_DIR)/dbg_config.gpr: $(XSL_TFM_GPR) $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate dbg_config.gpr" "xsltproc -o $@ --stringparam \
		GPRNAME dbg_config $< $(POLICY_SRC)"
