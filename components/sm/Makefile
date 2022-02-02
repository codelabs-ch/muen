COMPONENT = sm

CSPEC_TARGETS     = $(GEN_DIR)/config.xml $(GEN_DIR)/config.gpr $(GEN_DIR)/cpu_values-target.ads
COMPONENT_TARGETS = $(CSPECS)

XSL_TFM_COMMON_CFG = ../xslt/config.xsl
XSL_TFM_COMMON_GPR = ../xslt/gpr_config.xsl
XSL_TFM_CFG        = misc/config.xsl
XSL_TFM_GPR        = misc/gpr.xsl
TFM_CPU_VALS       = misc/cpu-values.py

include ../component_spark.mk

$(GEN_DIR)/config.xml: $(XSL_TFM_CFG) $(XSL_TFM_COMMON_CFG) $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate config.xml" \
		"xsltproc -o $@ --stringparam COMPONENTNAME $(COMPONENT) $< $(POLICY_SRC)"
$(GEN_DIR)/config.gpr: $(XSL_TFM_GPR) $(XSL_TFM_COMMON_GPR) $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate config.gpr" \
		"xsltproc -o $@ --stringparam GPRNAME config $< $(POLICY_SRC)"
$(GEN_DIR)/cpu_values-target.ads: $(TFM_CPU_VALS) $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate `basename $@`" "$< --out $@ \
		--src_policy $(POLICY_SRC)"
