COMPONENT = isolation_tests

CSPEC_TARGETS     = $(GEN_DIR)/reference_values.ads
COMPONENT_TARGETS = $(CSPECS)

TFM_REF_VALS = misc/reference_values.py

NO_PROOF = true

include ../component_spark.mk

$(GEN_DIR)/reference_values.ads: $(TFM_REF_VALS) $(POLICY_SRC)
	@$(E) $(COMPONENT) "Generate `basename $@`" "$< --out $@ \
		--src_policy $(POLICY_SRC)"
