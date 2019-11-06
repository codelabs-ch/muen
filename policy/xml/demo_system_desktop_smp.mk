DESK_POLICY = $(SYSTEM_DIR)/demo_system_desktop.xml
DESK_CPUS   = $(shell xmllint --xpath 'string(/hardware/processor/@cpuCores)' $(HARDWARE))

SYSTEM_DEPS  = $(wildcard $(SYSTEM_DIR)/smp/*)
SYSTEM_DEPS += $(DESK_POLICY)

SYSTEM_SCHED_PRESENT = true

$(SYSTEM): $(SYSTEM_DEPS)
	@$(E) policy "Create $(SYSTEM)" \
		"cd $(SYSTEM_DIR)/smp/ && ./smpify.py $(DESK_CPUS)"
