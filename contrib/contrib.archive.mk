include ../contrib.mk

ifndef CMD_DL
define CMD_DL
	@cd $(TMP) && wget $(QUIET_OPT) -c $(SRC)
endef
endif

$(DLA):
	$(CMD_DL)

$(STAMP_UNPACK): $(DLA)
	@tar xfz $^ -C $(TMP)
	@touch $@

patch: $(STAMP_PATCH)
