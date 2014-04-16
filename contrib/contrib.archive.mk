include ../contrib.mk

$(DLA):
	$(CMD_DL)

$(STAMP_UNPACK): $(DLA)
	@tar xfz $^ -C $(TMP)
	@touch $@

patch: $(STAMP_PATCH)
