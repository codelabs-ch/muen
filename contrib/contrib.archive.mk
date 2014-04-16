include ../contrib.mk

$(DLA):
	$(CMD_DL)

download: $(STAMP_UNPACK)
$(STAMP_UNPACK): $(DLA)
	@tar xfz $^ -C $(TMP)
	@touch $@
