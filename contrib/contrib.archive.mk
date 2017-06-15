include ../contrib.mk

ifndef CMD_DL
define CMD_DL
	@cd $(TMP) && wget $(QUIET_OPT) -c $(SRC)
endef
endif

$(STAMP_UNPACK): $(STAMP_DOWNLOAD)
	@tar xfz $(TMP)/$(DLA) -C $(TMP)
	@touch $@

patch: $(STAMP_PATCH)
