include ../contrib.mk

ifndef CMD_DL
define CMD_DL
	@git clone $(QUIET_OPT) $(SRC) $(WRK)
endef
endif

$(STAMP_UNPACK): $(STAMP_DOWNLOAD)
	@cd $(WRK) && git checkout $(QUIET_OPT) $(REV)
	@touch $@
