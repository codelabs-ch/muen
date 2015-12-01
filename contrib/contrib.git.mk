include ../contrib.mk

$(STAMP_UNPACK):
	@git clone $(QUIET_OPT) $(SRC) $(WRK)
	@cd $(WRK) && git checkout $(QUIET_OPT) $(REV)
	@touch $@
