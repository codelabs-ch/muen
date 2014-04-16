include ../contrib.mk

$(STAMP_UNPACK):
	@git clone $(SRC) $(WRK)
	@cd $(WRK) && git checkout $(REV)
	@touch $@
