include ../contrib.mk

$(WRK):
	@git clone $(SRC) $@
	@cd $(WRK) && git checkout $(REV)
