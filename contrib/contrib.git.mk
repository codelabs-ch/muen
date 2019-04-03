include ../contrib.mk

ifndef CMD_DL
define CMD_DL
git clone $(QUIET_OPT) $(SRC) $(WRK)
endef
endif

ifndef CMD_UNPACK
define CMD_UNPACK
cd $(WRK) && git checkout $(QUIET_OPT) $(REV)
endef
endif
