include ../contrib.mk

ifndef CMD_DL
define CMD_DL
cd $(TMP) && wget $(QUIET_OPT) -c $(SRC)
endef
endif

ifndef CMD_UNPACK
define CMD_UNPACK
tar xfJ $(TMP)/$(DLA) -C $(TMP)
endef
endif

patch: $(STAMP_PATCH)
