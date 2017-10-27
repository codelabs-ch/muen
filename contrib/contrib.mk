include ../Makeconf

ifdef QUIET
BUILD_OPTS   := $(BUILD_OPTS) -s > /dev/null
INSTALL_OPTS := $(BUILD_OPTS) -s > /dev/null
QUIET_OPT    := -q
endif

ifndef CMD_BUILD
define CMD_BUILD
	@+$(MAKE) -C $(WRK) $(BUILD_OPTS)
endef
endif

ifndef CMD_INSTALL
define CMD_INSTALL
	@+$(MAKE) -C $(WRK) $(INSTALL_OPTS) install
endef
endif

$(STAMP_PATCH): $(STAMP_UNPACK) $(PATCHES)
	@for p in $(PATCHES); do patch -d $(WRK) -p1 < $$p || exit 1; done
	@touch $@

$(STAMP_UNPACK): $(STAMP_DOWNLOAD)
	$(CMD_UNPACK)
	@touch $@

$(STAMP_DOWNLOAD):
	$(CMD_DL)
	@touch $@

$(STAMP_CONFIGURE): $(STAMP_PATCH)
	@$(CMD_CONFIGURE) $(QUIET_OPT)
	@touch $@

ifdef CMD_CONFIGURE
$(STAMP_BUILD): $(STAMP_CONFIGURE)
else
$(STAMP_BUILD): $(STAMP_PATCH)
endif
	@$(CMD_BUILD)
	@touch $@

install_default: $(STAMP_INSTALL)
$(STAMP_INSTALL): $(STAMP_BUILD)
	$(CMD_INSTALL)
	@touch $@

download: $(STAMP_DOWNLOAD)

clean:
	@rm -rf $(TMP)
	$(CMD_CLEAN_EXTRA)
