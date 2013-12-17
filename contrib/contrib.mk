include ../Makeconf

ifndef CMD_DL
define CMD_DL
	@cd $(TMP) && wget $(SRC)
endef
endif

ifndef CMD_BUILD
define CMD_BUILD
	@$(MAKE) -C $(WRK) $(BUILD_OPTS)
endef
endif

ifndef CMD_INSTALL
define CMD_INSTALL
	@$(MAKE) -C $(WRK) $(INSTALL_OPTS) install
endef
endif

$(STAMP_CONFIGURE): $(WRK)
	$(CMD_CONFIGURE)
	@touch $@

ifdef CMD_CONFIGURE
$(STAMP_BUILD): $(STAMP_CONFIGURE)
else
$(STAMP_BUILD): $(WRK)
endif
	@$(CMD_BUILD)
	@touch $@

install_default: $(STAMP_INSTALL)
$(STAMP_INSTALL): $(STAMP_BUILD)
	$(CMD_INSTALL)
	@touch $@

clean:
	@rm -rf $(TMP)
	$(CMD_CLEAN_EXTRA)
