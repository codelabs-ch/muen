include ../contrib.conf.mk
include ../../projects/exec.mk

ifndef CMD_BUILD
define CMD_BUILD
$(MAKE) -C $(WRK) $(BUILD_OPTS)
endef
endif

ifndef CMD_INSTALL
define CMD_INSTALL
$(MAKE) -C $(WRK) $(INSTALL_OPTS) install
endef
endif

LOG := tmp/$(PKG).log

$(STAMP_PATCH): $(STAMP_UNPACK) $(PATCHES)
	@for p in $(PATCHES); do $(E) $(PKG) "Patch $$p" \
		"patch -d $(WRK) -p1 < $$p" $(LOG) || exit 1; done
	@touch $@

$(STAMP_UNPACK): $(STAMP_DOWNLOAD)
	@$(E) $(PKG) Unpack "$(CMD_UNPACK)" $(LOG)
	@touch $@

$(STAMP_DOWNLOAD):
	@$(E) $(PKG) Download "$(CMD_DL)" $(LOG)
	@touch $@

$(STAMP_CONFIGURE): $(STAMP_PATCH)
	@$(E) $(PKG) Configure "$(CMD_CONFIGURE)" $(LOG)
	@touch $@

ifdef CMD_CONFIGURE
$(STAMP_BUILD): $(STAMP_CONFIGURE)
else
$(STAMP_BUILD): $(STAMP_PATCH)
endif
	@$(E) $(PKG) Build "$(CMD_BUILD)" $(LOG)
	@touch $@

install_default: $(STAMP_INSTALL)
$(STAMP_INSTALL): $(STAMP_BUILD)
	@$(E) $(PKG) Install "$(CMD_INSTALL)" $(LOG)
	@touch $@

download: $(STAMP_DOWNLOAD)

clean:
	@rm -rf $(TMP)
	@$(CMD_CLEAN_EXTRA)
