PACK_DIR = $(CURDIR)/../pack/obj
HTTP_LOG = $(CURDIR)/http.log

TARGET_PWR_MECH ?= amt

TARGET_START    ?= target_start_$(TARGET_PWR_MECH)
TARGET_SHUTDOWN ?= target_shutdown_$(TARGET_PWR_MECH)

PING_CMD    ?= ping -c 3
DEPLOY_PORT ?= 8000
TARGET_IP   ?= 192.168.254.2

TARGET_EGNAME ?= eg1

HTTP_STATE = `cat $(HTTP_LOG)`

all: deploy

deploy: $(PACK_DIR)/muen.img .server $(TARGET_START)

shutdown: $(TARGET_SHUTDOWN)

.server:
	(cd $(PACK_DIR) && python -m SimpleHTTPServer $(DEPLOY_PORT) >$(HTTP_LOG) 2>&1 & echo "$$!" > $@)
	@sleep 1
	@if [ "$(HTTP_STATE)" != "" ]; then                                   \
		echo HTTP start failed, check '$(HTTP_LOG)'; rm .server | exit 1; \
	fi

clean:
	@kill `cat .server 2>/dev/null` 2>/dev/null || true
	@rm -f .server $(HTTP_LOG)

target_start_amt:
	$(PING_CMD) $(TARGET_IP) >/dev/null
	./start_shutdown_amtc.sh $(TARGET_IP) start

target_shutdown_amt:
	./start_shutdown_amtc.sh $(TARGET_IP) shutdown

target_start_egctl:
	./start_shutdown_egctl.sh $(TARGET_EGNAME) start

target_shutdown_egctl:
	./start_shutdown_egctl.sh $(TARGET_EGNAME) shutdown
