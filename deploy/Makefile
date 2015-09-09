PACK_DIR = $(CURDIR)/../pack/obj
HTTP_LOG = $(CURDIR)/http.log

PING_CMD  ?= ping -c 3
TARGET_IP ?= 192.168.254.2

HTTP_STATE = `cat $(HTTP_LOG)`

all: deploy

deploy: $(PACK_DIR)/muen.img .server
	$(PING_CMD) $(TARGET_IP) >/dev/null
	./start_target.sh $(TARGET_IP)

.server:
	(cd $(PACK_DIR) && python -m SimpleHTTPServer >$(HTTP_LOG) 2>&1 & echo "$$!" > $@)
	@sleep 1
	@if [ "$(HTTP_STATE)" != "" ]; then                                   \
		echo HTTP start failed, check '$(HTTP_LOG)'; rm .server | exit 1; \
	fi

clean:
	@kill `cat .server 2>/dev/null` 2>/dev/null || true
	@rm -f .server $(HTTP_LOG)
