PACK_DIR = $(CURDIR)/../pack/obj

TARGET_IP = 192.168.254.2

TARGET_STATE = `amttool $(TARGET_IP) | sed -n 's/Powerstate: \s //p'`

all: deploy

deploy: $(PACK_DIR)/muen.img .server
	ping -c 3 $(TARGET_IP) >/dev/null
	@echo Target is in $(TARGET_STATE) state
	@if [ "$(TARGET_STATE)" = "S0" ]; then     \
		echo y | amttool $(TARGET_IP) reset;   \
	else                                       \
		echo y | amttool $(TARGET_IP) powerup; \
	fi

.server:
	(cd $(PACK_DIR) && python -m SimpleHTTPServer >/dev/null 2>&1 & echo "$$!" > $@)

clean:
	@kill `cat .server 2>/dev/null` 2>/dev/null || true
	@rm -f .server
