PACK_DIR = $(CURDIR)/../pack/obj

TARGET_IP = 192.168.254.2

all: deploy

deploy: $(PACK_DIR)/muen.img .server
	echo y | amttool $(TARGET_IP) powerup
	echo y | amttool $(TARGET_IP) reset

.server:
	(cd $(PACK_DIR) && python -m SimpleHTTPServer >/dev/null 2>&1 & echo "$$!" > $@)

clean:
	@kill `cat .server 2>/dev/null` 2>/dev/null || true
	@rm -f .server
