NUM_CPUS  := $(shell getconf _NPROCESSORS_ONLN)
BUILD_OPTS = -p -j$(NUM_CPUS) --RTS=zfp

TARGET_IP = 192.168.254.2

all: kernel

kernel:
	gprbuild $(BUILD_OPTS) -Pkernel

deploy: kernel .server
	echo y | amttool $(TARGET_IP) reset

.server:
	(cd obj && python -m SimpleHTTPServer >/dev/null 2>&1 & echo "$$!" > $@)

clean:
	rm -rf obj/*
	kill `cat .server 2>/dev/null` 2>/dev/null || true
	rm -f .server
