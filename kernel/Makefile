NUM_CPUS := $(shell getconf _NPROCESSORS_ONLN)

BUILD_OPTS = -p -j$(NUM_CPUS) --RTS=zfp

all: kernel

kernel:
	gprbuild $(BUILD_OPTS) -P$@
	cp obj/$@ obj/kernel.elf
	objcopy -O binary obj/$@ obj/$@

clean:
	rm -rf obj
