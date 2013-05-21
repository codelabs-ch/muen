include ../../Makeconf

all: src/kernel

src/kernel: src/*.c src/*.h src/*.S
	@make -j$(NUM_CPUS) -C src

clean:
	@make clean -C src
