include ../../Makeconf

all: src/kernel

src/kernel:
	@make -j$(NUM_CPUS) -C src

clean:
	@make clean -C src
