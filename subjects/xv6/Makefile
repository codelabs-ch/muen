include ../../Makeconf

all: src/kernelmemfs

src/kernelmemfs: src/*.c src/*.h src/*.S
	@make -j$(NUM_CPUS) -C src kernelmemfs

clean:
	@make clean -C src
