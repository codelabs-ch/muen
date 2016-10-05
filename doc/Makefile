all: build_webpage

build_webpage:
	$(MAKE) -C webpage

build_report:
	$(MAKE) -C report

build_shmstream:
	$(MAKE) -C shmstream

build_toolchain:
	$(MAKE) -C toolchain

clean:
	$(MAKE) -C report clean
	$(MAKE) -C shmstream clean
	$(MAKE) -C toolchain clean
	$(MAKE) -C webpage clean
