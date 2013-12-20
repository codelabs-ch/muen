include ../../Makeconf

all: $(TOOL)

$(TOOL):
	@gprbuild $(BUILD_OPTS) -P$@

clean:
	@rm -rf bin obj
