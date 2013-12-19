include ../../Makeconf

tests: test_libmuxml
	@obj/tests/test_runner

test_libmuxml:
	@gprbuild $(BUILD_OPTS) -P$@

clean:
	@rm -rf lib obj
