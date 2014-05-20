clean_cov:
	rm -f $(OBJ_DIR)/tests/*.gcda

cov: clean_cov build_cov
	$(OBJ_DIR)/tests/gnattest/harness/test_runner || true
	lcov -c -d $(OBJ_DIR)/tests --no-recursion -o $(OBJ_DIR)/cov.info
	lcov -e $(OBJ_DIR)/cov.info "$(SRC_DIR)/*.adb" -o $(OBJ_DIR)/cov.info
	genhtml --no-branch-coverage $(OBJ_DIR)/cov.info -o $(OBJ_DIR)/cov
