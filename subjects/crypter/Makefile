PACKAGE = crypter

include ../../Makeconf
include ../../Makespark

SPARK_FILES += $(TOP_DIR)/subjects/src/interrupts.ad*

SPARK_DIRS +=														\
	-directory=$(TOP_DIR)/subjects/src								\
	-directory=$(TOP_DIR)/contrib/libsparkcrypto/src/spark			\
	-directory=$(TOP_DIR)/contrib/libsparkcrypto/src/shared/generic	\
	-directory=$(TOP_DIR)/contrib/libsparkcrypto/src/shared/little_endian

all: $(ALL)

$(OBJ_DIR)/$(PACKAGE):
	gprbuild $(BUILD_OPTS) -P$(PACKAGE)

clean:
	rm -rf $(OBJ_DIR)
