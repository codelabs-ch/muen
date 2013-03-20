SUBJECTS = \
	../subjects/tau0/obj/tau0 \
	../subjects/test/obj/subject_1 \
	../subjects/test/obj/subject_2

BUILD_OPTS = -p -j0

all: config_subjects

skc_tool:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

config_subjects: skc_tool $(SUBJECTS)
	obj/skconfig $(SUBJECTS)

clean:
	@rm -rf obj include/skc-*.ads
