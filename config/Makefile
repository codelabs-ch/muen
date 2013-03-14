SUBJECTS = \
	../subjects/test/obj/subject_0 \
	../subjects/test/obj/subject_1

BUILD_OPTS = -p -j0

all: config_subjects

skconfig:
	@LIBRARY_PATH=/usr/lib/x86_64-linux-gnu gprbuild $(BUILD_OPTS) -P$@

config_subjects: skconfig $(SUBJECTS)
	obj/skconfig $(SUBJECTS)

clean:
	@rm -rf obj subjects/config_subjects-bins.ads
