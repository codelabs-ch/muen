include ../Makeconf

SUBJECTS =  \
	linux   \
	sm      \
	tau0    \
	time    \
	vt

all: $(SUBJECTS)

$(SUBJECTS):
	@$(MAKE) -C $@

clean:
	@for subj in $(SUBJECTS); do make clean -C $$subj; done

.PHONY: $(SUBJECTS)
