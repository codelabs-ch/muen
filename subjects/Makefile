include ../Makeconf

SUBJECTS =  \
	crypter \
	idle    \
	linux   \
	sm      \
	tau0    \
	time    \
	vt      \
	xv6

all: $(SUBJECTS)

$(SUBJECTS):
	@$(MAKE) -C $@

clean:
	@for subj in $(SUBJECTS); do make clean -C $$subj; done

.PHONY: $(SUBJECTS)
