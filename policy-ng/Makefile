POLICY   = xml/demo_system.xml
KNL_NAME = kernel

BINS =                                \
	$(TOP_DIR)/kernel/obj/kernel      \
	$(TOP_DIR)/subjects/tau0/obj/tau0 \
	$(TOP_DIR)/subjects/vt/obj/vt     \
	$(TOP_DIR)/subjects/time/obj/time \
	$(TOP_DIR)/subjects/sm/obj/sm     \
	$(TOP_DIR)/subjects/linux/src/arch/x86/boot/bzImage

GENS =            \
	$(MUACPIGEN)  \
	$(MUIOBMGEN)  \
	$(MUMSRBMGEN) \
	$(MUPTGEN)    \
	$(MUSPECGEN)  \
	$(MUZPGEN)

GEN_OPTS = -o $(OBJ_DIR) $(POLICY)

include ../Makeconf

all: clean $(OBJ_DIR) run

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

copy_bins:
	@for bin in $(BINS); do cp $$bin $(OBJ_DIR) || exit 1; done

run_gens:
	@for gen in $(GENS); do $$gen $(GEN_OPTS) || exit 1; done

build_knl:
	@rm -f ../policy/include/*
	@cp $(OBJ_DIR)/* ../policy/include/
	@make clean -C ../kernel
	@make -C ../kernel

build_subj:
	@make clean -C ../subjects/tau0
	@make -C ../subjects

build_image: run_gens build_knl build_subj copy_bins
	@$(MUPACK) -k $(KNL_NAME) -i $(OBJ_DIR) -o $(OBJ_DIR) $(POLICY)

run: build_image
	@IMAGE=$(OBJ_DIR)/$(KNL_NAME) make -C ../emulate

clean:
	@rm -rf $(OBJ_DIR)
