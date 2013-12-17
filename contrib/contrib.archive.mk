include ../contrib.mk

$(DLA):
	$(CMD_DL)

$(WRK): $(DLA)
	@tar xfz $^ -C $(TMP)
