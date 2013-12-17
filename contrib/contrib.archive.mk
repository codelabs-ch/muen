include ../contrib.mk

$(DLA):
	$(CMD_DL)

download: $(WRK)
$(WRK): $(DLA)
	@tar xfz $^ -C $(TMP)
