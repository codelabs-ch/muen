.global trampoline_start
.global trampoline_end

trampoline_start:
	cli
	hlt
trampoline_end:
