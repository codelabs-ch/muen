.global trampoline_start
.global trampoline_end

.section .trampoline, "a"

trampoline_start:
	cli
	hlt
trampoline_end:
