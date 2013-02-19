.global subject_main_pointer

.text

subject_main_pointer:
	.quad subject_main

subject_main:
	movl $0x07690748, 0xB80F0
	hlt
