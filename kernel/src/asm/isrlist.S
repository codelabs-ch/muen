.data
.global isrlist
isrlist:

.text

.macro isr_entry
	.align 4, 0x90
1:	.data
	.quad 1b
	.previous
.endm

.macro exception_with_code vector
	isr_entry
	push $\vector
	jmp handle_all
.endm

.macro exception vector
	isr_entry
	push $0
	push $\vector
	jmp handle_all
.endm

handle_all:
	push %r15
	push %r14
	push %r13
	push %r12
	push %r11
	push %r10
	push %r9
	push %r8
	push %rbp
	push %rsi
	push %rdi
	push %rdx
	push %rcx
	push %rbx
	push %rax
	mov %rsp, %rdi

	call dispatch_interrupt
	iret

exception				0
exception				1
exception				2
exception				3
exception				4
exception				5
exception				6
exception				7
exception_with_code 	8
exception				9
exception_with_code		10
exception_with_code		11
exception_with_code		12
exception_with_code		13
exception_with_code		14
exception				15
exception				16
exception_with_code		17
exception				18
exception				19
