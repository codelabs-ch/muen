#include "policy.h"

.data
.global isrlist
isrlist:

.text

.macro save_regs
	push %r15
	push %r14
	push %r13
	push %r12
	push %r11
	push %r10
	push %r9
	push %r8
	push %rsi
	push %rdi
	push %rdx
	push %rcx
	push %rbx
	push %rax
.endm

.macro restore_regs
	pop %rax
	pop %rbx
	pop %rcx
	pop %rdx
	pop %rdi
	pop %rsi
	pop %r8
	pop %r9
	pop %r10
	pop %r11
	pop %r12
	pop %r13
	pop %r14
	pop %r15
.endm

.macro reserved_entry
	.data
	.quad 0
	.previous
.endm

.macro isr_entry
	.align 4, 0x90
1:	.data
	.quad 1b
	.previous
.endm

.macro interrupt vector
	isr_entry
	save_regs
	mov $\vector, %rdi
	call dispatch_interrupt
	restore_regs
	iretq
.endm

.rept 32
	reserved_entry
.endr

.set cur_vector, 32
// Add one entry per subject
.rept SUBJECT_COUNT
	interrupt cur_vector
.set cur_vector, cur_vector + 1
.endr
