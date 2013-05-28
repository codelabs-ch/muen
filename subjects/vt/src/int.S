.global isr_handler_ptr

.text
handle_all:
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

	call dispatch_interrupt

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
	iretq

.data
isr_handler_ptr:
	.quad handle_all

